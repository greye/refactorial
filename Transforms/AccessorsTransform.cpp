#include "Transforms.h"

#include <clang/AST/AST.h>
#include <clang/AST/ParentMap.h>
#include <llvm/Support/raw_ostream.h>
#include <pcrecpp.h>

#include <cassert>
#include <cctype>
#include <cstdio>

using namespace clang;
using namespace clang::tooling;
using namespace std;

namespace {
class PatternTransform {
public:
    PatternTransform(pcrecpp::RE pattern, std::string matcher)
      : pattern(std::move(pattern)),
        matcher(std::move(matcher))
    {}

    PatternTransform(const PatternTransform &x)
      : pattern(x.pattern), matcher(x.matcher)
    {}

    PatternTransform(PatternTransform &&x)
      : pattern(std::move(x.pattern)),
        matcher(std::move(x.matcher))
    {}

    bool matches(const std::string &input, std::string *output) const {
        if (pattern.FullMatch(input)) {
            if (output) {
                pattern.Extract(matcher, input, output);
            }
            return true;
        }
        return false;
    }

private:
    pcrecpp::RE pattern;
    std::string matcher;
};

struct Accessors {
	std::string getter;
	std::string setter;

	enum {
		SNAKE_CASE = 0,
		LE_CAMEL_CASE,
		BE_CAMEL_CASE,
		INVALID_CASE,

		EMPTY_PREFIX = 0,
		DEFAULT_PREFIX,
		INVALID_PREFIX
	};

	bool hasGetter() const {
		return !getter.empty();
	}

	bool hasSetter() const {
		return !setter.empty();
	}

	static Accessors javaStyle(const std::string &member) {
		Accessors a;
		a.getter = accessor("get", member, LE_CAMEL_CASE, DEFAULT_PREFIX);
		a.setter = accessor("set", member, LE_CAMEL_CASE, DEFAULT_PREFIX);
		return a;
	}

	static std::string accessor(
		std::string prefix,
		std::string member,
		int caseStyle,
		int prefixStyle) {
		assert(caseStyle >= 0 && caseStyle < INVALID_CASE);
		assert(prefixStyle >=0 && prefixStyle < INVALID_PREFIX);

		if (caseStyle == SNAKE_CASE) {
			prefix += '_';
			makeLower(&member);
		} else {
			member.front() = std::toupper(member.front());
		}

		if (prefixStyle == EMPTY_PREFIX) {
			prefix.clear();
		}

		prefix += member;
		switch (caseStyle) {
		default: break;
		case LE_CAMEL_CASE:
			prefix.front() = std::tolower(prefix.front());
			break;

		case BE_CAMEL_CASE:
			prefix.front() = std::toupper(prefix.front());
			break;
		}
		return prefix;
	}

private:
	static void makeLower(std::string *s) {
		for (auto &c : *s) {
			c = std::tolower(c);
		}
	}
};
}

class AccessorsTransform : public Transform
{
private:
	std::map<const FieldDecl *, Accessors> fieldRanges;
	std::vector<PatternTransform> patterns;

	ASTContext *ctx;
public:
    AccessorsTransform() {
        const auto &accessors = TransformRegistry::get().config["Accessors"];
        assert(accessors.IsSequence() &&
               "'Accessors' section is expected to be sequence");

		fprintf(stderr, "AccessorsTransform() started...\n");
        for (auto it : accessors) {
			fprintf(stderr, "reading line...");
			fflush(stderr);
            if (it.IsMap()) {
                for (const auto &entry : it) {
                    auto pattern = pcrecpp::RE(entry.first.as<std::string>());
                    auto matcher = entry.second.as<std::string>();
                    patterns.push_back(PatternTransform(pattern, matcher));
                }
            } else if (it.IsScalar()) {
                auto fullName = it.as<std::string>();
                auto pattern = pcrecpp::RE(fullName);
                auto matcher = fullName.substr(fullName.find_last_of(':') + 1);

                patterns.push_back(PatternTransform(pattern, matcher));
            } else {
                assert(!"'Accessors' section allows maps and scalars only");
            }
			fprintf(stderr, "done.\n");
        }
		fprintf(stderr, "done.\n");
    }

	void HandleTranslationUnit(ASTContext &Ctx) {
		ctx = &Ctx;
		collect(ctx->getTranslationUnitDecl());
		insertAccessors();
	}
	void collect(const DeclContext *ns_decl) {
		for(auto subdecl = ns_decl->decls_begin(); subdecl != ns_decl->decls_end(); subdecl++) {
			/*
			 * Adding accessors requires a few things to work properly:
			 * First, the member variables must be made protected if not
			 * already protected or private. Second, the actual accessor
			 * methods must be declared and implemented. Finally, usage
			 * of the member variable must be replaced with usage of the
			 * accessors.
			 *
			 * There is one problem. Usage of a member variable allows
			 * non-const references and pointers to escape, which is
			 * potentially not thread-safe, even if this was not the
			 * intention of the programmer. To that effect, I am
			 * allowing references to escape, but displaying a warning
			 * that this may not be desired, and the action to take
			 * to manually fix this up if this is the case.
			 *
			 * This can not be automatically fixed up. We don't know
			 * to do in cases like these:
			 *
			 * Foo foo;
			 * int &z = foo.x;
			 *
			 * Since 'z' can now be either written to or read from,
			 * and statically tracking pointer origins is infeasible,
			 * we simply warn the user that a non-const reference has
			 * escaped.
			 */
			if(const CXXRecordDecl *rc_decl = dyn_cast<CXXRecordDecl>(*subdecl)) {
				for(const FieldDecl *member : rc_decl->fields()) {
					std::string fullName = member->getQualifiedNameAsString();
					std::string output;
					for(const auto &pattern : patterns) {
						if (pattern.matches(fullName, &output)) {
							break;
						}
					}
					if(!output.empty()) {
						fieldRanges[member] = Accessors::javaStyle(output);
					}
				}
			}
			if(const FunctionDecl *fn_decl = dyn_cast<FunctionDecl>(*subdecl)) {
				if(fn_decl->getNameAsString() != "main")
					continue;
				if(Stmt *body = fn_decl->getBody())
				{
					collect(body, ParentMap(body));
				}
			}
			//recurse into inner contexts
			if(const DeclContext *inner_dc_decl = dyn_cast<DeclContext>(*subdecl))
				collect(inner_dc_decl);
		}
	}
	void rewrite(const BinaryOperator *bin_op, const ParentMap &PM) {
		auto *lhs_expr = dyn_cast<MemberExpr>(bin_op->getLHS());
		if(!lhs_expr)
		{
			collect(bin_op->getLHS(), PM);
			collect(bin_op->getRHS(), PM);
			return;
		}
		auto it = fieldRanges.find(dyn_cast<FieldDecl>(lhs_expr->getMemberDecl()));
		if (it != fieldRanges.end())
		{
			const Stmt *top_stmt_within_compound = bin_op;
			while(isa<Expr>(PM.getParent(top_stmt_within_compound))
				  || isa<DeclStmt>(PM.getParent(top_stmt_within_compound)))
				top_stmt_within_compound = PM.getParent(top_stmt_within_compound);

			string stmts_str, base_str;
			llvm::raw_string_ostream sstr(stmts_str), base_sstr(base_str);
			lhs_expr->getBase()->printPretty(base_sstr, nullptr, PrintingPolicy(ctx->getLangOpts()));

			const std::string &getterName = it->second.getter;
			const std::string &setterName = it->second.setter;

			if(bin_op->isCompoundAssignmentOp())
			{

				if(bin_op!=top_stmt_within_compound)
				{
					// rewrite something like:
					// int z = (foo.x+=3);
					// to
					// foo.setX(foo.getX()+3);
					// int z = foo.getX();

					sstr << base_sstr.str() << "." << setterName << "( ";
					sstr << base_sstr.str() << "." << getterName << "() ";
					sstr << BinaryOperator::getOpcodeStr(BinaryOperator::getOpForCompoundAssignment(bin_op->getOpcode())) << " ";
					bin_op->getRHS()->printPretty(sstr, nullptr, PrintingPolicy(ctx->getLangOpts()));
					collect(bin_op->getRHS(), PM);
					sstr << " );\n";
					insert(top_stmt_within_compound->getLocStart(), sstr.str());
					replace(bin_op->getSourceRange(), base_sstr.str() + "." + getterName + "()");
				}
				else
				{
					// just a simple compound assignment, e.g.
					// foo.x+=3;
					// to
					// foo.setX(foo.getX() + 3);
					sstr << base_sstr.str() << "." << setterName << "( ";
					sstr << base_sstr.str() << "." << getterName << "() ";
					sstr << BinaryOperator::getOpcodeStr(BinaryOperator::getOpForCompoundAssignment(bin_op->getOpcode())) << " ";
					collect(bin_op->getRHS(), PM);
					bin_op->getRHS()->printPretty(sstr, nullptr, PrintingPolicy(ctx->getLangOpts()));
					sstr << " )";
					replace(bin_op->getSourceRange(), sstr.str());
				}
			}
			else if(bin_op->getOpcode() == clang::BO_Assign)
			{
				sstr << base_sstr.str() << "." << setterName << "( ";
				collect(bin_op->getRHS(), PM);
				// bin_op->getRHS()->printPretty(sstr, *ctx, 0, PrintingPolicy(ctx->getLangOpts()));
				sstr << " )";
				replace(bin_op->getSourceRange(), sstr.str());
			}
		}
	}
	void rewrite(const UnaryOperator *un_op, const ParentMap &PM) {
		MemberExpr *sub_expr;
		if(!(sub_expr = dyn_cast<MemberExpr>(un_op->getSubExpr())))
		{
			collect(un_op->getSubExpr(), PM);
			return;
		}
		auto it = fieldRanges.find(dyn_cast<FieldDecl>(sub_expr->getMemberDecl()));
		if (it != fieldRanges.end())
		{
			const Stmt *top_stmt_within_compound = un_op;
			while(isa<Expr>(PM.getParent(top_stmt_within_compound))
				  || isa<DeclStmt>(PM.getParent(top_stmt_within_compound)))
				top_stmt_within_compound = PM.getParent(top_stmt_within_compound);
			const Stmt *top_stmt_or_compound = top_stmt_within_compound;
			if(isa<CompoundStmt>(PM.getParent(top_stmt_within_compound)))
				top_stmt_or_compound = PM.getParent(top_stmt_within_compound);

			string base_str;
			llvm::raw_string_ostream base_sstr(base_str);
			sub_expr->getBase()->printPretty(base_sstr, nullptr, PrintingPolicy(ctx->getLangOpts()));
			base_str = base_sstr.str();

			const std::string &getterName = it->second.getter;
			const std::string &setterName = it->second.setter;

			if(!un_op->isIncrementDecrementOp())
			{
				collect(un_op->getSubExpr(), PM);
			}
			else
			{
				string incrStmt;
				{
					llvm::raw_string_ostream sstr(incrStmt);
					sstr << base_str << "." << setterName << "( ";
					sstr << base_str << "." << getterName << "() ";
					sstr << (un_op->isIncrementOp()?"+":"-") << " 1)";
					incrStmt = sstr.str();
				}

				string getStmt;
				{
					llvm::raw_string_ostream sstr(getStmt);
					sstr << base_str << "." << getterName << "()";
					getStmt = sstr.str();
				}

				bool onlyStmt = top_stmt_within_compound == top_stmt_or_compound;
				bool onlyExpr = un_op == top_stmt_within_compound;

				bool needToInsertBraces = false;
				if( const IfStmt *if_stmt = dyn_cast<IfStmt>(PM.getParent(top_stmt_or_compound)) )
				{
					if(if_stmt->getThen() == top_stmt_or_compound
					   || if_stmt->getElse() == top_stmt_or_compound)
					{
						if(onlyExpr)
							replace(un_op->getSourceRange(), incrStmt);
						else
						{
							replace(un_op->getSourceRange(), getStmt);
							if(un_op->isPrefix())
							{
								insert(un_op->getLocStart(), incrStmt + ";\n");
							}
							else
							{
								assert(un_op->isPostfix());
								insert(findLocAfterSemi(un_op->getLocEnd()), incrStmt + ";\n");
							}
							if(onlyStmt)
								needToInsertBraces = true;
						}
					}
					else
					{
						assert(top_stmt_within_compound == top_stmt_or_compound);
						assert(if_stmt->getCond() == top_stmt_within_compound);
						if(un_op->isPrefix())
						{
							insert(if_stmt->getLocStart(), incrStmt);
						}
						else
						{
							assert(un_op->isPostfix());
							insert(if_stmt->getLocEnd(), incrStmt);
						}
					}
				}
				else if( const ForStmt *for_stmt = dyn_cast<ForStmt>(PM.getParent(top_stmt_or_compound)) )
				{
					if(for_stmt->getBody() == top_stmt_or_compound)
					{
						if(onlyExpr)
							replace(un_op->getSourceRange(), incrStmt);
						else
						{
							replace(un_op->getSourceRange(), getStmt);
							if(un_op->isPrefix())
							{
								insert(un_op->getLocStart(), incrStmt);
							}
							else
							{
								assert(un_op->isPostfix());
								insert(un_op->getLocEnd(), incrStmt);
							}
							if(onlyStmt)
								needToInsertBraces = true;
						}
					}
					else if( for_stmt->getInit() == top_stmt_within_compound )
					{
						assert(onlyStmt); //no blocks in initializer
						if(onlyExpr)
							replace(un_op->getSourceRange(), incrStmt);
						else
						{
							insert(for_stmt->getLocStart(), incrStmt + ";\n");
							if(un_op->isPostfix())
								//not sure if this is legit, but i can't think of a better way
								replace(un_op->getSourceRange(), "(" + getStmt + " - 1)");
							else
								replace(un_op->getSourceRange(), getStmt );
						}
					}
					else if( for_stmt->getInc() == top_stmt_within_compound )
					{
						assert(onlyStmt); //no blocks in increment
						if(onlyExpr)
						{
							replace(top_stmt_within_compound->getSourceRange(), incrStmt);
						}
					}
					else
					{
						assert(onlyStmt); //no blocks in conditions
						assert(for_stmt->getCond() == top_stmt_within_compound);
						if(un_op->isPrefix())
						{
							//rewriter.InsertTextBefore(for_stmt->getLocStart(), incrStmt);
						}
						else
						{
							assert(un_op->isPostfix());
							//rewriter.InsertTextAfter(for_stmt->getLocEnd(), incrStmt);
						}
					}
				}
					/*
				bool needToInsertBraces =
					(
						(ifStmt = dyn_cast<IfStmt>(PM.getParent(top_stmt_within_compound))
						 && ((ifStmt->getThen() == top_stmt_within_compound)
							 || ifStmt->getElse() == top_stmt_within_compound))
						|| (forStmt = dyn_cast<ForStmt>(PM.getParent(top_stmt_within_compound))
							&& forStmt->getBody() == top_stmt_within_compound)
						|| (whileStmt = dyn_cast<WhileStmt>(PM
						)
					&& un_op!=top_stmt_within_compound;

				if(un_op==top_stmt_within_compound)
				{
					rewriter.ReplaceText(un_op->getSourceRange(), sstr.str());
				}
				else if(un_op->isPrefix())
				{
					rewriter.ReplaceText(un_op->getSourceRange(),
										 base_str + "." + getterName + "()");
					rewriter.InsertTextBefore(top_stmt_within_compound->getLocStart(),
											  sstr.str() + ";\n");
				}
				else
				{
					assert(un_op->isPostfix());
					rewriter.ReplaceText(un_op->getSourceRange(),
										 base_sstr.str() + "." + getterName + "()");
					rewriter.InsertTextAfterToken(top_stmt_within_compound->getLocEnd(),
												  ";\n" + sstr.str());
												  }*/
				if(needToInsertBraces)
				{
					insert(top_stmt_within_compound->getLocStart(), "{\n");
					SourceLocation locAfterSemi = findLocAfterToken(top_stmt_or_compound->getLocEnd(), tok::semi);
					insert(locAfterSemi, "}\n");
				}
			}
		}
	}
	void rewrite(const MemberExpr *mem_expr, const ParentMap &PM) {
		auto it = fieldRanges.find(dyn_cast<FieldDecl>(mem_expr->getMemberDecl()));
		if (it != fieldRanges.end())
		{
			const Stmt *top_stmt_within_compound = mem_expr;
			while(isa<Expr>(PM.getParent(top_stmt_within_compound))
				  || isa<DeclStmt>(PM.getParent(top_stmt_within_compound)))
				top_stmt_within_compound = PM.getParent(top_stmt_within_compound);

			string stmts_str, base_str;
			llvm::raw_string_ostream sstr(stmts_str), base_sstr(base_str);
			mem_expr->getBase()->printPretty(base_sstr, nullptr, PrintingPolicy(ctx->getLangOpts()));

			sstr << base_sstr.str() << "." << it->second.getter << "()";
			replace(mem_expr->getSourceRange(), sstr.str());
		}
	}
void collect(const Stmt *stmt, const ParentMap &PM) {
	if(const BinaryOperator *bin_op = dyn_cast<BinaryOperator>(stmt))
		rewrite(bin_op, PM);
	else if(const UnaryOperator *un_op = dyn_cast<UnaryOperator>(stmt))
		rewrite(un_op, PM);
	else if(const MemberExpr *mem_expr = dyn_cast<MemberExpr>(stmt))
	{
		rewrite(mem_expr, PM);
	}
	else
	{
		for(auto child = stmt->child_begin();
		    child != stmt->child_end();
		    ++child)
		{
			//I don't currently understand why, but sometimes, a child
			// statement can be null. I've seen it happen in an IfStmt.
			if(*child)
				collect(*child, PM);
		}
	}
}
void insertAccessors() {
	for(auto iter = fieldRanges.begin(); iter != fieldRanges.end(); ++iter)
	{
		const FieldDecl *field = iter->first;
		const CXXRecordDecl *parent = dyn_cast<CXXRecordDecl>(field->getParent());
		string varname = field->getNameAsString();

		const auto &fieldType = field->getType().getNonReferenceType();
		string ctype = fieldType.withConst().getAsString();
		string type = fieldType.getAsString();

		stringstream sstr;
		//const getter
		sstr << ctype << " &" << iter->second.getter << "() const { return " << varname << "; }\n";
		//non-const getter
		sstr << type << " &" << iter->second.getter << "()  { return " << varname << "; }\n";
		//setter
		sstr << "void " << iter->second.setter << "(" << ctype << " &x) { this->" << varname << " = x; }\n";

		bool hasUserDefinedMethods = false;
		for(auto iter = parent->method_begin(); iter != parent->method_end(); ++iter)
			if(iter->isUserProvided())
				hasUserDefinedMethods = true;
		if(!hasUserDefinedMethods)
		{
			insert(parent->getRBraceLoc(), sstr.str());
		}
		else
		{
			SourceLocation loc;
			CXXRecordDecl::method_iterator lastMethod = parent->method_begin();
			CXXRecordDecl::method_iterator check = lastMethod;
			do {
				if(!lastMethod->isUserProvided())
				{
					++check;
					++lastMethod;
					continue;
				}
				loc = lastMethod->getSourceRange().getEnd();
				++check;
				if(check==parent->method_end())
					break;
				++lastMethod;
			} while(1);
			insert(loc, sstr.str());
		}
	}
}
};
REGISTER_TRANSFORM(AccessorsTransform);

