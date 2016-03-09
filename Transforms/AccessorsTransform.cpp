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
typedef llvm::raw_string_ostream sstream;

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

	std::string exprGet(std::string prefix) const {
		sstream s(prefix);
		s << getter << "()";
		s.flush();

		return prefix;
	}

	std::string exprSet(std::string prefix, const std::string &value) const {
		sstream s(prefix);
		s << setter << "(" << value << ")";
		s.flush();

		return prefix;
	}

	std::string exprMod(std::string prefix, const std::string &op) const {
		return exprSet(prefix, exprGet(prefix) + op);
	}

	std::string exprModGet(std::string prefix, const std::string &op) const {
		std::string expr;
		sstream s(expr);

		s << "(" << exprMod(prefix, op) << ", " << exprGet(prefix) << ")";
		s.flush();

		return expr;
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

	std::string stringOf(const Expr *stmt) const {
		std::string str;
		sstream s(str);
		stmt->printPretty(s, nullptr, PrintingPolicy(ctx->getLangOpts()));
		s.flush();

		return str;
	}

	std::string accessPrefix(const MemberExpr *e) const {
		std::string prefix = stringOf(e->getBase());
		prefix += (e->isArrow()) ? "->" : ".";
		return prefix;
	}

	std::string assignmentOp(const BinaryOperator *op) const {
		if (op->isCompoundAssignmentOp()) {
			std::string opStr = " ";
			sstream s(opStr);
			s << op->getOpcodeStr(op->getOpForCompoundAssignment(op->getOpcode()))
			  << " "
			  << stringOf(op->getRHS());
			s.flush();

			return opStr;
		}
		return std::string();
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
			const Stmt *top_stmt_within_compound = getRootStmt(bin_op, PM);

			std::string prefix = accessPrefix(lhs_expr);

			string stmts_str;
			llvm::raw_string_ostream sstr(stmts_str);

			if(bin_op->isCompoundAssignmentOp())
			{

				if(bin_op!=top_stmt_within_compound)
				{
					// rewrite something like:
					// int z = (foo.x+=3);
					// to
					// foo.setX(foo.getX()+3);
					// int z = foo.getX();

					collect(bin_op->getRHS(), PM);
					replace(
						bin_op->getSourceRange(),
						it->second.exprModGet(prefix, assignmentOp(bin_op)));
				}
				else
				{
					// just a simple compound assignment, e.g.
					// foo.x+=3;
					// to
					// foo.setX(foo.getX() + 3);
					collect(bin_op->getRHS(), PM);
					replace(
						bin_op->getSourceRange(),
						it->second.exprMod(prefix, assignmentOp(bin_op)));
				}
			}
			else if(bin_op->getOpcode() == clang::BO_Assign)
			{
				collect(bin_op->getRHS(), PM);
				replace(
					bin_op->getSourceRange(),
					it->second.exprSet(prefix, stringOf(bin_op->getRHS())));
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
			const Stmt *top_stmt_within_compound = getRootStmt(un_op, PM);

			const Stmt *top_stmt_or_compound = top_stmt_within_compound;
			if(isa<CompoundStmt>(PM.getParent(top_stmt_within_compound)))
				top_stmt_or_compound = PM.getParent(top_stmt_within_compound);

			string base_str = accessPrefix(sub_expr);

			if(!un_op->isIncrementDecrementOp())
			{
				collect(un_op->getSubExpr(), PM);
			}
			else
			{
				std::string op(" + 1");
				if (un_op->isDecrementOp()) {
					op[1] = '-';
				}
				string incrStmt = it->second.exprMod(base_str, op);
				string getStmt = it->second.exprGet(base_str);

				bool onlyExpr = un_op == top_stmt_within_compound;

				const Stmt *parent = PM.getParent(top_stmt_or_compound);
				if( auto *if_stmt = dyn_cast<IfStmt>(parent))
				{
					if(if_stmt->getThen() == top_stmt_or_compound
					   || if_stmt->getElse() == top_stmt_or_compound)
					{
						expandUnary(un_op, top_stmt_within_compound, top_stmt_or_compound, getStmt, incrStmt);
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
				else if( auto *for_stmt = dyn_cast<ForStmt>(parent))
				{
					if(for_stmt->getBody() == top_stmt_or_compound)
					{
						expandUnary(un_op, top_stmt_within_compound, top_stmt_or_compound, getStmt, incrStmt);
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
				} else if (auto *while_stmt = dyn_cast<WhileStmt>(parent)) {
					if (while_stmt->getBody() == top_stmt_or_compound)
					{
						expandUnary(un_op, top_stmt_within_compound, top_stmt_or_compound, getStmt, incrStmt);
					}
				} else if (auto *do_stmt = dyn_cast<DoStmt>(parent)) {
					if (do_stmt->getBody() == top_stmt_or_compound)
					{
						expandUnary(un_op, top_stmt_within_compound, top_stmt_or_compound, getStmt, incrStmt);
					}
				}
				// TODO DoStmt, WhileStmt

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
			}
		}
	}


	void expandUnary(
		const UnaryOperator *op,
		const Stmt *entry,
		const Stmt *scope,
		const std::string &get,
		const std::string &set) {
		if (op == entry) { // unary operation as stmt
			replace(op->getSourceRange(), set);
		} else { // unary operation as expr in stmt
			expandUnary(op, get, set);
			// adding stmt require to make compound stmt, i.e. insert braces
			if (entry == scope) {
				insert(entry->getLocStart(), "{\n");
				insert(findLocAfterToken(scope->getLocEnd(), tok::semi), "}\n");
			}
		}
	}

	void expandUnary(
		const UnaryOperator *op,
		const std::string &get,
		const std::string &set) {
		replace(op->getSourceRange(), get);
		if(op->isPrefix()) {
			insert(op->getLocStart(), set + ";\n");
		} else {
			assert(op->isPostfix());
			auto stmtLoc = findLocAfterSemi(op->getLocEnd()).getLocWithOffset(-1);
			insert(stmtLoc, "\n" + set + ";\n");
		}
	}

	void rewrite(const MemberExpr *mem_expr, const ParentMap &PM) {
		auto it = fieldRanges.find(dyn_cast<FieldDecl>(mem_expr->getMemberDecl()));
		if (it != fieldRanges.end())
		{
			replace(
				mem_expr->getSourceRange(),
				it->second.exprGet(accessPrefix(mem_expr)));
		}
	}

	const Stmt *getRootStmt(const Stmt *s, const ParentMap &PM) {
		const Stmt *parent = PM.getParent(s);
		while (isa<Expr>(parent) || isa<DeclStmt>(parent)) {
			s = parent;
			parent = PM.getParent(parent);
		}
		return s;
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
		sstr << "\n";

		//const getter
		sstr << ctype << " &" << iter->second.getter << "() const { return " << varname << "; }\n";
		//non-const getter
		sstr << type << " &" << iter->second.getter << "()  { return " << varname << "; }\n";
		//setter
		sstr << "void " << iter->second.setter << "(" << ctype << " &x) { this->" << varname << " = x; }\n";

		SourceLocation insertLoc;
		for (const auto *method : parent->methods()) {
			if (method->isUserProvided()) {
				insertLoc = method->getSourceRange().getEnd().getLocWithOffset(1);
			}
		}
		if (insertLoc.isInvalid()) {
			insertLoc = parent->getRBraceLoc();
		}
		insert(insertLoc, sstr.str());
	}
}
};
REGISTER_TRANSFORM(AccessorsTransform);

