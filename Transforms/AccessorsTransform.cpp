#include "Transforms.h"

#include <clang/AST/AST.h>
#include <clang/AST/ParentMap.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
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

	static Accessors cxxStyle(const std::string &member) {
		Accessors a;
		a.getter = accessor("get", member, LE_CAMEL_CASE, EMPTY_PREFIX);
		a.setter = accessor("set", member, LE_CAMEL_CASE, EMPTY_PREFIX);
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

static StringRef
getLineIndentation(SourceLocation Loc, SourceManager *SourceMgr) {
  std::pair<FileID, unsigned> DL = SourceMgr->getDecomposedLoc(Loc);
  FileID FID = DL.first;
  unsigned StartOffs = DL.second;

  StringRef MB = SourceMgr->getBufferData(FID);

  unsigned lineNo = SourceMgr->getLineNumber(FID, StartOffs) - 1;
  const SrcMgr::ContentCache *
      Content = SourceMgr->getSLocEntry(FID).getFile().getContentCache();
  unsigned lineOffs = Content->SourceLineCache[lineNo];

  // Find the whitespace at the start of the line.
  unsigned i = lineOffs;
  while (isWhitespace(MB[i]))
    ++i;
  return MB.substr(lineOffs, i-lineOffs);
}

class AccessorsTransform :
	public Transform,
	public RecursiveASTVisitor<AccessorsTransform> {

private:
	std::map<const FieldDecl *, Accessors> accessors;
	ParentMap stmtGraph;
	llvm::DenseSet<const RecordDecl *> records;
	llvm::DenseSet<const MemberExpr *> observed;

	std::vector<PatternTransform> patterns;
	ASTContext *ctx;

public:
    AccessorsTransform() : stmtGraph(nullptr) {
        const auto &accessors = TransformRegistry::get().config["Accessors"];
        assert(accessors.IsSequence() &&
               "'Accessors' section is expected to be sequence");

        for (auto it : accessors) {
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
        }
    }

	void HandleTranslationUnit(ASTContext &ctx) override {
		this->ctx = &ctx;
		initStmtGraph(nullptr);

		this->TraverseDecl(ctx.getTranslationUnitDecl());

		insertAccessors();
		accessors.clear();
		records.clear();
		observed.clear();
	}

	bool VisitRecordDecl(const RecordDecl *decl) {
		if (records.find(decl) != records.end()) {
			return true;
		}
		for (const FieldDecl *member : decl->fields()) {
			std::string fullName = member->getQualifiedNameAsString();
			std::string output;
			for (const auto &pattern : patterns) {
				if (pattern.matches(fullName, &output)) {
					break;
				}
			}
			if(!output.empty()) {
				accessors[member] = Accessors::javaStyle(output);
			}
		}
		records.insert(decl);

		for (const Decl *child : decl->decls()) {
			if (auto *subrecord = dyn_cast<RecordDecl>(child)) {
				VisitRecordDecl(subrecord);
			}
		}
		return true;
	}

	bool VisitStmt(Stmt *s) {
		if (!stmtGraph.hasParent(s)) {
			initStmtGraph(s);
		}
		return true;
	}

	bool VisitUnaryOperator(const UnaryOperator *op) {
		if (!op->isIncrementDecrementOp()) {
			return true;
		}
		// TODO warn about &-op
		if (auto rv = findAccessors(op->getSubExpr())) {
			const char *opStr = (op->isIncrementOp()) ? " + 1" : " - 1";
			const Stmt *outmost = getOutmostStmt(op);
			if (outmost == op) {
				replace(
					op->getSourceRange(),
					rv.methods->exprMod(accessPrefix(rv.expr), opStr));
				observed.insert(rv.expr);
			} else if (op->isPrefix()) {
				replace(
					op->getSourceRange(),
					rv.methods->exprModGet(accessPrefix(rv.expr), opStr));
				observed.insert(rv.expr);
			} else {
				llvm::errs()
				<< "Skipping unary post-operator, replacing with "
				<< rv.methods->exprGet(accessPrefix(rv.expr))
				<< op->getOpcodeStr(op->getOpcode())
				<< "\n";
			}
		}
		return true;
	}

	bool VisitBinaryOperator(const BinaryOperator *op) {
		if (!op->isAssignmentOp()) {
			return true;
		}
		if (auto rv = findAccessors(op->getLHS())) {
			const Stmt *outmost = getOutmostStmt(op);
			std::string obj = accessPrefix(rv.expr);

			std::string prefix;
			{
				sstream s(prefix);
				if (op != outmost) {
					s << "(";
				}
				s << obj << rv.methods->setter << "(";
				if (op->isCompoundAssignmentOp()) {
					s << rv.methods->exprGet(obj)
					  << " "
					  << op->getOpcodeStr(op->getOpForCompoundAssignment(op->getOpcode()))
					  << " ";
				}
				s.flush();
			}

			std::string suffix(")");
			{
				sstream s(suffix);
				if (op != outmost) {
					s << ", " << rv.methods->exprGet(obj) << ")";
				}
				s.flush();
			}

			auto range = SourceRange(
				op->getLHS()->getLocStart(),
				op->getRHS()->getLocStart());

			replaceText(range, prefix);
			insertAfterToken(op->getLocEnd(), suffix);

			observed.insert(rv.expr);
		}
		return true;
	}

	bool VisitMemberExpr(const MemberExpr *e) {
		if (observed.find(e) != observed.end()) {
			return true;
		}
		if (auto rv = findAccessors(e)) {
			replace(e->getSourceRange(), rv.methods->exprGet(accessPrefix(e)));
		}
		return true;
	}

private:
	const Stmt *getBlockStmt(const Stmt *s) {
		auto *block = dyn_cast<CompoundStmt>(stmtGraph.getParent(s));
		return (block) ? block : s;
	}

	const Stmt *getOutmostStmt(const Stmt *s) const {
		const Stmt *parent = stmtGraph.getParent(s);
		while (isa<Expr>(parent) || isa<DeclStmt>(parent)) {
			s = parent;
			parent = stmtGraph.getParent(parent);
		}
		return s;
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

		// prettyPrint() may have already inserted access
		// consider replacing only member location
		const size_t last = prefix.size() - 1;
		if (prefix[last] != '.' &&
			(prefix[last] != '>' || prefix[last - 1] != '-')) {
			prefix += (e->isArrow()) ? "->" : ".";
		}
		return prefix;
	}

	struct AccessorQueryResult {
		const MemberExpr *expr;
		const Accessors *methods;

		operator bool() const {
			return expr && methods;
		}
	};

	const AccessorQueryResult findAccessors(const Expr *e) const {
		return findAccessors(dyn_cast<MemberExpr>(e));
	}

	const AccessorQueryResult findAccessors(const MemberExpr *e) const {
		if (e && isa<FieldDecl>(e->getMemberDecl())) {
			auto it = accessors.find(cast<FieldDecl>(e->getMemberDecl()));
			if (it != accessors.end()) {
				return { e, &it->second };
			}
		}
		return { nullptr, nullptr };
	}

private:
	void insertAccessors() {
		for (auto &entry : accessors) {
			const FieldDecl *field = entry.first;

			StringRef indent;
			SourceLocation insertLoc = findInsertLoc(field, &indent);

			std::string methods;
			sstream s(methods);
			s << "\n";

			StringRef name = field->getName();
			const auto &type = field->getType().getNonReferenceType();

			s << indent;
			type.withConst().print(s, ctx->getPrintingPolicy());
			s << " &" << entry.second.getter << "() const { return " << name << "; }\n";

			s << indent;
			type.print(s, ctx->getPrintingPolicy());
			s << " &" << entry.second.getter << "() { return " << name << "; }\n";

			s << indent << "void " << entry.second.setter << "(";
			type.withConst().print(s, ctx->getPrintingPolicy());
			s << " &x) { this->" << name << " = x; }\n";

			s.flush();
			insert(insertLoc, methods);
		}
	}

	SourceLocation findInsertLoc(const FieldDecl *decl, StringRef *indent) {
		if (auto *record = dyn_cast<CXXRecordDecl>(decl->getParent())) {
			StringRef lineIndent;
			SourceLocation insertLoc;
			for (const auto *method : record->methods()) {
				if (method->isUserProvided()) {
					lineIndent = getLineIndentation(
						method->getLocStart(),
						&ci->getSourceManager());
					insertLoc = getLocAfter(method->getLocEnd());
				}
			}
			if (insertLoc.isValid()) {
				if (indent) {
					*indent = lineIndent;
				}
				return insertLoc;
			}
		}
		if (indent) {
			*indent = getLineIndentation(
				decl->getLocStart(),
				&ci->getSourceManager());
		}
		return decl->getParent()->getRBraceLoc();
	}

	void insertAfterToken(SourceLocation loc, std::string text) {
		insert(getLocAfter(loc), text);
	}

	SourceLocation getLocAfter(SourceLocation loc) {
		return getLocForEndOfToken(loc);
	}

	void initStmtGraph(Stmt *s) {
		stmtGraph.~ParentMap();
		if (s) {
			new (&stmtGraph) ParentMap(s);
		} else {
			// NOTE dirty hack, ask Clang developers to fix upstream
			// (3.9.0 trunk still affected)
			typedef llvm::DenseMap<Stmt *, Stmt *> StmtMap;
			*reinterpret_cast<StmtMap **>(&stmtGraph) = new StmtMap();
		}
	}
};

REGISTER_TRANSFORM(AccessorsTransform);
