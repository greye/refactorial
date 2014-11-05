#include "UsrRename.h"
#include "UsrUtils.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Tooling/Refactoring.h"
#include "llvm/ADT/SmallVector.h"

using namespace llvm;
using namespace clang;

namespace {
// \brief This visitor recursively searches for all instances of a USR in a
// translation unit and stores them for later usage.
class UsrRenameVisitor : public RecursiveASTVisitor<UsrRenameVisitor> {
public:
    UsrRenameVisitor(UsrRename &rename,
                    tooling::Replacements &rs)
      : rename(rename), rs(rs)
    {}

    bool VisitNamedDecl(const NamedDecl *decl) {
        processDecl(decl);
        return true;
    }

    bool VisitDeclRefExpr(const DeclRefExpr *expr) {
        checkNestedNameSpecifierLoc(expr->getQualifierLoc());
        processDecl(expr->getFoundDecl());
        return true;
    }

    bool VisitMemberExpr(const MemberExpr *Expr) {
        processDecl(Expr->getFoundDecl().getDecl());
        return true;
    }

private:
  // Namespace traversal:
    void checkNestedNameSpecifierLoc(NestedNameSpecifierLoc loc) {
        while (loc) {
            processDecl(loc.getNestedNameSpecifier()->getAsNamespace());
            loc = loc.getPrefix();
        }
    }

    void processDecl(const Decl *decl) {
        if (getUsrForDecl(decl, usr) && rename.isToRename(usr)) {
            auto range =
                CharSourceRange::getTokenRange(decl->getSourceRange());
            const auto &srcMgr = decl->getASTContext().getSourceManager();
            rs.insert(tooling::Replacement(srcMgr,
                                           range,
                                           rename.getUsrName(usr)));
        }
    }
private:
    UsrRename &rename;
    tooling::Replacements &rs;
    std::string usr;
};

class UsrRenameConsumer : public ASTConsumer {
public:
    UsrRenameConsumer(UsrRename &rename, tooling::Replacements &rs)
      : visitor(rename, rs)
    {}

    void HandleTranslationUnit(ASTContext &ctx) override {
        visitor.TraverseDecl(ctx.getTranslationUnitDecl());
    }
private:
    UsrRenameVisitor visitor;
};
} // namespace

std::unique_ptr<ASTConsumer>
UsrRenameAction::newASTConsumer() {
    return std::unique_ptr<ASTConsumer>(new UsrRenameConsumer(rename, rs));
}
