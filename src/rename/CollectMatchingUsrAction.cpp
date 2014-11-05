#include "CollectMatchingUsrAction.h"
#include "UsrUtils.h"

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Lex/Lexer.h"
#include "clang/Index/USRGeneration.h"
#include "llvm/ADT/SmallVector.h"

using namespace llvm;
using namespace clang;

/*
// Get the USRs for the constructors of the class.
static std::vector<std::string>
getAllConstructorUSRs(const CXXRecordDecl *Decl) {
    std::vector<std::string> USRs;

    // We need to get the definition of the record (as opposed to any
    // forward declarations) in order to find the constructor and
    // destructor.
    const auto *RecordDecl = Decl->getDefinition();

    // Iterate over all the constructors and add their USRs.
    for (const auto &CtorDecl : RecordDecl->ctors())
        USRs.push_back(getUsrForDecl(CtorDecl));

    // Ignore destructors. GetLocationsOfUSR will find the declaration of
    // and explicit calls to a destructor through TagTypeLoc (and it is
    // better for the purpose of renaming).
    //
    // For example, in the following code segment,
    //  1  class C {
    //  2    ~C();
    //  3  };
    // At line 3, there is a NamedDecl starting from '~' and a TagTypeLoc
    // starting from 'C'.

    return USRs;
}
*/

namespace {
class NamedDeclVisitor : public RecursiveASTVisitor<NamedDeclVisitor> {
public:

    NamedDeclVisitor(const std::vector<MatchRule> &rules,
                     StringRefMap &matchedNames,
                     StringRefMap &usrIndex)
      : rules(rules), matchedNames(matchedNames), usrIndex(usrIndex)
    {}

    bool VisitNamedDecl(const NamedDecl *decl) {
        for (const MatchRule &rule : rules) {
            if (processIfMatches(decl, rule)) {
                break;
            }
        }
        return true;
    }

private:
    bool processIfMatches(const NamedDecl *decl, const MatchRule &rule) {
        if (!rule.first->accepts(decl)) {
            return false;
        }
        StringRef qualName = decl->getQualifiedName();
        if (!matches(qualName, rule.second)) {
            return false;
        }
        auto rcName =
            matchedNames.insert(
                std::make_pair(
                    std::string(qualName.data(), qualName.size()),
                    &rule.second));

        if (!rcName.second) {
            // FIXME
            assert(
                *rcName.first->second == rule.second && "Pattern mismatch");
        }
        std::string usr;
        getUsrForDecl(decl, usr);
        auto rcUsr = usrIndex.insert(std::make_pair(std::move(usr),
                                                    &rcName.first->first));

        if (auto cdecl = dyn_cast<CXXRecordDecl>(decl)) {
        }
        return true;
    }

    bool matches(const std::string &s, const std::string &pattern) const {
        auto it = matchedNames.find(s);
        if (*it->second == pattern) {
            return true;
        }
        return true;
    }

private:
    const std::vector<MatchRule> &rules;
    StringRefMap &matchedNames;
    StringRefMap &usrIndex;
};

class CollectMatchingUsrConsumer : public ASTConsumer {
public:
    CollectMatchingUsrConsumer(const std::vector<MatchRule> &rules,
                               StringRefMap &matchedNames,
                               StringRefMap &usrIndex)
      : visitor(rules, matchedNames, usrIndex)
    {}

    void HandleTranslationUnit(ASTContext &ctx) override {
        visitor.TraverseDecl(ctx.getTranslationUnitDecl());
    }

private:
    NamedDeclVisitor visitor;
};
}

CollectMatchingUsrAction::CollectMatchingUsrAction(
    DeclFilter *filter,
    std::string pattern)
  : fallbackRules(1, std::make_pair(filter, std::move(pattern))),
    rules(fallbackRules)
{}
    
CollectMatchingUsrAction::CollectMatchingUsrAction(
    const std::vector<MatchRule> &rules)
  : rules(rules)
{}

std::unique_ptr<ASTConsumer>
CollectMatchingUsrAction::newASTConsumer() {
    return std::unique_ptr<ASTConsumer>(
            new CollectMatchingUsrConsumer(rules, matchedNames, usrIndex));
}
