#ifndef CLANG_RENAME_TYPE_FIND_ACTION_H
#define CLANG_RENAME_TYPE_FIND_ACTION_H

#include <map>
#include <memory>
#include <string>
#include <vector>

namespace clang {
class ASTConsumer;
class NamedDecl;
}

class DeclFilter {
public:
    virtual bool accepts(const NamedDecl *decl) = 0;
};

namespace {
typedef std::pair<DeclFilter *, std::string> MatchRule;
typedef std::map<std::string, const std::string *> StringRefMap;
}

class CollectMatchingUsrAction {
public:
    CollectMatchingUsrAction(DeclFilter &isAcceptable,
                             std::string pattern);

    CollectMatchingUsrAction(const std::vector<MatchRule> &rules);

    std::unique_ptr<clang::ASTConsumer> newASTConsumer();

    const StringRefMap &getMatchedNames() const {
        return matchedNames;
    }

    const StringRefMap &getUsrIndex() const {
        return usrIndex;
    }

private:
    // name -> pattern
    StringRefMap matchedNames;
    // USR -> name
    StringRefMap usrIndex;

    std::vector<MatchRule> fallbackRules;
    const std::vector<MatchRule> &rules;
};

#endif
