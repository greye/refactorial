#include "CollectMatchingUsrAction.h"
#include "UsrRename.h"

namespace {
class MyUsrRename : public UsrRename {
public:
    MyUsrRename(const CollectMatchingUsrAction &action)
      : matchedNames(action.getMatchedNames()),
        usrIndex(action.getUsrIndex())
    {}

    bool isToRename(const std::string &usr) override {
        return usrIndex.find(usr) != usrIndex.end();
    }

    std::string getUsrName(const std::string &usr) override {
        auto usrIt = usrIndex.find(usr);
        assert(usrIt != usrIndex.end());

        auto nameIt = matchedNames.find(*usrIt->second);
        assert(nameIt != matchedNames.end());

        return extract(nameIt->first, *nameIt->second);
    }

private:
    std::string extract(const std::string &name, const std::string &pat) {
        return name;
    }

private:
    const StringRefMap &matchedNames;
    const StringRefMap &usrIndex;
};
}

void
renameMatchingUsr(ASTContext &ctx, Replacements &rs) {
    CollectMatchingUsrAction collect(rules);
    collect.newASTConsumer()->HandleTranslationUnit(ctx);

    MyUsrRename renameRules(collect);
    UsrRenameAction rename(renameRules, rs);
    rename.newASTConsumer()->HandleTranslationUnit(ctx);
}
