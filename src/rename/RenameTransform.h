#ifndef REFACTORIAL_RENAME_TRANSFORM_H
#define REFACTORIAL_RENAME_TRANSFORM_H

#include "Transforms/Transforms.h"

#include <pcrecpp.h>

class RenameTransform : public Transform {
public:
    RenameTransform();

    void HandleTranslationUnit(ASTContext &ctx) override;

protected:

    bool loadConfig(
        const std::string& transformName,
        const std::string& renameKeyName,
        const std::string& ignoreKeyName = "Ignore");

    bool shouldIgnore(clang::SourceLocation L);

    bool nameMatches(
        const clang::NamedDecl *decl,
        std::string &newName,
        bool checkOnly = false);

    bool stringMatches(const std::string &name, std::string &outNewName);

private:
    std::vector<pcrecpp::RE> ignoreList;

    typedef std::pair<pcrecpp::RE, std::string> REStringPair;
    std::vector<REStringPair> renameList;

    std::map<const clang::Decl *, std::string> nameMap;
    std::map<std::string, std::string> matchedStringMap;
    std::set<std::string> unmatchedStringSet;
};

#endif
