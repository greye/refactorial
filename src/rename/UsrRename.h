#ifndef CLANG_RENAME_USR_RENAME
#define CLANG_RENAME_USR_RENAME

#include <clang/Tooling/Refactoring.h>

#include <string>
#include <memory>

namespace clang {
class ASTConsumer;
}

class UsrRename {
public:
    virtual bool isToRename(const std::string &usr) = 0;
    virtual std::string getUsrName(const std::string &usr) = 0;
};

class UsrRenameAction {
public:
    UsrRenameAction(UsrRename &rename, clang::tooling::Replacements &rs)
      : rename(rename), rs(rs)
    {}

    std::unique_ptr<clang::ASTConsumer> newASTConsumer();

private:
    UsrRename &rename;
    clang::tooling::Replacements &rs;
};

#endif
