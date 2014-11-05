#ifndef CLANG_RENAME_USR_UTILS
#define CLANG_RENAME_USR_UTILS

#include <string>

namespace clang {
class Decl;
}

bool getUsrForDecl(const clang::Decl *decl, std::string &s);

#endif
