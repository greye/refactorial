#include "UsrUtils.h"

#include <clang/Index/USRGeneration.h>
#include <llvm/ADT/SmallVector.h>

bool
getUsrForDecl(const clang::Decl *decl, std::string &s) {
    llvm::SmallVector<char, 128> buf;

    // FIXME: Add test for the nullptr case.
    if (decl == nullptr || clang::index::generateUSRForDecl(decl, buf)) {
        return false;
    }
    s.assign(buf.data(), buf.size());
    return true;
}
