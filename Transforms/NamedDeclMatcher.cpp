#include "NamedDeclMatcher.h"
#include "Transforms.h"

#include <pcrecpp.h>
#include <clang/AST/Decl.h>
#include <clang/AST/Stmt.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/Tooling/Refactoring.h>
#include <llvm/Support/raw_ostream.h>

using namespace clang;

bool
NamedDeclMatcher::loadConfig(
    const std::string& transformName,
    const std::string& renameKeyName,
    const std::string& ignoreKeyName)
{
    auto S = TransformRegistry::get().config[transformName];
    if (!S.IsMap()) {
      llvm::errs() << "Error: Cannot find config entry \"" << transformName
                   << "\" or entry is not a map\n";
      return false;
    }

    auto IG = S[ignoreKeyName];

    if (IG && !IG.IsSequence()) {
      llvm::errs() << "Error: Config key \"" << ignoreKeyName
                   << "\" must be a sequence\n";
      return false;
    }

    for (auto I = IG.begin(), E = IG.end(); I != E; ++I) {
      if (I->IsScalar()) {
        auto P = I->as<std::string>();
        ignoreList.push_back(pcrecpp::RE(P));
        llvm::errs() << "Ignoring: " << P << "\n";
      }
    }

    auto RN = S[renameKeyName];
    if (!RN.IsSequence()) {
      llvm::errs() << "\"" << renameKeyName << "\" is not specified or is"
                   << " not a sequence\n";
      return false;
    }

    for (auto I = RN.begin(), E = RN.end(); I != E; ++I) {
      if (!I->IsMap()) {
        llvm::errs() << "Error: \"" << renameKeyName
                     << "\" contains non-map items\n";
        return false;
      }

      for (auto MI = I->begin(), ME = I->end(); MI != ME; ++MI) {
        auto F = MI->first.as<std::string>();
        auto T = MI->second.as<std::string>();
        pcrecpp::RE re(F);
        renameList.push_back(REStringPair(re, T));

        llvm::errs() << "renames: " << F << " -> " << T << "\n";
      }
    }

    return true;
}

bool
NamedDeclMatcher::shouldIgnore(clang::SourceLocation L)
{
    if (!L.isValid()) {
      return true;
    }

    clang::SourceManager &SM = ci->getSourceManager();
    clang::FullSourceLoc FSL(L, SM);
    const clang::FileEntry *FE = SM.getFileEntryForID(FSL.getFileID());
    if (!FE) {
      // attempt to get the spelling location
      auto SL = SM.getSpellingLoc(L);
      if (!SL.isValid()) {
        return true;
      }

      clang::FullSourceLoc FSL2(SL, SM);
      FE = SM.getFileEntryForID(FSL2.getFileID());
      if (!FE) {
        return true;
      }
    }

    auto FN = FE->getName();

    for (auto I = ignoreList.begin(), E = ignoreList.end(); I != E; ++I) {
      if (I->FullMatch(FN)) {
        return true;
      }
    }

    return false;
}

  // if we have a NamedDecl and the fully-qualified name matches
bool
NamedDeclMatcher::nameMatches(
    const clang::NamedDecl *D,
    std::string &outNewName,
    bool checkOnly)
{
    if (!D) {
      return false;
    }

    auto I = nameMap.find(D);
    if (I != nameMap.end()) {
      outNewName = (*I).second;
      return true;
    }

    // if we only need a quick look-up of renamed Decl's
    // (e.g. for renamed parent classes, overridden methods etc.)
    if (checkOnly) {
      return false;
    }

    if (!D->getLocation().isValid()) {
      return false;
    }

    auto QN = D->getQualifiedNameAsString();
    if (QN.size() == 0) {
      return false;
    }

    // special handling for TagDecl
    if (auto T = llvm::dyn_cast<clang::TagDecl>(D)) {
      auto KN = T->getKindName();
      assert(KN && "getKindName() must return a non-NULL value");
      QN.insert(0, KN);
      QN.insert(KN.size(), " ");
    }

    for (auto I = renameList.begin(), E = renameList.end(); I != E; ++I) {
      if (I->first.FullMatch(QN)) {
        I->first.Extract(I->second, QN, &outNewName);
        nameMap[D] = outNewName;
        return true;
      }
    }
    return false;
}

  // useful when we can't just rely on Decl, e.g. built-in type
  // unmatched names are cached to speed things up
bool
NamedDeclMatcher::stringMatches(std::string name, std::string &outNewName)
{
    auto I = matchedStringMap.find(name);
    if (I != matchedStringMap.end()) {
      outNewName = (*I).second;
      return true;
    }

    auto J = unmatchedStringSet.find(name);
    if (J != unmatchedStringSet.end()) {
      return false;
    }

    for (auto I = renameList.begin(), E = renameList.end(); I != E; ++I) {
      if (I->first.FullMatch(name)) {
        std::string newName;
        I->first.Extract(I->second, name, &newName);
        matchedStringMap[name] = newName;
        outNewName = newName;
        return true;
      }
    }

    unmatchedStringSet.insert(name);
    return false;
}

bool
NamedDeclMatcher::stmtInSameFileAsDecl(clang::Stmt *S, clang::Decl *D)
{
    return ci->getSourceManager().isWrittenInSameFile(
        S->getLocStart(),
        D->getLocation());
}

void
NamedDeclMatcher::renameLocation(clang::SourceLocation L, std::string& N)
{
    if (L.isValid()) {
      if (L.isMacroID()) {
        // TODO: emit error using diagnostics
        clang::SourceManager &SM = ci->getSourceManager();
        if (SM.isMacroArgExpansion(L) || SM.isInSystemMacro(L)) {
          // see if it's the macro expansion we can handle
          // e.g.
          //   #define call(x) x
          //   call(y());   // if we want to rename y()
          L = SM.getSpellingLoc(L);

          // this falls through to the rename routine below
        }
        else {
          // if the spelling location is from an actual file that we can
          // touch, then do the replacement, but show a warning
          clang::SourceManager &SM = ci->getSourceManager();
          auto SL = SM.getSpellingLoc(L);
          clang::FullSourceLoc FSL(SL, SM);
          const clang::FileEntry *FE = SM.getFileEntryForID(FSL.getFileID());
          if (FE) {
            llvm::errs() << "Warning: Rename attempted as a result of macro "
                         << "expansion may break things, at: " << loc(L) << "\n";
            L = SL;
            // this falls through to the rename routine below
          }
          else {
            // cannot handle this case
            llvm::errs() << "Error: Token is resulted from macro expansion"
              " and is therefore not renamed, at: " << loc(L) << "\n";
            return;
          }
        }
      }

      if (shouldIgnore(L)) {
        return;
      }

      clang::Preprocessor &P = ci->getPreprocessor();
      auto LE = P.getLocForEndOfToken(L);
      if (LE.isValid()) {

        // getLocWithOffset returns the location *past* the token, hence -1
        auto E = LE.getLocWithOffset(-1);

        // TODO: Determine if it's a wrtiable file

        // TODO: Determine if the location has already been touched or
        // needs skipping (such as in refactoring API user's code, then
        // the API headers need no changing since later the new API will be
        // in place)

        // llvm::errs() << "rep: " << loc(L) << ", " << loc(E) << "\n";
        // replace(clang::SourceRange(L, E), N, ci->getSourceManager());
        auto Range = CharSourceRange(SourceRange(L, E), true);
        auto Edit = tooling::Replacement(ci->getSourceManager(), Range, N);
        TransformRegistry::get().replacements->insert(Edit);
      }
    }
}

const std::string &
NamedDeclMatcher::indent()
{
    return indentString;
}

void
NamedDeclMatcher::pushIndent()
{
    indentLevel++;
    indentString.resize(indentString.size() + 2, ' ');
}

void
NamedDeclMatcher::popIndent()
{
    assert(indentLevel >= 0 && "indentLevel must be >= 0");
    indentLevel--;
    indentString.resize(indentString.size() - 2);
}

std::string
NamedDeclMatcher::loc(clang::SourceLocation L)
{
    return L.printToString(ci->getSourceManager());
}

std::string
NamedDeclMatcher::range(clang::SourceRange R)
{
    std::string src;
    llvm::raw_string_ostream sst(src);
    sst << "(";
    R.getBegin().print(sst, ci->getSourceManager());
    sst << ", ";
    R.getEnd().print(sst, ci->getSourceManager());
    sst << ")";
    return sst.str();
}
