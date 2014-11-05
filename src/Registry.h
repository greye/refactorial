#ifndef REFACTORIAL_REGISTRY_H
#define REFACTORIAL_REGISTRY_H

#include <clang/Tooling/Replacement.h>
#include <memory>

namespace clang {
class ASTConsumer;
}

class ConsumerFactory {
public:
    virtual clang::ASTConsumer *newASTConsumer() = 0;
};

std::unique_ptr<ConsumerFactory>
createTransform(const YAML::Node &node, clang::tooling::Replacements &rs);

#endif
