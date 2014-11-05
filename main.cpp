#include <yaml-cpp/yaml.h>

#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Refactoring.h>
#include <clang/Tooling/Tooling.h>

#include <iostream>
#include <fstream>

using namespace clang;
using namespace clang::tooling;

#include "Transforms/Transforms.h"

static llvm::cl::OptionCategory optionCategory("Refactorial options");

static llvm::cl::opt<std::string> refactor_specifications(
        "rulespec",
        llvm::cl::desc("file with refactoring information, overrides stdin"),
        llvm::cl::Optional);

int main(int argc, const char **argv)
{
    CommonOptionsParser cmdl(argc, argv);

    // read refactoring specifications. either read from stdin or file
    std::vector<YAML::Node> config;
    if (refactor_specifications.empty()) {
        llvm::errs() << "falling back to read from stdin\n";
        config = YAML::LoadAll(std::cin);
    } else {
        std::ifstream fin(refactor_specifications.c_str(), std::ifstream::binary);
        if (fin) {
            config = YAML::LoadAll(fin);
        } else {
            llvm::errs() << "barf: cannot open file '" << refactor_specifications << "'\n";
            exit(-1);
        }
        fin.close();
    }

	RefactoringTool rt(cmdl.getCompilations(), cmdl.getSourcePathList());

	IgnoringDiagConsumer ignore;
	rt.setDiagnosticConsumer(&ignore);

	// iterate over refactoring specification
	for(const YAML::Node &configSection : config) {
		TransformRegistry::get().config = YAML::Node();

		//figure out which files we need to work on
		/*
		std::vector<std::string> inputFiles;
		if(configSection["Files"])
			inputFiles = configSection["Files"].as<std::vector<std::string> >();
		if(!configSection["Transforms"]) {
			llvm::errs() << "No transforms specified in this configuration section:\n";
			llvm::errs() << YAML::Dump(configSection) << "\n";
		}
		
		//load up the compilation database
        if (inputFiles.empty()) {
            inputFiles = Compilations.getAllFiles();
            llvm::errs() << "no input files in refactoring yml given. "
                << "using " << inputFiles.size() << " compile units "
                << "from compile_commands.json\n";
        }
		*/


		TransformRegistry::get().config = configSection["Transforms"];
		TransformRegistry::get().replacements = &rt.getReplacements();
		
		//finally, run
		for(const auto &config : configSection["Transforms"]) {
			std::string transId = trans.first.as<std::string>() + "Transform";
			llvm::errs() << "Doing a '" << transId << "'\n";
			rt.runAndSave(new TransformFactory(TransformRegistry::get()[transId]));
			auto transform = createTransform(config, rt.getReplacements());
			rt.runAndSave(tooling::newFrontendActionFactory(transform.get()));
		}
	}
	return 0;
}
