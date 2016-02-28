local function runcmd(cmd)
    local s, _ = string.gsub(os.outputof(cmd), "\n", "")
    return s
end

local llvm_version

solution "refactorial"
targetdir   (_WORKING_DIR)
location    (_WORKING_DIR)

configurations { "Debug", "Release" }
    flags { "ExtraWarnings", "FatalWarnings", "NoRTTI" }

configuration "Release"
    defines { "NDEBUG" }
    flags { "Optimize" }

configuration {"linux", "gmake"}
    buildoptions { "-std=c++11", "-pedantic", "-Wno-unused-parameter" }
    buildoptions { runcmd("llvm-config --cppflags") }
    includedirs { runcmd("llvm-config --includedir") }
    libdirs { runcmd("llvm-config --libdir") }
    linkoptions { runcmd("llvm-config --ldflags") }
    llvm_version = runcmd("llvm-config --version")
configuration {}

platforms { "native", "x32", "x64" }

project "refactorial"
    includedirs "./"
    language "C++"
    kind "ConsoleApp"
    files { "*.cpp", "Transforms/*.cpp" }
    links {
        "clangTooling",
        "clangFrontend",
        "clangSerialization",
        "clangParse",
        "clangSema",
        "clangAnalysis",
        "clangEdit",
        "clangRewrite",
        "clangDriver",
        "clangAST",
        "clangASTMatchers",
        "clangLex",
        "clangBasic",
        "LLVM-"..llvm_version,
        "yaml-cpp",
        "pcrecpp"
    }
