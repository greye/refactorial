CMAKE_MINIMUM_REQUIRED (VERSION 2.6)

SET(CMAKE_BUILD_TYPE None)
SET(CMAKE_C_COMPILER clang)
SET(CMAKE_CXX_COMPILER clang++)
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_LINKER_FLAGS} -framework Foundation")
PROJECT (foo)
ADD_EXECUTABLE (foo foo.mm)
