cmake_minimum_required(VERSION 3.14)
project(RubyCompiler)

include(FetchContent)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

FetchContent_Declare(googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG main
)
FetchContent_MakeAvailable(googletest)

FetchContent_Declare(fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt.git
  GIT_TAG master
)
FetchContent_MakeAvailable(fmt)

FetchContent_Declare(mimalloc
  GIT_REPOSITORY https://github.com/microsoft/mimalloc.git
  GIT_TAG main 
)

if(WIN32)
  set(MI_BUILD_SHARED ON CACHE BOOL "Build shared library" FORCE)
  set(MI_OVERRIDE ON CACHE BOOL "Override malloc and free" FORCE)
endif()

FetchContent_MakeAvailable(mimalloc)

enable_testing()

function(add_mimalloc_to_target target)
  if(WIN32)
    target_link_libraries(${target} PRIVATE mimalloc)
    target_compile_definitions(${target} PRIVATE "INCLUDE_MI_VERSION")
    if(MSVC)
      target_link_options(${target} PRIVATE "/INCLUDE:mi_version")
    endif()
  else()
    target_link_libraries(${target} PRIVATE mimalloc)
  endif()
endfunction()

file(GLOB_RECURSE COMPILER_SOURCES 
  "src/lexer/*.cpp"
  "src/parser/*.cpp"
  "src/backend/*.cpp"
)

file(GLOB_RECURSE COMPILER_HEADERS 
  "src/lexer/*.h"
  "src/parser/*.h"
  "src/backend/*.h"
)

add_library(compiler ${COMPILER_SOURCES} ${COMPILER_HEADERS})
target_include_directories(compiler PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/src)
target_link_libraries(compiler PRIVATE fmt)

add_executable(ruby_compiler src/main.cpp)
add_mimalloc_to_target(ruby_compiler)
target_link_libraries(ruby_compiler PRIVATE compiler fmt)

file(GLOB_RECURSE TEST_SOURCES "tests/*.test.cpp")

add_executable(compiler_tests ${TEST_SOURCES})
target_link_libraries(compiler_tests PRIVATE fmt compiler GTest::gtest GTest::gtest_main)