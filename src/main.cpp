#include <fmt/base.h>

#include <fstream>
#include <iostream>
#include <string>

#include "backend/worker.h"
#include "lexer/tokenizer.h"
#include "parser/parser.h"

void PrintUsage(const std::string& program) {
  fmt::println(
      "Usage: {} <input_file> <output_file>\n"
      "  <input_file>  - Source Ruby file to compile\n"
      "  <output_file> - Output assembly file",
      program);
}

int main(int argc, char* argv[]) {
  if (argc != 3) {
    PrintUsage(argv[0]);
    return 1;
  }

  std::ifstream input(argv[1]);
  if (!input) {
    fmt::print("Error: Could not open input file '{}'\n", argv[1]);
    return 1;
  }

  std::ofstream output_file(argv[2]);
  if (!output_file) {
    fmt::print("Error: Could not open output file '{}'\n", argv[2]);
    return 1;
  }

  try {
    lexer::Tokenizer tokenizer(&input);
    tokenizer.TokenizeVariant();

    parser::Parser parser(&tokenizer);
    parser::AstNode root = parser.Parse();

    fmt::print("\n=== Abstract Syntax Tree ===\n");
    fmt::print("{}\n", root->ToString());
    fmt::print("===========================\n\n");

    backend::Worker worker(std::move(root));
    std::string assembly = worker.GenerateAssembly();

    output_file << assembly;

    fmt::print("Compilation successful. Assembly written to '{}'\n", argv[2]);
  } catch (const std::exception& e) {
    fmt::print("Error: {}\n", e.what());
    return 1;
  }

  return 0;
}