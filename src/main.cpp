#include <fstream>
#include <iostream>
#include <string>

#include "backend/worker.h"
#include "lexer/tokenizer.h"
#include "parser/parser.h"

void PrintUsage(const std::string& program) {
  std::cerr << "Usage: " << program << " <input_file> <output_file>"
            << std::endl;
  std::cerr << "  <input_file>  - Source Ruby file to compile" << std::endl;
  std::cerr << "  <output_file> - Output assembly file" << std::endl;
}

int main(int argc, char* argv[]) {
  if (argc != 3) {
    PrintUsage(argv[0]);
    return 1;
  }

  std::ifstream input(argv[1]);
  if (!input) {
    std::cerr << "Error: Could not open input file '" << argv[1] << "'"
              << std::endl;
    return 1;
  }

  std::ofstream output_file(argv[2]);
  if (!output_file) {
    std::cerr << "Error: Could not open output file '" << argv[2] << "'"
              << std::endl;
    return 1;
  }

  try {
    lexer::Tokenizer tokenizer(&input);
    tokenizer.TokenizeVariant();

    parser::Parser parser(&tokenizer);

    parser::AstNode ast = parser.Parse();

    backend::Worker worker(ast);
    std::string assembly = worker.GenerateAssembly();

    output_file << assembly;

    std::cout << "Compilation successful. Assembly written to '" << argv[2]
              << "'" << std::endl;
  } catch (const std::exception& e) {
    std::cerr << "Error during compilation: " << e.what() << std::endl;
    return 1;
  }

  return 0;
}