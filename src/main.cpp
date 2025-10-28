#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

#include "lexer/lexer.h"
#include "parser/parser.h"
#include "codegen/codegen.h"

#include <llvm/Support/Error.h>

std::string readFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + filename);
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

void printUsage(const char* program_name) {
    std::cerr << "Usage: " << program_name << " [options] <input.sm>\n";
    std::cerr << "Options:\n";
    std::cerr << "  -o <output>    Output executable name (default: a.out)\n";
    std::cerr << "  -c             Compile to object file only\n";
    std::cerr << "  -nostdlib      Do not link with standard library\n";
    std::cerr << "  -h, --help     Show this help message\n";
}

int main(int argc, char** argv) {
    if (argc < 2) {
        printUsage(argv[0]);
        return 1;
    }
    
    std::string input_file;
    std::string output_file = "a.out";
    bool compile_only = false;
    bool no_stdlib = false;

    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        
        if (arg == "-h" || arg == "--help") {
            printUsage(argv[0]);
            return 0;
        } else if (arg == "-o") {
            if (i + 1 < argc) {
                output_file = argv[++i];
            } else {
                std::cerr << "Error: -o requires an argument\n";
                return 1;
            }
        } else if (arg == "-c") {
            compile_only = true;
        } else if (arg == "-nostdlib") {
            no_stdlib = true;
        } else if (arg[0] != '-') {
            input_file = arg;
        } else {
            std::cerr << "Unknown option: " << arg << "\n";
            return 1;
        }
    }
    
    if (input_file.empty()) {
        std::cerr << "Error: No input file specified\n";
        printUsage(argv[0]);
        return 1;
    }
    
    try {
        std::string source = readFile(input_file);
        
        Summit::Lexer lexer(source);
        auto tokens = lexer.tokenize();
        
        Summit::Parser parser(std::move(tokens));
        auto ast = parser.parse();
        
        Summit::CodeGenerator codegen;
        codegen.generate(*ast);
        
        if (compile_only) {
            codegen.emitObjectFile(output_file);
        } else {
            std::vector<std::string> libs;
            if (!no_stdlib) {
                libs.push_back("build/linux/x86_64/release/libsummit_std.a");;
            }
            
            codegen.emitExecutable(output_file, libs, no_stdlib);
        }
        
        return 0;
        
    } catch (const llvm::Error& e) {
        std::string error_str;
        llvm::raw_string_ostream error_stream(error_str);
        error_stream << e;
        std::cerr << "LLVM Error: " << error_str << "\n";
        return 1;
    } catch (const llvm::StringError& e) {
        std::cerr << "LLVM String Error: " << e.getMessage() << "\n";
        return 1;
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    } catch (...) {
        std::cerr << "Unknown error occurred\n";
        return 1;
    }
}