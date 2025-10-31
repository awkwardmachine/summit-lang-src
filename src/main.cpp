#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <filesystem>
#include <llvm/Support/Error.h>

#include "lexer/lexer.h"
#include "parser/parser.h"
#include "codegen/codegen.h"

// file reading utility
std::string readFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + filename);
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

// cli help message
void printUsage(const char* program_name) {
    std::cerr << "Usage: " << program_name << " [options] <input.sm>\n";
    std::cerr << "       " << program_name << " run <filename/filename.sm>\n";
    std::cerr << "Options:\n";
    std::cerr << "  -o <output>    Output executable name\n";
    std::cerr << "  -c             Compile to object file only\n";
    std::cerr << "  -nostdlib      Do not link with standard library\n";
    std::cerr << "  -window        Show console window when using generated executable\n";
    std::cerr << "  -h, --help     Show this help message\n";
}

// figure out what to name the output if user didnt specify
std::string getDefaultOutputName(const std::string& input_file, bool compile_only) {
    std::filesystem::path input_path(input_file);
    std::string stem = input_path.stem().string();
    
    if (compile_only) {
        return stem + ".o";
    }
    
    // windows needs .exe extension
#ifdef _WIN32
    return stem + ".exe";
#else
    return stem;
#endif
}

std::string resolveInputFile(const std::string& input_arg) {
    if (std::filesystem::exists(input_arg)) {
        return input_arg;
    }
    
    std::string with_ext = input_arg + ".sm";
    if (std::filesystem::exists(with_ext)) {
        return with_ext;
    }
    
    throw std::runtime_error("Could not find file: " + input_arg + " or " + with_ext);
}

int main(int argc, char** argv) {
    if (argc < 2) {
        printUsage(argv[0]);
        return 1;
    }
    
    // parse command line args
    std::string input_file;
    std::string output_file;
    bool output_specified = false;
    bool compile_only = false;
    bool no_stdlib = false;
    bool no_window = true;
    bool run_mode = false;
    
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        
        if (arg == "run") {
            run_mode = true;
            if (i + 1 < argc) {
                input_file = argv[++i];
            } else {
                std::cerr << "Error: run requires a filename\n";
                return 1;
            }
            break;
        } else if (arg == "-h" || arg == "--help") {
            printUsage(argv[0]);
            return 0;
        } else if (arg == "-o") {
            if (i + 1 < argc) {
                output_file = argv[++i];
                output_specified = true;
            } else {
                std::cerr << "Error: -o requires an argument\n";
                return 1;
            }
        } else if (arg == "-c") {
            compile_only = true;
        } else if (arg == "-nostdlib") {
            no_stdlib = true;
        } else if (arg == "-window") {
            no_window = false;
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
        input_file = resolveInputFile(input_file);
    } catch (const std::exception& e) {
        std::cerr << e.what() << "\n";
        return 1;
    }
    
    // set to run mode
    if (run_mode) {
        output_file = getDefaultOutputName(input_file, false);
        if (std::filesystem::exists(output_file)) {
            std::string run_cmd = output_file;
            return std::system(run_cmd.c_str());
        }
        no_window = false;
    }
    
    // set default output name if needed
    if (!output_specified && !run_mode) {
        output_file = getDefaultOutputName(input_file, compile_only);
    }
    
    try {
        // read the source file
        std::string source = readFile(input_file);
        
        // run through the compilation pipeline
        // lexer -> parser -> codegen
        Summit::Lexer lexer(source);
        auto tokens = lexer.tokenize();
        
        Summit::Parser parser(std::move(tokens));
        auto ast = parser.parse();
        
        Summit::CodeGenerator codegen;
        codegen.generate(*ast);
        
        // emit either object file or executable
        if (compile_only) {
            codegen.emitObjectFile(output_file);
            std::cout << "Object file created: " << output_file << std::endl;
        } else {
            std::vector<std::string> libs;
            if (!no_stdlib) {
                // TODO: this path is kinda hardcoded, should probably be more flexible
                libs.push_back("build/linux/x86_64/release/libsummit_std.a");
            }
            
            codegen.emitExecutable(output_file, libs, no_stdlib, no_window);
            
            if (run_mode) {
                std::string run_cmd = output_file;
                int result = std::system(run_cmd.c_str());
                return result;
            }
        }
        
        return 0;
        
    } catch (const llvm::Error& e) {
        // llvm errors need special handling
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