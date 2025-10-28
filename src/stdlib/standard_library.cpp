#include <iostream>
#include <cmath>
#include <cstdint>
#include "stdlib/standard_library.h"

extern "C" {

// String I/O functions
void summit_io_println(const char* str) {
    std::cout << str << std::endl;
}

void summit_io_print(const char* str) {
    std::cout << str << std::flush;
}

// Integer I/O functions
void summit_io_println_int(int64_t value) {
    std::cout << value << std::endl;
}

void summit_io_print_int(int64_t value) {
    std::cout << value << std::flush;
}

// Float I/O functions
void summit_io_println_float(double value) {
    std::cout << value << std::endl;
}

void summit_io_print_float(double value) {
    std::cout << value << std::flush;
}

// Math functions
double summit_math_sqrt(double x) {
    return std::sqrt(x);
}

double summit_math_pow(double x, double y) {
    return std::pow(x, y);
}

double summit_math_sin(double x) {
    return std::sin(x);
}

double summit_math_cos(double x) {
    return std::cos(x);
}

}

namespace Summit {
namespace stdlib {

void initialize() {
    
}

}
}