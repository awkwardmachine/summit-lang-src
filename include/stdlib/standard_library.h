#pragma once
#include <cstdint>

#ifdef _WIN32
    #ifdef SUMMIT_STD_EXPORT
        #define SUMMIT_API __declspec(dllexport)
    #else
        #define SUMMIT_API __declspec(dllimport)
    #endif
#else
    #define SUMMIT_API
#endif

extern "C" {
    // String I/O functions
    SUMMIT_API void summit_io_println(const char* str);
    SUMMIT_API void summit_io_print(const char* str);
    
    // Integer I/O functions
    SUMMIT_API void summit_io_println_int(int64_t value);
    SUMMIT_API void summit_io_print_int(int64_t value);
    
    // Float I/O functions
    SUMMIT_API void summit_io_println_float(double value);
    SUMMIT_API void summit_io_print_float(double value);
    
    // Math functions
    SUMMIT_API double summit_math_sqrt(double x);
    SUMMIT_API double summit_math_pow(double x, double y);
    SUMMIT_API double summit_math_sin(double x);
    SUMMIT_API double summit_math_cos(double x);
}

namespace Summit {
namespace stdlib {
    
    void initialize();
    
}
}