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
    // string I/O functions
    SUMMIT_API void summit_io_println(const char* str);
    SUMMIT_API void summit_io_print(const char* str);
    
    // integer I/O functions
    SUMMIT_API void summit_io_println_int(int64_t value);
    SUMMIT_API void summit_io_print_int(int64_t value);
    
    SUMMIT_API void summit_io_println_uint(uint64_t value);
    SUMMIT_API void summit_io_print_uint(uint64_t value);
    
    // float I/O functions
    SUMMIT_API void summit_io_println_f32(float value);
    SUMMIT_API void summit_io_print_f32(float value);
    SUMMIT_API void summit_io_println_f64(double value);
    SUMMIT_API void summit_io_print_f64(double value);
    
    // boolean I/O functions
    SUMMIT_API void summit_io_println_bool(bool value);
    SUMMIT_API void summit_io_print_bool(bool value);
    
    // math functions
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