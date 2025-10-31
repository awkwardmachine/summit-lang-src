#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cstdint>

#ifdef _WIN32
#include <io.h>
#include <windows.h>

#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#else
#include <unistd.h>
#endif

extern "C" {

// string I/O functions
void summit_io_println(const char* str) {
    printf("%s\n", str);
    fflush(stdout);
}

void summit_io_print(const char* str) {
    printf("%s", str);
    fflush(stdout);
}

void summit_io_println_int(int64_t value) {
    #ifdef _WIN32
    printf("%I64d\n", value);
    #else
    printf("%lld\n", (long long)value);
    #endif
    fflush(stdout);
}

void summit_io_print_int(int64_t value) {
    #ifdef _WIN32
    printf("%I64d", value);
    #else
    printf("%lld", (long long)value);
    #endif
    fflush(stdout);
}

void summit_io_println_uint(uint64_t value) {
    #ifdef _WIN32
    printf("%I64u\n", value);
    #else
    printf("%llu\n", (unsigned long long)value);
    #endif
    fflush(stdout);
}

void summit_io_print_uint(uint64_t value) {
    #ifdef _WIN32
    printf("%I64u", value);
    #else
    printf("%llu", (unsigned long long)value);
    #endif
    fflush(stdout);
}

// float I/O functions
void summit_io_println_f32(float value) {
    if (value == floorf(value)) {
        printf("%.1f\n", value);
    } else {
        printf("%.7g\n", value);
    }
    fflush(stdout);
}

void summit_io_print_f32(float value) {
    if (value == floorf(value)) {
        printf("%.1f", value);
    } else {
        printf("%.7g", value);
    }
    fflush(stdout);
}

void summit_io_println_f64(double value) {
    if (value == floor(value)) {
        printf("%.1f\n", value);
    } else {
        printf("%.15g\n", value);
    }
    fflush(stdout);
}

void summit_io_print_f64(double value) {
    if (value == floor(value)) {
        printf("%.1f", value);
    } else {
        printf("%.15g", value);
    }
    fflush(stdout);
}

// boolean I/O functions
void summit_io_println_bool(bool value) {
    printf("%s\n", value ? "true" : "false");
    fflush(stdout);
}

void summit_io_print_bool(bool value) {
    printf("%s", value ? "true" : "false");
    fflush(stdout);
}

// math functions
double summit_math_sqrt(double x) {
    return sqrt(x);
}

double summit_math_pow(double x, double y) {
    return pow(x, y);
}

double summit_math_sin(double x) {
    return sin(x);
}

double summit_math_cos(double x) {
    return cos(x);
}

// Additional utility functions
double summit_math_floor(double x) {
    return floor(x);
}

double summit_math_ceil(double x) {
    return ceil(x);
}

double summit_math_abs(double x) {
    return fabs(x);
}

}

namespace Summit {
namespace stdlib {
void initialize() {
    #ifdef _WIN32
    #else
    #endif
}
}
}