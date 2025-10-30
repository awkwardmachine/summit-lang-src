#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cstdint>

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
    printf("%lld\n", (long long)value);
    fflush(stdout);
}

void summit_io_print_int(int64_t value) {
    printf("%lld", (long long)value);
    fflush(stdout);
}

void summit_io_println_uint(uint64_t value) {
    printf("%llu\n", (unsigned long long)value);
    fflush(stdout);
}

void summit_io_print_uint(uint64_t value) {
    printf("%llu", (unsigned long long)value);
    fflush(stdout);
}

// float I/O functions
void summit_io_println_f32(float value) {
    if (value == floor(value)) {
        printf("%.1f\n", value);
    } else {
        printf("%.7g\n", value);
    }
    fflush(stdout);
}

void summit_io_print_f32(float value) {
    if (value == floor(value)) {
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

}

namespace Summit {
namespace stdlib {
void initialize() {
    
}
}
}