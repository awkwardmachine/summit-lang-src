#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${MSYSTEM:-}" ]]; then
    echo "Please run this script inside an MSYS2 shell (UCRT64, CLANG64, or MINGW64)."
    exit 1
fi

if command -v clang++ >/dev/null 2>&1; then
    CXX=$(command -v clang++)
    echo "Using compiler: $CXX"
elif command -v g++ >/dev/null 2>&1; then
    CXX=$(command -v g++)
    echo "Using compiler: $CXX"
else
    echo "No C++ compiler found (clang++ or g++)."
    exit 1
fi

if command -v llvm-config >/dev/null 2>&1; then
    LLVM_CXXFLAGS=$(llvm-config --cxxflags | sed 's/-fno-exceptions//g')
    LLVM_LDFLAGS=$(llvm-config --ldflags --libs core orcjit native all)
else
    echo "llvm-config not found. Install MSYS2 package 'mingw-w64-ucrt-x86_64-llvm'."
    exit 1
fi

SRC_DIR=src
BUILD_DIR=build-windows
BIN_DIR="$BUILD_DIR/bin"
TARGET="$BIN_DIR/summit.exe"

CXXFLAGS="-std=c++20 -O2 \
-fdata-sections -ffunction-sections -DNDEBUG \
-Isrc -Iinclude -Iinclude/codegen -Iinclude/stdlib -Iinclude/lexer -Iinclude/parser \
-I/ucrt64/include \
$LLVM_CXXFLAGS \
-fmerge-all-constants -fno-stack-protector -fno-math-errno -fno-ident -w \
-D__USE_MINGW_ANSI_STDIO=1"

LDFLAGS="$LLVM_LDFLAGS -ltommath -lstdc++ -lstdc++fs -Wl,--gc-sections,--as-needed,--strip-all,-s"


run() { echo "+ $*"; "$@"; }

build() {
    run mkdir -p "$BIN_DIR"
    echo "Starting compilation..."
    START_TIME=$(date +%s)

    mapfile -d '' SRCS < <(find "$SRC_DIR" -name '*.cpp' -print0)
    JOBS=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
    echo "Detected $JOBS CPU cores. Compiling in parallel..."

    export SRC_DIR BUILD_DIR CXX CXXFLAGS

    for src in "${SRCS[@]}"; do
        obj="$BUILD_DIR/${src#$SRC_DIR/}"
        obj="${obj%.cpp}.o"
        mkdir -p "$(dirname "$obj")"
    done

    printf "%s\0" "${SRCS[@]}" | \
    xargs -0 -n1 -P"$JOBS" bash -c '
        src="$0"
        obj="$BUILD_DIR/${src#$SRC_DIR/}"
        obj="${obj%.cpp}.o"
        echo "Compiling $src -> $obj"
        "$CXX" $CXXFLAGS -c "$src" -o "$obj"
    '

    OBJS=$(find "$BUILD_DIR" -name '*.o')
    if [[ -z "$OBJS" ]]; then
        echo "No object files found to link."
        exit 1
    fi

    echo "Linking..."
    run "$CXX" $OBJS $LDFLAGS -o "$TARGET"

    if command -v strip >/dev/null 2>&1; then
        run strip --strip-all "$TARGET" 2>/dev/null || true
    fi
    if command -v upx >/dev/null 2>&1; then
        run upx --lzma --no-progress "$TARGET" >/dev/null 2>&1 || true
    fi

    END_TIME=$(date +%s)
    echo "Done: $TARGET"
    echo "Compilation time: $((END_TIME - START_TIME))s"
}

clean() {
    run rm -rf "$BUILD_DIR"
    echo "Cleaned build directory."
}

case "${1:-build}" in
    build) build ;;
    clean) clean ;;
    *) echo "Usage: $0 [build|clean]"; exit 1 ;;
esac
