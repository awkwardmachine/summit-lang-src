#!/bin/bash
OS_TYPE="unknown"
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" || "$OSTYPE" == "cygwin" ]]; then
    OS_TYPE="windows"
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    OS_TYPE="linux"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    OS_TYPE="macos"
fi

echo "Detected OS: $OS_TYPE"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
if [[ "$SCRIPT_DIR" == */stdlib ]]; then
    cd "$SCRIPT_DIR/.."
fi

STDLIB_DIR="src/stdlib"
BUILD_DIR="build-stdlib/stdlib"
LIB_DIR="lib"
INCLUDE_DIR="include"

mkdir -p "$BUILD_DIR"
mkdir -p "$LIB_DIR"
mkdir -p "$INCLUDE_DIR"

echo "Building Summit standard library..."
echo "Working directory: $(pwd)"

if [ ! -f "$STDLIB_DIR/standard_library.cpp" ]; then
    echo "Error: Cannot find $STDLIB_DIR/standard_library.cpp"
    echo "Available files in $STDLIB_DIR/:"
    ls -la "$STDLIB_DIR/" 2>/dev/null || echo "Directory not found"
    exit 1
fi

CPP_SOURCES=($(find "$STDLIB_DIR" -name "*.cpp"))
if [ ${#CPP_SOURCES[@]} -eq 0 ]; then
    echo "Error: No C++ source files found in $STDLIB_DIR/"
    echo "Available files:"
    ls -la "$STDLIB_DIR/" 2>/dev/null
    exit 1
fi

echo "Found ${#CPP_SOURCES[@]} C++ source files:"
for source in "${CPP_SOURCES[@]}"; do
    echo "  - $source"
done

OBJECT_FILES=()

for source_file in "${CPP_SOURCES[@]}"; do
    base_name=$(basename "$source_file" .cpp)
   
    if [ "$OS_TYPE" = "windows" ]; then
        object_file="$BUILD_DIR/$base_name.obj"
    else
        object_file="$BUILD_DIR/$base_name.o"
    fi
   
    echo "Compiling $source_file..."
   
    if [ "$OS_TYPE" = "windows" ]; then
        clang++ -std=c++17 -c -Os -ffunction-sections -fdata-sections -DNDEBUG -I"$INCLUDE_DIR" -I"src" "$source_file" -o "$object_file"
    else
        clang++ -std=c++17 -c -Os -fPIC -ffunction-sections -fdata-sections -DNDEBUG -I"$INCLUDE_DIR" -I"src" "$source_file" -o "$object_file"
    fi
   
    if [ $? -ne 0 ]; then
        echo "Failed to compile $source_file"
        exit 1
    fi
    OBJECT_FILES+=("$object_file")
done

echo "Creating static library from ${#OBJECT_FILES[@]} object files..."

if [ "$OS_TYPE" = "windows" ]; then
    if command -v llvm-ar >/dev/null 2>&1; then
        echo "Using llvm-ar..."
        llvm-ar rcs "$LIB_DIR/libsummit_std.a" "${OBJECT_FILES[@]}"
    elif command -v ar >/dev/null 2>&1; then
        echo "Using ar..."
        ar rcs "$LIB_DIR/libsummit_std.a" "${OBJECT_FILES[@]}"
    elif command -v lib.exe >/dev/null 2>&1; then
        echo "Using lib.exe..."
        lib.exe /OUT:"$LIB_DIR/libsummit_std.lib" "${OBJECT_FILES[@]}" 2>/dev/null
        if command -v ar >/dev/null 2>&1; then
            ar rcs "$LIB_DIR/libsummit_std.a" "${OBJECT_FILES[@]}"
        fi
    else
        echo "Error: No archiver found (llvm-ar, ar, or lib.exe)"
        exit 1
    fi
   
    if [ $? -ne 0 ]; then
        echo "Failed to create static library"
        exit 1
    fi
    
    echo "Standard library built successfully:"
    if [ -f "$LIB_DIR/libsummit_std.a" ]; then
        echo "  Static: $LIB_DIR/libsummit_std.a"
    fi
    if [ -f "$LIB_DIR/libsummit_std.lib" ]; then
        echo "  Static: $LIB_DIR/libsummit_std.lib"
    fi
    
    if clang++ -shared "${OBJECT_FILES[@]}" -o "$LIB_DIR/libsummit_std.dll" \
        -Wl,--gc-sections,--strip-all,-s \
        -Os -ffunction-sections -fdata-sections \
        -static-libgcc -static-libstdc++ 2>/dev/null; then
        
        echo "  Dynamic: $LIB_DIR/libsummit_std.dll"
        
        if [ -f "$LIB_DIR/libsummit_std.dll" ]; then
            SIZE=$(stat -c%s "$LIB_DIR/libsummit_std.dll" 2>/dev/null || stat -f%z "$LIB_DIR/libsummit_std.dll" 2>/dev/null)
            SIZE_KB=$((SIZE / 1024))
            echo "  DLL size: ${SIZE_KB} KB"
        fi

        if command -v strip >/dev/null 2>&1; then
            strip --strip-all "$LIB_DIR/libsummit_std.dll" 2>/dev/null || true
            if [ -f "$LIB_DIR/libsummit_std.dll" ]; then
                SIZE=$(stat -c%s "$LIB_DIR/libsummit_std.dll" 2>/dev/null || stat -f%z "$LIB_DIR/libsummit_std.dll" 2>/dev/null)
                SIZE_KB=$((SIZE / 1024))
                echo "  Stripped DLL size: ${SIZE_KB} KB"
            fi
        fi
        
        if command -v dlltool >/dev/null 2>&1; then
            if [ ! -f "$STDLIB_DIR/libsummit_std.def" ]; then
                echo "EXPORTS" > "$LIB_DIR/libsummit_std.def"
                echo "  summit_io_println" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_io_print" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_io_println_int" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_io_print_int" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_io_println_uint" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_io_print_uint" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_io_println_f32" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_io_print_f32" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_io_println_f64" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_io_print_f64" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_io_println_bool" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_io_print_bool" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_math_sqrt" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_math_pow" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_math_sin" >> "$LIB_DIR/libsummit_std.def"
                echo "  summit_math_cos" >> "$LIB_DIR/libsummit_std.def"
            fi
            
            dlltool --dllname libsummit_std.dll \
                    --def "$LIB_DIR/libsummit_std.def" \
                    --output-lib "$LIB_DIR/libsummit_std.dll.a" 2>/dev/null && \
                    echo "  Import library: $LIB_DIR/libsummit_std.dll.a"
        fi
        
        FULL_LIB_PATH="$(cd "$LIB_DIR" && pwd)"
        
        if command -v cygpath &> /dev/null; then
            WINDOWS_LIB_PATH=$(cygpath -w "$FULL_LIB_PATH")
        else
            WINDOWS_LIB_PATH=$(echo "$FULL_LIB_PATH" | sed 's|^/\([a-z]\)/|\1:/|' | sed 's|/|\\|g')
        fi
        
        echo ""
        echo "Setting up SUMMIT_LIB environment variable..."
        echo "Library Location: $WINDOWS_LIB_PATH"

        echo "Setting SUMMIT_LIB permanently..."
        setx SUMMIT_LIB "$WINDOWS_LIB_PATH" > /dev/null 2>&1 || echo "Note: Could not set permanent environment variable"
        
        export SUMMIT_LIB="$FULL_LIB_PATH"
        
        echo ""
        echo "SUMMIT_LIB environment variable set!"
        echo "  SUMMIT_LIB=$WINDOWS_LIB_PATH"
        echo ""

    else
        echo "Failed to create dynamic library (continuing with static only)"
    fi
else
    if command -v llvm-ar >/dev/null 2>&1; then
        llvm-ar rcs "$LIB_DIR/libsummit_std.a" "${OBJECT_FILES[@]}"
    else
        ar rcs "$LIB_DIR/libsummit_std.a" "${OBJECT_FILES[@]}"
    fi
    
    if [ $? -ne 0 ]; then
        echo "Failed to create static library"
        exit 1
    fi
   
    echo "Standard library built successfully:"
    echo "  Static: $LIB_DIR/libsummit_std.a"
   
    echo "Creating shared library..."
    if [ "$OS_TYPE" = "macos" ]; then
        if clang++ -shared -fPIC "${OBJECT_FILES[@]}" -o "$LIB_DIR/libsummit_std.dylib" \
            -Os -Wl,-dead_strip 2>/dev/null; then
            echo "  Shared: $LIB_DIR/libsummit_std.dylib"
            
            strip -x "$LIB_DIR/libsummit_std.dylib" 2>/dev/null || true

            FULL_LIB_PATH="$(cd "$LIB_DIR" && pwd)"

            SHELL_PROFILE=""
            if [ -f "$HOME/.zshrc" ]; then
                SHELL_PROFILE="$HOME/.zshrc"
            elif [ -f "$HOME/.bashrc" ]; then
                SHELL_PROFILE="$HOME/.bashrc"
            elif [ -f "$HOME/.bash_profile" ]; then
                SHELL_PROFILE="$HOME/.bash_profile"
            fi
            
            if [ -n "$SHELL_PROFILE" ]; then
                if ! grep -q "SUMMIT_LIB" "$SHELL_PROFILE" 2>/dev/null; then
                    echo "" >> "$SHELL_PROFILE"
                    echo "export SUMMIT_LIB=\"$FULL_LIB_PATH\"" >> "$SHELL_PROFILE"
                    echo ""
                    echo "SUMMIT_LIB environment variable added to $SHELL_PROFILE"
                else
                    echo ""
                    echo "SUMMIT_LIB environment variable already in $SHELL_PROFILE"
                fi
            fi
            
            export SUMMIT_LIB="$FULL_LIB_PATH"
            
            echo "  SUMMIT_LIB=$SUMMIT_LIB"
            echo ""
            echo "Please restart your terminal or run: source $SHELL_PROFILE"
        else
            echo "Failed to create shared library (continuing with static only)"
        fi
    else
        if clang++ -shared -fPIC "${OBJECT_FILES[@]}" -o "$LIB_DIR/libsummit_std.so" \
            -Os -Wl,--gc-sections,-s 2>/dev/null; then
            echo "  Shared: $LIB_DIR/libsummit_std.so"
            
            strip --strip-all "$LIB_DIR/libsummit_std.so" 2>/dev/null || true
            
            FULL_LIB_PATH="$(cd "$LIB_DIR" && pwd)"
            
            SHELL_PROFILE=""
            if [ -f "$HOME/.bashrc" ]; then
                SHELL_PROFILE="$HOME/.bashrc"
            elif [ -f "$HOME/.bash_profile" ]; then
                SHELL_PROFILE="$HOME/.bash_profile"
            elif [ -f "$HOME/.profile" ]; then
                SHELL_PROFILE="$HOME/.profile"
            fi
            
            if [ -n "$SHELL_PROFILE" ]; then
                if ! grep -q "SUMMIT_LIB" "$SHELL_PROFILE" 2>/dev/null; then
                    echo "" >> "$SHELL_PROFILE"
                    echo "export SUMMIT_LIB=\"$FULL_LIB_PATH\"" >> "$SHELL_PROFILE"
                    echo ""
                    echo "SUMMIT_LIB environment variable added to $SHELL_PROFILE"
                else
                    echo ""
                    echo "SUMMIT_LIB environment variable already in $SHELL_PROFILE"
                fi
            fi
            
            export SUMMIT_LIB="$FULL_LIB_PATH"
            
            echo "  SUMMIT_LIB=$SUMMIT_LIB"
            echo ""
            echo "Please restart your terminal or run: source $SHELL_PROFILE"
        else
            echo "Failed to create shared library (continuing with static only)"
        fi
    fi
fi

echo ""
echo "Library contents:"
if command -v llvm-ar >/dev/null 2>&1; then
    llvm-ar t "$LIB_DIR/libsummit_std.a" 2>/dev/null || echo "Could not list library contents"
elif command -v ar >/dev/null 2>&1; then
    ar t "$LIB_DIR/libsummit_std.a" 2>/dev/null || echo "Could not list library contents"
fi

echo ""
echo "Build complete!"
echo "Output directory: $LIB_DIR/"
ls -lh "$LIB_DIR/" 2>/dev/null || echo "Could not list output directory"