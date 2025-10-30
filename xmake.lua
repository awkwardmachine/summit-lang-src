set_project("summit")
set_version("0.1.0")
set_languages("c++20")
set_warnings("none")

add_rules("mode.release", "mode.debug")

target("summit")
    set_kind("binary")
    add_files("src/*.cpp")
    add_files("src/lexer/*.cpp")
    add_files("src/parser/*.cpp")
    add_files("src/codegen/*.cpp")
    add_includedirs("include", "include/lexer", "include/parser", "include/codegen", "include/stdlib")

    add_cxflags("-fexceptions")
    add_ldflags("-fexceptions")
    
    on_load(function (target)
        local llvm_cxxflags = os.iorun("llvm-config --cxxflags")
        local llvm_ldflags = os.iorun("llvm-config --ldflags --system-libs --libs core native bitwriter")
        
        if llvm_cxxflags and llvm_ldflags then
            for flag in llvm_cxxflags:gmatch("%S+") do
                if flag:startswith("-I") then
                    target:add("includedirs", flag:sub(3))
                else
                    target:add("cxxflags", flag)
                end
            end
            
            for flag in llvm_ldflags:gmatch("%S+") do
                if flag:startswith("-L") then
                    target:add("linkdirs", flag:sub(3))
                elseif flag:startswith("-l") then
                    target:add("links", flag:sub(3))
                else
                    target:add("ldflags", flag)
                end
            end
        else
            target:add("links", "LLVM-18")
        end
    end)
    
    if is_mode("release") then
        add_cxflags(
            "-Os",
            "-fdata-sections",
            "-ffunction-sections",
            "-DNDEBUG",
            "-fmerge-all-constants",
            "-fno-stack-protector",
            "-fno-math-errno",
            "-fno-ident",
            "-fno-plt",
            "-fno-semantic-interposition",
            "-march=native",
            "-flto",
            "-fvisibility=hidden", 
            "-fvisibility-inlines-hidden"
        )
        
        add_ldflags(
            "-Wl,--gc-sections",
            "-Wl,--as-needed",
            "-Wl,--strip-all",
            "-Wl,-z,noseparate-code",
            "-Wl,--no-export-dynamic",
            "-Wl,--hash-style=gnu",
            "-s",
            "-flto",
            "-Wl,-O3"
        )
        
        set_symbols("hidden")
        set_strip("all")
        set_optimize("smallest")
        
    elseif is_mode("debug") then
        add_cxflags("-g", "-O0")
        set_symbols("debug")
    end
    
    after_build(function (target)
        local size = os.filesize(target:targetfile())
        local size_kb = math.floor(size / 1024)
        print("Built Summit: " .. target:targetfile() .. " (" .. size_kb .. " KB)")
    end)