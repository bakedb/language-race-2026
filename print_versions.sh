#!/usr/bin/env bash

# Helper function
check_version() {
    local name="$1"
    shift
    local cmds=("$@")

    echo "=== $name ==="

    for cmd in "${cmds[@]}"; do
        if command -v $cmd >/dev/null 2>&1; then
            # Try common version flags
            for flag in "--version" "-version" "-v" "version"; do
                if $cmd $flag >/dev/null 2>&1; then
                    $cmd $flag
                    echo
                    return
                fi
            done

            # If none of the flags worked, just run the command
            $cmd
            echo
            return
        fi
    done

    echo "$name not installed."
    echo
}

# Core languages
check_version "Python" python python3
check_version "C Compiler (GCC)" gcc
check_version "C++ Compiler (G++)" g++
check_version "Ruby" ruby
check_version "Node.js (JavaScript)" node
check_version "TypeScript" tsc
check_version "Go" go
check_version "Rust" rustc
check_version "R" R
check_version "Julia" julia
check_version "Kotlin" kotlinc
check_version "Swift" swift
check_version "Zig" zig
check_version "D (LDC)" ldc2 ldc
check_version "Odin" odin
check_version "Crystal" crystal
check_version "C# / .NET" dotnet
check_version "Haskell (GHC)" ghc
check_version "Elixir" elixir
check_version "F#" fsharpc
check_version "Lua" lua lua5.4 lua5.3
check_version "PHP" php
check_version "Perl" perl
check_version "COBOL (GnuCOBOL)" cobc
check_version "Brainfuck (bf)" bf
check_version "Bash" bash
