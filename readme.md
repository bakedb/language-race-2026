# The Great Language Race of 2026
> (I actually started on 31 December 2025, but nobody cares)

Welcome to the Great Language Race of 2026, in which 28 different programming languages battle it out in a variety of tasks to see which one is the fastest and most efficient of them all!

# Tasks
Every language is given the same 6 tasks:
* Calculate a predetermined set of 100 math equations stored in different files: 20 in a .txt, 20 in a .md, 20 in a .json, 20 in a .yaml, and 20 in a raw file with no extension.
* Calculate 1000 random numbers from 0 to 999, store it in a .txt file, and calculate the mean of the set.
* Return beemoviescript.txt line by line, and then return the 3 most commonly used letters.
* On a locally hosted web server (can be found in /webserver/index.html), go to [location]:[port]/test-[i], where i equals a number 0-99, and compare the hash string found there with the string in compare.json. All 100 hashes should be compared, and the language should output if the comparison passed or failed in test-result.json.
* Hash every file in a 1000-file directory.
* Measure compile time for each program in the language, and calculate the mean. (The language will not be doing this one) For non-compiled languages, measure the startup time of each program (from when it is launched to when the "Hello, World!" message appears).

# Languages
The 28 contestants for the 2026 race are:
* Python
* C
* C++
* Nim
* Ruby
* Java
* Javascript
* Typescript
* Go
* Rust
* R
* Julia
* Kotlin
* Swift
* Zig
* D
* Odin
* Crystal
* C#
* Haskell
* Elixir
* F#
* Lua
* PHP
* Perl
* Bash (to prove why heavy computation in Bash is not a good idea)
* COBOL (because why not)
* Brainfuck (because why not)

# Rules
Every language will do the tests 10 times each, and the runs will be timed starting from once the executable/script is launched to once the program finishes. The mean in seconds of the 10 runs will be the language's score for that run. If the language fails a task, it gets no points for the round.

Every program in every language, after any import statements, should have "Hello, World!" output into the terminal.

All of my tests will be run on Arch Linux with Kernel version 6.18.2, but you can run your own tests on different distros, kernel versions, or operating systems.

# About These Results
These are the versions of each language that was used in these tests:
```
=== Python ===
Python 3.13.11

=== C Compiler (GCC) ===
gcc (GCC) 15.2.1 20251112
Copyright (C) 2025 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


=== C++ Compiler (G++) ===
g++ (GCC) 15.2.1 20251112
Copyright (C) 2025 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


=== Ruby ===
ruby 3.4.7 (2025-10-08 revision 7a5688e2a2) +PRISM [x86_64-linux]

=== Node.js (JavaScript) ===
v25.2.1

=== TypeScript ===
Version 5.9.3

=== Go ===
go version go1.25.5 X:nodwarf5 linux/amd64

=== Rust ===
Rust not installed.

=== R ===
R not installed.

=== Julia ===
Julia not installed.

=== Kotlin ===
Kotlin not installed.

=== Swift ===
Swift not installed.

=== Zig ===
0.15.2

=== D (LDC) ===
LDC - the LLVM D compiler (1.41.0):
  based on DMD v2.111.0 and LLVM 20.1.8
  built with LDC - the LLVM D compiler (1.41.0)
  Default target: x86_64-pc-linux-gnu
  Host CPU: znver2
  http://dlang.org - http://wiki.dlang.org/LDC


  Registered Targets:
    aarch64     - AArch64 (little endian)
    aarch64_32  - AArch64 (little endian ILP32)
    aarch64_be  - AArch64 (big endian)
    amdgcn      - AMD GCN GPUs
    arm         - ARM
    arm64       - ARM64 (little endian)
    arm64_32    - ARM64 (little endian ILP32)
    armeb       - ARM (big endian)
    avr         - Atmel AVR Microcontroller
    bpf         - BPF (host endian)
    bpfeb       - BPF (big endian)
    bpfel       - BPF (little endian)
    hexagon     - Hexagon
    lanai       - Lanai
    loongarch32 - 32-bit LoongArch
    loongarch64 - 64-bit LoongArch
    mips        - MIPS (32-bit big endian)
    mips64      - MIPS (64-bit big endian)
    mips64el    - MIPS (64-bit little endian)
    mipsel      - MIPS (32-bit little endian)
    msp430      - MSP430 [experimental]
    nvptx       - NVIDIA PTX 32-bit
    nvptx64     - NVIDIA PTX 64-bit
    ppc32       - PowerPC 32
    ppc32le     - PowerPC 32 LE
    ppc64       - PowerPC 64
    ppc64le     - PowerPC 64 LE
    r600        - AMD GPUs HD2XXX-HD6XXX
    riscv32     - 32-bit RISC-V
    riscv64     - 64-bit RISC-V
    sparc       - Sparc
    sparcel     - Sparc LE
    sparcv9     - Sparc V9
    spirv       - SPIR-V Logical
    spirv32     - SPIR-V 32-bit
    spirv64     - SPIR-V 64-bit
    systemz     - SystemZ
    thumb       - Thumb
    thumbeb     - Thumb (big endian)
    ve          - VE
    wasm32      - WebAssembly 32-bit
    wasm64      - WebAssembly 64-bit
    x86         - 32-bit X86: Pentium-Pro and above
    x86-64      - 64-bit X86: EM64T and AMD64
    xcore       - XCore

=== Odin ===
Odin not installed.

=== Crystal ===
Crystal 1.18.2 (2025-10-21)

LLVM: 21.1.6
Default target: x86_64-pc-linux-gnu

=== C# / .NET ===
10.0.100

=== Haskell (GHC) ===
The Glorious Glasgow Haskell Compilation System, version 9.6.6

=== Elixir ===
Erlang/OTP 28 [erts-16.2] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [jit:ns]

Elixir 1.19.4 (compiled with Erlang/OTP 28)

=== F# ===
F# not installed.

=== Lua ===
Lua 5.4.8  Copyright (C) 1994-2025 Lua.org, PUC-Rio

=== PHP ===
PHP 8.5.1 (cli) (built: Dec 16 2025 18:26:57) (NTS)
Copyright (c) The PHP Group
Zend Engine v4.5.1, Copyright (c) Zend Technologies
    with Zend OPcache v8.5.1, Copyright (c), by Zend Technologies

=== Perl ===

This is perl 5, version 42, subversion 0 (v5.42.0) built for x86_64-linux-thread-multi

Copyright 1987-2025, Larry Wall

Perl may be copied only under the terms of either the Artistic License or the
GNU General Public License, which may be found in the Perl 5 source kit.

Complete documentation for Perl, including FAQ lists, should be found on
this system using "man perl" or "perldoc perl".  If you have access to the
Internet, point your browser at https://www.perl.org/, the Perl Home Page.


=== COBOL (GnuCOBOL) ===
COBOL (GnuCOBOL) not installed.

=== Brainfuck (bf) ===
Brainfuck (bf) not installed.

=== Bash ===
GNU bash, version 5.3.9(1)-release (x86_64-pc-linux-gnu)
Copyright (C) 2025 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>

This is free software; you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
```
You can print these for yourself using print_versions.sh. Be sure to use ```chmod +x [script file]``` first.

Here are the specs of the system that I tested each language on:

```
CPU: AMD Ryzen 5 3600 (12 cores) @ 4.21 GHz

GPU: AMD Radeon RX 580 Series [Discrete]

Memory: 31.25 GiB

Swap: 4.00 GiB

Disk (/): 31.20 GiB - ext4

Disk (/home): 84.33 GiB - ext4

Disk (/run/media/user/[id]): 1.88 TiB - ext4

Locale: en_US.UTF-8

Shell: bash 5.3.9

DE: KDE Plasma 6.5.4

WM: KWin (Wayland)
```

