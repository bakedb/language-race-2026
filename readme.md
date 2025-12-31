# The Great Language Race of 2026
> (I actually started on 31 December 2025, but nobody cares)

Welcome to the Great Language Race of 2026, in which 28 different programming languages battle it out in a variety of tasks to see which one is the fastest and most efficient of them all!

# Tasks
Every language is given the same 5 tasks:
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
