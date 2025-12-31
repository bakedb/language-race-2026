#!/usr/bin/env bash

echo "Select your package manager:"
echo "1) apt (Debian/Ubuntu)"
echo "2) pacman (Arch)"
echo "3) dnf (Fedora/RHEL)"
read -rp "Enter number: " PM

case "$PM" in
    1) PKG="apt";   UPDATE="sudo apt update";   INSTALL="sudo apt install -y";   QUERY="apt-cache show";;
    2) PKG="pacman"; UPDATE="sudo pacman -Sy"; INSTALL="sudo pacman -S --noconfirm"; QUERY="pacman -Si";;
    3) PKG="dnf";   UPDATE="sudo dnf check-update"; INSTALL="sudo dnf install -y"; QUERY="dnf info";;
    *) echo "Invalid selection"; exit 1;;
esac

install_pkg() {
    local pkg="$1"
    echo "Checking for $pkg..."
    if $QUERY "$pkg" >/dev/null 2>&1; then
        echo "Installing $pkg..."
        $INSTALL "$pkg"
    else
        echo "Package $pkg not found for $PKG — skipping."
    fi
}

echo "Updating package lists..."
$UPDATE

# Core languages
install_pkg python3
install_pkg gcc
install_pkg g++
install_pkg ruby
install_pkg nodejs
install_pkg golang
install_pkg r-base
install_pkg php
install_pkg perl
install_pkg lua5.4
install_pkg elixir
install_pkg ghc
install_pkg cabal-install
install_pkg ldc
install_pkg gnucobol
install_pkg crystal
install_pkg zig

# Java
install_pkg openjdk-21-jdk
install_pkg openjdk-17-jdk

# Brainfuck interpreter
install_pkg bf

# Rust (rustup)
if ! command -v rustc >/dev/null; then
    echo "Installing Rust via rustup..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
else
    echo "Rust already installed."
fi

# Typescript (npm)
if command -v npm >/dev/null; then
    echo "Installing TypeScript via npm..."
    sudo npm install -g typescript
else
    echo "npm not found — skipping TypeScript."
fi

# Kotlin (sdkman)
if ! command -v kotlinc >/dev/null; then
    echo "Installing Kotlin via SDKMAN..."
    curl -s "https://get.sdkman.io" | bash
    source "$HOME/.sdkman/bin/sdkman-init.sh"
    sdk install kotlin
else
    echo "Kotlin already installed."
fi

# .NET SDK (C# / F#)
if ! command -v dotnet >/dev/null; then
    echo ".NET SDK not found — installing..."
    case "$PKG" in
        apt)
            wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
            sudo dpkg -i packages-microsoft-prod.deb
            sudo apt update
            sudo apt install -y dotnet-sdk-8.0
            ;;
        dnf)
            sudo dnf install -y dotnet-sdk-8.0
            ;;
        pacman)
            echo ".NET SDK must be installed from AUR on Arch."
            ;;
    esac
else
    echo ".NET SDK already installed."
fi

# Nim
if ! command -v nim >/dev/null; then
    echo "Installing Nim via choosenim..."
    curl https://nim-lang.org/choosenim/init.sh -sSf | sh -s -- -y
else
    echo "Nim already installed."
fi

# Odin (manual)
if ! command -v odin >/dev/null; then
    echo "Odin is not available in most repos."
    echo "Download manually from: https://github.com/odin-lang/Odin/releases"
fi

# Swift (manual)
if ! command -v swift >/dev/null; then
    echo "Swift requires manual installation on Linux."
    echo "Download from: https://swift.org/download/"
fi

# Julia (manual fallback)
if ! command -v julia >/dev/null; then
    echo "Julia not found in repos — install manually:"
    echo "https://julialang.org/downloads/"
fi

echo "Installation process complete."
