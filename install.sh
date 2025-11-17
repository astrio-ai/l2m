#!/bin/bash
# L2M Installer Script
# This script downloads and installs L2M from GitHub Releases or PyPI

set -euo pipefail

# Configuration
REPO="astrio-ai/l2m"
INSTALL_DIR="${HOME}/.local/bin"
BINARY_NAME="l2m"

# Detect OS and architecture
detect_platform() {
    local os=""
    local arch=""
    
    case "$(uname -s)" in
        Linux*)     os="linux" ;;
        Darwin*)    os="darwin" ;;
        MINGW*|MSYS*|CYGWIN*)
                    os="windows" ;;
        *)          echo "Error: Unsupported operating system: $(uname -s)" >&2
                    exit 1 ;;
    esac
    
    case "$(uname -m)" in
        x86_64|amd64)   arch="x64" ;;
        arm64|aarch64)  arch="arm64" ;;
        *)              echo "Error: Unsupported architecture: $(uname -m)" >&2
                        exit 1 ;;
    esac
    
    echo "${os}-${arch}"
}

# Get latest release version from GitHub API
get_latest_version() {
    local version
    version=$(curl -s "https://api.github.com/repos/${REPO}/releases/latest" 2>/dev/null | grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/' || echo "")
    
    if [ -z "$version" ]; then
        # Fallback: try to get version from PyPI
        version=$(curl -s "https://pypi.org/pypi/l2m/json" 2>/dev/null | grep '"version":' | head -1 | sed -E 's/.*"([^"]+)".*/\1/' || echo "")
    fi
    
    echo "$version"
}

# Install via pip
install_via_pip() {
    if ! command -v pip3 &> /dev/null && ! command -v pip &> /dev/null; then
        return 1
    fi
    
    local pip_cmd="pip3"
    if ! command -v pip3 &> /dev/null; then
        pip_cmd="pip"
    fi
    
    # Install using pip
    $pip_cmd install --user l2m >/dev/null 2>&1 || return 1
    
    # Find where pip installed the binary
    local bin_path
    bin_path=$($pip_cmd show -f l2m 2>/dev/null | grep "Location:" | awk '{print $2}' || echo "")
    
    if [ -z "$bin_path" ]; then
        # Try alternative method
        bin_path=$(python3 -m site --user-base 2>/dev/null || echo "")
        if [ -n "$bin_path" ]; then
            bin_path="${bin_path}/bin/l2m"
        fi
    else
        # pip installs scripts to bin directory in user site-packages
        local user_base
        user_base=$(python3 -m site --user-base 2>/dev/null || echo "")
        if [ -n "$user_base" ]; then
            bin_path="${user_base}/bin/l2m"
        fi
    fi
    
    # Verify binary exists
    if [ -f "$bin_path" ] || command -v l2m &> /dev/null; then
        echo "pip (user install)"
        return 0
    fi
    
    return 1
}

# Download and extract binary
install_via_binary() {
    local platform=$1
    local version=$2
    local download_url="https://github.com/${REPO}/releases/download/${version}/l2m-${platform}.zip"
    local temp_dir=$(mktemp -d)
    local zip_file="${temp_dir}/l2m.zip"
    
    # Download the zip file
    if ! curl -fsSL -o "$zip_file" "$download_url" 2>/dev/null; then
        rm -rf "$temp_dir"
        return 1
    fi
    
    # Extract the zip file
    unzip -q "$zip_file" -d "$temp_dir" 2>/dev/null || {
        rm -rf "$temp_dir"
        return 1
    }
    
    # Create install directory if it doesn't exist
    mkdir -p "$INSTALL_DIR"
    
    # Find the binary (handle nested structure from release workflow)
    local binary_path=""
    local extracted_dir="${temp_dir}/l2m-${platform}"
    
    if [ -d "$extracted_dir" ]; then
        if [ -f "${extracted_dir}/l2m" ]; then
            binary_path="${extracted_dir}/l2m"
        elif [ -f "${extracted_dir}/l2m.exe" ]; then
            binary_path="${extracted_dir}/l2m.exe"
            BINARY_NAME="l2m.exe"
        fi
    fi
    
    # Fallback: check root of temp directory
    if [ -z "$binary_path" ]; then
        if [ -f "${temp_dir}/l2m" ]; then
            binary_path="${temp_dir}/l2m"
        elif [ -f "${temp_dir}/l2m.exe" ]; then
            binary_path="${temp_dir}/l2m.exe"
            BINARY_NAME="l2m.exe"
        fi
    fi
    
    if [ -z "$binary_path" ]; then
        rm -rf "$temp_dir"
        return 1
    fi
    
    # Make binary executable (for Unix-like systems)
    if [[ "$platform" != *"windows"* ]]; then
        chmod +x "$binary_path"
    fi
    
    # Install binary
    cp "$binary_path" "${INSTALL_DIR}/${BINARY_NAME}"
    
    # Cleanup
    rm -rf "$temp_dir"
    
    # Verify installation
    if [ -f "${INSTALL_DIR}/${BINARY_NAME}" ]; then
        echo "${INSTALL_DIR}/${BINARY_NAME}"
        return 0
    fi
    
    return 1
}

# Add to PATH in shell config (silently)
add_to_path() {
    local shell_config=""
    local path_line=""
    
    case "$SHELL" in
        */zsh)
            shell_config="${HOME}/.zshrc"
            path_line='export PATH="${HOME}/.local/bin:$PATH"'
            ;;
        */bash)
            shell_config="${HOME}/.bashrc"
            path_line='export PATH="${HOME}/.local/bin:$PATH"'
            ;;
        */fish)
            shell_config="${HOME}/.config/fish/config.fish"
            path_line='set -gx PATH "${HOME}/.local/bin" $PATH'
            ;;
        *)
            shell_config="${HOME}/.profile"
            path_line='export PATH="${HOME}/.local/bin:$PATH"'
            ;;
    esac
    
    # Check if PATH already includes .local/bin
    if echo "$PATH" | grep -q "${HOME}/.local/bin"; then
        return
    fi
    
    # Check if the path line already exists in config
    if [ -f "$shell_config" ] && grep -q ".local/bin" "$shell_config"; then
        return
    fi
    
    # Add to shell config
    echo "" >> "$shell_config"
    echo "# L2M binary location" >> "$shell_config"
    echo "$path_line" >> "$shell_config"
}

# Main installation flow
main() {
    echo "Setting up L2M..."
    echo ""
    
    # Check dependencies
    if ! command -v curl &> /dev/null; then
        echo "Error: curl is required but not installed" >&2
        exit 1
    fi
    
    # Get version
    local version
    version=$(get_latest_version)
    
    # Try binary installation first
    local platform
    local install_location=""
    
    platform=$(detect_platform 2>/dev/null || echo "")
    
    if [ -n "$platform" ]; then
        # Check if unzip is available for binary installation
        if command -v unzip &> /dev/null; then
            install_location=$(install_via_binary "$platform" "$version" 2>/dev/null || echo "")
        fi
    fi
    
    # Fallback to pip if binary installation failed
    if [ -z "$install_location" ]; then
        install_location=$(install_via_pip 2>/dev/null || echo "")
    fi
    
    if [ -z "$install_location" ]; then
        echo "Error: Failed to install L2M" >&2
        exit 1
    fi
    
    # Add to PATH
    add_to_path
    
    # Get actual installed version if possible
    local installed_version=""
    # Try to get version from pip if installed via pip
    if [[ "$install_location" == *"pip"* ]]; then
        local pip_cmd="pip3"
        if ! command -v pip3 &> /dev/null; then
            pip_cmd="pip"
        fi
        installed_version=$($pip_cmd show l2m 2>/dev/null | grep "^Version:" | awk '{print $2}' || echo "")
    fi
    
    # Fallback: try --version flag
    if [ -z "$installed_version" ]; then
        if command -v l2m &> /dev/null; then
            installed_version=$(l2m --version 2>/dev/null | head -1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' || echo "")
        elif [ -f "$install_location" ] && [ "$install_location" != "pip (user install)" ]; then
            installed_version=$("$install_location" --version 2>/dev/null | head -1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' || echo "")
        fi
    fi
    
    # Success message
    echo ""
    echo "L2M successfully installed!"
    if [ -n "$installed_version" ]; then
        echo "Version: $installed_version"
    elif [ -n "$version" ]; then
        echo "Version: $version"
    fi
    echo "Location: $install_location"
    echo ""
    echo "Next: Run l2m --help to get started"
    echo ""
    echo "Installation complete!"
}

# Run main function
main "$@"
