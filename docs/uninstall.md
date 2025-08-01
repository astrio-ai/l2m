# 🗑️ Uninstall Guide

This guide explains how to completely remove Legacy2Modern CLI from your system, regardless of how it was installed.

## 📋 Uninstall Methods

### Method 1: Homebrew Installation

If you installed via Homebrew:

```bash
# Uninstall the CLI
brew uninstall legacy2modern-cli

# Verify removal
which legacy2modern
which l2m
```

### Method 2: Pip Installation

If you installed via pip:

```bash
# Uninstall the CLI package
pip uninstall legacy2modern-cli

# Or if installed in editable mode
pip uninstall -e .

# Verify removal
which legacy2modern
which l2m
```

### Method 3: Manual Installation

If you installed manually:

```bash
# Remove the installed scripts
sudo rm -f /usr/local/bin/legacy2modern
sudo rm -f /usr/local/bin/l2m

# Or if installed in user directory
rm -f ~/.local/bin/legacy2modern
rm -f ~/.local/bin/l2m
```

### Method 4: Direct Run (No Installation)

If you were running directly without installation, simply delete the cloned repository:

```bash
# Navigate to the parent directory
cd ..

# Remove the entire project directory
rm -rf legacy2modern-cli
```

## 🧹 Clean Up Dependencies

### Remove Python Dependencies

```bash
# List installed packages related to the project
pip list | grep -i legacy

# Remove specific dependencies (optional)
pip uninstall rich click typer prompt_toolkit pygments
```

### Remove Configuration Files

```bash
# Remove any configuration files (if they exist)
rm -f ~/.config/legacy2modern/config.json
rm -f ~/.legacy2modern/config.json
```

### Remove Cache Files

```bash
# Remove Python cache files
find . -name "*.pyc" -delete
find . -name "__pycache__" -type d -exec rm -rf {} +

# Remove pip cache (optional)
pip cache purge
```

## 🔍 Verification

After uninstalling, verify that the CLI is completely removed:

```bash
# Check if commands still exist
which legacy2modern
which l2m

# Try to run the commands (should fail)
legacy2modern --help
l2m --help

# Check pip for any remaining packages
pip list | grep -i legacy
```

## 🚨 Troubleshooting

### If Commands Still Work

If the commands still work after uninstalling:

```bash
# Check where the commands are located
which legacy2modern
which l2m

# Check your PATH
echo $PATH

# Remove from PATH if needed
# Edit your shell profile (.bashrc, .zshrc, etc.)
```

### If Dependencies Remain

```bash
# Force remove all related packages
pip uninstall -y legacy2modern-cli rich click typer prompt_toolkit pygments

# Check for any remaining files
find /usr/local -name "*legacy*" 2>/dev/null
find ~/.local -name "*legacy*" 2>/dev/null
```

### If Homebrew Issues

```bash
# Clean up Homebrew
brew cleanup

# Check for any orphaned files
brew doctor
```

## 📝 Complete Removal Checklist

- [ ] Uninstalled via Homebrew or pip
- [ ] Removed CLI commands from PATH
- [ ] Cleaned up Python dependencies
- [ ] Removed configuration files
- [ ] Cleared cache files
- [ ] Verified commands no longer work
- [ ] Checked for orphaned files

## 🔄 Reinstall

If you want to reinstall later:

```bash
# Homebrew (recommended)
brew install legacy2modern-cli

# Or manual installation
git clone https://github.com/astrio-ai/legacy2modern.git
cd legacy2modern
./install.sh
```

## 📞 Support

If you encounter issues during uninstallation:

1. Check the [GitHub Issues](https://github.com/astrio-ai/legacy2modern/issues)
2. Join our [Discord](https://discord.gg/2BVwAUzW)
3. Contact us at [naingoolwin.astrio@gmail.com](mailto:naingoolwin.astrio@gmail.com) 