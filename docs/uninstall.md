# 🗑️ Uninstall Guide

This guide explains how to completely remove Legacy2Modern CLI from your system, regardless of how it was installed.

## 📋 Uninstall Methods

### Method 1: Homebrew Installation

If you installed via Homebrew:

```bash
# Uninstall the CLI
brew uninstall legacy2modern

# Verify removal
which legacy2modern
which l2m
```

### Method 2: Pip Installation

If you installed via pip:

```bash
# Uninstall the CLI package
pip uninstall legacy2modern

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
rm -rf legacy2modern
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

### Remove Output Files

```bash
# Remove generated output files
rm -rf output/
rm -f parser_output.json
```

## 🔍 Verification

After uninstalling, verify that the CLI is completely removed:

```bash
# Check if commands still exist
which legacy2modern
which l2m

# Check if Python can still import the package
python -c "import engine; print('Package still installed')" 2>/dev/null || echo "Package removed successfully"
```

## 🚨 Troubleshooting

### If Commands Still Work

If the `legacy2modern` command still works after uninstalling:

1. **Check PATH**: The script might still be in your PATH
   ```bash
   echo $PATH
   which legacy2modern
   which l2m
   ```

2. **Remove from PATH**: Remove any remaining references
   ```bash
   # Check your shell configuration files
   grep -r "legacy2modern" ~/.bashrc ~/.zshrc ~/.bash_profile ~/.zprofile 2>/dev/null
   ```

### If Package Still Importable

If Python can still import the package:

1. **Check pip list**: Verify the package is actually uninstalled
   ```bash
   pip list | grep legacy
   ```

2. **Check site-packages**: Look for remaining files
   ```bash
   python -c "import site; print(site.getsitepackages())"
   ```

3. **Force removal**: If necessary, manually remove files
   ```bash
   # Find and remove any remaining files
   find /usr/local/lib/python* -name "*legacy*" -delete
   find ~/.local/lib/python* -name "*legacy*" -delete
   ```

## 📝 Complete Cleanup Checklist

After uninstalling, ensure you've removed:

- [ ] CLI executable (`legacy2modern`)
- [ ] Python package (`legacy2modern`)
- [ ] Configuration files (`~/.config/legacy2modern/`)
- [ ] Cache files (`__pycache__`, `*.pyc`)
- [ ] Output files (`output/`, `parser_output.json`)
- [ ] Environment variables (if set manually)
- [ ] PATH modifications (if added manually)

## 🔄 Reinstallation

If you want to reinstall after uninstalling:

```bash
# PyPI (recommended)
pip install legacy2modern

# Homebrew (macOS)
brew install legacy2modern

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