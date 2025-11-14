# How GitHub Releases Work

## Overview

GitHub releases allow you to package and distribute your software with:
- **Tags**: Mark specific commits (e.g., `v0.1.0`)
- **Release Notes**: Describe what changed
- **Assets**: Attach binaries, installers, or source code archives

## How Multiple Assets Are Attached

### Method 1: GitHub Actions (Automated) ✅ Recommended

When you create a release, a GitHub Actions workflow automatically:
1. **Builds binaries** for multiple platforms (Linux, macOS, Windows)
2. **Packages them** into zip files
3. **Uploads them** to the release as assets

**Example workflow** (`.github/workflows/release.yml`):
- Triggers when a release is created
- Builds on multiple OS runners (ubuntu, macos, windows)
- Creates executables using PyInstaller (for Python) or similar
- Uploads each binary as a separate asset

### Method 2: Manual Upload

1. Build binaries locally for each platform
2. Go to GitHub Releases page
3. Click "Edit release"
4. Drag and drop files into the "Attach binaries" area
5. Save

### Method 3: GitHub CLI

```bash
# Build your binaries first, then:
gh release upload v0.1.0 dist/l2m-linux-x64.zip
gh release upload v0.1.0 dist/l2m-darwin-arm64.zip
gh release upload v0.1.0 dist/l2m-windows-x64.zip
```

## Common Asset Types

- **Binaries**: Compiled executables (`.exe`, `.bin`, or platform-specific)
- **Archives**: Zipped packages (`.zip`, `.tar.gz`)
- **Installers**: Platform installers (`.dmg`, `.deb`, `.rpm`, `.msi`)
- **Source Code**: GitHub auto-generates these (zip/tar.gz of the tag)

## Best Practices

1. **Use Semantic Versioning**: `v1.0.0`, `v1.1.0`, `v2.0.0`
2. **Name assets clearly**: `l2m-linux-x64.zip`, `l2m-darwin-arm64.zip`
3. **Include checksums**: SHA256 hashes for security
4. **Write good release notes**: What changed, migration notes, breaking changes
5. **Automate with CI/CD**: Use GitHub Actions to build and upload automatically

## For Python Projects

To create standalone executables:

```bash
# Install PyInstaller
pip install pyinstaller

# Build executable
pyinstaller --onefile --name l2m cli/main.py

# Package it
zip l2m-linux-x64.zip dist/l2m
```

Then upload to your release!

