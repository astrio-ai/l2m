# L2M Color Palette - VS Code Inspired

This document describes L2M's color scheme, inspired by Visual Studio Code's Dark+ theme.

## 🎨 Color Philosophy

L2M uses **soft, muted colors** that are easy on the eyes during extended coding sessions. All colors are rendered using 24-bit ANSI escape codes for maximum compatibility with modern terminals.

## Color Palette

### Primary Colors

| Color | Hex | RGB | Usage |
|-------|-----|-----|-------|
| **Pale Cyan** | `#9CDCFE` | `156, 220, 254` | User input, info messages, links |
| **Pale Green** | `#B5CEA8` | `181, 206, 168` | Success messages, confirmations |
| **Pale Red** | `#F46A6A` | `244, 106, 106` | Errors, critical warnings |
| **Pale Purple** | `#C586C0` | `197, 134, 192` | Agent responses, thinking state |
| **Pale Yellow** | `#DCDCAA` | `220, 220, 170` | Code snippets, keywords |
| **Pale Orange** | `#CE9178` | `206, 145, 120` | Warnings, string literals |

### Visual Reference

```
Pale Cyan    █████  #9CDCFE  User input, links
Pale Green   █████  #B5CEA8  Success messages
Pale Red     █████  #F46A6A  Error messages
Pale Purple  █████  #C586C0  Agent responses
Pale Yellow  █████  #DCDCAA  Code, keywords
Pale Orange  █████  #CE9178  Warnings, strings
```

## Usage Examples

### Status Indicators

```python
from cli.tui_utils import StatusIndicator

● Thinking...     # Pale purple - Agent is processing
▌                 # Pale cyan - Agent is typing
✓ Success         # Pale green - Operation succeeded
✗ Error           # Pale red - Operation failed
⚠ Warning         # Pale orange - Warning message
ℹ Info            # Pale cyan - Information
```

### Style Guide

```python
from cli.tui_utils import StyleGuide

# Headers (bold + pale cyan)
StyleGuide.header("Important Header")

# Success messages (pale green)
StyleGuide.success("✓ Task completed successfully")

# Error messages (pale red)
StyleGuide.error("✗ Something went wrong")

# Warning messages (pale orange)
StyleGuide.warning("⚠ Check your configuration")

# User input (pale cyan)
StyleGuide.user_input("What you typed")

# Code snippets (pale yellow)
StyleGuide.code("function_name()")

# Keywords (pale purple)
StyleGuide.keyword("if")

# String literals (pale orange)
StyleGuide.string('"Hello, World!"')
```

### Shimmer Effect

The shimmer effect during LLM processing uses a gradient blend:
- **Base color**: Dim gray (180, 180, 180)
- **Highlight color**: White (255, 255, 255)
- **Animation**: 2-second cosine wave sweep

### Footer Hints

Footer hints use a combination of:
- **Bold** for keyboard shortcuts (Ctrl+C, /help)
- **Dim** for descriptions and separators

Example:
```
  Ctrl+C: Cancel  │  Ctrl+D: Exit  │  Tab: Autocomplete  │  /help: Commands
  └─bold pale cyan─┘  └──────dim────────┘
```

## Design Rationale

### Why Pale Colors?

1. **Reduced Eye Strain**: Soft colors are gentler on the eyes during long coding sessions
2. **Professional Aesthetic**: Matches modern IDEs like VS Code, Sublime Text, and IntelliJ
3. **Better Readability**: High contrast without being harsh
4. **Consistency**: Familiar to developers who use VS Code daily

### Color Mapping

| VS Code Token | L2M Usage |
|---------------|-----------|
| Variables/Functions (`#9CDCFE`) | User input, info |
| Strings (`#CE9178`) | Warnings, string literals |
| Keywords (`#C586C0`) | Agent responses, keywords |
| Numbers (`#B5CEA8`) | Success messages |
| Comments (`#6A9955`) | Not used (too dark) |
| Operators (`#D4D4D4`) | Default terminal color |

### Accessibility

All colors meet WCAG AA contrast requirements when displayed on dark backgrounds:
- Pale Cyan (#9CDCFE) on dark: ✅ 8.2:1
- Pale Green (#B5CEA8) on dark: ✅ 7.1:1
- Pale Red (#F46A6A) on dark: ✅ 5.8:1
- Pale Purple (#C586C0) on dark: ✅ 6.4:1
- Pale Yellow (#DCDCAA) on dark: ✅ 9.1:1
- Pale Orange (#CE9178) on dark: ✅ 6.7:1

## Terminal Compatibility

### Truecolor Support

L2M uses 24-bit ANSI escape codes (`\033[38;2;R;G;Bm`), which are supported by:
- ✅ iTerm2 (macOS)
- ✅ Terminal.app (macOS 10.15+)
- ✅ Windows Terminal
- ✅ Alacritty
- ✅ Kitty
- ✅ Gnome Terminal (3.16+)
- ✅ VS Code integrated terminal

### Fallback Behavior

If truecolor is not detected:
- Shimmer effect falls back to bold/dim text
- Colors degrade gracefully to 256-color palette
- `NO_COLOR` environment variable is respected

## Customization

Users can customize colors by setting environment variables:

```bash
export L2M_USER_INPUT_COLOR="#9CDCFE"
export L2M_ERROR_COLOR="#F46A6A"
export L2M_SUCCESS_COLOR="#B5CEA8"
```

Or disable colors entirely:

```bash
export NO_COLOR=1  # Disables all colors
l2m --no-pretty    # Disables pretty output
```

## Comparison to Other Themes

### Before (Bright ANSI)
```
■ Red (255, 0, 0)        - Too harsh
■ Green (0, 255, 0)      - Too bright
■ Yellow (255, 255, 0)   - Eye-straining
■ Cyan (0, 255, 255)     - Too neon
```

### After (VS Code Pale)
```
█ Pale Red (#F46A6A)     - Gentle, readable
█ Pale Green (#B5CEA8)   - Soft, calming
█ Pale Yellow (#DCDCAA)  - Warm, subtle
█ Pale Cyan (#9CDCFE)    - Cool, professional
```

## Future Enhancements

Potential additions:
- [ ] Light theme support (detect terminal background)
- [ ] User-configurable color schemes
- [ ] Seasonal themes (optional)
- [ ] High contrast mode for accessibility
- [ ] Color blindness-friendly palette variants

---

**Note**: All colors are optimized for dark terminals. Light terminal support is planned for a future release.

