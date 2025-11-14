# L2M Go TUI

A Go-based Terminal User Interface for Legacy2Modern using Bubble Tea.

## Features

- ✨ Clean, minimal interface inspired by Codex
- 🎨 Input box with background highlighting
- 🚀 Fast, responsive UI built with Go
- 🔗 Communicates with Python L2M backend

## Installation

1. Install Go (1.21 or later):
```bash
brew install go  # macOS
```

2. Install dependencies:
```bash
cd tui-go
go mod download
```

3. Build the TUI:
```bash
go build -o l2m-tui
```

## Usage

Run the TUI:
```bash
./l2m-tui
```

Or use the launcher script from the project root:
```bash
./l2m-tui.sh
```

## Keyboard Shortcuts

- `Enter` - Submit input
- `Ctrl+C` or `Ctrl+D` - Exit
- `←` / `→` - Move cursor
- `Backspace` - Delete character

## Architecture

```
┌─────────────┐
│   Go TUI    │  ← User interaction, beautiful UI
│ (Bubble Tea)│
└──────┬──────┘
       │ stdin/stdout
       ↓
┌─────────────┐
│  Python L2M │  ← Core logic, LLM interaction
│   Backend   │
└─────────────┘
```

The Go TUI handles all display and input, while the Python backend handles LLM communication and file operations.

## Current Status

- [x] Basic TUI structure
- [x] Input box with background
- [x] Message display (user/assistant)
- [ ] Python backend integration (in progress)
- [ ] Streaming responses
- [ ] File operations display
- [ ] Advanced styling

## Development

To run in development mode:
```bash
go run main.go
```

## TODO

- Implement Python subprocess communication
- Add streaming response display
- Handle file operations
- Add syntax highlighting for code blocks
- Implement multiline input
- Add history navigation

