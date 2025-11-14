# Installation Guide for L2M Go TUI

## Step 1: Install Go

### macOS
```bash
brew install go
```

### Linux
```bash
# Download and install Go 1.21+
wget https://go.dev/dl/go1.21.6.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.21.6.linux-amd64.tar.gz
export PATH=$PATH:/usr/local/go/bin
```

### Windows
Download from: https://go.dev/dl/

## Step 2: Verify Installation
```bash
go version
# Should show: go version go1.21.x ...
```

## Step 3: Build the TUI
```bash
cd tui-go
go mod download
go build -o l2m-tui
```

## Step 4: Run
```bash
./l2m-tui
```

Or use the launcher from the project root:
```bash
./l2m-tui.sh
```

## Quick Start (macOS)
```bash
# Install Go
brew install go

# Build and run
./l2m-tui.sh
```

## Features You'll Get

✅ **Input box with background** - Finally! The chatbox-style input you wanted
✅ **Clean, minimal UI** - No cluttered keyboard shortcuts
✅ **Fast rendering** - Go's performance
✅ **Proper styling** - Full control over colors and layout
✅ **Smooth experience** - Better than prompt_toolkit limitations

## What's Next

After you install Go and test the basic UI, we'll integrate:
1. Python L2M backend communication
2. Streaming responses
3. File operations display
4. Advanced features

