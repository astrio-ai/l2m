# Sandbox Environment

This directory contains the Docker sandbox environment for Autogen's `LocalCommandLineCodeExecutor`. The sandbox provides an isolated environment for running Node.js applications, development servers, and build processes.

## Features

- **Node.js 20 LTS** - Latest LTS version with full npm/yarn/pnpm support
- **Git** - Version control for cloning repositories and managing code
- **Build Tools** - Essential tools for compiling and building applications
- **Development Tools** - Pre-installed common development packages
- **Security** - Runs as non-root user for better security
- **Port Exposure** - Common development ports (3000, 3001, 8080, 8000, 5173, 4173)

## Pre-installed Tools

### Package Managers
- npm (latest)
- yarn
- pnpm

### Development Tools
- TypeScript
- ESLint
- Prettier
- Nodemon
- Concurrently
- Cross-env

### Framework CLI Tools
- create-react-app
- create-next-app
- @vue/cli
- @angular/cli
- Vite
- Webpack CLI

### System Tools
- Git
- Python 3 (with pip and venv)
- Build essentials
- curl, wget, unzip

## Usage with Autogen

The sandbox can be used with Autogen's `LocalCommandLineCodeExecutor` to run commands in an isolated Docker container:

### Basic Usage

```python
from autogen import LocalCommandLineCodeExecutor

# Initialize the executor with the sandbox
executor = LocalCommandLineCodeExecutor(
    docker_image="sandbox:latest",  # Build from the Dockerfile
    work_dir="/workspace"
)

# Run commands in the sandbox
result = executor.execute("npm install")
result = executor.execute("npm run dev")
```

### Enhanced Integration

For more advanced usage, use the integrated `SandboxExecutor`:

```python
from engine.agents.autogen_integration.sandbox_executor import (
    SandboxConfig, SandboxExecutor, SandboxAgent
)

# Create a configured sandbox executor
config = SandboxConfig(
    command_timeout=300,
    env_vars={"NODE_ENV": "development"}
)
executor = SandboxExecutor(config)

# Execute commands with enhanced features
result = executor.execute("npm install")
executor.cleanup()
```

### Agent Integration

Integrate with existing agents:

```python
from engine.agents.autogen_integration.sandbox_executor import create_sandbox_agent

# Create a sandbox agent
agent = create_sandbox_agent("development-agent")

# Execute development tasks
task = {
    "type": "sequence",
    "commands": [
        "npx create-react-app my-app --yes",
        "cd my-app",
        "npm install",
        "npm start"
    ]
}

result = agent.execute_task(task)
agent.cleanup()
```

### Quick Testing

Use the CLI test script for quick testing:

```bash
cd tests
python test_sandbox_cli.py "node --version"
python test_sandbox_cli.py "npm install react"
```

### Full Demo

Run the comprehensive integration demo:

```bash
cd sandbox
python integration_example.py
```

### All Tests

Run all sandbox tests:

```bash
cd tests
python run_sandbox_tests.py
```

## Building the Sandbox

To build the Docker image:

```bash
cd sandbox
docker build -t sandbox:latest .
```

## Example Commands

The sandbox can run various development commands:

```bash
# Node.js development
npm install
npm run dev
npm run build
npm test

# Package management
yarn install
pnpm install

# Framework-specific commands
npx create-react-app my-app
npx create-next-app my-app
npx @vue/cli create my-app

# Git operations
git clone https://github.com/user/repo.git
git add .
git commit -m "Update"
git push

# Build tools
npx tsc
npx webpack
npx vite build
```

## Security Considerations

- The container runs as a non-root user (`developer`)
- The workspace directory is owned by the developer user
- System packages are installed as root but runtime operations are as developer
- The container is isolated from the host system

## Port Mapping

Common development ports are exposed:
- 3000: React development server
- 3001: Alternative React port
- 8080: Webpack dev server
- 8000: Python development server
- 5173: Vite development server
- 4173: Vite preview server

## Environment Variables

- `NODE_ENV=development`
- `PATH` includes local node_modules/.bin
- Working directory: `/workspace`

## Integration Features

The sandbox integration provides several advanced features:

### 🚀 **Enhanced Executor**
- **Automatic Image Building**: Automatically builds the sandbox image if not found
- **Command History**: Tracks all executed commands with timestamps
- **Error Handling**: Robust error handling and recovery mechanisms
- **Resource Management**: Configurable timeouts and resource limits
- **Environment Variables**: Custom environment variable injection

### 🤖 **Agent Integration**
- **SandboxAgent**: AI-powered agent for intelligent development tasks
- **Task Execution**: Execute complex multi-step development workflows
- **Project Management**: Automated project setup and configuration
- **Dependency Management**: Intelligent package installation and management

### 🛡️ **Security & Isolation**
- **Docker Isolation**: Complete isolation from the host system
- **Non-root Execution**: Runs as non-root user for enhanced security
- **Resource Limits**: Configurable memory and CPU limits
- **Auto-cleanup**: Automatic resource cleanup after execution

### 📊 **Monitoring & Logging**
- **Command Tracking**: Detailed logging of all executed commands
- **Performance Metrics**: Execution time and resource usage tracking
- **Error Reporting**: Comprehensive error reporting and debugging
- **Status Monitoring**: Real-time status updates during execution

## Troubleshooting

If you encounter permission issues, ensure the container is running as the developer user. The Dockerfile automatically sets up the correct permissions.

For persistent data, consider mounting volumes or using Docker volumes to persist node_modules and other generated files between container runs.

### Common Issues

1. **Docker not available**: Install Docker Desktop or Docker Engine
2. **Image build fails**: Check Docker daemon is running and has sufficient resources
3. **Permission denied**: Ensure Docker has proper permissions to build and run containers
4. **Port conflicts**: Change exposed ports in the configuration if needed
5. **Timeout errors**: Increase timeout values in SandboxConfig for long-running commands 