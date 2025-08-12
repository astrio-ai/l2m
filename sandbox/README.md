# Sandbox Environment Setup Guide

This guide will walk you through setting up and using the Docker sandbox environment for your own projects. The sandbox provides an isolated environment for running Node.js applications, development servers, and build processes.

## Prerequisites

Before you begin, ensure you have the following installed:
- **Docker Desktop** or **Docker Engine** (version 20.10 or later)
- **Git** (for cloning repositories)
- **Python 3.8+** (for running the integration scripts)

## Step 1: Build the Docker Image

First, build the sandbox Docker image:

```bash
# Navigate to the sandbox directory
cd sandbox

# Build the Docker image
docker build -t sandbox:latest .
```

This will create a Docker image with:
- Node.js 20 LTS
- npm, yarn, pnpm package managers
- Git for version control
- Common development tools (TypeScript, ESLint, Prettier, etc.)
- Framework CLI tools (create-react-app, create-next-app, etc.)

## Step 2: Test the Sandbox Environment

Verify that the sandbox is working correctly:

```bash
# Test basic Node.js functionality
docker run --rm sandbox:latest node --version

# Test npm functionality
docker run --rm sandbox:latest npm --version

# Test Git functionality
docker run --rm sandbox:latest git --version
```

## Step 3: Set Up Your Project

### Option A: Using an Existing Project

If you have an existing project, you can mount it into the sandbox:

```bash
# Navigate to your project directory
cd /path/to/your/project

# Run the sandbox with your project mounted
docker run -it --rm \
  -v $(pwd):/workspace \
  -p 3000:3000 \
  -p 3001:3001 \
  -p 8080:8080 \
  -p 8000:8000 \
  -p 5173:5173 \
  -p 4173:4173 \
  sandbox:latest bash
```

### Option B: Creating a New Project

Start with a fresh workspace and create a new project:

```bash
# Run the sandbox with a fresh workspace
docker run -it --rm \
  -v $(pwd)/my-new-project:/workspace \
  -p 3000:3000 \
  -p 3001:3001 \
  -p 8080:8080 \
  -p 8000:8000 \
  -p 5173:5173 \
  -p 4173:4173 \
  sandbox:latest bash
```

## Step 4: Working Inside the Sandbox

Once inside the sandbox container, you can:

### Install Dependencies
```bash
# Using npm
npm install

# Using yarn
yarn install

# Using pnpm
pnpm install
```

### Start Development Servers
```bash
# React development server
npm start

# Next.js development server
npm run dev

# Vite development server
npm run dev

# Webpack development server
npm run start
```

### Run Build Commands
```bash
# Build for production
npm run build

# Run tests
npm test

# Lint code
npm run lint
```

## Step 5: Using with Autogen (Optional)

If you're using this sandbox with Autogen for AI-powered development:

### Basic Autogen Integration

```python
from autogen import LocalCommandLineCodeExecutor

# Initialize the executor with the sandbox
executor = LocalCommandLineCodeExecutor(
    docker_image="sandbox:latest",
    work_dir="/workspace"
)

# Run commands in your project
result = executor.execute("npm install")
result = executor.execute("npm run dev")
```

### Advanced Integration

```python
from engine.agents.autogen_integration.sandbox_executor import (
    SandboxConfig, SandboxExecutor
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

## Step 6: Development Workflow

### Typical Development Session

1. **Start the sandbox** with your project mounted
2. **Install dependencies** if needed
3. **Start development server** (npm start, npm run dev, etc.)
4. **Access your app** at `http://localhost:3000` (or other configured port)
5. **Make changes** to your code
6. **See live updates** in your browser
7. **Stop the server** with Ctrl+C when done

### Example: React App Development

```bash
# Inside the sandbox container
npx create-react-app my-app --yes
cd my-app
npm start
```

Then open `http://localhost:3000` in your browser.

### Example: Next.js App Development

```bash
# Inside the sandbox container
npx create-next-app@latest my-next-app --yes --typescript --tailwind --eslint
cd my-next-app
npm run dev
```

Then open `http://localhost:3000` in your browser.

## Port Configuration

The sandbox exposes these common development ports:
- **3000**: React development server (default)
- **3001**: Alternative React port
- **8080**: Webpack dev server
- **8000**: Python development server
- **5173**: Vite development server
- **4173**: Vite preview server

To use a different port, modify the port mapping in the docker run command:

```bash
docker run -it --rm \
  -v $(pwd):/workspace \
  -p 4000:3000 \  # Map host port 4000 to container port 3000
  sandbox:latest bash
```

## Environment Variables

The sandbox comes with these default environment variables:
- `NODE_ENV=development`
- `PATH` includes local node_modules/.bin
- Working directory: `/workspace`

You can add custom environment variables:

```bash
docker run -it --rm \
  -v $(pwd):/workspace \
  -e NODE_ENV=production \
  -e API_KEY=your-api-key \
  sandbox:latest bash
```

## Troubleshooting

### Common Issues and Solutions

1. **Docker not running**
   ```bash
   # Start Docker Desktop or Docker daemon
   sudo systemctl start docker  # Linux
   # Or start Docker Desktop application
   ```

2. **Permission denied errors**
   ```bash
   # Ensure Docker has proper permissions
   sudo usermod -aG docker $USER
   # Log out and back in, or restart your terminal
   ```

3. **Port already in use**
   ```bash
   # Use a different port mapping
   -p 4000:3000  # Instead of -p 3000:3000
   ```

4. **Container exits immediately**
   ```bash
   # Use -it flag for interactive mode
   docker run -it --rm sandbox:latest bash
   ```

5. **Changes not persisting**
   ```bash
   # Ensure you're mounting the correct directory
   -v $(pwd):/workspace  # Mount current directory
   ```

### Getting Help

If you encounter issues:
1. Check that Docker is running and accessible
2. Verify the image was built successfully
3. Ensure you have sufficient disk space
4. Check Docker logs: `docker logs <container_id>`

## Security Notes

- The sandbox runs as a non-root user (`developer`)
- The container is isolated from your host system
- No persistent data is stored unless you mount volumes
- Always use `--rm` flag to automatically clean up containers

## Next Steps

Once you're comfortable with the basic setup, you can:
- Explore the advanced Autogen integration features
- Set up automated workflows
- Configure custom development environments
- Integrate with your CI/CD pipeline 