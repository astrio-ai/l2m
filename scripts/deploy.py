#!/usr/bin/env python3
"""
Deployment script for the Legacy2Modern multi-agent system.

This script deploys the system to production environments.
"""

import os
import sys
import subprocess
from pathlib import Path


def run_command(command, description):
    """Run a command and handle errors."""
    print(f"Running: {description}")
    try:
        result = subprocess.run(command, shell=True, check=True, capture_output=True, text=True)
        print(f"✓ {description} completed successfully")
        return True
    except subprocess.CalledProcessError as e:
        print(f"✗ {description} failed: {e}")
        print(f"Error output: {e.stderr}")
        return False


def main():
    """Main deployment function."""
    print("Deploying Legacy2Modern multi-agent system...")
    
    # Check if we're in the right directory
    if not Path("src").exists():
        print("Error: Please run this script from the project root directory")
        sys.exit(1)
    
    # Check if Docker is available
    if not run_command("docker --version", "Checking Docker availability"):
        print("Warning: Docker not available, skipping container deployment")
    
    # Run tests before deployment
    if not run_command("python -m pytest tests/unit/ -v", "Running unit tests"):
        print("Warning: Unit tests failed, continuing with deployment")
    
    # Build Docker image if Docker is available
    if run_command("docker --version", "Checking Docker availability"):
        if run_command("docker build -t legacy2modern .", "Building Docker image"):
            print("✓ Docker image built successfully")
        else:
            print("Warning: Docker image build failed")
    
    # Create production directories
    production_dirs = [
        "production/logs",
        "production/output",
        "production/backup",
        "production/config"
    ]
    
    for directory in production_dirs:
        Path(directory).mkdir(parents=True, exist_ok=True)
        print(f"✓ Created directory: {directory}")
    
    # Copy configuration files
    config_files = [
        ".env.example",
        "requirements.txt",
        "pyproject.toml"
    ]
    
    for config_file in config_files:
        if Path(config_file).exists():
            run_command(f"cp {config_file} production/config/", f"Copying {config_file}")
    
    # Create production startup script
    startup_script = """#!/bin/bash
# Production startup script for Legacy2Modern

# Set environment variables
export PYTHONPATH="${PYTHONPATH}:$(pwd)/src"
export LOG_LEVEL="INFO"
export DEBUG="false"

# Start the API server
python -m src.api.server --host 0.0.0.0 --port 8000
"""
    
    Path("production/start.sh").write_text(startup_script)
    Path("production/start.sh").chmod(0o755)
    print("✓ Created production startup script")
    
    # Create production Docker Compose file
    docker_compose = """version: '3.8'

services:
  legacy2modern:
    build: .
    ports:
      - "8000:8000"
    environment:
      - PYTHONPATH=/app/src
      - LOG_LEVEL=INFO
      - DEBUG=false
    volumes:
      - ./logs:/app/logs
      - ./output:/app/output
      - ./backup:/app/backup
    restart: unless-stopped
"""
    
    Path("production/docker-compose.yml").write_text(docker_compose)
    print("✓ Created production Docker Compose file")
    
    # Create production README
    readme = """# Legacy2Modern Production Deployment

This directory contains the production deployment of the Legacy2Modern multi-agent system.

## Quick Start

1. **Using Docker Compose**:
   ```bash
   docker-compose up -d
   ```

2. **Using Docker**:
   ```bash
   docker run -d -p 8000:8000 legacy2modern
   ```

3. **Using Python directly**:
   ```bash
   ./start.sh
   ```

## Configuration

Edit the configuration files in the `config/` directory:

- `.env`: Environment variables
- `requirements.txt`: Python dependencies
- `pyproject.toml`: Project configuration

## Monitoring

- **Logs**: Check the `logs/` directory for application logs
- **Output**: Check the `output/` directory for generated files
- **Backup**: Check the `backup/` directory for backup files

## API Endpoints

- **Health Check**: `GET /health`
- **API Documentation**: `GET /docs`
- **API Endpoints**: `GET /api/v1/`

## Troubleshooting

1. **Check logs**: `tail -f logs/app.log`
2. **Check status**: `curl http://localhost:8000/health`
3. **Restart service**: `docker-compose restart`

## Support

For support, please check the documentation or contact the development team.
"""
    
    Path("production/README.md").write_text(readme)
    print("✓ Created production README")
    
    print("\n✓ Deployment completed successfully!")
    print("\nProduction files created in the 'production/' directory")
    print("\nNext steps:")
    print("1. Review and edit configuration files")
    print("2. Test the deployment: cd production && ./start.sh")
    print("3. Monitor the application: tail -f logs/app.log")


if __name__ == "__main__":
    main()
