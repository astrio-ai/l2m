"""
Sandbox integration with Autogen's LocalCommandLineCodeExecutor.

This module provides a wrapper around Autogen's LocalCommandLineCodeExecutor
that uses the Docker sandbox environment for isolated code execution.
"""

import os
import logging
import subprocess
import sys
from pathlib import Path
from typing import Optional, Dict, Any, List, Union
from dataclasses import dataclass, field

# AutoGen imports with better error handling
logger = logging.getLogger(__name__)

def _check_autogen_availability():
    """Check AutoGen availability and provide detailed diagnostics."""
    python_path = sys.executable
    logger.info(f"Checking AutoGen availability for Python: {python_path}")
    
    # Check if we're in a conda environment
    conda_env = os.environ.get('CONDA_DEFAULT_ENV', None)
    if conda_env:
        logger.info(f"Running in conda environment: {conda_env}")
    
    # Try importing AutoGen packages
    autogen_packages = {}
    
    for package in ['autogen_core', 'autogen_agentchat', 'autogen_ext']:
        try:
            module = __import__(package)
            version = getattr(module, '__version__', 'unknown')
            autogen_packages[package] = {
                'available': True,
                'version': version,
                'path': getattr(module, '__file__', 'unknown')
            }
            logger.info(f"✅ {package} v{version} available at {module.__file__}")
        except ImportError as e:
            autogen_packages[package] = {
                'available': False,
                'error': str(e)
            }
            logger.warning(f"❌ {package} not available: {e}")
    
    return autogen_packages

# Check AutoGen availability
AUTOGEN_DIAGNOSTICS = _check_autogen_availability()

AUTOGEN_AVAILABLE = False
DockerCommandLineCodeExecutor = None

# Try importing DockerCommandLineCodeExecutor from different sources
try:
    from autogen_ext.code_executors.docker import DockerCommandLineCodeExecutor
    AUTOGEN_AVAILABLE = True
    logger.info("✅ AutoGen DockerCommandLineCodeExecutor imported from autogen_ext.code_executors.docker")
except ImportError:
    try:
        from autogen import DockerCommandLineCodeExecutor
        AUTOGEN_AVAILABLE = True
        logger.info("✅ AutoGen DockerCommandLineCodeExecutor imported from autogen")
    except ImportError:
        try:
            from autogen_agentchat import DockerCommandLineCodeExecutor
            AUTOGEN_AVAILABLE = True
            logger.info("✅ AutoGen DockerCommandLineCodeExecutor imported from autogen_agentchat")
        except ImportError:
            logger.warning("❌ AutoGen DockerCommandLineCodeExecutor not available - using fallback executor")
            DockerCommandLineCodeExecutor = None

class FallbackCommandLineExecutor:
    """Enhanced fallback executor when DockerCommandLineCodeExecutor is not available."""
    
    def __init__(self, image="sandbox:latest", work_dir="/workspace", bind_dir=None, **kwargs):
        self.image = image
        self.work_dir = work_dir
        self.bind_dir = bind_dir
        self.kwargs = kwargs
        self.container_id = None
        logger.info(f"Initialized fallback executor with image: {image}")
    
    def execute_code_blocks(self, code_blocks):
        """Execute code blocks using Docker directly with improved volume mounting."""
        results = []
        
        for code_block in code_blocks:
            try:
                # For now, we'll treat all code blocks as shell commands
                command = code_block.code
                
                # Build Docker command with proper volume mounting
                docker_cmd = ["docker", "run", "--rm"]
                
                # Add volume mount if specified
                if self.bind_dir:
                    # Parse bind_dir format: "host_path:container_path"
                    if ":" in self.bind_dir:
                        host_path, container_path = self.bind_dir.split(":", 1)
                        docker_cmd.extend(["-v", f"{host_path}:{container_path}"])
                        work_dir = container_path
                    else:
                        docker_cmd.extend(["-v", f"{self.bind_dir}:{self.work_dir}"])
                        work_dir = self.work_dir
                else:
                    work_dir = self.work_dir
                
                docker_cmd.extend(["-w", work_dir, self.image, "/bin/bash", "-c", command])
                
                logger.info(f"Executing command: {' '.join(docker_cmd)}")
                
                result = subprocess.run(
                    docker_cmd,
                    capture_output=True,
                    text=True,
                    timeout=300
                )
                
                results.append({
                    "exit_code": result.returncode,
                    "stdout": result.stdout,
                    "stderr": result.stderr,
                    "success": result.returncode == 0
                })
                
            except subprocess.TimeoutExpired:
                results.append({
                    "exit_code": -1,
                    "stdout": "",
                    "stderr": "Command timed out",
                    "success": False
                })
            except Exception as e:
                results.append({
                    "exit_code": -1,
                    "stdout": "",
                    "stderr": str(e),
                    "success": False
                })
        
        return results
    
    def execute(self, command):
        """Execute a single command using Docker directly with improved volume mounting."""
        try:
            # Build Docker command with volume mounting
            docker_cmd = ["docker", "run", "--rm", "-w", self.work_dir]
            
            # Add volume mount if specified
            if self.bind_dir:
                # Parse bind_dir format: "host_path:container_path"
                if ":" in self.bind_dir:
                    host_path, container_path = self.bind_dir.split(":", 1)
                    docker_cmd.extend(["-v", f"{host_path}:{container_path}"])
                    docker_cmd[-3] = container_path  # Update work_dir
                else:
                    docker_cmd.extend(["-v", f"{self.bind_dir}:{self.work_dir}"])
            
            docker_cmd.extend([self.image, "/bin/bash", "-c", command])
            
            logger.info(f"Executing command: {' '.join(docker_cmd)}")
            
            result = subprocess.run(
                docker_cmd,
                capture_output=True,
                text=True,
                timeout=300
            )
            
            return {
                "exit_code": result.returncode,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "success": result.returncode == 0
            }
            
        except subprocess.TimeoutExpired:
            return {
                "exit_code": -1,
                "stdout": "",
                "stderr": "Command timed out",
                "success": False
            }
        except Exception as e:
            return {
                "exit_code": -1,
                "stdout": "",
                "stderr": str(e),
                "success": False
            }
    
    def start(self):
        """Start the executor (no-op for fallback)."""
        logger.info("Fallback executor started")
    
    def stop(self):
        """Stop the executor (no-op for fallback)."""
        logger.info("Fallback executor stopped")
    
    def restart(self):
        """Restart the executor (no-op for fallback)."""
        logger.info("Fallback executor restarted")

@dataclass
class SandboxConfig:
    """Enhanced configuration for the sandbox environment."""
    
    # Docker configuration
    docker_image: str = "sandbox:latest"
    work_dir: str = "/workspace"
    
    # Volume mounting - improved format support
    mount_host_path: Optional[str] = None
    mount_container_path: str = "/workspace"
    bind_dir: Optional[str] = None  # Alternative format: "host_path:container_path"
    
    # Resource limits
    memory_limit: str = "2g"
    cpu_limit: str = "2"
    
    # Network configuration
    network_mode: str = "bridge"
    expose_ports: List[int] = field(default_factory=lambda: [3000, 3001, 8080, 8000, 5173, 4173])
    
    # Security
    run_as_user: str = "developer"
    read_only_root: bool = False
    
    # Timeout settings
    command_timeout: int = 300  # 5 minutes
    build_timeout: int = 600    # 10 minutes
    
    # Environment variables
    env_vars: Dict[str, str] = field(default_factory=dict)
    
    # Auto-cleanup
    auto_cleanup: bool = True
    preserve_workspace: bool = False
    
    # AutoGen integration settings
    use_autogen_executor: bool = True
    fallback_on_autogen_error: bool = True
    log_executor_choice: bool = True

class SandboxExecutor:
    """
    Enhanced executor that uses Autogen's LocalCommandLineCodeExecutor with Docker sandbox.
    
    This class provides a higher-level interface for running commands in an isolated
    Docker environment, with additional features like:
    - Automatic sandbox building
    - Volume mounting for persistent workspaces
    - Resource management
    - Error handling and recovery
    - Command history and logging
    - Improved AutoGen integration
    """
    
    def __init__(self, config: Optional[SandboxConfig] = None):
        """Initialize the sandbox executor with improved AutoGen integration."""
        self.config = config or SandboxConfig()
        self.executor: Optional[Union[DockerCommandLineCodeExecutor, FallbackCommandLineExecutor]] = None
        self.command_history: List[Dict[str, Any]] = []
        self.workspace_path: Optional[Path] = None
        self.executor_type: str = "unknown"
        
        # Initialize the executor
        self._initialize_executor()
        
        logger.info(f"SandboxExecutor initialized with image: {self.config.docker_image}")
    
    def _initialize_executor(self):
        """Initialize the DockerCommandLineCodeExecutor with sandbox configuration."""
        try:
            # Check if sandbox image exists
            if not self._check_sandbox_image():
                logger.info("Sandbox image not found. Building...")
                self._build_sandbox_image()
            
            # Create executor configuration
            executor_config = self._create_executor_config()
            
            # Initialize the executor based on availability and configuration
            if (self.config.use_autogen_executor and 
                AUTOGEN_AVAILABLE and 
                DockerCommandLineCodeExecutor):
                
                try:
                    self.executor = DockerCommandLineCodeExecutor(**executor_config)
                    self.executor_type = "autogen"
                    logger.info("✅ DockerCommandLineCodeExecutor initialized successfully")
                except Exception as e:
                    logger.error(f"Failed to initialize AutoGen executor: {e}")
                    if self.config.fallback_on_autogen_error:
                        logger.info("Falling back to custom executor")
                        self.executor = FallbackCommandLineExecutor(**executor_config)
                        self.executor_type = "fallback"
                    else:
                        raise
            else:
                if self.config.log_executor_choice:
                    if not self.config.use_autogen_executor:
                        logger.info("AutoGen executor disabled by configuration")
                    elif not AUTOGEN_AVAILABLE:
                        logger.info("AutoGen not available, using fallback executor")
                    elif not DockerCommandLineCodeExecutor:
                        logger.info("DockerCommandLineCodeExecutor not available, using fallback executor")
                
                self.executor = FallbackCommandLineExecutor(**executor_config)
                self.executor_type = "fallback"
                logger.info("FallbackCommandLineExecutor initialized")
            
        except Exception as e:
            logger.error(f"Failed to initialize executor: {e}")
            raise
    
    def _create_executor_config(self) -> Dict[str, Any]:
        """Create configuration for DockerCommandLineCodeExecutor with improved volume mounting."""
        config = {
            "image": self.config.docker_image,
            "work_dir": self.config.work_dir,
        }
        
        # Handle volume mounting with multiple format support
        if self.config.bind_dir:
            # Use bind_dir if specified (format: "host_path:container_path")
            config["bind_dir"] = self.config.bind_dir
            if ":" in self.config.bind_dir:
                container_path = self.config.bind_dir.split(":", 1)[1]
                config["work_dir"] = container_path
            logger.info(f"Volume mount configured via bind_dir: {self.config.bind_dir}")
        elif self.config.mount_host_path and self.config.mount_container_path:
            # Use mount_host_path and mount_container_path
            config["bind_dir"] = f"{self.config.mount_host_path}:{self.config.mount_container_path}"
            config["work_dir"] = self.config.mount_container_path
            logger.info(f"Volume mount configured: {self.config.mount_host_path} → {self.config.mount_container_path}")
        
        # Add timeout
        if self.config.command_timeout:
            config["timeout"] = self.config.command_timeout
        
        # Add auto_remove
        config["auto_remove"] = self.config.auto_cleanup
        
        # Add stop_container
        config["stop_container"] = True
        
        # Add environment variables if specified
        if self.config.env_vars:
            config["env_vars"] = self.config.env_vars
        
        return config
    
    async def execute_async(self, command: str, timeout: Optional[int] = None) -> Dict[str, Any]:
        """
        Execute a command in the sandbox environment asynchronously.
        
        Args:
            command: The command to execute
            timeout: Optional timeout override
            
        Returns:
            Dictionary containing execution results
        """
        if not self.executor:
            raise RuntimeError("Executor not initialized")
        
        try:
            # Record command in history
            command_record = {
                "command": command,
                "timestamp": self._get_timestamp(),
                "status": "running",
                "executor_type": self.executor_type
            }
            self.command_history.append(command_record)
            
            # Execute the command based on executor type
            if self.executor_type == "autogen":
                result_dict = await self._execute_with_autogen(command, timeout)
            else:
                result_dict = self.executor.execute(command)
            
            # Update command record
            command_record["status"] = "completed"
            command_record["result"] = result_dict
            
            logger.info(f"Command executed successfully: {command}")
            return result_dict
            
        except Exception as e:
            # Update command record with error
            if command_record:
                command_record["status"] = "failed"
                command_record["error"] = str(e)
            
            logger.error(f"Command execution failed: {command} - {e}")
            raise
    
    async def _execute_with_autogen(self, command: str, timeout: Optional[int] = None) -> Dict[str, Any]:
        """Execute command using AutoGen executor."""
        try:
            # Start the executor if it has a start method
            if hasattr(self.executor, 'start'):
                await self._start_executor()
            
            # Create a simple CodeBlock-like object
            class SimpleCodeBlock:
                def __init__(self, code: str, language: str = "bash"):
                    self.code = code
                    self.language = language
            
            code_block = SimpleCodeBlock(code=command, language="bash")
            
            # Try to import CancellationToken
            try:
                from autogen_core import CancellationToken
                cancellation_token = CancellationToken()
            except ImportError:
                # Create a simple fallback token
                class SimpleCancellationToken:
                    def __init__(self):
                        self.cancelled = False
                cancellation_token = SimpleCancellationToken()
            
            # Execute using AutoGen's interface
            if hasattr(self.executor, 'execute_code_blocks'):
                result = await self.executor.execute_code_blocks([code_block], cancellation_token)
                
                # Handle different result formats
                if hasattr(result, 'exit_code'):
                    # AutoGen executor returns a CommandLineCodeResult
                    result_dict = {
                        "exit_code": result.exit_code,
                        "stdout": getattr(result, 'stdout', '') or getattr(result, 'output', ''),
                        "stderr": getattr(result, 'stderr', '') or getattr(result, 'error', ''),
                        "success": result.exit_code == 0
                    }
                else:
                    result_dict = {"exit_code": 0, "stdout": "", "stderr": "", "success": True}
            else:
                # Fall back to direct execution
                result_dict = self.executor.execute(command)
            
            return result_dict
            
        except Exception as e:
            logger.error(f"AutoGen execution failed: {e}")
            # Fall back to direct Docker execution
            return await self._execute_docker_direct(command)
    
    def execute(self, command: str, timeout: Optional[int] = None) -> Dict[str, Any]:
        """
        Execute a command in the sandbox environment (synchronous wrapper).
        
        Args:
            command: The command to execute
            timeout: Optional timeout override
            
        Returns:
            Dictionary containing execution results
        """
        import asyncio
        try:
            # Check if we're already in an event loop
            try:
                loop = asyncio.get_running_loop()
                # We're in an event loop, create a task
                import concurrent.futures
                with concurrent.futures.ThreadPoolExecutor() as executor:
                    future = executor.submit(asyncio.run, self.execute_async(command, timeout))
                    return future.result()
            except RuntimeError:
                # No event loop running, we can use asyncio.run
                return asyncio.run(self.execute_async(command, timeout))
        except Exception as e:
            logger.error(f"Async execution failed: {e}")
            # Fall back to direct Docker execution
            return self._execute_docker_direct_sync(command)
    
    async def _start_executor(self):
        """Start the AutoGen executor if it's available."""
        if hasattr(self.executor, 'start'):
            try:
                await self.executor.start()
                logger.info("AutoGen executor started successfully")
            except Exception as e:
                logger.error(f"Failed to start AutoGen executor: {e}")
    
    async def _stop_executor(self):
        """Stop the AutoGen executor if it's available."""
        if hasattr(self.executor, 'stop'):
            try:
                await self.executor.stop()
                logger.info("AutoGen executor stopped successfully")
            except Exception as e:
                logger.error(f"Failed to stop AutoGen executor: {e}")
    
    def _check_sandbox_image(self) -> bool:
        """Check if the sandbox Docker image exists."""
        try:
            result = subprocess.run(
                ["docker", "image", "inspect", self.config.docker_image],
                capture_output=True,
                text=True,
                timeout=10
            )
            return result.returncode == 0
        except Exception as e:
            logger.warning(f"Error checking sandbox image: {e}")
            return False
    
    def _build_sandbox_image(self):
        """Build the sandbox Docker image."""
        try:
            # Get the sandbox directory path
            sandbox_dir = Path(__file__).parent.parent.parent.parent / "sandbox"
            
            if not sandbox_dir.exists():
                raise FileNotFoundError(f"Sandbox directory not found: {sandbox_dir}")
            
            # Run the build script
            build_script = sandbox_dir / "build.sh"
            if build_script.exists():
                result = subprocess.run(
                    [str(build_script)],
                    cwd=str(sandbox_dir),
                    capture_output=True,
                    text=True,
                    timeout=self.config.build_timeout
                )
                
                if result.returncode != 0:
                    raise RuntimeError(f"Build failed: {result.stderr}")
                
                logger.info("Sandbox image built successfully")
            else:
                # Fallback: build directly with docker
                result = subprocess.run(
                    ["docker", "build", "-t", self.config.docker_image, "."],
                    cwd=str(sandbox_dir),
                    capture_output=True,
                    text=True,
                    timeout=self.config.build_timeout
                )
                
                if result.returncode != 0:
                    raise RuntimeError(f"Build failed: {result.stderr}")
                
                logger.info("Sandbox image built successfully")
                
        except Exception as e:
            logger.error(f"Failed to build sandbox image: {e}")
            raise
    
    async def _execute_docker_direct(self, command: str) -> Dict[str, Any]:
        """Execute command directly using Docker."""
        import subprocess
        import json
        
        try:
            # Create a temporary directory for the command
            import tempfile
            import os
            
            with tempfile.TemporaryDirectory() as temp_dir:
                # Create a script file
                script_path = os.path.join(temp_dir, "command.sh")
                with open(script_path, 'w') as f:
                    f.write(f"#!/bin/bash\n{command}")
                os.chmod(script_path, 0o755)
                
                # Run the command in Docker
                docker_cmd = [
                    "docker", "run", "--rm",
                    "-v", f"{temp_dir}:/workspace",
                    "-w", "/workspace",
                    self.config.docker_image,
                    "/bin/bash", "-c", f"chmod +x /workspace/command.sh && /workspace/command.sh"
                ]
                
                result = subprocess.run(
                    docker_cmd,
                    capture_output=True,
                    text=True,
                    timeout=self.config.command_timeout or 60
                )
                
                return {
                    "exit_code": result.returncode,
                    "stdout": result.stdout,
                    "stderr": result.stderr,
                    "success": result.returncode == 0
                }
        except Exception as e:
            logger.error(f"Direct Docker execution failed: {e}")
            return {
                "exit_code": 1,
                "stdout": "",
                "stderr": str(e),
                "success": False
            }
    
    def _execute_docker_direct_sync(self, command: str) -> Dict[str, Any]:
        """Execute command directly using Docker (synchronous)."""
        import asyncio
        try:
            # Check if we're already in an event loop
            try:
                loop = asyncio.get_running_loop()
                # We're in an event loop, create a task
                import concurrent.futures
                with concurrent.futures.ThreadPoolExecutor() as executor:
                    future = executor.submit(asyncio.run, self._execute_docker_direct(command))
                    return future.result()
            except RuntimeError:
                # No event loop running, we can use asyncio.run
                return asyncio.run(self._execute_docker_direct(command))
        except Exception as e:
            logger.error(f"Direct Docker execution failed: {e}")
            return {
                "exit_code": 1,
                "stdout": "",
                "stderr": str(e),
                "success": False
            }
    
    def execute_sequence(self, commands: List[str], stop_on_error: bool = True) -> List[Dict[str, Any]]:
        """
        Execute a sequence of commands in the sandbox.
        
        Args:
            commands: List of commands to execute
            stop_on_error: Whether to stop execution on first error
            
        Returns:
            List of execution results
        """
        results = []
        
        for i, command in enumerate(commands):
            try:
                logger.info(f"Executing command {i+1}/{len(commands)}: {command}")
                result = self.execute(command)
                results.append({
                    "command": command,
                    "index": i,
                    "success": True,
                    "result": result
                })
                
            except Exception as e:
                error_result = {
                    "command": command,
                    "index": i,
                    "success": False,
                    "error": str(e)
                }
                results.append(error_result)
                
                if stop_on_error:
                    logger.error(f"Sequence stopped at command {i+1} due to error")
                    break
        
        return results
    
    def setup_workspace(self, workspace_path: Optional[str] = None) -> str:
        """
        Set up a workspace in the sandbox.
        
        Args:
            workspace_path: Optional path to mount as workspace
            
        Returns:
            Path to the workspace in the container
        """
        if workspace_path:
            self.workspace_path = Path(workspace_path)
            if not self.workspace_path.exists():
                self.workspace_path.mkdir(parents=True, exist_ok=True)
            
            logger.info(f"Workspace set up at: {self.workspace_path}")
            return str(self.workspace_path)
        else:
            # Use default workspace
            return self.config.work_dir
    
    def install_dependencies(self, package_manager: str = "npm", packages: Optional[List[str]] = None) -> Dict[str, Any]:
        """
        Install dependencies in the sandbox.
        
        Args:
            package_manager: Package manager to use (npm, yarn, pnpm)
            packages: Optional list of packages to install
            
        Returns:
            Installation result
        """
        if package_manager == "npm":
            if packages:
                command = f"npm install {' '.join(packages)}"
            else:
                command = "npm install"
        elif package_manager == "yarn":
            if packages:
                command = f"yarn add {' '.join(packages)}"
            else:
                command = "yarn install"
        elif package_manager == "pnpm":
            if packages:
                command = f"pnpm add {' '.join(packages)}"
            else:
                command = "pnpm install"
        else:
            raise ValueError(f"Unsupported package manager: {package_manager}")
        
        return self.execute(command)
    
    def run_dev_server(self, framework: str = "react", port: int = 3000) -> Dict[str, Any]:
        """
        Start a development server in the sandbox.
        
        Args:
            framework: Framework to use (react, next, vue, etc.)
            port: Port to run the server on
            
        Returns:
            Server start result
        """
        if framework == "react":
            command = f"npm start -- --port {port}"
        elif framework == "next":
            command = f"npm run dev -- -p {port}"
        elif framework == "vue":
            command = f"npm run serve -- --port {port}"
        elif framework == "vite":
            command = f"npm run dev -- --port {port}"
        else:
            command = f"npm run dev"
        
        return self.execute(command)
    
    def get_command_history(self) -> List[Dict[str, Any]]:
        """Get the history of executed commands."""
        return self.command_history.copy()
    
    def clear_history(self):
        """Clear the command history."""
        self.command_history.clear()
    
    def _get_timestamp(self) -> str:
        """Get current timestamp string."""
        from datetime import datetime
        return datetime.now().isoformat()
    
    def cleanup(self):
        """Clean up resources."""
        if self.executor:
            # AutoGen executor cleanup if needed
            import asyncio
            try:
                # Check if we're already in an event loop
                try:
                    loop = asyncio.get_running_loop()
                    # We're in an event loop, create a task
                    import concurrent.futures
                    with concurrent.futures.ThreadPoolExecutor() as executor:
                        future = executor.submit(asyncio.run, self._stop_executor())
                        future.result()
                except RuntimeError:
                    # No event loop running, we can use asyncio.run
                    asyncio.run(self._stop_executor())
            except Exception as e:
                logger.error(f"Error stopping executor: {e}")
        
        logger.info("SandboxExecutor cleanup completed")


class SandboxAgent:
    """
    Agent that can execute code in the sandbox environment.
    
    This agent combines the sandbox executor with AI capabilities
    to provide intelligent code execution and development assistance.
    """
    
    def __init__(self, name: str, config: Optional[SandboxConfig] = None):
        """Initialize the sandbox agent."""
        self.name = name
        self.config = config or SandboxConfig()
        self.executor = SandboxExecutor(config)
        
        logger.info(f"SandboxAgent '{name}' initialized")
    
    def execute_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute a development task in the sandbox.
        
        Args:
            task: Task specification with commands and metadata
            
        Returns:
            Task execution results
        """
        task_type = task.get("type", "command")
        commands = task.get("commands", [])
        
        logger.info(f"Executing task '{task_type}' with {len(commands)} commands")
        
        if task_type == "sequence":
            results = self.executor.execute_sequence(commands)
        elif task_type == "single":
            if len(commands) == 1:
                results = [self.executor.execute(commands[0])]
            else:
                raise ValueError("Single task type requires exactly one command")
        else:
            raise ValueError(f"Unknown task type: {task_type}")
        
        return {
            "task_type": task_type,
            "commands": commands,
            "results": results,
            "success": all(r.get("success", True) for r in results)
        }
    
    def setup_project(self, project_type: str, project_name: str) -> Dict[str, Any]:
        """
        Set up a new project in the sandbox.
        
        Args:
            project_type: Type of project (react, next, vue, etc.)
            project_name: Name of the project
            
        Returns:
            Setup results
        """
        if project_type == "react":
            command = f"npx create-react-app {project_name} --yes"
        elif project_type == "next":
            command = f"npx create-next-app {project_name} --yes"
        elif project_type == "vue":
            command = f"npx @vue/cli create {project_name} --default"
        else:
            raise ValueError(f"Unsupported project type: {project_type}")
        
        return self.executor.execute(command)
    
    def cleanup(self):
        """Clean up the agent and executor."""
        self.executor.cleanup()
        logger.info(f"SandboxAgent '{self.name}' cleanup completed")


# Convenience functions
def create_sandbox_executor(config: Optional[SandboxConfig] = None) -> SandboxExecutor:
    """Create a sandbox executor with default configuration."""
    return SandboxExecutor(config)


def create_sandbox_agent(name: str, config: Optional[SandboxConfig] = None) -> SandboxAgent:
    """Create a sandbox agent with default configuration."""
    return SandboxAgent(name, config)


def execute_in_sandbox(command: str, config: Optional[SandboxConfig] = None) -> Dict[str, Any]:
    """Execute a single command in the sandbox."""
    executor = SandboxExecutor(config)
    try:
        return executor.execute(command)
    finally:
        executor.cleanup() 