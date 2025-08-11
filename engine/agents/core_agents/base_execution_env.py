"""
Base execution environment interface for running modernization commands.
"""

import asyncio
import logging
import subprocess
import tempfile
import os
from abc import ABC, abstractmethod
from typing import Dict, List, Optional, Any, Union, Tuple
from dataclasses import dataclass
from pathlib import Path

logger = logging.getLogger(__name__)

@dataclass
class ExecutionResult:
    """Result of a command execution."""
    success: bool
    stdout: str
    stderr: str
    return_code: int
    execution_time: float
    command: str
    working_dir: str

@dataclass
class EnvironmentInfo:
    """Information about the execution environment."""
    os_type: str
    python_version: str
    node_version: Optional[str] = None
    npm_version: Optional[str] = None
    available_tools: List[str] = None

class BaseExecutionEnv(ABC):
    """
    Abstract base class for execution environments.
    
    Provides a unified interface for running commands, managing files,
    and interacting with the system environment.
    """
    
    def __init__(self, working_dir: Optional[str] = None):
        self.working_dir = working_dir or os.getcwd()
        self.temp_dir = None
        self.environment_info = None
        
    @abstractmethod
    async def execute_command(
        self,
        command: Union[str, List[str]],
        cwd: Optional[str] = None,
        env: Optional[Dict[str, str]] = None,
        timeout: Optional[float] = None
    ) -> ExecutionResult:
        """
        Execute a command in the environment.
        
        Args:
            command: Command to execute (string or list of arguments)
            cwd: Working directory for the command
            env: Environment variables to set
            timeout: Command timeout in seconds
            
        Returns:
            ExecutionResult with command output and status
        """
        pass
    
    @abstractmethod
    async def create_temp_directory(self, prefix: str = "l2m_") -> str:
        """Create a temporary directory and return its path."""
        pass
    
    @abstractmethod
    async def cleanup_temp_directory(self, temp_dir: str):
        """Clean up a temporary directory."""
        pass
    
    @abstractmethod
    async def file_exists(self, file_path: str) -> bool:
        """Check if a file exists."""
        pass
    
    @abstractmethod
    async def read_file(self, file_path: str) -> str:
        """Read contents of a file."""
        pass
    
    @abstractmethod
    async def write_file(self, file_path: str, content: str):
        """Write content to a file."""
        pass
    
    @abstractmethod
    async def delete_file(self, file_path: str):
        """Delete a file."""
        pass
    
    @abstractmethod
    async def list_directory(self, dir_path: str) -> List[str]:
        """List contents of a directory."""
        pass
    
    @abstractmethod
    async def create_directory(self, dir_path: str):
        """Create a directory."""
        pass
    
    @abstractmethod
    async def get_environment_info(self) -> EnvironmentInfo:
        """Get information about the current environment."""
        pass

class LocalExecutionEnv(BaseExecutionEnv):
    """
    Local execution environment for running commands on the local system.
    """
    
    def __init__(self, working_dir: Optional[str] = None):
        super().__init__(working_dir)
        self._temp_dirs: List[str] = []
    
    async def execute_command(
        self,
        command: Union[str, List[str]],
        cwd: Optional[str] = None,
        env: Optional[Dict[str, str]] = None,
        timeout: Optional[float] = None
    ) -> ExecutionResult:
        """Execute a command using subprocess."""
        start_time = asyncio.get_event_loop().time()
        
        # Prepare command
        if isinstance(command, str):
            cmd_list = command.split()
        else:
            cmd_list = command
        
        # Set working directory
        work_dir = cwd or self.working_dir
        
        # Prepare environment
        process_env = os.environ.copy()
        if env:
            process_env.update(env)
        
        try:
            # Run command
            process = await asyncio.create_subprocess_exec(
                *cmd_list,
                cwd=work_dir,
                env=process_env,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            # Wait for completion with timeout
            if timeout:
                stdout, stderr = await asyncio.wait_for(
                    process.communicate(),
                    timeout=timeout
                )
            else:
                stdout, stderr = await process.communicate()
            
            execution_time = asyncio.get_event_loop().time() - start_time
            
            return ExecutionResult(
                success=process.returncode == 0,
                stdout=stdout.decode('utf-8', errors='ignore'),
                stderr=stderr.decode('utf-8', errors='ignore'),
                return_code=process.returncode,
                execution_time=execution_time,
                command=" ".join(cmd_list),
                working_dir=work_dir
            )
            
        except asyncio.TimeoutError:
            # Kill process if it times out
            if process:
                process.kill()
                await process.wait()
            
            execution_time = asyncio.get_event_loop().time() - start_time
            
            return ExecutionResult(
                success=False,
                stdout="",
                stderr=f"Command timed out after {timeout} seconds",
                return_code=-1,
                execution_time=execution_time,
                command=" ".join(cmd_list),
                working_dir=work_dir
            )
        
        except Exception as e:
            execution_time = asyncio.get_event_loop().time() - start_time
            
            return ExecutionResult(
                success=False,
                stdout="",
                stderr=str(e),
                return_code=-1,
                execution_time=execution_time,
                command=" ".join(cmd_list),
                working_dir=work_dir
            )
    
    async def create_temp_directory(self, prefix: str = "l2m_") -> str:
        """Create a temporary directory."""
        temp_dir = tempfile.mkdtemp(prefix=prefix)
        self._temp_dirs.append(temp_dir)
        return temp_dir
    
    async def cleanup_temp_directory(self, temp_dir: str):
        """Clean up a temporary directory."""
        try:
            import shutil
            shutil.rmtree(temp_dir)
            if temp_dir in self._temp_dirs:
                self._temp_dirs.remove(temp_dir)
        except Exception as e:
            logger.warning(f"Failed to cleanup temp directory {temp_dir}: {e}")
    
    async def cleanup_all_temp_directories(self):
        """Clean up all temporary directories created by this environment."""
        for temp_dir in self._temp_dirs[:]:  # Copy list to avoid modification during iteration
            await self.cleanup_temp_directory(temp_dir)
    
    async def file_exists(self, file_path: str) -> bool:
        """Check if a file exists."""
        return os.path.isfile(file_path)
    
    async def read_file(self, file_path: str) -> str:
        """Read contents of a file."""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                return f.read()
        except Exception as e:
            logger.error(f"Failed to read file {file_path}: {e}")
            raise
    
    async def write_file(self, file_path: str, content: str):
        """Write content to a file."""
        try:
            # Ensure directory exists
            os.makedirs(os.path.dirname(file_path), exist_ok=True)
            
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(content)
        except Exception as e:
            logger.error(f"Failed to write file {file_path}: {e}")
            raise
    
    async def delete_file(self, file_path: str):
        """Delete a file."""
        try:
            os.remove(file_path)
        except Exception as e:
            logger.error(f"Failed to delete file {file_path}: {e}")
            raise
    
    async def list_directory(self, dir_path: str) -> List[str]:
        """List contents of a directory."""
        try:
            return os.listdir(dir_path)
        except Exception as e:
            logger.error(f"Failed to list directory {dir_path}: {e}")
            return []
    
    async def create_directory(self, dir_path: str):
        """Create a directory."""
        try:
            os.makedirs(dir_path, exist_ok=True)
        except Exception as e:
            logger.error(f"Failed to create directory {dir_path}: {e}")
            raise
    
    async def get_environment_info(self) -> EnvironmentInfo:
        """Get information about the current environment."""
        if self.environment_info:
            return self.environment_info
        
        # Get OS type
        import platform
        os_type = platform.system()
        
        # Get Python version
        python_version = platform.python_version()
        
        # Get Node.js version
        node_version = None
        npm_version = None
        try:
            result = await self.execute_command(["node", "--version"])
            if result.success:
                node_version = result.stdout.strip()
            
            result = await self.execute_command(["npm", "--version"])
            if result.success:
                npm_version = result.stdout.strip()
        except Exception:
            pass
        
        # Check available tools
        available_tools = []
        tools_to_check = ["git", "npm", "yarn", "python", "pip", "docker"]
        
        for tool in tools_to_check:
            try:
                result = await self.execute_command([tool, "--version"])
                if result.success:
                    available_tools.append(tool)
            except Exception:
                pass
        
        self.environment_info = EnvironmentInfo(
            os_type=os_type,
            python_version=python_version,
            node_version=node_version,
            npm_version=npm_version,
            available_tools=available_tools
        )
        
        return self.environment_info
    
    def __del__(self):
        """Cleanup on destruction."""
        if hasattr(self, '_temp_dirs'):
            for temp_dir in self._temp_dirs:
                try:
                    import shutil
                    shutil.rmtree(temp_dir)
                except Exception:
                    pass 