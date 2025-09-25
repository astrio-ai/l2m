"""
File operation tools for agents.

This module contains tools for file operations such as reading, writing,
scanning directories, and creating backups.
"""

from typing import Any, Dict, List, Optional
from pathlib import Path
import shutil
import os

from src.core.tools.base_tool import BaseAgentTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class FileReaderTool(BaseAgentTool):
    """Tool for reading files."""
    
    def __init__(self):
        super().__init__(
            name="read_file",
            description="Read contents of a file"
        )
    
    async def run(self, file_path: str, encoding: str = "utf-8") -> str:
        """Read file contents."""
        try:
            with open(file_path, 'r', encoding=encoding) as f:
                content = f.read()
            self.log_usage({"file_path": file_path, "encoding": encoding}, f"Read {len(content)} characters")
            return content
        except Exception as e:
            self.logger.error(f"Error reading file {file_path}: {e}")
            raise


class FileWriterTool(BaseAgentTool):
    """Tool for writing files."""
    
    def __init__(self):
        super().__init__(
            name="write_file",
            description="Write contents to a file"
        )
    
    async def run(self, file_path: str, content: str, encoding: str = "utf-8") -> bool:
        """Write content to file."""
        try:
            # Create directory if it doesn't exist
            os.makedirs(os.path.dirname(file_path), exist_ok=True)
            
            with open(file_path, 'w', encoding=encoding) as f:
                f.write(content)
            
            self.log_usage({"file_path": file_path, "content_length": len(content)}, "File written successfully")
            return True
        except Exception as e:
            self.logger.error(f"Error writing file {file_path}: {e}")
            raise


class DirectoryScannerTool(BaseAgentTool):
    """Tool for scanning directories."""
    
    def __init__(self):
        super().__init__(
            name="scan_directory",
            description="Scan directory for files matching patterns"
        )
    
    async def run(self, directory_path: str, pattern: str = "*", recursive: bool = True) -> List[str]:
        """Scan directory for files."""
        try:
            path = Path(directory_path)
            if recursive:
                files = list(path.rglob(pattern))
            else:
                files = list(path.glob(pattern))
            
            file_paths = [str(f) for f in files if f.is_file()]
            self.log_usage({"directory_path": directory_path, "pattern": pattern, "recursive": recursive}, f"Found {len(file_paths)} files")
            return file_paths
        except Exception as e:
            self.logger.error(f"Error scanning directory {directory_path}: {e}")
            raise


class BackupTool(BaseAgentTool):
    """Tool for creating backups."""
    
    def __init__(self):
        super().__init__(
            name="create_backup",
            description="Create backup of files or directories"
        )
    
    async def run(self, source_path: str, backup_location: str) -> str:
        """Create backup of source path."""
        try:
            source = Path(source_path)
            backup_path = Path(backup_location)
            
            # Create backup directory if it doesn't exist
            backup_path.mkdir(parents=True, exist_ok=True)
            
            if source.is_file():
                # Backup single file
                backup_file = backup_path / source.name
                shutil.copy2(source, backup_file)
            else:
                # Backup directory
                backup_dir = backup_path / source.name
                shutil.copytree(source, backup_dir, dirs_exist_ok=True)
            
            self.log_usage({"source_path": source_path, "backup_location": backup_location}, f"Backup created at {backup_path}")
            return str(backup_path)
        except Exception as e:
            self.logger.error(f"Error creating backup: {e}")
            raise
