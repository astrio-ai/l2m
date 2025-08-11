"""
Manage a dictionary of files in memory (filename → content).
"""

import hashlib
import logging
import os
from typing import Dict, List, Optional, Any, Set, Tuple
from pathlib import Path
from dataclasses import dataclass, field
from datetime import datetime

logger = logging.getLogger(__name__)

@dataclass
class FileInfo:
    """Information about a file in the dictionary."""
    path: str
    content: str
    size: int
    hash: str
    last_modified: datetime
    metadata: Dict[str, Any] = field(default_factory=dict)

class FilesDict:
    """
    Manages a dictionary of files in memory.
    
    Provides efficient file operations, content tracking, and metadata
    management for the modernization process.
    """
    
    def __init__(self):
        self.files: Dict[str, FileInfo] = {}
        self.file_hashes: Dict[str, str] = {}  # path -> hash
        self.hash_to_paths: Dict[str, Set[str]] = {}  # hash -> set of paths
        self.directory_structure: Dict[str, Set[str]] = {}  # dir -> set of files
        
    def add_file(self, path: str, content: str, metadata: Optional[Dict[str, Any]] = None) -> FileInfo:
        """
        Add a file to the dictionary.
        
        Args:
            path: File path
            content: File content
            metadata: Optional metadata
            
        Returns:
            FileInfo object for the added file
        """
        # Normalize path
        path = self._normalize_path(path)
        
        # Calculate file properties
        size = len(content)
        content_hash = self._calculate_hash(content)
        last_modified = datetime.now()
        
        # Create file info
        file_info = FileInfo(
            path=path,
            content=content,
            size=size,
            hash=content_hash,
            last_modified=last_modified,
            metadata=metadata or {}
        )
        
        # Update internal structures
        old_hash = self.file_hashes.get(path)
        if old_hash:
            self._remove_hash_mapping(path, old_hash)
        
        self.files[path] = file_info
        self.file_hashes[path] = content_hash
        self._add_hash_mapping(path, content_hash)
        self._update_directory_structure(path)
        
        logger.debug(f"Added file: {path} (size: {size}, hash: {content_hash[:8]})")
        return file_info
    
    def get_file(self, path: str) -> Optional[FileInfo]:
        """Get file information by path."""
        path = self._normalize_path(path)
        return self.files.get(path)
    
    def get_content(self, path: str) -> Optional[str]:
        """Get file content by path."""
        file_info = self.get_file(path)
        return file_info.content if file_info else None
    
    def update_file(self, path: str, content: str, metadata: Optional[Dict[str, Any]] = None) -> FileInfo:
        """Update an existing file."""
        path = self._normalize_path(path)
        
        if path not in self.files:
            raise KeyError(f"File not found: {path}")
        
        return self.add_file(path, content, metadata)
    
    def remove_file(self, path: str) -> bool:
        """
        Remove a file from the dictionary.
        
        Returns:
            True if file was removed, False if not found
        """
        path = self._normalize_path(path)
        
        if path not in self.files:
            return False
        
        file_info = self.files[path]
        self._remove_hash_mapping(path, file_info.hash)
        self._remove_from_directory_structure(path)
        
        del self.files[path]
        del self.file_hashes[path]
        
        logger.debug(f"Removed file: {path}")
        return True
    
    def list_files(self, directory: Optional[str] = None, pattern: Optional[str] = None) -> List[str]:
        """
        List files in the dictionary.
        
        Args:
            directory: Optional directory to list files from
            pattern: Optional glob pattern to filter files
            
        Returns:
            List of file paths
        """
        if directory:
            directory = self._normalize_path(directory)
            files = self.directory_structure.get(directory, set())
        else:
            files = set(self.files.keys())
        
        if pattern:
            import fnmatch
            files = {f for f in files if fnmatch.fnmatch(f, pattern)}
        
        return sorted(list(files))
    
    def find_duplicates(self) -> Dict[str, List[str]]:
        """
        Find files with duplicate content.
        
        Returns:
            Dictionary mapping content hash to list of file paths
        """
        duplicates = {}
        for hash_value, paths in self.hash_to_paths.items():
            if len(paths) > 1:
                duplicates[hash_value] = sorted(list(paths))
        
        return duplicates
    
    def search_content(self, query: str, case_sensitive: bool = False) -> List[Tuple[str, int, str]]:
        """
        Search for content in files.
        
        Args:
            query: Search query
            case_sensitive: Whether search should be case sensitive
            
        Returns:
            List of tuples (file_path, line_number, line_content)
        """
        results = []
        
        if not case_sensitive:
            query = query.lower()
        
        for path, file_info in self.files.items():
            lines = file_info.content.splitlines()
            
            for i, line in enumerate(lines, 1):
                search_line = line if case_sensitive else line.lower()
                if query in search_line:
                    results.append((path, i, line))
        
        return results
    
    def get_file_stats(self) -> Dict[str, Any]:
        """Get statistics about files in the dictionary."""
        total_files = len(self.files)
        total_size = sum(f.size for f in self.files.values())
        unique_hashes = len(self.hash_to_paths)
        duplicates = len([h for h, paths in self.hash_to_paths.items() if len(paths) > 1])
        
        # File type distribution
        extensions = {}
        for path in self.files.keys():
            ext = Path(path).suffix.lower()
            extensions[ext] = extensions.get(ext, 0) + 1
        
        return {
            'total_files': total_files,
            'total_size': total_size,
            'unique_content_files': unique_hashes,
            'duplicate_files': duplicates,
            'file_extensions': extensions,
            'average_file_size': total_size / total_files if total_files > 0 else 0
        }
    
    def export_to_directory(self, output_dir: str, overwrite: bool = False) -> List[str]:
        """
        Export all files to a directory.
        
        Args:
            output_dir: Output directory path
            overwrite: Whether to overwrite existing files
            
        Returns:
            List of exported file paths
        """
        exported_files = []
        output_path = Path(output_dir)
        
        for path, file_info in self.files.items():
            file_path = output_path / path
            
            # Check if file exists
            if file_path.exists() and not overwrite:
                logger.warning(f"File exists, skipping: {file_path}")
                continue
            
            # Create directory if needed
            file_path.parent.mkdir(parents=True, exist_ok=True)
            
            # Write file
            try:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(file_info.content)
                exported_files.append(str(file_path))
                logger.debug(f"Exported: {file_path}")
            except Exception as e:
                logger.error(f"Failed to export {file_path}: {e}")
        
        return exported_files
    
    def import_from_directory(self, input_dir: str, pattern: str = "**/*") -> List[str]:
        """
        Import files from a directory.
        
        Args:
            input_dir: Input directory path
            pattern: Glob pattern for files to import
            
        Returns:
            List of imported file paths
        """
        imported_files = []
        input_path = Path(input_dir)
        
        for file_path in input_path.glob(pattern):
            if file_path.is_file():
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        content = f.read()
                    
                    # Calculate relative path
                    rel_path = str(file_path.relative_to(input_path))
                    
                    # Add to dictionary
                    self.add_file(rel_path, content)
                    imported_files.append(rel_path)
                    logger.debug(f"Imported: {rel_path}")
                    
                except Exception as e:
                    logger.error(f"Failed to import {file_path}: {e}")
        
        return imported_files
    
    def get_dependencies(self, file_path: str) -> List[str]:
        """
        Get dependencies for a file (imports, includes, etc.).
        
        Args:
            file_path: Path to the file
            
        Returns:
            List of dependency paths
        """
        file_info = self.get_file(file_path)
        if not file_info:
            return []
        
        dependencies = []
        content = file_info.content
        
        # Common import patterns
        import re
        import_patterns = [
            r'import\s+["\']([^"\']+)["\']',  # ES6 imports
            r'require\s*\(\s*["\']([^"\']+)["\']',  # CommonJS requires
            r'#include\s+["<]([^">]+)[">]',  # C/C++ includes
            r'import\s+([a-zA-Z_][a-zA-Z0-9_.]*)',  # Python imports
            r'from\s+["\']([^"\']+)["\']',  # ES6 from imports
        ]
        
        for pattern in import_patterns:
            matches = re.findall(pattern, content)
            dependencies.extend(matches)
        
        return list(set(dependencies))  # Remove duplicates
    
    def _normalize_path(self, path: str) -> str:
        """Normalize a file path."""
        # Convert to forward slashes
        path = path.replace('\\', '/')
        
        # Remove leading/trailing slashes
        path = path.strip('/')
        
        # Handle relative paths
        if path.startswith('./'):
            path = path[2:]
        
        return path
    
    def _calculate_hash(self, content: str) -> str:
        """Calculate SHA-256 hash of content."""
        return hashlib.sha256(content.encode('utf-8')).hexdigest()
    
    def _add_hash_mapping(self, path: str, content_hash: str):
        """Add hash mapping for a file."""
        if content_hash not in self.hash_to_paths:
            self.hash_to_paths[content_hash] = set()
        self.hash_to_paths[content_hash].add(path)
    
    def _remove_hash_mapping(self, path: str, content_hash: str):
        """Remove hash mapping for a file."""
        if content_hash in self.hash_to_paths:
            self.hash_to_paths[content_hash].discard(path)
            if not self.hash_to_paths[content_hash]:
                del self.hash_to_paths[content_hash]
    
    def _update_directory_structure(self, path: str):
        """Update directory structure when adding a file."""
        path_parts = path.split('/')
        
        for i in range(len(path_parts) - 1):
            directory = '/'.join(path_parts[:i + 1])
            if directory not in self.directory_structure:
                self.directory_structure[directory] = set()
            self.directory_structure[directory].add(path)
    
    def _remove_from_directory_structure(self, path: str):
        """Remove file from directory structure."""
        path_parts = path.split('/')
        
        for i in range(len(path_parts) - 1):
            directory = '/'.join(path_parts[:i + 1])
            if directory in self.directory_structure:
                self.directory_structure[directory].discard(path)
                if not self.directory_structure[directory]:
                    del self.directory_structure[directory] 