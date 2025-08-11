"""
Process diffs and apply changes to the working directory.
"""

import difflib
import logging
import os
import re
from typing import Dict, List, Optional, Any, Tuple, Union
from pathlib import Path

logger = logging.getLogger(__name__)

class DiffProcessor:
    """
    Processes diffs and applies changes to the working directory.
    
    Handles unified diff format, patch files, and custom diff formats
    used by AI agents for code modifications.
    """
    
    def __init__(self, working_dir: str = "."):
        self.working_dir = Path(working_dir).resolve()
        
    def parse_unified_diff(self, diff_text: str) -> List[Dict[str, Any]]:
        """
        Parse unified diff format and extract changes.
        
        Args:
            diff_text: Unified diff text
            
        Returns:
            List of file changes with metadata
        """
        changes = []
        current_file = None
        current_hunks = []
        
        lines = diff_text.split('\n')
        i = 0
        
        while i < len(lines):
            line = lines[i]
            
            # File header
            if line.startswith('--- ') or line.startswith('+++ '):
                if current_file and current_hunks:
                    changes.append({
                        'file': current_file,
                        'hunks': current_hunks.copy()
                    })
                    current_hunks = []
                
                if line.startswith('--- '):
                    # Original file
                    current_file = line[4:].split('\t')[0]
                elif line.startswith('+++ '):
                    # Modified file
                    current_file = line[4:].split('\t')[0]
            
            # Hunk header
            elif line.startswith('@@'):
                hunk = self._parse_hunk_header(line)
                hunk['lines'] = []
                current_hunks.append(hunk)
            
            # Content lines
            elif current_hunks and line.startswith((' ', '+', '-')):
                current_hunks[-1]['lines'].append(line)
            
            i += 1
        
        # Add the last file
        if current_file and current_hunks:
            changes.append({
                'file': current_file,
                'hunks': current_hunks
            })
        
        return changes
    
    def _parse_hunk_header(self, header: str) -> Dict[str, Any]:
        """Parse a hunk header line."""
        # Format: @@ -start,count +start,count @@
        match = re.match(r'^@@ -(\d+),?(\d+)? \+(\d+),?(\d+)? @@', header)
        if match:
            old_start = int(match.group(1))
            old_count = int(match.group(2)) if match.group(2) else 1
            new_start = int(match.group(3))
            new_count = int(match.group(4)) if match.group(4) else 1
            
            return {
                'old_start': old_start,
                'old_count': old_count,
                'new_start': new_start,
                'new_count': new_count
            }
        
        return {}
    
    def apply_unified_diff(self, diff_text: str, dry_run: bool = False) -> Dict[str, Any]:
        """
        Apply a unified diff to files.
        
        Args:
            diff_text: Unified diff text
            dry_run: If True, don't actually apply changes
            
        Returns:
            Dictionary with results of the operation
        """
        changes = self.parse_unified_diff(diff_text)
        results = {
            'success': True,
            'files_modified': [],
            'files_created': [],
            'files_deleted': [],
            'errors': []
        }
        
        for change in changes:
            file_path = change['file']
            full_path = self.working_dir / file_path.lstrip('/')
            
            try:
                if not dry_run:
                    self._apply_file_changes(full_path, change['hunks'])
                
                if full_path.exists():
                    results['files_modified'].append(str(full_path))
                else:
                    results['files_created'].append(str(full_path))
                    
            except Exception as e:
                results['errors'].append(f"Error processing {file_path}: {e}")
                results['success'] = False
        
        return results
    
    def _apply_file_changes(self, file_path: Path, hunks: List[Dict[str, Any]]):
        """Apply changes to a specific file."""
        # Read original file
        original_lines = []
        if file_path.exists():
            with open(file_path, 'r', encoding='utf-8') as f:
                original_lines = f.readlines()
        
        # Apply hunks
        new_lines = original_lines.copy()
        
        for hunk in hunks:
            new_lines = self._apply_hunk(new_lines, hunk)
        
        # Write modified file
        file_path.parent.mkdir(parents=True, exist_ok=True)
        with open(file_path, 'w', encoding='utf-8') as f:
            f.writelines(new_lines)
    
    def _apply_hunk(self, lines: List[str], hunk: Dict[str, Any]) -> List[str]:
        """Apply a single hunk to a list of lines."""
        new_lines = lines.copy()
        hunk_lines = hunk['lines']
        
        # Calculate positions
        old_start = hunk['old_start'] - 1  # Convert to 0-based index
        new_start = hunk['new_start'] - 1
        
        # Remove old lines
        if 'old_count' in hunk:
            del new_lines[old_start:old_start + hunk['old_count']]
        
        # Insert new lines
        new_content = []
        for line in hunk_lines:
            if line.startswith('+'):
                new_content.append(line[1:] + '\n')
            elif line.startswith(' '):
                new_content.append(line[1:] + '\n')
        
        new_lines[old_start:old_start] = new_content
        
        return new_lines
    
    def create_diff(self, original_content: str, modified_content: str, filename: str = "file") -> str:
        """
        Create a unified diff between original and modified content.
        
        Args:
            original_content: Original file content
            modified_content: Modified file content
            filename: Name of the file being diffed
            
        Returns:
            Unified diff text
        """
        original_lines = original_content.splitlines(keepends=True)
        modified_lines = modified_content.splitlines(keepends=True)
        
        diff = difflib.unified_diff(
            original_lines,
            modified_lines,
            fromfile=f"a/{filename}",
            tofile=f"b/{filename}",
            lineterm=""
        )
        
        return '\n'.join(diff)
    
    def parse_agent_diff(self, diff_text: str) -> List[Dict[str, Any]]:
        """
        Parse custom diff format used by AI agents.
        
        This handles various formats that agents might use to describe changes.
        """
        changes = []
        
        # Pattern for agent-style diffs
        patterns = [
            # Replace pattern: REPLACE filename.ext:line_start-line_end with new_content
            r'REPLACE\s+([\w\-\.\/\\]+):(\d+)-(\d+)\s+with\s+(.*?)(?=\n[A-Z]|$)',
            # Insert pattern: INSERT filename.ext:line_number new_content
            r'INSERT\s+([\w\-\.\/\\]+):(\d+)\s+(.*?)(?=\n[A-Z]|$)',
            # Delete pattern: DELETE filename.ext:line_start-line_end
            r'DELETE\s+([\w\-\.\/\\]+):(\d+)-(\d+)',
            # Add file pattern: ADD filename.ext content
            r'ADD\s+([\w\-\.\/\\]+)\s+(.*?)(?=\n[A-Z]|$)'
        ]
        
        for pattern in patterns:
            matches = re.findall(pattern, diff_text, re.DOTALL | re.IGNORECASE)
            for match in matches:
                if len(match) == 4:  # REPLACE
                    changes.append({
                        'type': 'replace',
                        'file': match[0],
                        'start_line': int(match[1]),
                        'end_line': int(match[2]),
                        'content': match[3].strip()
                    })
                elif len(match) == 3:  # INSERT
                    changes.append({
                        'type': 'insert',
                        'file': match[0],
                        'line': int(match[1]),
                        'content': match[2].strip()
                    })
                elif len(match) == 3:  # DELETE
                    changes.append({
                        'type': 'delete',
                        'file': match[0],
                        'start_line': int(match[1]),
                        'end_line': int(match[2])
                    })
                elif len(match) == 2:  # ADD
                    changes.append({
                        'type': 'add',
                        'file': match[0],
                        'content': match[1].strip()
                    })
        
        return changes
    
    def apply_agent_diff(self, diff_text: str, dry_run: bool = False) -> Dict[str, Any]:
        """
        Apply agent-style diff to files.
        
        Args:
            diff_text: Agent diff text
            dry_run: If True, don't actually apply changes
            
        Returns:
            Dictionary with results of the operation
        """
        changes = self.parse_agent_diff(diff_text)
        results = {
            'success': True,
            'files_modified': [],
            'files_created': [],
            'files_deleted': [],
            'errors': []
        }
        
        for change in changes:
            try:
                file_path = self.working_dir / change['file']
                
                if change['type'] == 'add':
                    if not dry_run:
                        file_path.parent.mkdir(parents=True, exist_ok=True)
                        with open(file_path, 'w', encoding='utf-8') as f:
                            f.write(change['content'])
                    results['files_created'].append(str(file_path))
                
                elif change['type'] in ['replace', 'insert', 'delete']:
                    if not dry_run:
                        self._apply_agent_change(file_path, change)
                    results['files_modified'].append(str(file_path))
                
            except Exception as e:
                results['errors'].append(f"Error processing {change['file']}: {e}")
                results['success'] = False
        
        return results
    
    def _apply_agent_change(self, file_path: Path, change: Dict[str, Any]):
        """Apply a single agent change to a file."""
        # Read current content
        lines = []
        if file_path.exists():
            with open(file_path, 'r', encoding='utf-8') as f:
                lines = f.readlines()
        
        if change['type'] == 'replace':
            # Replace lines
            start = change['start_line'] - 1
            end = change['end_line']
            new_content = change['content'].splitlines(keepends=True)
            lines[start:end] = new_content
        
        elif change['type'] == 'insert':
            # Insert at line
            line_num = change['line'] - 1
            new_content = change['content'].splitlines(keepends=True)
            lines[line_num:line_num] = new_content
        
        elif change['type'] == 'delete':
            # Delete lines
            start = change['start_line'] - 1
            end = change['end_line']
            del lines[start:end]
        
        # Write back
        with open(file_path, 'w', encoding='utf-8') as f:
            f.writelines(lines)
    
    def validate_diff(self, diff_text: str) -> Tuple[bool, List[str]]:
        """
        Validate a diff for safety and correctness.
        
        Returns:
            Tuple of (is_valid, list_of_errors)
        """
        errors = []
        
        # Check for dangerous patterns
        dangerous_patterns = [
            r'\.\./',  # Directory traversal
            r'~',      # Home directory
            r'/etc/', r'/var/', r'/usr/', r'/bin/', r'/sbin/',  # System directories
        ]
        
        for pattern in dangerous_patterns:
            if re.search(pattern, diff_text):
                errors.append(f"Dangerous pattern found: {pattern}")
        
        # Check for reasonable file extensions
        file_extensions = re.findall(r'\.(\w+)(?:\s|$|:)', diff_text)
        valid_extensions = {
            'js', 'jsx', 'ts', 'tsx', 'py', 'html', 'css', 'json', 'md', 'txt',
            'astro', 'vue', 'svelte', 'yml', 'yaml', 'xml', 'svg'
        }
        
        for ext in file_extensions:
            if ext.lower() not in valid_extensions:
                errors.append(f"Unsupported file extension: {ext}")
        
        return len(errors) == 0, errors 