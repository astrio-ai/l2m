"""
Parse agent conversation output into actual project files.
"""

import re
import logging
from typing import Dict, List, Optional, Any, Tuple
from pathlib import Path

logger = logging.getLogger(__name__)

class ChatToFiles:
    """
    Converts agent conversation output into actual project files.
    
    Parses code blocks, file paths, and other structured content
    from agent responses and creates the corresponding files.
    """
    
    def __init__(self):
        self.file_patterns = [
            # ```filename.ext\ncontent\n```
            r'```(\w+[\/\\]?[\w\-\.\/\\]*)\.(\w+)\n(.*?)```',
            # ```ext:filename.ext\ncontent\n```
            r'```(\w+):([\w\-\.\/\\]+)\n(.*?)```',
            # ```\nfilename.ext\ncontent\n```
            r'```\n([\w\-\.\/\\]+)\n(.*?)```',
            # File: filename.ext\ncontent
            r'File:\s*([\w\-\.\/\\]+)\n(.*?)(?=\nFile:|$)',
            # Create file: filename.ext\ncontent
            r'Create file:\s*([\w\-\.\/\\]+)\n(.*?)(?=\nCreate file:|$)'
        ]
        
        self.code_block_pattern = r'```(\w+)?\n(.*?)```'
        self.file_extension_map = {
            'js': 'javascript',
            'jsx': 'javascript',
            'ts': 'typescript',
            'tsx': 'typescript',
            'py': 'python',
            'html': 'html',
            'css': 'css',
            'json': 'json',
            'md': 'markdown',
            'txt': 'text',
            'astro': 'astro',
            'vue': 'vue',
            'svelte': 'svelte'
        }
    
    def parse_conversation(self, conversation_text: str) -> Dict[str, str]:
        """
        Parse conversation text and extract files.
        
        Args:
            conversation_text: Raw conversation text from agents
            
        Returns:
            Dictionary mapping file paths to file contents
        """
        files = {}
        
        # Try each pattern to extract files
        for pattern in self.file_patterns:
            matches = re.findall(pattern, conversation_text, re.DOTALL | re.MULTILINE)
            
            for match in matches:
                if len(match) == 3:  # Pattern with extension
                    filename, ext, content = match
                    filepath = f"{filename}.{ext}"
                elif len(match) == 2:  # Pattern without extension
                    filepath, content = match
                else:
                    continue
                
                # Clean up the content
                content = self._clean_content(content)
                
                if content.strip():
                    files[filepath] = content
        
        # Also look for code blocks that might be files
        code_blocks = re.findall(self.code_block_pattern, conversation_text, re.DOTALL)
        
        for lang, content in code_blocks:
            # Try to infer filename from context
            filename = self._infer_filename_from_context(conversation_text, lang, content)
            if filename:
                files[filename] = self._clean_content(content)
        
        return files
    
    def _clean_content(self, content: str) -> str:
        """Clean up file content."""
        # Remove leading/trailing whitespace
        content = content.strip()
        
        # Remove common prefixes that might be added by agents
        prefixes_to_remove = [
            "Here's the code:",
            "Here is the code:",
            "The code is:",
            "Code:",
            "File content:",
            "Content:"
        ]
        
        for prefix in prefixes_to_remove:
            if content.startswith(prefix):
                content = content[len(prefix):].strip()
        
        return content
    
    def _infer_filename_from_context(self, context: str, lang: str, content: str) -> Optional[str]:
        """Try to infer filename from context."""
        # Look for mentions of files before the code block
        lines = context.split('\n')
        
        for i, line in enumerate(lines):
            if '```' in line and lang in line:
                # Look backwards for file mentions
                for j in range(max(0, i-10), i):
                    prev_line = lines[j].strip()
                    
                    # Check for common file mention patterns
                    if any(pattern in prev_line.lower() for pattern in [
                        'file:', 'create', 'generate', 'write', 'save'
                    ]):
                        # Extract potential filename
                        words = prev_line.split()
                        for word in words:
                            if '.' in word and any(ext in word for ext in self.file_extension_map.keys()):
                                return word
        
        # If no filename found, try to create one based on language
        if lang:
            ext = self._get_extension_for_language(lang)
            if ext:
                return f"generated_file.{ext}"
        
        return None
    
    def _get_extension_for_language(self, lang: str) -> Optional[str]:
        """Get file extension for a given language."""
        lang_lower = lang.lower()
        
        # Direct mapping
        if lang_lower in self.file_extension_map:
            return lang_lower
        
        # Reverse mapping
        for ext, language in self.file_extension_map.items():
            if language == lang_lower:
                return ext
        
        # Common mappings
        mappings = {
            'javascript': 'js',
            'typescript': 'ts',
            'python': 'py',
            'html': 'html',
            'css': 'css',
            'json': 'json',
            'markdown': 'md',
            'text': 'txt',
            'astro': 'astro',
            'vue': 'vue',
            'svelte': 'svelte',
            'jsx': 'jsx',
            'tsx': 'tsx'
        }
        
        return mappings.get(lang_lower)
    
    def extract_file_operations(self, conversation_text: str) -> List[Dict[str, Any]]:
        """
        Extract file operations from conversation.
        
        Returns:
            List of file operations with type, path, and content
        """
        operations = []
        
        # Look for file creation patterns
        create_patterns = [
            r'create\s+file:\s*([\w\-\.\/\\]+)\n(.*?)(?=\n|$)',
            r'create\s+([\w\-\.\/\\]+)\n(.*?)(?=\n|$)',
            r'new\s+file:\s*([\w\-\.\/\\]+)\n(.*?)(?=\n|$)'
        ]
        
        for pattern in create_patterns:
            matches = re.findall(pattern, conversation_text, re.DOTALL | re.IGNORECASE)
            for filepath, content in matches:
                operations.append({
                    'type': 'create',
                    'path': filepath.strip(),
                    'content': self._clean_content(content)
                })
        
        # Look for file modification patterns
        modify_patterns = [
            r'update\s+file:\s*([\w\-\.\/\\]+)\n(.*?)(?=\n|$)',
            r'modify\s+([\w\-\.\/\\]+)\n(.*?)(?=\n|$)',
            r'edit\s+([\w\-\.\/\\]+)\n(.*?)(?=\n|$)'
        ]
        
        for pattern in modify_patterns:
            matches = re.findall(pattern, conversation_text, re.DOTALL | re.IGNORECASE)
            for filepath, content in matches:
                operations.append({
                    'type': 'modify',
                    'path': filepath.strip(),
                    'content': self._clean_content(content)
                })
        
        # Look for file deletion patterns
        delete_patterns = [
            r'delete\s+file:\s*([\w\-\.\/\\]+)',
            r'remove\s+([\w\-\.\/\\]+)',
            r'delete\s+([\w\-\.\/\\]+)'
        ]
        
        for pattern in delete_patterns:
            matches = re.findall(pattern, conversation_text, re.IGNORECASE)
            for filepath in matches:
                operations.append({
                    'type': 'delete',
                    'path': filepath.strip(),
                    'content': None
                })
        
        return operations
    
    def validate_file_path(self, filepath: str) -> bool:
        """Validate if a file path is safe and reasonable."""
        # Check for dangerous patterns
        dangerous_patterns = [
            '..',  # Directory traversal
            '~',   # Home directory
            '/etc', '/var', '/usr', '/bin', '/sbin',  # System directories
            'C:\\', 'D:\\',  # Windows system drives
        ]
        
        for pattern in dangerous_patterns:
            if pattern in filepath:
                return False
        
        # Check for reasonable file extensions
        valid_extensions = set(self.file_extension_map.keys()) | {
            'jsx', 'tsx', 'astro', 'vue', 'svelte', 'md', 'txt', 'yml', 'yaml'
        }
        
        if '.' in filepath:
            ext = filepath.split('.')[-1].lower()
            if ext not in valid_extensions:
                return False
        
        return True
    
    def sanitize_file_path(self, filepath: str) -> str:
        """Sanitize a file path to make it safe."""
        # Remove any dangerous characters
        filepath = re.sub(r'[<>:"|?*]', '_', filepath)
        
        # Remove leading/trailing dots and slashes
        filepath = filepath.strip('./\\')
        
        # Ensure it doesn't start with system paths
        if filepath.startswith(('etc/', 'var/', 'usr/', 'bin/', 'sbin/')):
            filepath = f"app_{filepath}"
        
        return filepath 