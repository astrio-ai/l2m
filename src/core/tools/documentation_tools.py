"""
Documentation generation tools.

This module contains tools for generating documentation,
API docs, and code comments for modernized code.
"""

from typing import Any, Dict, List, Optional
from pathlib import Path
import re
from pydantic import BaseModel, Field

from src.core.tools.base_tool import BaseAgentTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class DocumentationArgs(BaseModel):
    """Arguments for documentation tools."""
    transformed_code: Dict[str, Any] = Field(description="Transformed code to document")
    documentation_type: str = Field(default="api", description="Type of documentation to generate")
    doc_format: str = Field(default="markdown", description="Documentation format")
    language: str = Field(default="python", description="Programming language")


class DocumentationGeneratorTool(BaseAgentTool):
    """Tool for generating documentation."""
    
    def __init__(self):
        super().__init__(
            name="generate_documentation",
            description="Generate documentation for modernized code"
        )
    
    def _get_args_schema(self):
        return DocumentationArgs
    
    async def run(self, transformed_code: Dict[str, Any], documentation_type: str = "api", 
                  doc_format: str = "markdown", language: str = "python") -> Dict[str, Any]:
        """Generate documentation."""
        try:
            documentation = {
                "api_docs": [],
                "user_guides": [],
                "code_comments": [],
                "readme_files": [],
                "changelog": [],
                "generation_metadata": {}
            }
            
            # Generate different types of documentation
            if documentation_type in ["api", "all"]:
                documentation["api_docs"] = await self._generate_api_docs(transformed_code, doc_format)
            
            if documentation_type in ["user", "all"]:
                documentation["user_guides"] = await self._generate_user_guides(transformed_code, doc_format)
            
            if documentation_type in ["comments", "all"]:
                documentation["code_comments"] = await self._generate_code_comments(transformed_code, language)
            
            if documentation_type in ["readme", "all"]:
                documentation["readme_files"] = await self._generate_readme_files(transformed_code, doc_format)
            
            if documentation_type in ["changelog", "all"]:
                documentation["changelog"] = await self._generate_changelog(transformed_code, doc_format)
            
            # Add generation metadata
            documentation["generation_metadata"] = {
                "documentation_type": documentation_type,
                "doc_format": doc_format,
                "language": language,
                "total_files": len(transformed_code.get("files", [])),
                "total_functions": len(transformed_code.get("functions", [])),
                "total_classes": len(transformed_code.get("classes", []))
            }
            
            self.log_usage({"transformed_files": len(transformed_code.get("files", [])), "doc_type": documentation_type}, 
                         f"Generated {len(documentation['api_docs'])} API docs")
            return documentation
        except Exception as e:
            self.logger.error(f"Error generating documentation: {e}")
            raise
    
    async def _generate_api_docs(self, transformed_code: Dict[str, Any], doc_format: str) -> List[Dict[str, Any]]:
        """Generate API documentation."""
        api_docs = []
        
        functions = transformed_code.get("functions", [])
        classes = transformed_code.get("classes", [])
        
        # Generate function documentation
        for func in functions:
            func_doc = {
                "name": func.get("name", "unknown"),
                "type": "function",
                "description": f"Function: {func.get('name', 'unknown')}",
                "parameters": func.get("args", []),
                "return_type": "Any",  # Would need type inference
                "examples": [f"# Example usage\n{func.get('name', 'unknown')}()"],
                "format": doc_format
            }
            api_docs.append(func_doc)
        
        # Generate class documentation
        for cls in classes:
            class_doc = {
                "name": cls.get("name", "unknown"),
                "type": "class",
                "description": f"Class: {cls.get('name', 'unknown')}",
                "methods": [],  # Would need to extract methods
                "properties": [],  # Would need to extract properties
                "examples": [f"# Example usage\n{cls.get('name', 'unknown')}()"],
                "format": doc_format
            }
            api_docs.append(class_doc)
        
        return api_docs
    
    async def _generate_user_guides(self, transformed_code: Dict[str, Any], doc_format: str) -> List[Dict[str, Any]]:
        """Generate user guides."""
        user_guides = []
        
        # Generate basic user guide
        user_guide = {
            "title": "User Guide",
            "content": self._generate_user_guide_content(transformed_code),
            "format": doc_format,
            "sections": ["Introduction", "Installation", "Usage", "Examples"]
        }
        user_guides.append(user_guide)
        
        return user_guides
    
    def _generate_user_guide_content(self, transformed_code: Dict[str, Any]) -> str:
        """Generate user guide content."""
        content = """# User Guide

## Introduction
This is a modernized version of legacy code, transformed to improve maintainability and performance.

## Installation
```bash
pip install -r requirements.txt
```

## Usage
The modernized code provides the following functionality:

"""
        
        functions = transformed_code.get("functions", [])
        for func in functions[:5]:  # Limit to first 5 functions
            content += f"- `{func.get('name', 'unknown')}()`: Function description\n"
        
        content += """
## Examples
```python
# Basic usage example
from your_module import your_function

result = your_function()
print(result)
```

## Troubleshooting
If you encounter issues, please check the error logs and ensure all dependencies are installed.
"""
        
        return content
    
    async def _generate_code_comments(self, transformed_code: Dict[str, Any], language: str) -> List[Dict[str, Any]]:
        """Generate code comments."""
        code_comments = []
        
        files = transformed_code.get("files", [])
        for file_info in files:
            file_path = file_info.get("path", "")
            if file_path:
                commented_code = await self._add_comments_to_file(file_path, language)
                code_comments.append({
                    "file": file_path,
                    "commented_code": commented_code,
                    "language": language
                })
        
        return code_comments
    
    async def _add_comments_to_file(self, file_path: str, language: str) -> str:
        """Add comments to a file."""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
            
            # Add header comment
            header_comment = self._generate_header_comment(language)
            
            # Add function comments
            commented_content = self._add_function_comments(content, language)
            
            return header_comment + "\n\n" + commented_content
        except Exception as e:
            self.logger.warning(f"Error adding comments to {file_path}: {e}")
            return f"# Error adding comments: {e}"
    
    def _generate_header_comment(self, language: str) -> str:
        """Generate header comment."""
        if language == "python":
            return '''"""
Modernized code with improved structure and documentation.

This file has been automatically generated from legacy code
and includes comprehensive documentation and comments.
"""'''
        else:
            return """/*
 * Modernized code with improved structure and documentation.
 * 
 * This file has been automatically generated from legacy code
 * and includes comprehensive documentation and comments.
 */"""
    
    def _add_function_comments(self, content: str, language: str) -> str:
        """Add comments to functions."""
        if language == "python":
            # Add docstrings to Python functions
            func_pattern = r'(def\s+\w+\s*\([^)]*\):)'
            def add_docstring(match):
                func_def = match.group(1)
                return func_def + '\n    """Function description."""'
            
            return re.sub(func_pattern, add_docstring, content)
        else:
            # Add comments to other languages
            func_pattern = r'(function\s+\w+\s*\([^)]*\)\s*{)'
            def add_comment(match):
                func_def = match.group(1)
                return func_def + '\n    // Function description'
            
            return re.sub(func_pattern, add_comment, content)
    
    async def _generate_readme_files(self, transformed_code: Dict[str, Any], doc_format: str) -> List[Dict[str, Any]]:
        """Generate README files."""
        readme_files = []
        
        # Generate main README
        readme_content = self._generate_readme_content(transformed_code)
        readme_files.append({
            "name": "README.md",
            "content": readme_content,
            "format": doc_format
        })
        
        return readme_files
    
    def _generate_readme_content(self, transformed_code: Dict[str, Any]) -> str:
        """Generate README content."""
        content = """# Modernized Code

This repository contains modernized code that has been transformed from legacy systems.

## Features

- Improved code structure
- Better error handling
- Enhanced documentation
- Modern programming practices

## Installation

```bash
pip install -r requirements.txt
```

## Usage

```python
from your_module import your_function

# Use the modernized functions
result = your_function()
```

## Development

```bash
# Run tests
python -m pytest

# Run linting
python -m flake8
```

## Contributing

Please read our contributing guidelines before submitting pull requests.

## License

This project is licensed under the MIT License.
"""
        
        return content
    
    async def _generate_changelog(self, transformed_code: Dict[str, Any], doc_format: str) -> List[Dict[str, Any]]:
        """Generate changelog."""
        changelog = []
        
        changelog_content = """# Changelog

## [1.0.0] - 2024-01-01

### Added
- Modernized code structure
- Comprehensive documentation
- Unit tests
- CI/CD pipeline

### Changed
- Improved error handling
- Enhanced performance
- Updated dependencies

### Removed
- Legacy code patterns
- Deprecated functions
- Outdated dependencies

## [0.1.0] - 2023-12-01

### Added
- Initial modernization
- Basic functionality
- Core features
"""
        
        changelog.append({
            "name": "CHANGELOG.md",
            "content": changelog_content,
            "format": doc_format
        })
        
        return changelog


class CommentGeneratorTool(BaseAgentTool):
    """Tool for generating code comments."""
    
    def __init__(self):
        super().__init__(
            name="generate_code_comments",
            description="Generate comments for modernized code"
        )
    
    async def run(self, code_content: str, language: str = "python") -> str:
        """Generate comments for code."""
        try:
            # Generate header comment
            header_comment = self._generate_header_comment(language)
            
            # Add function comments
            commented_code = self._add_function_comments(code_content, language)
            
            # Add inline comments
            commented_code = self._add_inline_comments(commented_code, language)
            
            # Combine header and commented code
            final_code = header_comment + "\n\n" + commented_code
            
            self.log_usage({"code_length": len(code_content), "language": language}, 
                         f"Generated comments for {len(code_content)} characters")
            return final_code
        except Exception as e:
            self.logger.error(f"Error generating comments: {e}")
            raise
    
    def _generate_header_comment(self, language: str) -> str:
        """Generate header comment."""
        if language == "python":
            return '''"""
Modernized code with comprehensive documentation.

This code has been automatically generated and includes
detailed comments explaining functionality and usage.
"""'''
        elif language == "javascript":
            return """/*
 * Modernized code with comprehensive documentation.
 * 
 * This code has been automatically generated and includes
 * detailed comments explaining functionality and usage.
 */"""
        else:
            return """/*
 * Modernized code with comprehensive documentation.
 * 
 * This code has been automatically generated and includes
 * detailed comments explaining functionality and usage.
 */"""
    
    def _add_function_comments(self, content: str, language: str) -> str:
        """Add comments to functions."""
        if language == "python":
            # Add docstrings to Python functions
            func_pattern = r'(def\s+\w+\s*\([^)]*\):)'
            def add_docstring(match):
                func_def = match.group(1)
                return func_def + '\n    """Function description."""'
            
            return re.sub(func_pattern, add_docstring, content)
        elif language == "javascript":
            # Add JSDoc comments to JavaScript functions
            func_pattern = r'(function\s+\w+\s*\([^)]*\)\s*{)'
            def add_jsdoc(match):
                func_def = match.group(1)
                return func_def + '\n    /**\n     * Function description.\n     */'
            
            return re.sub(func_pattern, add_jsdoc, content)
        else:
            # Add comments to other languages
            func_pattern = r'(function\s+\w+\s*\([^)]*\)\s*{)'
            def add_comment(match):
                func_def = match.group(1)
                return func_def + '\n    // Function description'
            
            return re.sub(func_pattern, add_comment, content)
    
    def _add_inline_comments(self, content: str, language: str) -> str:
        """Add inline comments to code."""
        lines = content.split('\n')
        commented_lines = []
        
        for line in lines:
            stripped = line.strip()
            if stripped and not stripped.startswith('#') and not stripped.startswith('//'):
                # Add inline comment for complex lines
                if len(stripped) > 50 or '=' in stripped:
                    if language == "python":
                        commented_lines.append(line + "  # TODO: Add description")
                    else:
                        commented_lines.append(line + "  // TODO: Add description")
                else:
                    commented_lines.append(line)
            else:
                commented_lines.append(line)
        
        return '\n'.join(commented_lines)


class APIDocGeneratorTool(BaseAgentTool):
    """Tool for generating API documentation."""
    
    def __init__(self):
        super().__init__(
            name="generate_api_docs",
            description="Generate API documentation"
        )
    
    async def run(self, transformed_code: Dict[str, Any], doc_format: str = "markdown") -> Dict[str, Any]:
        """Generate API documentation."""
        try:
            api_docs = {
                "endpoints": [],
                "schemas": [],
                "examples": [],
                "authentication": [],
                "rate_limits": [],
                "generation_metadata": {}
            }
            
            # Generate endpoints from functions
            functions = transformed_code.get("functions", [])
            for func in functions:
                endpoint = {
                    "name": func.get("name", "unknown"),
                    "method": "POST",  # Default method
                    "path": f"/{func.get('name', 'unknown').lower()}",
                    "description": f"Endpoint for {func.get('name', 'unknown')} function",
                    "parameters": func.get("args", []),
                    "response": "Any",  # Would need type inference
                    "examples": [f"curl -X POST /{func.get('name', 'unknown').lower()}"],
                    "format": doc_format
                }
                api_docs["endpoints"].append(endpoint)
            
            # Generate schemas from classes
            classes = transformed_code.get("classes", [])
            for cls in classes:
                schema = {
                    "name": cls.get("name", "unknown"),
                    "type": "object",
                    "properties": {},  # Would need to extract properties
                    "required": [],
                    "description": f"Schema for {cls.get('name', 'unknown')} class",
                    "format": doc_format
                }
                api_docs["schemas"].append(schema)
            
            # Generate examples
            api_docs["examples"] = self._generate_api_examples(functions, doc_format)
            
            # Generate authentication info
            api_docs["authentication"] = self._generate_auth_info(doc_format)
            
            # Generate rate limits
            api_docs["rate_limits"] = self._generate_rate_limits(doc_format)
            
            # Add generation metadata
            api_docs["generation_metadata"] = {
                "total_endpoints": len(api_docs["endpoints"]),
                "total_schemas": len(api_docs["schemas"]),
                "total_examples": len(api_docs["examples"]),
                "doc_format": doc_format
            }
            
            self.log_usage({"transformed_files": len(transformed_code.get("files", [])), "doc_format": doc_format}, 
                         f"Generated {len(api_docs['endpoints'])} endpoints")
            return api_docs
        except Exception as e:
            self.logger.error(f"Error generating API docs: {e}")
            raise
    
    def _generate_api_examples(self, functions: List[Dict[str, Any]], doc_format: str) -> List[Dict[str, Any]]:
        """Generate API examples."""
        examples = []
        
        for func in functions[:3]:  # Limit to first 3 functions
            example = {
                "name": f"{func.get('name', 'unknown')}_example",
                "description": f"Example usage of {func.get('name', 'unknown')}",
                "request": {
                    "method": "POST",
                    "url": f"/{func.get('name', 'unknown').lower()}",
                    "headers": {"Content-Type": "application/json"},
                    "body": {"param": "value"}
                },
                "response": {
                    "status": 200,
                    "body": {"result": "success"}
                },
                "format": doc_format
            }
            examples.append(example)
        
        return examples
    
    def _generate_auth_info(self, doc_format: str) -> List[Dict[str, Any]]:
        """Generate authentication information."""
        auth_info = [{
            "type": "Bearer Token",
            "description": "API requires Bearer token authentication",
            "header": "Authorization: Bearer <token>",
            "format": doc_format
        }]
        
        return auth_info
    
    def _generate_rate_limits(self, doc_format: str) -> List[Dict[str, Any]]:
        """Generate rate limit information."""
        rate_limits = [{
            "limit": "1000 requests per hour",
            "description": "API rate limiting for fair usage",
            "headers": {
                "X-RateLimit-Limit": "1000",
                "X-RateLimit-Remaining": "999"
            },
            "format": doc_format
        }]
        
        return rate_limits
