"""
Code search and discovery tools.

This module contains tools for searching code patterns, finding references,
and discovering code relationships.
"""

from typing import Any, Dict, List, Optional
import re
import os
from pathlib import Path
from pydantic import BaseModel, Field

from src.core.tools.base_tool import BaseAgentTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class PatternSearchArgs(BaseModel):
    """Arguments for pattern search tools."""
    codebase_path: str = Field(description="Path to the codebase to search")
    patterns: List[str] = Field(description="Patterns to search for")
    file_extensions: Optional[List[str]] = Field(default=None, description="File extensions to include")
    case_sensitive: bool = Field(default=False, description="Whether search should be case sensitive")
    whole_word: bool = Field(default=False, description="Whether to match whole words only")


class ReferenceSearchArgs(BaseModel):
    """Arguments for reference search tools."""
    codebase_path: str = Field(description="Path to the codebase to search")
    symbol: str = Field(description="Symbol to find references for")
    symbol_type: str = Field(default="function", description="Type of symbol to search for")
    include_definitions: bool = Field(default=True, description="Whether to include definition locations")


class PatternSearchTool(BaseAgentTool):
    """Tool for searching code patterns."""
    
    def __init__(self):
        super().__init__(
            name="search_patterns",
            description="Search for specific code patterns"
        )
    
    def _get_args_schema(self):
        return PatternSearchArgs
    
    async def run(self, codebase_path: str, patterns: List[str], file_extensions: List[str] = None, 
                  case_sensitive: bool = False, whole_word: bool = False) -> Dict[str, Any]:
        """Search for patterns in codebase."""
        try:
            results = {
                "matches": [],
                "files_searched": 0,
                "patterns_found": {},
                "search_metadata": {}
            }
            
            codebase_path = Path(codebase_path)
            if not codebase_path.exists():
                raise ValueError(f"Codebase path does not exist: {codebase_path}")
            
            # Default file extensions
            if file_extensions is None:
                file_extensions = ['.py', '.js', '.ts', '.java', '.c', '.cpp', '.cobol', '.cbl']
            
            # Search for each pattern
            for pattern in patterns:
                pattern_matches = await self._search_pattern_in_codebase(
                    codebase_path, pattern, file_extensions, case_sensitive, whole_word
                )
                results["patterns_found"][pattern] = len(pattern_matches)
                results["matches"].extend(pattern_matches)
            
            # Count files searched
            results["files_searched"] = len(set(match["file"] for match in results["matches"]))
            
            # Add search metadata
            results["search_metadata"] = {
                "total_patterns": len(patterns),
                "total_matches": len(results["matches"]),
                "case_sensitive": case_sensitive,
                "whole_word": whole_word,
                "file_extensions": file_extensions
            }
            
            self.log_usage({"codebase_path": str(codebase_path), "patterns": len(patterns)}, 
                         f"Found {len(results['matches'])} matches")
            return results
        except Exception as e:
            self.logger.error(f"Error searching patterns: {e}")
            raise
    
    async def _search_pattern_in_codebase(self, codebase_path: Path, pattern: str, 
                                        file_extensions: List[str], case_sensitive: bool, 
                                        whole_word: bool) -> List[Dict[str, Any]]:
        """Search for a specific pattern in the codebase."""
        matches = []
        
        # Prepare regex pattern
        regex_pattern = self._prepare_regex_pattern(pattern, case_sensitive, whole_word)
        
        try:
            compiled_pattern = re.compile(regex_pattern)
        except re.error as e:
            self.logger.error(f"Invalid regex pattern '{pattern}': {e}")
            return matches
        
        # Search through files
        for file_path in codebase_path.rglob('*'):
            if file_path.is_file() and file_path.suffix.lower() in file_extensions:
                file_matches = await self._search_pattern_in_file(file_path, compiled_pattern, pattern)
                matches.extend(file_matches)
        
        return matches
    
    def _prepare_regex_pattern(self, pattern: str, case_sensitive: bool, whole_word: bool) -> str:
        """Prepare regex pattern for searching."""
        # Escape special regex characters if not already a regex
        if not self._is_regex_pattern(pattern):
            pattern = re.escape(pattern)
        
        # Add word boundaries if whole word matching is requested
        if whole_word:
            pattern = r'\b' + pattern + r'\b'
        
        return pattern
    
    def _is_regex_pattern(self, pattern: str) -> bool:
        """Check if pattern contains regex special characters."""
        regex_chars = r'[]{}()*+?^$|\.'
        return any(char in pattern for char in regex_chars)
    
    async def _search_pattern_in_file(self, file_path: Path, compiled_pattern: re.Pattern, 
                                    original_pattern: str) -> List[Dict[str, Any]]:
        """Search for pattern in a specific file."""
        matches = []
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            for line_num, line in enumerate(lines, 1):
                for match in compiled_pattern.finditer(line):
                    matches.append({
                        "file": str(file_path),
                        "line": line_num,
                        "column": match.start(),
                        "text": match.group(),
                        "context": line.strip(),
                        "pattern": original_pattern
                    })
        except Exception as e:
            self.logger.warning(f"Error reading file {file_path}: {e}")
        
        return matches


class ReferenceFinderTool(BaseAgentTool):
    """Tool for finding code references."""
    
    def __init__(self):
        super().__init__(
            name="find_references",
            description="Find references to specific code elements"
        )
    
    def _get_args_schema(self):
        return ReferenceSearchArgs
    
    async def run(self, codebase_path: str, symbol: str, symbol_type: str = "function", 
                  include_definitions: bool = True) -> Dict[str, Any]:
        """Find references to a symbol."""
        try:
            results = {
                "references": [],
                "definition_location": None,
                "usage_count": 0,
                "symbol_type": symbol_type,
                "search_metadata": {}
            }
            
            codebase_path = Path(codebase_path)
            if not codebase_path.exists():
                raise ValueError(f"Codebase path does not exist: {codebase_path}")
            
            # Find references based on symbol type
            if symbol_type == "function":
                references = await self._find_function_references(codebase_path, symbol)
            elif symbol_type == "class":
                references = await self._find_class_references(codebase_path, symbol)
            elif symbol_type == "variable":
                references = await self._find_variable_references(codebase_path, symbol)
            else:
                references = await self._find_generic_references(codebase_path, symbol)
            
            results["references"] = references
            results["usage_count"] = len(references)
            
            # Find definition location if requested
            if include_definitions:
                definition = await self._find_definition(codebase_path, symbol, symbol_type)
                results["definition_location"] = definition
            
            # Add search metadata
            results["search_metadata"] = {
                "symbol": symbol,
                "symbol_type": symbol_type,
                "include_definitions": include_definitions,
                "files_searched": len(set(ref["file"] for ref in references))
            }
            
            self.log_usage({"codebase_path": str(codebase_path), "symbol": symbol, "symbol_type": symbol_type}, 
                         f"Found {results['usage_count']} references")
            return results
        except Exception as e:
            self.logger.error(f"Error finding references: {e}")
            raise
    
    async def _find_function_references(self, codebase_path: Path, symbol: str) -> List[Dict[str, Any]]:
        """Find references to a function."""
        references = []
        
        # Search for function calls
        call_patterns = [
            rf'\b{symbol}\s*\(',  # function()
            rf'\b{symbol}\s*\[',  # function[]
            rf'\.{symbol}\s*\(',  # obj.function()
        ]
        
        for pattern in call_patterns:
            pattern_matches = await self._search_pattern_in_codebase(codebase_path, pattern, ['.py', '.js', '.ts', '.java'])
            for match in pattern_matches:
                references.append({
                    "file": match["file"],
                    "line": match["line"],
                    "column": match["column"],
                    "context": match["context"],
                    "type": "function_call",
                    "symbol": symbol
                })
        
        return references
    
    async def _find_class_references(self, codebase_path: Path, symbol: str) -> List[Dict[str, Any]]:
        """Find references to a class."""
        references = []
        
        # Search for class usage patterns
        class_patterns = [
            rf'\b{symbol}\s*\(',  # Class()
            rf'\b{symbol}\s*\[',  # Class[]
            rf'new\s+{symbol}',   # new Class
            rf'extends\s+{symbol}',  # extends Class
            rf'implements\s+{symbol}',  # implements Class
        ]
        
        for pattern in class_patterns:
            pattern_matches = await self._search_pattern_in_codebase(codebase_path, pattern, ['.py', '.js', '.ts', '.java'])
            for match in pattern_matches:
                references.append({
                    "file": match["file"],
                    "line": match["line"],
                    "column": match["column"],
                    "context": match["context"],
                    "type": "class_usage",
                    "symbol": symbol
                })
        
        return references
    
    async def _find_variable_references(self, codebase_path: Path, symbol: str) -> List[Dict[str, Any]]:
        """Find references to a variable."""
        references = []
        
        # Search for variable usage
        var_patterns = [
            rf'\b{symbol}\b',  # variable
            rf'\.{symbol}\b',  # obj.variable
        ]
        
        for pattern in var_patterns:
            pattern_matches = await self._search_pattern_in_codebase(codebase_path, pattern, ['.py', '.js', '.ts', '.java'])
            for match in pattern_matches:
                references.append({
                    "file": match["file"],
                    "line": match["line"],
                    "column": match["column"],
                    "context": match["context"],
                    "type": "variable_usage",
                    "symbol": symbol
                })
        
        return references
    
    async def _find_generic_references(self, codebase_path: Path, symbol: str) -> List[Dict[str, Any]]:
        """Find generic references to a symbol."""
        references = []
        
        # Simple text search
        pattern = rf'\b{symbol}\b'
        pattern_matches = await self._search_pattern_in_codebase(codebase_path, pattern, ['.py', '.js', '.ts', '.java'])
        
        for match in pattern_matches:
            references.append({
                "file": match["file"],
                "line": match["line"],
                "column": match["column"],
                "context": match["context"],
                "type": "generic_reference",
                "symbol": symbol
            })
        
        return references
    
    async def _search_pattern_in_codebase(self, codebase_path: Path, pattern: str, file_extensions: List[str]) -> List[Dict[str, Any]]:
        """Search for pattern in codebase (helper method)."""
        matches = []
        
        try:
            compiled_pattern = re.compile(pattern, re.IGNORECASE)
        except re.error:
            return matches
        
        for file_path in codebase_path.rglob('*'):
            if file_path.is_file() and file_path.suffix.lower() in file_extensions:
                try:
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        lines = f.readlines()
                    
                    for line_num, line in enumerate(lines, 1):
                        for match in compiled_pattern.finditer(line):
                            matches.append({
                                "file": str(file_path),
                                "line": line_num,
                                "column": match.start(),
                                "context": line.strip()
                            })
                except Exception:
                    continue
        
        return matches
    
    async def _find_definition(self, codebase_path: Path, symbol: str, symbol_type: str) -> Optional[Dict[str, Any]]:
        """Find the definition of a symbol."""
        # Search for definition patterns
        if symbol_type == "function":
            def_patterns = [rf'def\s+{symbol}\s*\(', rf'function\s+{symbol}\s*\(', rf'async\s+def\s+{symbol}\s*\(']
        elif symbol_type == "class":
            def_patterns = [rf'class\s+{symbol}\s*[\(:]', rf'interface\s+{symbol}']
        else:
            def_patterns = [rf'\b{symbol}\s*=']
        
        for pattern in def_patterns:
            matches = await self._search_pattern_in_codebase(codebase_path, pattern, ['.py', '.js', '.ts', '.java'])
            if matches:
                return {
                    "file": matches[0]["file"],
                    "line": matches[0]["line"],
                    "column": matches[0]["column"],
                    "context": matches[0]["context"],
                    "type": "definition"
                }
        
        return None


class CodeDiscoveryTool(BaseAgentTool):
    """Tool for discovering code structure."""
    
    def __init__(self):
        super().__init__(
            name="discover_code_structure",
            description="Discover code structure and relationships"
        )
    
    async def run(self, codebase_path: str, discovery_type: str = "all") -> Dict[str, Any]:
        """Discover code structure."""
        try:
            discovery = {
                "modules": [],
                "functions": [],
                "classes": [],
                "interfaces": [],
                "dependencies": [],
                "entry_points": [],
                "discovery_metadata": {}
            }
            
            codebase_path = Path(codebase_path)
            if not codebase_path.exists():
                raise ValueError(f"Codebase path does not exist: {codebase_path}")
            
            # Discover based on type
            if discovery_type in ["all", "modules"]:
                discovery["modules"] = await self._discover_modules(codebase_path)
            
            if discovery_type in ["all", "functions"]:
                discovery["functions"] = await self._discover_functions(codebase_path)
            
            if discovery_type in ["all", "classes"]:
                discovery["classes"] = await self._discover_classes(codebase_path)
            
            if discovery_type in ["all", "interfaces"]:
                discovery["interfaces"] = await self._discover_interfaces(codebase_path)
            
            if discovery_type in ["all", "dependencies"]:
                discovery["dependencies"] = await self._discover_dependencies(codebase_path)
            
            if discovery_type in ["all", "entry_points"]:
                discovery["entry_points"] = await self._discover_entry_points(codebase_path)
            
            # Add discovery metadata
            discovery["discovery_metadata"] = {
                "discovery_type": discovery_type,
                "total_modules": len(discovery["modules"]),
                "total_functions": len(discovery["functions"]),
                "total_classes": len(discovery["classes"]),
                "total_interfaces": len(discovery["interfaces"]),
                "total_dependencies": len(discovery["dependencies"]),
                "total_entry_points": len(discovery["entry_points"])
            }
            
            self.log_usage({"codebase_path": str(codebase_path), "discovery_type": discovery_type}, 
                         f"Discovered {len(discovery['modules'])} modules")
            return discovery
        except Exception as e:
            self.logger.error(f"Error discovering code structure: {e}")
            raise
    
    async def _discover_modules(self, codebase_path: Path) -> List[Dict[str, Any]]:
        """Discover modules in the codebase."""
        modules = []
        
        for file_path in codebase_path.rglob('*.py'):
            if file_path.is_file():
                modules.append({
                    "name": file_path.stem,
                    "path": str(file_path),
                    "type": "python_module",
                    "size": file_path.stat().st_size
                })
        
        return modules
    
    async def _discover_functions(self, codebase_path: Path) -> List[Dict[str, Any]]:
        """Discover functions in the codebase."""
        functions = []
        
        for file_path in codebase_path.rglob('*.py'):
            if file_path.is_file():
                try:
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                    
                    # Find function definitions
                    func_pattern = r'def\s+(\w+)\s*\('
                    for match in re.finditer(func_pattern, content):
                        functions.append({
                            "name": match.group(1),
                            "file": str(file_path),
                            "line": content[:match.start()].count('\n') + 1,
                            "type": "function"
                        })
                except Exception:
                    continue
        
        return functions
    
    async def _discover_classes(self, codebase_path: Path) -> List[Dict[str, Any]]:
        """Discover classes in the codebase."""
        classes = []
        
        for file_path in codebase_path.rglob('*.py'):
            if file_path.is_file():
                try:
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                    
                    # Find class definitions
                    class_pattern = r'class\s+(\w+)'
                    for match in re.finditer(class_pattern, content):
                        classes.append({
                            "name": match.group(1),
                            "file": str(file_path),
                            "line": content[:match.start()].count('\n') + 1,
                            "type": "class"
                        })
                except Exception:
                    continue
        
        return classes
    
    async def _discover_interfaces(self, codebase_path: Path) -> List[Dict[str, Any]]:
        """Discover interfaces in the codebase."""
        interfaces = []
        
        # Python doesn't have explicit interfaces, but we can find abstract classes
        for file_path in codebase_path.rglob('*.py'):
            if file_path.is_file():
                try:
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                    
                    # Find abstract classes (interfaces)
                    if 'ABC' in content or 'abstractmethod' in content:
                        class_pattern = r'class\s+(\w+)'
                        for match in re.finditer(class_pattern, content):
                            interfaces.append({
                                "name": match.group(1),
                                "file": str(file_path),
                                "line": content[:match.start()].count('\n') + 1,
                                "type": "interface"
                            })
                except Exception:
                    continue
        
        return interfaces
    
    async def _discover_dependencies(self, codebase_path: Path) -> List[Dict[str, Any]]:
        """Discover dependencies in the codebase."""
        dependencies = []
        
        for file_path in codebase_path.rglob('*.py'):
            if file_path.is_file():
                try:
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                    
                    # Find import statements
                    import_pattern = r'(?:from\s+(\S+)\s+)?import\s+(\S+)'
                    for match in re.finditer(import_pattern, content):
                        module = match.group(1) or match.group(2)
                        if module and not module.startswith('.'):
                            dependencies.append({
                                "module": module,
                                "file": str(file_path),
                                "line": content[:match.start()].count('\n') + 1,
                                "type": "import"
                            })
                except Exception:
                    continue
        
        return dependencies
    
    async def _discover_entry_points(self, codebase_path: Path) -> List[Dict[str, Any]]:
        """Discover entry points in the codebase."""
        entry_points = []
        
        for file_path in codebase_path.rglob('*.py'):
            if file_path.is_file():
                try:
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                    
                    # Find main functions or entry points
                    if '__main__' in content or 'if __name__' in content:
                        entry_points.append({
                            "name": file_path.stem,
                            "file": str(file_path),
                            "type": "main_entry_point"
                        })
                except Exception:
                    continue
        
        return entry_points
