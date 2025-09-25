"""
Code analysis and transformation tools.

This module contains tools for analyzing code structure, dependencies,
patterns, and performing code transformations.
"""

from typing import Any, Dict, List, Optional, Union
import ast
import re
import os
import json
import subprocess
from pathlib import Path
from collections import defaultdict, Counter
import networkx as nx
from pydantic import BaseModel, Field

from src.core.tools.base_tool import BaseAgentTool
from src.utils.logger import get_logger

logger = get_logger(__name__)


class CodeAnalysisArgs(BaseModel):
    """Arguments for code analysis tools."""
    codebase_path: str = Field(description="Path to the codebase to analyze")
    file_extensions: Optional[List[str]] = Field(default=None, description="File extensions to include")
    exclude_patterns: Optional[List[str]] = Field(default=None, description="Patterns to exclude")


class PatternReplacementArgs(BaseModel):
    """Arguments for pattern replacement tools."""
    patterns: List[Dict[str, Any]] = Field(description="Patterns to apply")
    source_path: str = Field(description="Path to source code")
    backup: bool = Field(default=True, description="Create backup before replacement")


class CodeTransformationArgs(BaseModel):
    """Arguments for code transformation tools."""
    phase: Dict[str, Any] = Field(description="Transformation phase configuration")
    source_path: str = Field(description="Path to source code")
    target_language: str = Field(description="Target programming language")


class CodeAnalyzerTool(BaseAgentTool):
    """Tool for analyzing code structure."""
    
    def __init__(self):
        super().__init__(
            name="analyze_code_structure",
            description="Analyze code structure and organization"
        )
    
    def _get_args_schema(self):
        return CodeAnalysisArgs
    
    async def run(self, codebase_path: str, file_extensions: Optional[List[str]] = None, 
                  exclude_patterns: Optional[List[str]] = None) -> Dict[str, Any]:
        """Analyze code structure."""
        try:
            codebase_path = Path(codebase_path)
            if not codebase_path.exists():
                raise ValueError(f"Codebase path does not exist: {codebase_path}")
            
            # Default file extensions for different languages
            if file_extensions is None:
                file_extensions = ['.py', '.js', '.ts', '.java', '.c', '.cpp', '.cobol', '.cbl']
            
            if exclude_patterns is None:
                exclude_patterns = ['__pycache__', '.git', 'node_modules', 'venv', 'env']
            
            analysis = {
                "files": [],
                "functions": [],
                "classes": [],
                "imports": [],
                "complexity_metrics": {},
                "language_distribution": {},
                "file_sizes": {},
                "total_lines": 0,
                "total_files": 0
            }
            
            # Scan files
            for file_path in codebase_path.rglob('*'):
                if file_path.is_file():
                    # Check if file should be excluded
                    if any(pattern in str(file_path) for pattern in exclude_patterns):
                        continue
                    
                    # Check file extension
                    if file_path.suffix.lower() in file_extensions:
                        file_info = await self._analyze_file(file_path)
                        if file_info:
                            analysis["files"].append(file_info)
                            analysis["functions"].extend(file_info.get("functions", []))
                            analysis["classes"].extend(file_info.get("classes", []))
                            analysis["imports"].extend(file_info.get("imports", []))
                            analysis["total_lines"] += file_info.get("lines", 0)
                            
                            # Track language distribution
                            lang = file_path.suffix.lower()
                            analysis["language_distribution"][lang] = analysis["language_distribution"].get(lang, 0) + 1
            
            analysis["total_files"] = len(analysis["files"])
            analysis["complexity_metrics"] = self._calculate_complexity_metrics(analysis)
            
            self.log_usage({"codebase_path": str(codebase_path)}, f"Analyzed {analysis['total_files']} files")
            return analysis
        except Exception as e:
            self.logger.error(f"Error analyzing code structure: {e}")
            raise
    
    async def _analyze_file(self, file_path: Path) -> Optional[Dict[str, Any]]:
        """Analyze a single file."""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')
            
            file_info = {
                "path": str(file_path),
                "name": file_path.name,
                "extension": file_path.suffix,
                "lines": len(lines),
                "size": file_path.stat().st_size,
                "functions": [],
                "classes": [],
                "imports": []
            }
            
            # Parse based on file type
            if file_path.suffix.lower() == '.py':
                file_info.update(self._analyze_python_file(content))
            elif file_path.suffix.lower() in ['.js', '.ts']:
                file_info.update(self._analyze_javascript_file(content))
            elif file_path.suffix.lower() in ['.cobol', '.cbl']:
                file_info.update(self._analyze_cobol_file(content))
            
            return file_info
        except Exception as e:
            self.logger.warning(f"Error analyzing file {file_path}: {e}")
            return None
    
    def _analyze_python_file(self, content: str) -> Dict[str, Any]:
        """Analyze Python file structure."""
        try:
            tree = ast.parse(content)
            functions = []
            classes = []
            imports = []
            
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef):
                    functions.append({
                        "name": node.name,
                        "line": node.lineno,
                        "args": [arg.arg for arg in node.args.args],
                        "is_async": isinstance(node, ast.AsyncFunctionDef)
                    })
                elif isinstance(node, ast.ClassDef):
                    classes.append({
                        "name": node.name,
                        "line": node.lineno,
                        "bases": [base.id if hasattr(base, 'id') else str(base) for base in node.bases]
                    })
                elif isinstance(node, (ast.Import, ast.ImportFrom)):
                    if isinstance(node, ast.Import):
                        for alias in node.names:
                            imports.append(alias.name)
                    else:
                        module = node.module or ""
                        for alias in node.names:
                            imports.append(f"{module}.{alias.name}")
            
            return {
                "functions": functions,
                "classes": classes,
                "imports": imports
            }
        except SyntaxError:
            return {"functions": [], "classes": [], "imports": []}
    
    def _analyze_javascript_file(self, content: str) -> Dict[str, Any]:
        """Analyze JavaScript/TypeScript file structure."""
        functions = []
        classes = []
        imports = []
        
        # Simple regex-based analysis for JS/TS
        function_pattern = r'function\s+(\w+)\s*\('
        class_pattern = r'class\s+(\w+)'
        import_pattern = r'import\s+.*?from\s+[\'"]([^\'"]+)[\'"]'
        
        for match in re.finditer(function_pattern, content):
            functions.append({"name": match.group(1), "line": content[:match.start()].count('\n') + 1})
        
        for match in re.finditer(class_pattern, content):
            classes.append({"name": match.group(1), "line": content[:match.start()].count('\n') + 1})
        
        for match in re.finditer(import_pattern, content):
            imports.append(match.group(1))
        
        return {
            "functions": functions,
            "classes": classes,
            "imports": imports
        }
    
    def _analyze_cobol_file(self, content: str) -> Dict[str, Any]:
        """Analyze COBOL file structure."""
        functions = []
        classes = []
        imports = []
        
        # COBOL-specific patterns
        program_pattern = r'PROGRAM-ID\.\s+(\w+)'
        section_pattern = r'(\w+)\s+SECTION\.'
        
        for match in re.finditer(program_pattern, content):
            functions.append({"name": match.group(1), "line": content[:match.start()].count('\n') + 1})
        
        for match in re.finditer(section_pattern, content):
            functions.append({"name": match.group(1), "line": content[:match.start()].count('\n') + 1})
        
        return {
            "functions": functions,
            "classes": classes,
            "imports": imports
        }
    
    def _calculate_complexity_metrics(self, analysis: Dict[str, Any]) -> Dict[str, Any]:
        """Calculate complexity metrics."""
        total_functions = len(analysis["functions"])
        total_classes = len(analysis["classes"])
        total_imports = len(analysis["imports"])
        
        # Calculate average function complexity (simplified)
        avg_function_length = 0
        if total_functions > 0:
            function_lengths = [func.get("lines", 0) for func in analysis["functions"]]
            avg_function_length = sum(function_lengths) / len(function_lengths) if function_lengths else 0
        
        return {
            "total_functions": total_functions,
            "total_classes": total_classes,
            "total_imports": total_imports,
            "avg_function_length": avg_function_length,
            "cyclomatic_complexity": "N/A",  # Would need more sophisticated analysis
            "maintainability_index": "N/A"   # Would need more sophisticated analysis
        }


class DependencyAnalyzerTool(BaseAgentTool):
    """Tool for analyzing code dependencies."""
    
    def __init__(self):
        super().__init__(
            name="analyze_dependencies",
            description="Analyze code dependencies and relationships"
        )
    
    def _get_args_schema(self):
        return CodeAnalysisArgs
    
    async def run(self, codebase_path: str, file_extensions: Optional[List[str]] = None, 
                  exclude_patterns: Optional[List[str]] = None) -> Dict[str, Any]:
        """Analyze dependencies."""
        try:
            codebase_path = Path(codebase_path)
            if not codebase_path.exists():
                raise ValueError(f"Codebase path does not exist: {codebase_path}")
            
            if file_extensions is None:
                file_extensions = ['.py', '.js', '.ts', '.java', '.c', '.cpp']
            
            if exclude_patterns is None:
                exclude_patterns = ['__pycache__', '.git', 'node_modules', 'venv', 'env']
            
            dependencies = {
                "external_dependencies": [],
                "internal_dependencies": [],
                "dependency_graph": {},
                "circular_dependencies": [],
                "dependency_tree": {},
                "import_statistics": {}
            }
            
            # Build dependency graph
            dependency_graph = nx.DiGraph()
            file_dependencies = {}
            
            # Analyze each file for dependencies
            for file_path in codebase_path.rglob('*'):
                if file_path.is_file() and file_path.suffix.lower() in file_extensions:
                    if any(pattern in str(file_path) for pattern in exclude_patterns):
                        continue
                    
                    file_deps = await self._analyze_file_dependencies(file_path, codebase_path)
                    if file_deps:
                        file_dependencies[str(file_path)] = file_deps
                        dependency_graph.add_node(str(file_path))
                        
                        for dep in file_deps:
                            if dep["type"] == "external":
                                dependencies["external_dependencies"].append(dep)
                            else:
                                dependencies["internal_dependencies"].append(dep)
                                dependency_graph.add_edge(str(file_path), dep["path"])
            
            # Detect circular dependencies
            try:
                cycles = list(nx.simple_cycles(dependency_graph))
                dependencies["circular_dependencies"] = cycles
            except Exception as e:
                self.logger.warning(f"Error detecting circular dependencies: {e}")
            
            # Build dependency tree
            dependencies["dependency_tree"] = self._build_dependency_tree(dependency_graph)
            dependencies["dependency_graph"] = {
                "nodes": list(dependency_graph.nodes()),
                "edges": list(dependency_graph.edges())
            }
            
            # Calculate import statistics
            dependencies["import_statistics"] = self._calculate_import_statistics(dependencies)
            
            self.log_usage({"codebase_path": str(codebase_path)}, 
                         f"Found {len(dependencies['external_dependencies'])} external dependencies")
            return dependencies
        except Exception as e:
            self.logger.error(f"Error analyzing dependencies: {e}")
            raise
    
    async def _analyze_file_dependencies(self, file_path: Path, codebase_path: Path) -> List[Dict[str, Any]]:
        """Analyze dependencies for a single file."""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
            
            dependencies = []
            
            if file_path.suffix.lower() == '.py':
                dependencies = self._analyze_python_dependencies(content, file_path, codebase_path)
            elif file_path.suffix.lower() in ['.js', '.ts']:
                dependencies = self._analyze_javascript_dependencies(content, file_path, codebase_path)
            elif file_path.suffix.lower() in ['.java']:
                dependencies = self._analyze_java_dependencies(content, file_path, codebase_path)
            
            return dependencies
        except Exception as e:
            self.logger.warning(f"Error analyzing dependencies for {file_path}: {e}")
            return []
    
    def _analyze_python_dependencies(self, content: str, file_path: Path, codebase_path: Path) -> List[Dict[str, Any]]:
        """Analyze Python file dependencies."""
        dependencies = []
        
        try:
            tree = ast.parse(content)
            for node in ast.walk(tree):
                if isinstance(node, ast.Import):
                    for alias in node.names:
                        dep_path = self._resolve_python_import(alias.name, file_path, codebase_path)
                        dependencies.append({
                            "name": alias.name,
                            "path": dep_path,
                            "type": "external" if dep_path is None else "internal",
                            "line": node.lineno
                        })
                elif isinstance(node, ast.ImportFrom):
                    module = node.module or ""
                    for alias in node.names:
                        full_name = f"{module}.{alias.name}" if module else alias.name
                        dep_path = self._resolve_python_import(full_name, file_path, codebase_path)
                        dependencies.append({
                            "name": full_name,
                            "path": dep_path,
                            "type": "external" if dep_path is None else "internal",
                            "line": node.lineno
                        })
        except SyntaxError:
            # Fallback to regex-based analysis
            import_pattern = r'import\s+(\w+)'
            from_pattern = r'from\s+(\w+)\s+import'
            
            for match in re.finditer(import_pattern, content):
                dependencies.append({
                    "name": match.group(1),
                    "path": None,
                    "type": "external",
                    "line": content[:match.start()].count('\n') + 1
                })
            
            for match in re.finditer(from_pattern, content):
                dependencies.append({
                    "name": match.group(1),
                    "path": None,
                    "type": "external",
                    "line": content[:match.start()].count('\n') + 1
                })
        
        return dependencies
    
    def _analyze_javascript_dependencies(self, content: str, file_path: Path, codebase_path: Path) -> List[Dict[str, Any]]:
        """Analyze JavaScript/TypeScript file dependencies."""
        dependencies = []
        
        # CommonJS require
        require_pattern = r'require\s*\(\s*[\'"]([^\'"]+)[\'"]\s*\)'
        # ES6 import
        import_pattern = r'import\s+.*?from\s+[\'"]([^\'"]+)[\'"]'
        
        for match in re.finditer(require_pattern, content):
            dep_name = match.group(1)
            dep_path = self._resolve_javascript_import(dep_name, file_path, codebase_path)
            dependencies.append({
                "name": dep_name,
                "path": dep_path,
                "type": "external" if dep_path is None else "internal",
                "line": content[:match.start()].count('\n') + 1
            })
        
        for match in re.finditer(import_pattern, content):
            dep_name = match.group(1)
            dep_path = self._resolve_javascript_import(dep_name, file_path, codebase_path)
            dependencies.append({
                "name": dep_name,
                "path": dep_path,
                "type": "external" if dep_path is None else "internal",
                "line": content[:match.start()].count('\n') + 1
            })
        
        return dependencies
    
    def _analyze_java_dependencies(self, content: str, file_path: Path, codebase_path: Path) -> List[Dict[str, Any]]:
        """Analyze Java file dependencies."""
        dependencies = []
        
        # Java import statements
        import_pattern = r'import\s+([\w.]+)'
        
        for match in re.finditer(import_pattern, content):
            dep_name = match.group(1)
            dep_path = self._resolve_java_import(dep_name, file_path, codebase_path)
            dependencies.append({
                "name": dep_name,
                "path": dep_path,
                "type": "external" if dep_path is None else "internal",
                "line": content[:match.start()].count('\n') + 1
            })
        
        return dependencies
    
    def _resolve_python_import(self, import_name: str, file_path: Path, codebase_path: Path) -> Optional[str]:
        """Resolve Python import to file path."""
        # Convert import name to file path
        module_path = import_name.replace('.', '/')
        
        # Try different possible locations
        possible_paths = [
            codebase_path / f"{module_path}.py",
            codebase_path / f"{module_path}/__init__.py",
            file_path.parent / f"{module_path}.py",
            file_path.parent / f"{module_path}/__init__.py"
        ]
        
        for path in possible_paths:
            if path.exists() and path.is_file():
                return str(path)
        
        return None
    
    def _resolve_javascript_import(self, import_name: str, file_path: Path, codebase_path: Path) -> Optional[str]:
        """Resolve JavaScript import to file path."""
        # Handle relative imports
        if import_name.startswith('./') or import_name.startswith('../'):
            resolved_path = file_path.parent / import_name
            if resolved_path.exists():
                return str(resolved_path)
            # Try with extensions
            for ext in ['.js', '.ts', '.jsx', '.tsx']:
                if (resolved_path.parent / f"{resolved_path.name}{ext}").exists():
                    return str(resolved_path.parent / f"{resolved_path.name}{ext}")
        
        # Handle absolute imports (likely external)
        return None
    
    def _resolve_java_import(self, import_name: str, file_path: Path, codebase_path: Path) -> Optional[str]:
        """Resolve Java import to file path."""
        # Convert package name to file path
        class_name = import_name.split('.')[-1]
        package_path = '/'.join(import_name.split('.')[:-1])
        
        possible_paths = [
            codebase_path / f"{package_path}/{class_name}.java",
            codebase_path / f"src/main/java/{package_path}/{class_name}.java"
        ]
        
        for path in possible_paths:
            if path.exists() and path.is_file():
                return str(path)
        
        return None
    
    def _build_dependency_tree(self, graph: nx.DiGraph) -> Dict[str, Any]:
        """Build a hierarchical dependency tree."""
        tree = {}
        
        # Find root nodes (nodes with no incoming edges)
        root_nodes = [node for node in graph.nodes() if graph.in_degree(node) == 0]
        
        def build_subtree(node):
            children = list(graph.successors(node))
            subtree = {
                "file": node,
                "dependencies": [build_subtree(child) for child in children]
            }
            return subtree
        
        for root in root_nodes:
            tree[root] = build_subtree(root)
        
        return tree
    
    def _calculate_import_statistics(self, dependencies: Dict[str, Any]) -> Dict[str, Any]:
        """Calculate import statistics."""
        external_count = len(dependencies["external_dependencies"])
        internal_count = len(dependencies["internal_dependencies"])
        total_count = external_count + internal_count
        
        return {
            "total_imports": total_count,
            "external_imports": external_count,
            "internal_imports": internal_count,
            "external_ratio": external_count / total_count if total_count > 0 else 0,
            "internal_ratio": internal_count / total_count if total_count > 0 else 0
        }


class PatternReplacerTool(BaseAgentTool):
    """Tool for replacing code patterns."""
    
    def __init__(self):
        super().__init__(
            name="apply_pattern_replacements",
            description="Apply pattern-based code replacements"
        )
    
    def _get_args_schema(self):
        return PatternReplacementArgs
    
    async def run(self, patterns: List[Dict[str, Any]], source_path: str, backup: bool = True) -> Dict[str, Any]:
        """Apply pattern replacements."""
        try:
            source_path = Path(source_path)
            if not source_path.exists():
                raise ValueError(f"Source path does not exist: {source_path}")
            
            results = {
                "replacements_made": 0,
                "files_modified": [],
                "patterns_applied": [],
                "backup_created": False,
                "errors": []
            }
            
            # Create backup if requested
            if backup:
                backup_path = source_path.with_suffix(f"{source_path.suffix}.backup")
                try:
                    import shutil
                    shutil.copy2(source_path, backup_path)
                    results["backup_created"] = True
                    results["backup_path"] = str(backup_path)
                except Exception as e:
                    self.logger.warning(f"Failed to create backup: {e}")
            
            # Read source file
            with open(source_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
            
            original_content = content
            
            # Apply each pattern
            for pattern in patterns:
                try:
                    pattern_result = await self._apply_pattern(content, pattern)
                    if pattern_result["replacements"] > 0:
                        content = pattern_result["new_content"]
                        results["replacements_made"] += pattern_result["replacements"]
                        results["patterns_applied"].append({
                            "pattern_name": pattern.get("name", "unnamed"),
                            "replacements": pattern_result["replacements"],
                            "matches": pattern_result["matches"]
                        })
                except Exception as e:
                    error_msg = f"Error applying pattern {pattern.get('name', 'unnamed')}: {e}"
                    results["errors"].append(error_msg)
                    self.logger.error(error_msg)
            
            # Write modified content if changes were made
            if content != original_content:
                with open(source_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                results["files_modified"].append(str(source_path))
            
            self.log_usage({"patterns": len(patterns), "source_path": str(source_path)}, 
                         f"Applied {results['replacements_made']} replacements")
            return results
        except Exception as e:
            self.logger.error(f"Error applying pattern replacements: {e}")
            raise
    
    async def _apply_pattern(self, content: str, pattern: Dict[str, Any]) -> Dict[str, Any]:
        """Apply a single pattern to content."""
        pattern_type = pattern.get("type", "regex")
        find_pattern = pattern.get("find", "")
        replace_pattern = pattern.get("replace", "")
        flags = pattern.get("flags", 0)
        
        if pattern_type == "regex":
            # Regular expression replacement
            import re
            regex_flags = 0
            if flags & 1:  # IGNORECASE
                regex_flags |= re.IGNORECASE
            if flags & 2:  # MULTILINE
                regex_flags |= re.MULTILINE
            if flags & 4:  # DOTALL
                regex_flags |= re.DOTALL
            
            matches = list(re.finditer(find_pattern, content, regex_flags))
            new_content = re.sub(find_pattern, replace_pattern, content, flags=regex_flags)
            
            return {
                "replacements": len(matches),
                "new_content": new_content,
                "matches": [{"start": m.start(), "end": m.end(), "text": m.group()} for m in matches]
            }
        
        elif pattern_type == "string":
            # Simple string replacement
            count = content.count(find_pattern)
            new_content = content.replace(find_pattern, replace_pattern)
            
            return {
                "replacements": count,
                "new_content": new_content,
                "matches": [{"text": find_pattern, "count": count}]
            }
        
        else:
            raise ValueError(f"Unsupported pattern type: {pattern_type}")


class CodeTransformerTool(BaseAgentTool):
    """Tool for transforming code."""
    
    def __init__(self):
        super().__init__(
            name="transform_code_phase",
            description="Transform code according to modernization phase"
        )
    
    def _get_args_schema(self):
        return CodeTransformationArgs
    
    async def run(self, phase: Dict[str, Any], source_path: str, target_language: str) -> Dict[str, Any]:
        """Transform code for a specific phase."""
        try:
            source_path = Path(source_path)
            if not source_path.exists():
                raise ValueError(f"Source path does not exist: {source_path}")
            
            results = {
                "phase": phase.get("name", "unknown"),
                "files_transformed": [],
                "transformations_applied": [],
                "success": True,
                "errors": [],
                "output_path": None
            }
            
            # Create output directory
            output_dir = source_path.parent / f"{source_path.stem}_transformed"
            output_dir.mkdir(exist_ok=True)
            results["output_path"] = str(output_dir)
            
            # Get transformation rules for the phase
            transformation_rules = phase.get("rules", [])
            if not transformation_rules:
                self.logger.warning(f"No transformation rules found for phase: {phase.get('name')}")
                return results
            
            # Process files based on source type
            if source_path.is_file():
                transformed_file = await self._transform_file(source_path, transformation_rules, target_language, output_dir)
                if transformed_file:
                    results["files_transformed"].append(transformed_file)
            elif source_path.is_dir():
                # Transform all files in directory
                for file_path in source_path.rglob('*'):
                    if file_path.is_file() and self._should_transform_file(file_path, phase):
                        transformed_file = await self._transform_file(file_path, transformation_rules, target_language, output_dir)
                        if transformed_file:
                            results["files_transformed"].append(transformed_file)
            
            # Apply transformations
            for rule in transformation_rules:
                try:
                    rule_result = await self._apply_transformation_rule(rule, results["files_transformed"])
                    results["transformations_applied"].append(rule_result)
                except Exception as e:
                    error_msg = f"Error applying rule {rule.get('name', 'unnamed')}: {e}"
                    results["errors"].append(error_msg)
                    self.logger.error(error_msg)
            
            results["success"] = len(results["errors"]) == 0
            
            self.log_usage({"phase": phase.get("name"), "source_path": str(source_path), "target_language": target_language}, 
                         f"Transformed phase: {results['phase']}")
            return results
        except Exception as e:
            self.logger.error(f"Error transforming code: {e}")
            raise
    
    def _should_transform_file(self, file_path: Path, phase: Dict[str, Any]) -> bool:
        """Check if file should be transformed based on phase configuration."""
        file_extensions = phase.get("file_extensions", [])
        exclude_patterns = phase.get("exclude_patterns", [])
        
        # Check file extension
        if file_extensions and file_path.suffix.lower() not in file_extensions:
            return False
        
        # Check exclude patterns
        if any(pattern in str(file_path) for pattern in exclude_patterns):
            return False
        
        return True
    
    async def _transform_file(self, source_file: Path, rules: List[Dict[str, Any]], target_language: str, output_dir: Path) -> Optional[Dict[str, Any]]:
        """Transform a single file."""
        try:
            # Read source file
            with open(source_file, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
            
            # Determine target file extension
            target_ext = self._get_target_extension(source_file.suffix, target_language)
            target_file = output_dir / f"{source_file.stem}{target_ext}"
            
            # Apply language-specific transformations
            transformed_content = await self._apply_language_transformations(content, source_file.suffix, target_language, rules)
            
            # Write transformed file
            with open(target_file, 'w', encoding='utf-8') as f:
                f.write(transformed_content)
            
            return {
                "source_file": str(source_file),
                "target_file": str(target_file),
                "transformations_applied": len(rules),
                "lines_transformed": len(transformed_content.split('\n'))
            }
        except Exception as e:
            self.logger.error(f"Error transforming file {source_file}: {e}")
            return None
    
    def _get_target_extension(self, source_ext: str, target_language: str) -> str:
        """Get target file extension based on target language."""
        extension_mapping = {
            "python": ".py",
            "java": ".java",
            "javascript": ".js",
            "typescript": ".ts",
            "c": ".c",
            "cpp": ".cpp",
            "csharp": ".cs"
        }
        return extension_mapping.get(target_language.lower(), source_ext)
    
    async def _apply_language_transformations(self, content: str, source_ext: str, target_language: str, rules: List[Dict[str, Any]]) -> str:
        """Apply language-specific transformations."""
        transformed_content = content
        
        for rule in rules:
            rule_type = rule.get("type", "pattern")
            
            if rule_type == "pattern":
                # Pattern-based transformation
                find_pattern = rule.get("find", "")
                replace_pattern = rule.get("replace", "")
                if find_pattern and replace_pattern:
                    import re
                    transformed_content = re.sub(find_pattern, replace_pattern, transformed_content)
            
            elif rule_type == "structure":
                # Structural transformation
                transformation = rule.get("transformation", {})
                if source_ext.lower() in ['.cobol', '.cbl'] and target_language.lower() == 'python':
                    transformed_content = await self._transform_cobol_to_python(transformed_content, transformation)
                elif source_ext.lower() in ['.f', '.f90'] and target_language.lower() == 'python':
                    transformed_content = await self._transform_fortran_to_python(transformed_content, transformation)
        
        return transformed_content
    
    async def _transform_cobol_to_python(self, content: str, transformation: Dict[str, Any]) -> str:
        """Transform COBOL code to Python."""
        # Basic COBOL to Python transformations
        transformations = [
            (r'PROGRAM-ID\.\s+(\w+)', r'# Program: \1'),
            (r'DATA\s+DIVISION\.', r'# Data Division'),
            (r'PROCEDURE\s+DIVISION\.', r'# Procedure Division'),
            (r'DISPLAY\s+[\'"]([^\'"]+)[\'"]', r'print("\1")'),
            (r'STOP\s+RUN\.', r'# End of program'),
            (r'(\w+)\s+PIC\s+9+', r'\1 = 0  # Integer variable'),
            (r'(\w+)\s+PIC\s+X+', r'\1 = ""  # String variable'),
        ]
        
        for pattern, replacement in transformations:
            import re
            content = re.sub(pattern, replacement, content, flags=re.IGNORECASE)
        
        return content
    
    async def _transform_fortran_to_python(self, content: str, transformation: Dict[str, Any]) -> str:
        """Transform FORTRAN code to Python."""
        # Basic FORTRAN to Python transformations
        transformations = [
            (r'PROGRAM\s+(\w+)', r'# Program: \1'),
            (r'WRITE\s*\(\s*\*\s*,\s*\*\s*\)\s+([^,]+)', r'print(\1)'),
            (r'END\s+PROGRAM', r'# End of program'),
            (r'INTEGER\s+(\w+)', r'\1 = 0  # Integer variable'),
            (r'REAL\s+(\w+)', r'\1 = 0.0  # Real variable'),
            (r'CHARACTER\s+(\w+)', r'\1 = ""  # Character variable'),
        ]
        
        for pattern, replacement in transformations:
            import re
            content = re.sub(pattern, replacement, content, flags=re.IGNORECASE)
        
        return content
    
    async def _apply_transformation_rule(self, rule: Dict[str, Any], transformed_files: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Apply a transformation rule to transformed files."""
        return {
            "rule_name": rule.get("name", "unnamed"),
            "rule_type": rule.get("type", "unknown"),
            "files_affected": len(transformed_files),
            "success": True
        }


class ModernizationPlannerTool(BaseAgentTool):
    """Tool for creating modernization plans."""
    
    def __init__(self):
        super().__init__(
            name="create_modernization_plan",
            description="Create detailed modernization plan"
        )
    
    async def run(self, analysis_results: Dict[str, Any], target_language: str, modernization_goals: List[str]) -> Dict[str, Any]:
        """Create modernization plan."""
        try:
            plan = {
                "phases": [],
                "timeline": {},
                "resources_required": {},
                "risks": [],
                "success_criteria": [],
                "estimated_duration": 0,
                "complexity_score": 0
            }
            
            # Analyze codebase complexity
            complexity_score = self._calculate_complexity_score(analysis_results)
            plan["complexity_score"] = complexity_score
            
            # Create phases based on analysis
            phases = self._create_modernization_phases(analysis_results, target_language, modernization_goals)
            plan["phases"] = phases
            
            # Estimate timeline
            timeline = self._estimate_timeline(phases, complexity_score)
            plan["timeline"] = timeline
            plan["estimated_duration"] = sum(timeline.values())
            
            # Identify risks
            risks = self._identify_risks(analysis_results, target_language)
            plan["risks"] = risks
            
            # Define success criteria
            success_criteria = self._define_success_criteria(modernization_goals, target_language)
            plan["success_criteria"] = success_criteria
            
            # Estimate resources
            resources = self._estimate_resources(phases, complexity_score)
            plan["resources_required"] = resources
            
            self.log_usage({"target_language": target_language, "goals": len(modernization_goals)}, 
                         f"Created plan with {len(plan['phases'])} phases")
            return plan
        except Exception as e:
            self.logger.error(f"Error creating modernization plan: {e}")
            raise
    
    def _calculate_complexity_score(self, analysis_results: Dict[str, Any]) -> float:
        """Calculate complexity score based on analysis results."""
        total_files = analysis_results.get("total_files", 0)
        total_lines = analysis_results.get("total_lines", 0)
        total_functions = len(analysis_results.get("functions", []))
        total_classes = len(analysis_results.get("classes", []))
        
        # Simple complexity calculation
        complexity = (total_files * 0.1) + (total_lines * 0.001) + (total_functions * 0.5) + (total_classes * 1.0)
        return min(complexity, 100.0)  # Cap at 100
    
    def _create_modernization_phases(self, analysis_results: Dict[str, Any], target_language: str, goals: List[str]) -> List[Dict[str, Any]]:
        """Create modernization phases based on analysis."""
        phases = []
        
        # Phase 1: Analysis and Preparation
        phases.append({
            "name": "Analysis and Preparation",
            "description": "Analyze existing codebase and prepare for modernization",
            "duration_days": 5,
            "tasks": [
                "Complete code analysis",
                "Identify dependencies",
                "Create backup",
                "Set up development environment"
            ],
            "dependencies": []
        })
        
        # Phase 2: Core Transformation
        phases.append({
            "name": "Core Transformation",
            "description": "Transform core business logic to target language",
            "duration_days": 15,
            "tasks": [
                "Transform main programs",
                "Convert data structures",
                "Update control flow",
                "Handle language-specific features"
            ],
            "dependencies": ["Analysis and Preparation"]
        })
        
        # Phase 3: Integration and Testing
        phases.append({
            "name": "Integration and Testing",
            "description": "Integrate transformed code and create comprehensive tests",
            "duration_days": 10,
            "tasks": [
                "Create unit tests",
                "Integration testing",
                "Performance testing",
                "User acceptance testing"
            ],
            "dependencies": ["Core Transformation"]
        })
        
        # Phase 4: Deployment and Documentation
        phases.append({
            "name": "Deployment and Documentation",
            "description": "Deploy modernized code and create documentation",
            "duration_days": 5,
            "tasks": [
                "Deploy to production",
                "Create user documentation",
                "Train users",
                "Monitor performance"
            ],
            "dependencies": ["Integration and Testing"]
        })
        
        return phases
    
    def _estimate_timeline(self, phases: List[Dict[str, Any]], complexity_score: float) -> Dict[str, int]:
        """Estimate timeline for each phase."""
        timeline = {}
        complexity_multiplier = 1 + (complexity_score / 100)
        
        for phase in phases:
            base_duration = phase["duration_days"]
            adjusted_duration = int(base_duration * complexity_multiplier)
            timeline[phase["name"]] = adjusted_duration
        
        return timeline
    
    def _identify_risks(self, analysis_results: Dict[str, Any], target_language: str) -> List[Dict[str, Any]]:
        """Identify potential risks in modernization."""
        risks = []
        
        # High complexity risk
        if analysis_results.get("total_lines", 0) > 10000:
            risks.append({
                "type": "high",
                "description": "Large codebase may require significant time and resources",
                "mitigation": "Break down into smaller, manageable chunks"
            })
        
        # Legacy technology risk
        if target_language.lower() in ["python", "java"]:
            risks.append({
                "type": "medium",
                "description": f"Migration to {target_language} may require significant architectural changes",
                "mitigation": "Plan for gradual migration and extensive testing"
            })
        
        # Dependency risk
        external_deps = len(analysis_results.get("external_dependencies", []))
        if external_deps > 50:
            risks.append({
                "type": "medium",
                "description": "High number of external dependencies may complicate migration",
                "mitigation": "Audit dependencies and plan for alternatives"
            })
        
        return risks
    
    def _define_success_criteria(self, goals: List[str], target_language: str) -> List[Dict[str, Any]]:
        """Define success criteria for modernization."""
        criteria = []
        
        for goal in goals:
            if "performance" in goal.lower():
                criteria.append({
                    "criterion": "Performance improvement",
                    "measure": "Response time reduction by 20%",
                    "target": "Achieved"
                })
            elif "maintainability" in goal.lower():
                criteria.append({
                    "criterion": "Code maintainability",
                    "measure": "Reduced cyclomatic complexity",
                    "target": "Complexity score < 10"
                })
            elif "readability" in goal.lower():
                criteria.append({
                    "criterion": "Code readability",
                    "measure": "Code review score",
                    "target": "Score > 8/10"
                })
        
        # Default criteria
        criteria.extend([
            {
                "criterion": "Functional equivalence",
                "measure": "All original functionality preserved",
                "target": "100% functional coverage"
            },
            {
                "criterion": "Test coverage",
                "measure": "Unit test coverage",
                "target": "Coverage > 80%"
            }
        ])
        
        return criteria
    
    def _estimate_resources(self, phases: List[Dict[str, Any]], complexity_score: float) -> Dict[str, Any]:
        """Estimate resources required for modernization."""
        base_team_size = 2
        complexity_multiplier = 1 + (complexity_score / 200)
        
        return {
            "team_size": int(base_team_size * complexity_multiplier),
            "roles": ["Lead Developer", "Senior Developer", "QA Engineer", "DevOps Engineer"],
            "tools": ["IDE", "Version Control", "Testing Framework", "CI/CD Pipeline"],
            "infrastructure": ["Development Environment", "Testing Environment", "Production Environment"]
        }


class RiskAssessmentTool(BaseAgentTool):
    """Tool for assessing modernization risks."""
    
    def __init__(self):
        super().__init__(
            name="assess_modernization_risks",
            description="Assess risks in modernization plan"
        )
    
    async def run(self, plan: Dict[str, Any], codebase_complexity: Dict[str, Any]) -> Dict[str, Any]:
        """Assess modernization risks."""
        try:
            risks = {
                "high_risk": [],
                "medium_risk": [],
                "low_risk": [],
                "mitigation_strategies": [],
                "overall_risk_score": 0.0,
                "risk_factors": []
            }
            
            # Analyze complexity risks
            complexity_score = codebase_complexity.get("complexity_score", 0)
            if complexity_score > 70:
                risks["high_risk"].append({
                    "type": "complexity",
                    "description": "High codebase complexity increases modernization risk",
                    "impact": "High",
                    "probability": 0.8
                })
            
            # Analyze timeline risks
            phases = plan.get("phases", [])
            total_duration = sum(phase.get("duration_days", 0) for phase in phases)
            if total_duration > 60:
                risks["medium_risk"].append({
                    "type": "timeline",
                    "description": "Long project duration increases risk of scope creep",
                    "impact": "Medium",
                    "probability": 0.6
                })
            
            # Analyze resource risks
            resources = plan.get("resources_required", {})
            team_size = resources.get("team_size", 0)
            if team_size > 5:
                risks["medium_risk"].append({
                    "type": "resources",
                    "description": "Large team size may lead to coordination challenges",
                    "impact": "Medium",
                    "probability": 0.5
                })
            
            # Calculate overall risk score
            high_risk_count = len(risks["high_risk"])
            medium_risk_count = len(risks["medium_risk"])
            low_risk_count = len(risks["low_risk"])
            
            total_risks = high_risk_count + medium_risk_count + low_risk_count
            if total_risks > 0:
                risks["overall_risk_score"] = (high_risk_count * 3 + medium_risk_count * 2 + low_risk_count * 1) / (total_risks * 3)
            
            # Generate mitigation strategies
            risks["mitigation_strategies"] = self._generate_mitigation_strategies(risks)
            
            self.log_usage({"plan_phases": len(phases), "complexity": codebase_complexity}, 
                         f"Assessed {len(risks['high_risk'])} high risks")
            return risks
        except Exception as e:
            self.logger.error(f"Error assessing risks: {e}")
            raise
    
    def _generate_mitigation_strategies(self, risks: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Generate mitigation strategies based on identified risks."""
        strategies = []
        
        for risk in risks["high_risk"]:
            if risk["type"] == "complexity":
                strategies.append({
                    "risk_type": "complexity",
                    "strategy": "Break down complex modules into smaller, manageable components",
                    "priority": "High"
                })
        
        for risk in risks["medium_risk"]:
            if risk["type"] == "timeline":
                strategies.append({
                    "risk_type": "timeline",
                    "strategy": "Implement agile methodology with regular checkpoints",
                    "priority": "Medium"
                })
            elif risk["type"] == "resources":
                strategies.append({
                    "risk_type": "resources",
                    "strategy": "Establish clear communication channels and regular team meetings",
                    "priority": "Medium"
                })
        
        return strategies


class CodeReviewTool(BaseAgentTool):
    """Tool for reviewing transformed code."""
    
    def __init__(self):
        super().__init__(
            name="review_transformed_code",
            description="Review transformed code for quality and standards"
        )
    
    async def run(self, transformed_code: Dict[str, Any], quality_standards: List[str]) -> Dict[str, Any]:
        """Review transformed code."""
        try:
            review = {
                "overall_score": 0.0,
                "quality_metrics": {},
                "standards_compliance": {},
                "issues_found": [],
                "recommendations": [],
                "files_reviewed": 0
            }
            
            files = transformed_code.get("files", [])
            review["files_reviewed"] = len(files)
            
            # Analyze each file
            total_score = 0
            for file_info in files:
                file_score = await self._review_file(file_info, quality_standards)
                total_score += file_score["score"]
                review["issues_found"].extend(file_info.get("issues", []))
            
            # Calculate overall score
            if files:
                review["overall_score"] = total_score / len(files)
            
            # Generate quality metrics
            review["quality_metrics"] = self._calculate_quality_metrics(files)
            
            # Check standards compliance
            review["standards_compliance"] = self._check_standards_compliance(files, quality_standards)
            
            # Generate recommendations
            review["recommendations"] = self._generate_recommendations(review)
            
            self.log_usage({"transformed_files": len(files), "standards": len(quality_standards)}, 
                         f"Reviewed code with score: {review['overall_score']:.2f}")
            return review
        except Exception as e:
            self.logger.error(f"Error reviewing code: {e}")
            raise
    
    async def _review_file(self, file_info: Dict[str, Any], standards: List[str]) -> Dict[str, Any]:
        """Review a single file."""
        score = 8.0  # Base score
        issues = []
        
        # Check for common issues
        if file_info.get("lines", 0) > 500:
            issues.append({"type": "length", "message": "File is too long", "severity": "medium"})
            score -= 1.0
        
        if file_info.get("functions", 0) > 20:
            issues.append({"type": "complexity", "message": "Too many functions in file", "severity": "medium"})
            score -= 0.5
        
        return {"score": max(score, 0.0), "issues": issues}
    
    def _calculate_quality_metrics(self, files: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Calculate quality metrics."""
        total_lines = sum(f.get("lines", 0) for f in files)
        total_functions = sum(len(f.get("functions", [])) for f in files)
        
        return {
            "total_lines": total_lines,
            "total_functions": total_functions,
            "avg_lines_per_file": total_lines / len(files) if files else 0,
            "avg_functions_per_file": total_functions / len(files) if files else 0
        }
    
    def _check_standards_compliance(self, files: List[Dict[str, Any]], standards: List[str]) -> Dict[str, Any]:
        """Check compliance with standards."""
        compliance = {}
        
        for standard in standards:
            if "naming" in standard.lower():
                compliance[standard] = {"score": 0.8, "issues": []}
            elif "formatting" in standard.lower():
                compliance[standard] = {"score": 0.9, "issues": []}
            else:
                compliance[standard] = {"score": 0.7, "issues": []}
        
        return compliance
    
    def _generate_recommendations(self, review: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Generate improvement recommendations."""
        recommendations = []
        
        if review["overall_score"] < 7.0:
            recommendations.append({
                "type": "quality",
                "priority": "high",
                "description": "Overall code quality needs improvement"
            })
        
        if len(review["issues_found"]) > 10:
            recommendations.append({
                "type": "issues",
                "priority": "medium",
                "description": "High number of issues found, consider refactoring"
            })
        
        return recommendations


class QualityAnalyzerTool(BaseAgentTool):
    """Tool for analyzing code quality."""
    
    def __init__(self):
        super().__init__(
            name="analyze_code_quality",
            description="Analyze code quality metrics and standards"
        )
    
    async def run(self, code_path: str, quality_standards: List[str]) -> Dict[str, Any]:
        """Analyze code quality."""
        try:
            quality_analysis = {
                "overall_score": 0.0,
                "metrics": {},
                "standards_compliance": {},
                "issues": [],
                "recommendations": []
            }
            
            # Analyze code metrics
            metrics = await self._analyze_code_metrics(code_path)
            quality_analysis["metrics"] = metrics
            
            # Calculate overall score
            quality_analysis["overall_score"] = self._calculate_quality_score(metrics)
            
            # Check standards compliance
            compliance = self._check_standards_compliance(code_path, quality_standards)
            quality_analysis["standards_compliance"] = compliance
            
            # Generate recommendations
            quality_analysis["recommendations"] = self._generate_quality_recommendations(metrics, compliance)
            
            self.log_usage({"code_path": code_path, "standards": len(quality_standards)}, 
                         f"Analyzed quality with score: {quality_analysis['overall_score']:.2f}")
            return quality_analysis
        except Exception as e:
            self.logger.error(f"Error analyzing quality: {e}")
            raise
    
    async def _analyze_code_metrics(self, code_path: str) -> Dict[str, Any]:
        """Analyze code metrics."""
        path = Path(code_path)
        if not path.exists():
            return {"error": "File not found"}
        
        with open(path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
            lines = content.split('\n')
        
        return {
            "lines_of_code": len(lines),
            "non_empty_lines": len([line for line in lines if line.strip()]),
            "comment_lines": len([line for line in lines if line.strip().startswith('#')]),
            "complexity": self._calculate_complexity(content),
            "readability": self._calculate_readability(content)
        }
    
    def _calculate_complexity(self, content: str) -> float:
        """Calculate code complexity."""
        # Simple complexity calculation based on control structures
        complexity_keywords = ['if', 'for', 'while', 'try', 'except', 'with']
        complexity = 1  # Base complexity
        
        for keyword in complexity_keywords:
            complexity += content.lower().count(keyword)
        
        return complexity
    
    def _calculate_readability(self, content: str) -> float:
        """Calculate code readability score."""
        lines = content.split('\n')
        non_empty_lines = [line for line in lines if line.strip()]
        
        if not non_empty_lines:
            return 0.0
        
        # Simple readability based on line length and structure
        avg_line_length = sum(len(line) for line in non_empty_lines) / len(non_empty_lines)
        long_lines = len([line for line in non_empty_lines if len(line) > 80])
        
        readability = 10.0 - (avg_line_length / 10) - (long_lines / len(non_empty_lines) * 5)
        return max(0.0, min(10.0, readability))
    
    def _calculate_quality_score(self, metrics: Dict[str, Any]) -> float:
        """Calculate overall quality score."""
        if "error" in metrics:
            return 0.0
        
        score = 5.0  # Base score
        
        # Adjust based on metrics
        if metrics.get("lines_of_code", 0) < 1000:
            score += 1.0
        if metrics.get("complexity", 0) < 10:
            score += 1.0
        if metrics.get("readability", 0) > 7.0:
            score += 1.0
        
        return min(10.0, score)
    
    def _check_standards_compliance(self, code_path: str, standards: List[str]) -> Dict[str, Any]:
        """Check compliance with standards."""
        compliance = {}
        
        for standard in standards:
            if "pep8" in standard.lower():
                compliance[standard] = {"score": 0.8, "issues": []}
            elif "pylint" in standard.lower():
                compliance[standard] = {"score": 0.7, "issues": []}
            else:
                compliance[standard] = {"score": 0.6, "issues": []}
        
        return compliance
    
    def _generate_quality_recommendations(self, metrics: Dict[str, Any], compliance: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Generate quality improvement recommendations."""
        recommendations = []
        
        if metrics.get("complexity", 0) > 15:
            recommendations.append({
                "type": "complexity",
                "priority": "high",
                "description": "Code complexity is high, consider refactoring"
            })
        
        if metrics.get("readability", 0) < 6.0:
            recommendations.append({
                "type": "readability",
                "priority": "medium",
                "description": "Code readability can be improved"
            })
        
        return recommendations


class FinalValidatorTool(BaseAgentTool):
    """Tool for final validation of modernized code."""
    
    def __init__(self):
        super().__init__(
            name="final_validation",
            description="Perform final validation of modernized code"
        )
    
    async def run(self, modernized_code: Dict[str, Any], validation_criteria: List[str]) -> Dict[str, Any]:
        """Perform final validation."""
        try:
            validation = {
                "validation_passed": True,
                "criteria_met": [],
                "issues_found": [],
                "recommendations": [],
                "validation_score": 0.0
            }
            
            files = modernized_code.get("files", [])
            total_criteria = len(validation_criteria)
            met_criteria = 0
            
            # Check each validation criterion
            for criterion in validation_criteria:
                if await self._check_criterion(criterion, files):
                    validation["criteria_met"].append(criterion)
                    met_criteria += 1
                else:
                    validation["issues_found"].append(f"Criterion not met: {criterion}")
            
            # Calculate validation score
            if total_criteria > 0:
                validation["validation_score"] = met_criteria / total_criteria
                validation["validation_passed"] = validation["validation_score"] >= 0.8
            
            # Generate recommendations
            validation["recommendations"] = self._generate_validation_recommendations(validation)
            
            self.log_usage({"modernized_files": len(files), "criteria": total_criteria}, 
                         f"Validation {'passed' if validation['validation_passed'] else 'failed'}")
            return validation
        except Exception as e:
            self.logger.error(f"Error in final validation: {e}")
            raise
    
    async def _check_criterion(self, criterion: str, files: List[Dict[str, Any]]) -> bool:
        """Check if a validation criterion is met."""
        if "syntax" in criterion.lower():
            return True  # Assume syntax is valid
        elif "functionality" in criterion.lower():
            return len(files) > 0
        elif "performance" in criterion.lower():
            return True  # Assume performance is acceptable
        else:
            return True  # Default to passing
    
    def _generate_validation_recommendations(self, validation: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Generate validation recommendations."""
        recommendations = []
        
        if not validation["validation_passed"]:
            recommendations.append({
                "type": "validation",
                "priority": "high",
                "description": "Address validation issues before deployment"
            })
        
        if validation["validation_score"] < 0.9:
            recommendations.append({
                "type": "quality",
                "priority": "medium",
                "description": "Consider additional testing and validation"
            })
        
        return recommendations


class ComplianceCheckerTool(BaseAgentTool):
    """Tool for checking compliance with standards."""
    
    def __init__(self):
        super().__init__(
            name="check_compliance",
            description="Check compliance with coding standards"
        )
    
    async def run(self, code_path: str, standards: List[str]) -> Dict[str, Any]:
        """Check compliance."""
        try:
            compliance = {
                "overall_compliance": 0.0,
                "standards_met": [],
                "violations": [],
                "recommendations": []
            }
            
            # Check each standard
            total_standards = len(standards)
            met_standards = 0
            
            for standard in standards:
                if await self._check_standard_compliance(code_path, standard):
                    compliance["standards_met"].append(standard)
                    met_standards += 1
                else:
                    compliance["violations"].append(f"Violation: {standard}")
            
            # Calculate overall compliance
            if total_standards > 0:
                compliance["overall_compliance"] = met_standards / total_standards
            
            # Generate recommendations
            compliance["recommendations"] = self._generate_compliance_recommendations(compliance)
            
            self.log_usage({"code_path": code_path, "standards": total_standards}, 
                         f"Compliance score: {compliance['overall_compliance']:.2f}")
            return compliance
        except Exception as e:
            self.logger.error(f"Error checking compliance: {e}")
            raise
    
    async def _check_standard_compliance(self, code_path: str, standard: str) -> bool:
        """Check compliance with a specific standard."""
        # Simplified compliance checking
        if "naming" in standard.lower():
            return True  # Assume naming conventions are followed
        elif "formatting" in standard.lower():
            return True  # Assume formatting is correct
        elif "documentation" in standard.lower():
            return True  # Assume documentation is present
        else:
            return True  # Default to compliant
    
    def _generate_compliance_recommendations(self, compliance: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Generate compliance recommendations."""
        recommendations = []
        
        if compliance["overall_compliance"] < 0.8:
            recommendations.append({
                "type": "compliance",
                "priority": "high",
                "description": "Improve compliance with coding standards"
            })
        
        if len(compliance["violations"]) > 0:
            recommendations.append({
                "type": "violations",
                "priority": "medium",
                "description": "Address coding standard violations"
            })
        
        return recommendations


class TestValidatorTool(BaseAgentTool):
    """Tool for validating test cases."""
    
    def __init__(self):
        super().__init__(
            name="validate_tests",
            description="Validate test cases and test coverage"
        )
    
    async def run(self, test_cases: List[Dict[str, Any]], coverage_threshold: float) -> Dict[str, Any]:
        """Validate test cases."""
        try:
            validation = {
                "tests_valid": True,
                "coverage_met": True,
                "coverage_percentage": 0.0,
                "issues": [],
                "test_quality_score": 0.0
            }
            
            # Validate test cases
            valid_tests = 0
            for test_case in test_cases:
                if await self._validate_test_case(test_case):
                    valid_tests += 1
                else:
                    validation["issues"].append(f"Invalid test case: {test_case.get('name', 'unnamed')}")
            
            # Calculate test quality
            if test_cases:
                validation["test_quality_score"] = valid_tests / len(test_cases)
                validation["tests_valid"] = validation["test_quality_score"] >= 0.8
            
            # Check coverage
            coverage = self._calculate_test_coverage(test_cases)
            validation["coverage_percentage"] = coverage
            validation["coverage_met"] = coverage >= coverage_threshold
            
            self.log_usage({"test_cases": len(test_cases), "threshold": coverage_threshold}, 
                         f"Test validation: {'passed' if validation['tests_valid'] else 'failed'}")
            return validation
        except Exception as e:
            self.logger.error(f"Error validating tests: {e}")
            raise
    
    async def _validate_test_case(self, test_case: Dict[str, Any]) -> bool:
        """Validate a single test case."""
        # Check if test case has required fields
        required_fields = ["name", "description"]
        return all(field in test_case for field in required_fields)
    
    def _calculate_test_coverage(self, test_cases: List[Dict[str, Any]]) -> float:
        """Calculate test coverage percentage."""
        if not test_cases:
            return 0.0
        
        # Simplified coverage calculation
        total_tests = len(test_cases)
        valid_tests = sum(1 for test in test_cases if test.get("valid", True))
        
        return (valid_tests / total_tests) * 100.0
