"""
Runs ESLint, Prettier, and other code quality tools on generated code.
"""

import asyncio
import json
import logging
import os
import subprocess
from typing import Dict, List, Optional, Any, Tuple
from pathlib import Path
from dataclasses import dataclass

logger = logging.getLogger(__name__)

@dataclass
class LintResult:
    """Result of a linting operation."""
    tool: str
    file_path: str
    success: bool
    issues: List[Dict[str, Any]]
    output: str
    error: Optional[str] = None

@dataclass
class LintIssue:
    """A single linting issue."""
    line: int
    column: int
    severity: str  # 'error', 'warning', 'info'
    message: str
    rule: Optional[str] = None
    fixable: bool = False

class LintingManager:
    """
    Manages code quality tools like ESLint, Prettier, and other linters.
    
    Provides a unified interface for running various linting tools
    and processing their results.
    """
    
    def __init__(self, working_dir: str = "."):
        self.working_dir = Path(working_dir).resolve()
        self.available_tools = self._detect_available_tools()
        
    def _detect_available_tools(self) -> Dict[str, bool]:
        """Detect which linting tools are available in the environment."""
        tools = {
            'eslint': False,
            'prettier': False,
            'black': False,
            'flake8': False,
            'pylint': False,
            'stylelint': False,
            'tsc': False  # TypeScript compiler
        }
        
        for tool in tools.keys():
            try:
                result = subprocess.run([tool, '--version'], 
                                      capture_output=True, 
                                      text=True, 
                                      timeout=5)
                tools[tool] = result.returncode == 0
            except (subprocess.TimeoutExpired, FileNotFoundError):
                tools[tool] = False
        
        logger.info(f"Available linting tools: {[k for k, v in tools.items() if v]}")
        return tools
    
    async def lint_file(self, file_path: str, tools: Optional[List[str]] = None) -> List[LintResult]:
        """
        Lint a single file with specified tools.
        
        Args:
            file_path: Path to the file to lint
            tools: List of tools to use (defaults to all available)
            
        Returns:
            List of lint results for each tool
        """
        if tools is None:
            tools = [tool for tool, available in self.available_tools.items() if available]
        
        results = []
        full_path = self.working_dir / file_path
        
        if not full_path.exists():
            logger.warning(f"File not found: {full_path}")
            return results
        
        # Determine file type and appropriate tools
        file_ext = full_path.suffix.lower()
        appropriate_tools = self._get_appropriate_tools(file_ext, tools)
        
        for tool in appropriate_tools:
            try:
                result = await self._run_linting_tool(tool, str(full_path))
                results.append(result)
            except Exception as e:
                logger.error(f"Error running {tool} on {file_path}: {e}")
                results.append(LintResult(
                    tool=tool,
                    file_path=file_path,
                    success=False,
                    issues=[],
                    output="",
                    error=str(e)
                ))
        
        return results
    
    async def lint_directory(self, directory: str = ".", tools: Optional[List[str]] = None) -> Dict[str, List[LintResult]]:
        """
        Lint all files in a directory.
        
        Args:
            directory: Directory to lint
            tools: List of tools to use
            
        Returns:
            Dictionary mapping file paths to lint results
        """
        if tools is None:
            tools = [tool for tool, available in self.available_tools.items() if available]
        
        results = {}
        dir_path = self.working_dir / directory
        
        if not dir_path.exists():
            logger.warning(f"Directory not found: {dir_path}")
            return results
        
        # Find all files to lint
        files_to_lint = []
        for ext, tool_list in self._get_file_extensions().items():
            if any(tool in tools for tool in tool_list):
                files_to_lint.extend(dir_path.glob(f"**/*{ext}"))
        
        # Remove duplicates and sort
        files_to_lint = sorted(set(files_to_lint))
        
        for file_path in files_to_lint:
            rel_path = str(file_path.relative_to(self.working_dir))
            file_results = await self.lint_file(rel_path, tools)
            if file_results:
                results[rel_path] = file_results
        
        return results
    
    async def _run_linting_tool(self, tool: str, file_path: str) -> LintResult:
        """Run a specific linting tool on a file."""
        if tool == 'eslint':
            return await self._run_eslint(file_path)
        elif tool == 'prettier':
            return await self._run_prettier(file_path)
        elif tool == 'black':
            return await self._run_black(file_path)
        elif tool == 'flake8':
            return await self._run_flake8(file_path)
        elif tool == 'pylint':
            return await self._run_pylint(file_path)
        elif tool == 'stylelint':
            return await self._run_stylelint(file_path)
        elif tool == 'tsc':
            return await self._run_typescript_compiler(file_path)
        else:
            raise ValueError(f"Unknown linting tool: {tool}")
    
    async def _run_eslint(self, file_path: str) -> LintResult:
        """Run ESLint on a file."""
        try:
            process = await asyncio.create_subprocess_exec(
                'eslint', '--format', 'json', file_path,
                cwd=self.working_dir,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await process.communicate()
            output = stdout.decode('utf-8')
            error_output = stderr.decode('utf-8')
            
            issues = []
            if output.strip():
                try:
                    eslint_results = json.loads(output)
                    for result in eslint_results:
                        for message in result.get('messages', []):
                            issues.append(LintIssue(
                                line=message.get('line', 0),
                                column=message.get('column', 0),
                                severity=message.get('severity', 'warning'),
                                message=message.get('message', ''),
                                rule=message.get('ruleId'),
                                fixable=message.get('fixable', False)
                            ))
                except json.JSONDecodeError:
                    pass
            
            return LintResult(
                tool='eslint',
                file_path=file_path,
                success=process.returncode == 0,
                issues=issues,
                output=output,
                error=error_output if error_output else None
            )
            
        except Exception as e:
            return LintResult(
                tool='eslint',
                file_path=file_path,
                success=False,
                issues=[],
                output="",
                error=str(e)
            )
    
    async def _run_prettier(self, file_path: str) -> LintResult:
        """Run Prettier on a file."""
        try:
            process = await asyncio.create_subprocess_exec(
                'prettier', '--check', '--loglevel', 'warn', file_path,
                cwd=self.working_dir,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await process.communicate()
            output = stdout.decode('utf-8')
            error_output = stderr.decode('utf-8')
            
            # Prettier doesn't provide detailed issue information in check mode
            # We just know if the file needs formatting or not
            issues = []
            if process.returncode != 0:
                issues.append(LintIssue(
                    line=0,
                    column=0,
                    severity='warning',
                    message='File needs formatting',
                    rule='prettier/prettier',
                    fixable=True
                ))
            
            return LintResult(
                tool='prettier',
                file_path=file_path,
                success=process.returncode == 0,
                issues=issues,
                output=output,
                error=error_output if error_output else None
            )
            
        except Exception as e:
            return LintResult(
                tool='prettier',
                file_path=file_path,
                success=False,
                issues=[],
                output="",
                error=str(e)
            )
    
    async def _run_black(self, file_path: str) -> LintResult:
        """Run Black on a Python file."""
        try:
            process = await asyncio.create_subprocess_exec(
                'black', '--check', '--quiet', file_path,
                cwd=self.working_dir,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await process.communicate()
            output = stdout.decode('utf-8')
            error_output = stderr.decode('utf-8')
            
            issues = []
            if process.returncode != 0:
                issues.append(LintIssue(
                    line=0,
                    column=0,
                    severity='warning',
                    message='File needs formatting',
                    rule='black',
                    fixable=True
                ))
            
            return LintResult(
                tool='black',
                file_path=file_path,
                success=process.returncode == 0,
                issues=issues,
                output=output,
                error=error_output if error_output else None
            )
            
        except Exception as e:
            return LintResult(
                tool='black',
                file_path=file_path,
                success=False,
                issues=[],
                output="",
                error=str(e)
            )
    
    async def _run_flake8(self, file_path: str) -> LintResult:
        """Run Flake8 on a Python file."""
        try:
            process = await asyncio.create_subprocess_exec(
                'flake8', '--format', 'json', file_path,
                cwd=self.working_dir,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await process.communicate()
            output = stdout.decode('utf-8')
            error_output = stderr.decode('utf-8')
            
            issues = []
            if output.strip():
                try:
                    flake8_results = json.loads(output)
                    for file_issues in flake8_results.values():
                        for issue in file_issues:
                            issues.append(LintIssue(
                                line=issue.get('line_number', 0),
                                column=issue.get('column_number', 0),
                                severity='error' if issue.get('error_code', '').startswith('E') else 'warning',
                                message=issue.get('text', ''),
                                rule=issue.get('error_code'),
                                fixable=False
                            ))
                except json.JSONDecodeError:
                    pass
            
            return LintResult(
                tool='flake8',
                file_path=file_path,
                success=process.returncode == 0,
                issues=issues,
                output=output,
                error=error_output if error_output else None
            )
            
        except Exception as e:
            return LintResult(
                tool='flake8',
                file_path=file_path,
                success=False,
                issues=[],
                output="",
                error=str(e)
            )
    
    async def _run_pylint(self, file_path: str) -> LintResult:
        """Run Pylint on a Python file."""
        try:
            process = await asyncio.create_subprocess_exec(
                'pylint', '--output-format', 'json', file_path,
                cwd=self.working_dir,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await process.communicate()
            output = stdout.decode('utf-8')
            error_output = stderr.decode('utf-8')
            
            issues = []
            if output.strip():
                try:
                    pylint_results = json.loads(output)
                    for issue in pylint_results:
                        issues.append(LintIssue(
                            line=issue.get('line', 0),
                            column=issue.get('column', 0),
                            severity=issue.get('type', 'warning'),
                            message=issue.get('message', ''),
                            rule=issue.get('symbol'),
                            fixable=False
                        ))
                except json.JSONDecodeError:
                    pass
            
            return LintResult(
                tool='pylint',
                file_path=file_path,
                success=process.returncode == 0,
                issues=issues,
                output=output,
                error=error_output if error_output else None
            )
            
        except Exception as e:
            return LintResult(
                tool='pylint',
                file_path=file_path,
                success=False,
                issues=[],
                output="",
                error=str(e)
            )
    
    async def _run_stylelint(self, file_path: str) -> LintResult:
        """Run Stylelint on a CSS file."""
        try:
            process = await asyncio.create_subprocess_exec(
                'stylelint', '--formatter', 'json', file_path,
                cwd=self.working_dir,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await process.communicate()
            output = stdout.decode('utf-8')
            error_output = stderr.decode('utf-8')
            
            issues = []
            if output.strip():
                try:
                    stylelint_results = json.loads(output)
                    for result in stylelint_results:
                        for warning in result.get('warnings', []):
                            issues.append(LintIssue(
                                line=warning.get('line', 0),
                                column=warning.get('column', 0),
                                severity=warning.get('severity', 'warning'),
                                message=warning.get('text', ''),
                                rule=warning.get('rule'),
                                fixable=warning.get('fixable', False)
                            ))
                except json.JSONDecodeError:
                    pass
            
            return LintResult(
                tool='stylelint',
                file_path=file_path,
                success=process.returncode == 0,
                issues=issues,
                output=output,
                error=error_output if error_output else None
            )
            
        except Exception as e:
            return LintResult(
                tool='stylelint',
                file_path=file_path,
                success=False,
                issues=[],
                output="",
                error=str(e)
            )
    
    async def _run_typescript_compiler(self, file_path: str) -> LintResult:
        """Run TypeScript compiler for type checking."""
        try:
            process = await asyncio.create_subprocess_exec(
                'tsc', '--noEmit', '--strict', file_path,
                cwd=self.working_dir,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await process.communicate()
            output = stdout.decode('utf-8')
            error_output = stderr.decode('utf-8')
            
            # Parse TypeScript compiler output
            issues = []
            for line in error_output.split('\n'):
                if line.strip() and ':' in line:
                    # Parse TypeScript error format: file(line,col): error TS1234: message
                    parts = line.split(':')
                    if len(parts) >= 4:
                        try:
                            line_num = int(parts[1].split('(')[1].split(',')[0])
                            col_num = int(parts[1].split(',')[1].split(')')[0])
                            error_code = parts[2].strip()
                            message = ':'.join(parts[3:]).strip()
                            
                            issues.append(LintIssue(
                                line=line_num,
                                column=col_num,
                                severity='error',
                                message=message,
                                rule=error_code,
                                fixable=False
                            ))
                        except (ValueError, IndexError):
                            pass
            
            return LintResult(
                tool='tsc',
                file_path=file_path,
                success=process.returncode == 0,
                issues=issues,
                output=output,
                error=error_output if error_output else None
            )
            
        except Exception as e:
            return LintResult(
                tool='tsc',
                file_path=file_path,
                success=False,
                issues=[],
                output="",
                error=str(e)
            )
    
    def _get_appropriate_tools(self, file_ext: str, available_tools: List[str]) -> List[str]:
        """Get appropriate tools for a file extension."""
        tool_mapping = {
            '.js': ['eslint', 'prettier'],
            '.jsx': ['eslint', 'prettier'],
            '.ts': ['eslint', 'prettier', 'tsc'],
            '.tsx': ['eslint', 'prettier', 'tsc'],
            '.py': ['black', 'flake8', 'pylint'],
            '.css': ['stylelint', 'prettier'],
            '.scss': ['stylelint', 'prettier'],
            '.sass': ['stylelint', 'prettier'],
            '.less': ['stylelint', 'prettier'],
            '.html': ['prettier'],
            '.json': ['prettier'],
            '.md': ['prettier'],
            '.astro': ['prettier'],
            '.vue': ['eslint', 'prettier'],
            '.svelte': ['prettier']
        }
        
        appropriate = tool_mapping.get(file_ext, [])
        return [tool for tool in appropriate if tool in available_tools]
    
    def _get_file_extensions(self) -> Dict[str, List[str]]:
        """Get mapping of file extensions to appropriate tools."""
        return {
            '.js': ['eslint', 'prettier'],
            '.jsx': ['eslint', 'prettier'],
            '.ts': ['eslint', 'prettier', 'tsc'],
            '.tsx': ['eslint', 'prettier', 'tsc'],
            '.py': ['black', 'flake8', 'pylint'],
            '.css': ['stylelint', 'prettier'],
            '.scss': ['stylelint', 'prettier'],
            '.sass': ['stylelint', 'prettier'],
            '.less': ['stylelint', 'prettier'],
            '.html': ['prettier'],
            '.json': ['prettier'],
            '.md': ['prettier'],
            '.astro': ['prettier'],
            '.vue': ['eslint', 'prettier'],
            '.svelte': ['prettier']
        }
    
    def get_summary(self, results: Dict[str, List[LintResult]]) -> Dict[str, Any]:
        """Get a summary of linting results."""
        total_files = len(results)
        total_issues = 0
        issues_by_severity = {'error': 0, 'warning': 0, 'info': 0}
        issues_by_tool = {}
        
        for file_results in results.values():
            for result in file_results:
                if not result.success:
                    total_issues += len(result.issues)
                    
                    for issue in result.issues:
                        issues_by_severity[issue.severity] += 1
                        
                        tool = result.tool
                        if tool not in issues_by_tool:
                            issues_by_tool[tool] = 0
                        issues_by_tool[tool] += 1
        
        return {
            'total_files': total_files,
            'total_issues': total_issues,
            'issues_by_severity': issues_by_severity,
            'issues_by_tool': issues_by_tool,
            'success_rate': (total_files - len([r for results_list in results.values() 
                                              for r in results_list if not r.success])) / total_files if total_files > 0 else 1.0
        } 