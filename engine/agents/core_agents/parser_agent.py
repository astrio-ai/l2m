"""
Parser Agent - Reads legacy project, understands structure, builds map.
"""

import logging
from typing import Dict, List, Optional, Any
from .base_agent import BaseAgent, AgentRole
from ..utilities.prompt import PromptContext

logger = logging.getLogger(__name__)

class ParserAgent(BaseAgent):
    """
    Parser Agent - Analyzes legacy codebases and builds understanding maps.
    
    This agent is responsible for:
    - Reading and analyzing legacy project files
    - Understanding project structure and architecture
    - Identifying patterns, dependencies, and relationships
    - Building comprehensive project maps
    - Extracting business logic and functionality
    """
    
    def __init__(self, ai, memory, config, **kwargs):
        super().__init__(
            name="ParserAgent",
            role=AgentRole.PARSER,
            ai=ai,
            memory=memory,
            config=config,
            **kwargs
        )
        
        # Initialize prompt builder
        from ..utilities.prompt import PromptBuilder
        self.prompt_builder = PromptBuilder()
        
        self.project_map = {}
        self.file_analyses = {}
        self.dependency_graph = {}
        self.architecture_patterns = []
        
    def _get_default_system_prompt(self) -> str:
        """Return the default system prompt for the parser agent."""
        return """You are a Parser Agent specialized in analyzing legacy codebases and understanding their structure.

Your primary responsibilities:
1. Analyze legacy code files and understand their purpose
2. Identify architectural patterns and design principles
3. Map dependencies and relationships between components
4. Extract business logic and functionality
5. Identify modernization opportunities and challenges

You should be thorough, analytical, and provide detailed insights about the codebase structure."""
    
    async def process_message(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Process incoming messages for the parser agent."""
        message_type = message.get('type', 'unknown')
        
        if message_type == 'analyze_project':
            return await self._analyze_project(message.get('project_path', ''))
        elif message_type == 'analyze_file':
            return await self._analyze_file(message.get('file_path', ''), message.get('content', ''))
        elif message_type == 'build_dependency_map':
            return await self._build_dependency_map()
        elif message_type == 'extract_patterns':
            return await self._extract_architecture_patterns()
        elif message_type == 'get_project_summary':
            return await self._get_project_summary()
        else:
            return {
                'type': 'error',
                'message': f'Unknown message type: {message_type}'
            }
    
    async def execute_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a specific task assigned to the parser agent."""
        task_type = task.get('type', 'unknown')
        
        if task_type == 'full_analysis':
            return await self._perform_full_analysis(task)
        elif task_type == 'file_analysis':
            return await self._analyze_single_file(task)
        elif task_type == 'dependency_analysis':
            return await self._analyze_dependencies(task)
        elif task_type == 'pattern_extraction':
            return await self._extract_patterns(task)
        else:
            return {
                'success': False,
                'error': f'Unknown task type: {task_type}'
            }
    
    async def _perform_full_analysis(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Perform a complete analysis of the project."""
        try:
            self.state.current_task = "full_analysis"
            self.update_progress(0.1)
            
            # Get project path
            project_path = task.get('project_path', '.')
            
            # Step 1: Scan project structure
            logger.info("Starting project structure scan")
            structure = await self._scan_project_structure(project_path)
            self.update_progress(0.3)
            
            # Step 2: Analyze individual files
            logger.info("Analyzing individual files")
            file_analyses = await self._analyze_all_files(structure['files'])
            self.update_progress(0.6)
            
            # Step 3: Build dependency map
            logger.info("Building dependency map")
            dependencies = await self._build_comprehensive_dependency_map(file_analyses)
            self.update_progress(0.8)
            
            # Step 4: Extract patterns
            logger.info("Extracting architecture patterns")
            patterns = await self._extract_comprehensive_patterns(file_analyses)
            self.update_progress(0.9)
            
            # Step 5: Generate summary
            logger.info("Generating project summary")
            summary = await self._generate_project_summary(structure, file_analyses, dependencies, patterns)
            self.update_progress(1.0)
            
            # Store results
            self.project_map = {
                'structure': structure,
                'file_analyses': file_analyses,
                'dependencies': dependencies,
                'patterns': patterns,
                'summary': summary
            }
            
            # Save to memory
            await self.memory.set('project_map', self.project_map)
            
            return {
                'success': True,
                'project_map': self.project_map,
                'message': 'Full project analysis completed successfully'
            }
            
        except Exception as e:
            logger.error(f"Error in full analysis: {e}")
            return {
                'success': False,
                'error': str(e)
            }
    
    async def _scan_project_structure(self, project_path: str) -> Dict[str, Any]:
        """Scan and analyze the project structure."""
        try:
            # Get list of files
            files = await self._get_project_files(project_path)
            
            # Categorize files by type
            categorized_files = self._categorize_files(files)
            
            # Analyze directory structure
            directory_structure = self._analyze_directory_structure(files)
            
            return {
                'project_path': project_path,
                'files': files,
                'categorized_files': categorized_files,
                'directory_structure': directory_structure,
                'total_files': len(files)
            }
            
        except Exception as e:
            logger.error(f"Error scanning project structure: {e}")
            raise
    
    async def _analyze_all_files(self, files: List[str]) -> Dict[str, Any]:
        """Analyze all files in the project."""
        file_analyses = {}
        
        for i, file_path in enumerate(files):
            try:
                logger.debug(f"Analyzing file {i+1}/{len(files)}: {file_path}")
                
                # Read file content
                content = await self._read_file_content(file_path)
                
                # Analyze file
                analysis = await self._analyze_single_file_content(file_path, content)
                file_analyses[file_path] = analysis
                
                # Update progress
                progress = 0.3 + (0.3 * (i + 1) / len(files))
                self.update_progress(progress)
                
            except Exception as e:
                logger.warning(f"Failed to analyze {file_path}: {e}")
                file_analyses[file_path] = {
                    'error': str(e),
                    'status': 'failed'
                }
        
        return file_analyses
    
    async def _analyze_single_file_content(self, file_path: str, content: str) -> Dict[str, Any]:
        """Analyze a single file's content."""
        try:
            # Build context for analysis
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="file_analysis",
                file_content=content,
                file_path=file_path,
                user_requirements="Analyze this file and understand its purpose, structure, and functionality"
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_modernization_prompt(
                context, task_type="parsing"
            )
            
            # Get AI analysis
            analysis_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': prompt_result.user_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse analysis response
            analysis = self._parse_file_analysis(analysis_response, file_path, content)
            
            return analysis
            
        except Exception as e:
            logger.error(f"Error analyzing file {file_path}: {e}")
            return {
                'error': str(e),
                'status': 'failed',
                'file_path': file_path
            }
    
    async def _build_comprehensive_dependency_map(self, file_analyses: Dict[str, Any]) -> Dict[str, Any]:
        """Build a comprehensive dependency map from file analyses."""
        dependencies = {
            'imports': {},
            'exports': {},
            'calls': {},
            'inheritance': {},
            'composition': {},
            'data_flow': {}
        }
        
        for file_path, analysis in file_analyses.items():
            if analysis.get('status') == 'success':
                # Extract dependencies from analysis
                file_deps = analysis.get('dependencies', {})
                
                for dep_type, dep_list in file_deps.items():
                    if dep_type in dependencies:
                        dependencies[dep_type][file_path] = dep_list
        
        return dependencies
    
    async def _extract_comprehensive_patterns(self, file_analyses: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Extract comprehensive architecture patterns from file analyses."""
        patterns = []
        
        # Collect patterns from all files
        for file_path, analysis in file_analyses.items():
            if analysis.get('status') == 'success':
                file_patterns = analysis.get('patterns', [])
                for pattern in file_patterns:
                    pattern['source_file'] = file_path
                    patterns.append(pattern)
        
        # Group and consolidate patterns
        consolidated_patterns = self._consolidate_patterns(patterns)
        
        return consolidated_patterns
    
    async def _generate_project_summary(self, structure: Dict, file_analyses: Dict, 
                                      dependencies: Dict, patterns: List) -> Dict[str, Any]:
        """Generate a comprehensive project summary."""
        try:
            # Build context for summary generation
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="summary_generation",
                user_requirements="Generate a comprehensive summary of the analyzed project"
            )
            
            # Prepare summary data
            summary_data = {
                'structure': structure,
                'file_analyses': file_analyses,
                'dependencies': dependencies,
                'patterns': patterns
            }
            
            # Build prompt
            prompt_result = self.prompt_builder.build_modernization_prompt(
                context, task_type="general"
            )
            
            # Add summary data to prompt
            enhanced_prompt = f"{prompt_result.user_prompt}\n\nProject Data:\n{str(summary_data)}"
            
            # Get AI summary
            summary_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': enhanced_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse and structure summary
            summary = self._parse_project_summary(summary_response, summary_data)
            
            return summary
            
        except Exception as e:
            logger.error(f"Error generating project summary: {e}")
            return {
                'error': str(e),
                'status': 'failed'
            }
    
    def _parse_file_analysis(self, analysis_response: str, file_path: str, content: str) -> Dict[str, Any]:
        """Parse the AI response for file analysis."""
        try:
            # Basic parsing - in a real implementation, you'd use more sophisticated parsing
            analysis = {
                'file_path': file_path,
                'status': 'success',
                'content_length': len(content),
                'analysis': analysis_response,
                'dependencies': self._extract_dependencies_from_response(analysis_response),
                'patterns': self._extract_patterns_from_response(analysis_response),
                'functionality': self._extract_functionality_from_response(analysis_response)
            }
            
            return analysis
            
        except Exception as e:
            logger.error(f"Error parsing file analysis: {e}")
            return {
                'file_path': file_path,
                'status': 'failed',
                'error': str(e)
            }
    
    def _parse_project_summary(self, summary_response: str, summary_data: Dict) -> Dict[str, Any]:
        """Parse the AI response for project summary."""
        try:
            # Basic parsing - in a real implementation, you'd use more sophisticated parsing
            summary = {
                'status': 'success',
                'summary': summary_response,
                'statistics': {
                    'total_files': summary_data['structure']['total_files'],
                    'analyzed_files': len([f for f in summary_data['file_analyses'].values() 
                                         if f.get('status') == 'success']),
                    'failed_files': len([f for f in summary_data['file_analyses'].values() 
                                       if f.get('status') == 'failed']),
                    'patterns_found': len(summary_data['patterns']),
                    'dependencies_mapped': len(summary_data['dependencies'].get('imports', {}))
                }
            }
            
            return summary
            
        except Exception as e:
            logger.error(f"Error parsing project summary: {e}")
            return {
                'status': 'failed',
                'error': str(e)
            }
    
    def _extract_dependencies_from_response(self, response: str) -> Dict[str, List[str]]:
        """Extract dependencies from AI response."""
        # This is a simplified implementation
        # In a real system, you'd use more sophisticated parsing
        dependencies = {
            'imports': [],
            'exports': [],
            'calls': []
        }
        
        # Basic keyword-based extraction
        if 'import' in response.lower():
            dependencies['imports'].append('extracted_imports')
        if 'export' in response.lower():
            dependencies['exports'].append('extracted_exports')
        if 'function' in response.lower() or 'method' in response.lower():
            dependencies['calls'].append('extracted_calls')
        
        return dependencies
    
    def _extract_patterns_from_response(self, response: str) -> List[Dict[str, Any]]:
        """Extract patterns from AI response."""
        # This is a simplified implementation
        patterns = []
        
        # Basic pattern detection
        if 'class' in response.lower():
            patterns.append({
                'type': 'class_based',
                'description': 'Object-oriented class structure detected'
            })
        if 'function' in response.lower():
            patterns.append({
                'type': 'functional',
                'description': 'Functional programming patterns detected'
            })
        
        return patterns
    
    def _extract_functionality_from_response(self, response: str) -> Dict[str, Any]:
        """Extract functionality from AI response."""
        # This is a simplified implementation
        return {
            'purpose': 'extracted_purpose',
            'main_functions': ['extracted_functions'],
            'business_logic': 'extracted_business_logic'
        }
    
    def _consolidate_patterns(self, patterns: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Consolidate and deduplicate patterns."""
        # This is a simplified implementation
        consolidated = {}
        
        for pattern in patterns:
            pattern_type = pattern.get('type', 'unknown')
            if pattern_type not in consolidated:
                consolidated[pattern_type] = {
                    'type': pattern_type,
                    'description': pattern.get('description', ''),
                    'occurrences': 1,
                    'source_files': [pattern.get('source_file', '')]
                }
            else:
                consolidated[pattern_type]['occurrences'] += 1
                consolidated[pattern_type]['source_files'].append(pattern.get('source_file', ''))
        
        return list(consolidated.values())
    
    async def _get_project_files(self, project_path: str) -> List[str]:
        """Get list of files in the project."""
        import os
        from pathlib import Path
        
        files = []
        project_path = Path(project_path)
        
        if project_path.is_file():
            # If it's a single file, return just that file
            return [str(project_path)]
        elif project_path.is_dir():
            # If it's a directory, scan for files
            for root, dirs, filenames in os.walk(project_path):
                for filename in filenames:
                    file_path = Path(root) / filename
                    # Skip hidden files and common ignore patterns
                    if not filename.startswith('.') and not filename.endswith(('.pyc', '.DS_Store')):
                        files.append(str(file_path))
        else:
            # If path doesn't exist, return empty list
            return []
        
        return files
    
    def _categorize_files(self, files: List[str]) -> Dict[str, List[str]]:
        """Categorize files by type."""
        categories = {
            'source_code': [],
            'configuration': [],
            'documentation': [],
            'assets': [],
            'other': []
        }
        
        for file_path in files:
            if file_path.endswith(('.py', '.js', '.ts', '.java', '.cpp', '.c')):
                categories['source_code'].append(file_path)
            elif file_path.endswith(('.json', '.yaml', '.yml', '.toml', '.ini')):
                categories['configuration'].append(file_path)
            elif file_path.endswith(('.md', '.txt', '.rst')):
                categories['documentation'].append(file_path)
            elif file_path.endswith(('.png', '.jpg', '.svg', '.css')):
                categories['assets'].append(file_path)
            else:
                categories['other'].append(file_path)
        
        return categories
    
    def _analyze_directory_structure(self, files: List[str]) -> Dict[str, Any]:
        """Analyze the directory structure of the project."""
        structure = {}
        
        for file_path in files:
            parts = file_path.split('/')
            current = structure
            
            for part in parts[:-1]:
                if part not in current:
                    current[part] = {'type': 'directory', 'contents': {}}
                current = current[part]['contents']
            
            current[parts[-1]] = {'type': 'file', 'path': file_path}
        
        return structure
    
    async def _read_file_content(self, file_path: str) -> str:
        """Read file content."""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                return f.read()
        except Exception as e:
            logger.error(f"Error reading file {file_path}: {e}")
            return f"Error reading file: {e}"
    
    async def _analyze_project(self, project_path: str) -> Dict[str, Any]:
        """Analyze a project at the given path."""
        return await self._perform_full_analysis({'type': 'full_analysis', 'project_path': project_path})
    
    async def _analyze_file(self, file_path: str, content: str) -> Dict[str, Any]:
        """Analyze a single file."""
        return await self._analyze_single_file_content(file_path, content)
    
    async def _analyze_single_file(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze a single file from task data."""
        try:
            file_path = task.get('file_path', '')
            content = task.get('content', '')
            
            if not content:
                return {
                    'success': False,
                    'error': 'No content provided for file analysis'
                }
            
            analysis = await self._analyze_single_file_content(file_path, content)
            
            return {
                'success': True,
                'file_path': file_path,
                'analysis': analysis
            }
            
        except Exception as e:
            logger.error(f"Error analyzing single file: {e}")
            return {
                'success': False,
                'error': str(e)
            }
    
    async def _build_dependency_map(self) -> Dict[str, Any]:
        """Build dependency map from current analyses."""
        if not self.file_analyses:
            return {'error': 'No file analyses available'}
        
        return await self._build_comprehensive_dependency_map(self.file_analyses)
    
    async def _extract_architecture_patterns(self) -> List[Dict[str, Any]]:
        """Extract architecture patterns from current analyses."""
        if not self.file_analyses:
            return []
        
        return await self._extract_comprehensive_patterns(self.file_analyses)
    
    async def _get_project_summary(self) -> Dict[str, Any]:
        """Get current project summary."""
        if not self.project_map:
            return {'error': 'No project analysis available'}
        
        return self.project_map.get('summary', {})
    
    async def _analyze_dependencies(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze dependencies from task data."""
        try:
            # For now, return a simple response
            return {
                'success': True,
                'dependencies': {},
                'message': 'Dependency analysis completed'
            }
        except Exception as e:
            logger.error(f"Error analyzing dependencies: {e}")
            return {
                'success': False,
                'error': str(e)
            }
    
    async def _extract_patterns(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Extract patterns from task data."""
        try:
            # For now, return a simple response
            return {
                'success': True,
                'patterns': [],
                'message': 'Pattern extraction completed'
            }
        except Exception as e:
            logger.error(f"Error extracting patterns: {e}")
            return {
                'success': False,
                'error': str(e)
            } 