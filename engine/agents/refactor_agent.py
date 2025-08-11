"""
Refactor Agent - Improves generated code for maintainability & performance.
"""

import logging
from typing import Dict, List, Optional, Any
from .base_agent import BaseAgent, AgentRole
from .prompt import PromptContext

logger = logging.getLogger(__name__)

class RefactorAgent(BaseAgent):
    """
    Refactor Agent - Improves and optimizes generated code.
    
    This agent is responsible for:
    - Improving code quality and maintainability
    - Optimizing performance and efficiency
    - Applying refactoring patterns and best practices
    - Reducing code complexity and technical debt
    - Ensuring code follows modern standards
    """
    
    def __init__(self, ai, memory, config, **kwargs):
        super().__init__(
            name="RefactorAgent",
            role=AgentRole.REFACTOR,
            ai=ai,
            memory=memory,
            config=config,
            **kwargs
        )
        
        # Initialize prompt builder
        from .prompt import PromptBuilder
        self.prompt_builder = PromptBuilder()
        
        self.refactored_files = {}
        self.optimization_suggestions = {}
        self.quality_metrics = {}
        
    def _get_default_system_prompt(self) -> str:
        """Return the default system prompt for the refactor agent."""
        return """You are a Refactor Agent specialized in improving and optimizing code quality.

Your primary responsibilities:
1. Improve code maintainability and readability
2. Optimize performance and efficiency
3. Apply refactoring patterns and best practices
4. Reduce code complexity and technical debt
5. Ensure code follows modern standards and conventions

You should focus on making code cleaner, more efficient, and easier to maintain while preserving functionality."""
    
    async def process_message(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Process incoming messages for the refactor agent."""
        message_type = message.get('type', 'unknown')
        
        if message_type == 'refactor_code':
            return await self._refactor_code(message.get('file_path', ''), message.get('content', ''))
        elif message_type == 'optimize_performance':
            return await self._optimize_performance(message.get('file_path', ''), message.get('content', ''))
        elif message_type == 'improve_maintainability':
            return await self._improve_maintainability(message.get('file_path', ''), message.get('content', ''))
        elif message_type == 'analyze_quality':
            return await self._analyze_code_quality(message.get('file_path', ''), message.get('content', ''))
        else:
            return {
                'type': 'error',
                'message': f'Unknown message type: {message_type}'
            }
    
    async def execute_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a specific task assigned to the refactor agent."""
        task_type = task.get('type', 'unknown')
        
        if task_type == 'full_refactoring':
            return await self._perform_full_refactoring(task)
        elif task_type == 'performance_optimization':
            return await self._optimize_performance_task(task)
        elif task_type == 'maintainability_improvement':
            return await self._improve_maintainability_task(task)
        elif task_type == 'quality_analysis':
            return await self._analyze_quality_task(task)
        else:
            return {
                'success': False,
                'error': f'Unknown task type: {task_type}'
            }
    
    async def _perform_full_refactoring(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Perform full refactoring of generated code."""
        try:
            self.state.current_task = "full_refactoring"
            self.update_progress(0.1)
            
            # Get modernization result
            modernization_result = await self.memory.get('modernization_result', {})
            if not modernization_result:
                return {
                    'success': False,
                    'error': 'No modernization result available. Run modernizer agent first.'
                }
            
            converted_files = modernization_result.get('converted_files', {})
            self.update_progress(0.2)
            
            # Step 1: Analyze code quality
            logger.info("Analyzing code quality")
            quality_analysis = await self._analyze_all_files_quality(converted_files)
            self.update_progress(0.4)
            
            # Step 2: Refactor for maintainability
            logger.info("Improving maintainability")
            maintainability_improvements = await self._improve_all_files_maintainability(converted_files)
            self.update_progress(0.6)
            
            # Step 3: Optimize performance
            logger.info("Optimizing performance")
            performance_optimizations = await self._optimize_all_files_performance(converted_files)
            self.update_progress(0.8)
            
            # Step 4: Apply best practices
            logger.info("Applying best practices")
            best_practices = await self._apply_best_practices(converted_files)
            self.update_progress(1.0)
            
            # Combine results
            refactoring_result = {
                'quality_analysis': quality_analysis,
                'maintainability_improvements': maintainability_improvements,
                'performance_optimizations': performance_optimizations,
                'best_practices': best_practices,
                'refactored_files': {}
            }
            
            # Merge all improvements into final refactored files
            for file_path in converted_files.keys():
                refactored_content = await self._merge_refactoring_improvements(
                    file_path, maintainability_improvements, performance_optimizations, best_practices
                )
                refactoring_result['refactored_files'][file_path] = refactored_content
            
            # Store results
            self.refactored_files = refactoring_result['refactored_files']
            await self.memory.set('refactoring_result', refactoring_result)
            
            return {
                'success': True,
                'refactoring_result': refactoring_result,
                'message': 'Full refactoring completed successfully'
            }
            
        except Exception as e:
            logger.error(f"Error in full refactoring: {e}")
            return {
                'success': False,
                'error': str(e)
            }
    
    async def _analyze_all_files_quality(self, converted_files: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze quality of all converted files."""
        quality_analysis = {}
        
        for file_path, file_data in converted_files.items():
            if file_data.get('status') == 'success':
                try:
                    content = file_data.get('converted_content', '')
                    analysis = await self._analyze_single_file_quality(file_path, content)
                    quality_analysis[file_path] = analysis
                except Exception as e:
                    logger.warning(f"Failed to analyze quality for {file_path}: {e}")
                    quality_analysis[file_path] = {'error': str(e), 'status': 'failed'}
        
        return quality_analysis
    
    async def _analyze_single_file_quality(self, file_path: str, content: str) -> Dict[str, Any]:
        """Analyze quality of a single file."""
        try:
            # Build context for quality analysis
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="quality_analysis",
                file_content=content,
                file_path=file_path,
                user_requirements="Analyze the quality of this code and identify areas for improvement"
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_refactoring_prompt(
                context, refactoring_type="quality_analysis"
            )
            
            # Get AI analysis
            analysis_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': prompt_result.user_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse analysis
            analysis = self._parse_quality_analysis(analysis_response, file_path, content)
            
            return analysis
            
        except Exception as e:
            logger.error(f"Error analyzing quality for {file_path}: {e}")
            return {
                'error': str(e),
                'status': 'failed',
                'file_path': file_path
            }
    
    async def _improve_all_files_maintainability(self, converted_files: Dict[str, Any]) -> Dict[str, Any]:
        """Improve maintainability of all converted files."""
        maintainability_improvements = {}
        
        for file_path, file_data in converted_files.items():
            if file_data.get('status') == 'success':
                try:
                    content = file_data.get('converted_content', '')
                    improvement = await self._improve_single_file_maintainability(file_path, content)
                    maintainability_improvements[file_path] = improvement
                except Exception as e:
                    logger.warning(f"Failed to improve maintainability for {file_path}: {e}")
                    maintainability_improvements[file_path] = {'error': str(e), 'status': 'failed'}
        
        return maintainability_improvements
    
    async def _improve_single_file_maintainability(self, file_path: str, content: str) -> Dict[str, Any]:
        """Improve maintainability of a single file."""
        try:
            # Build context for maintainability improvement
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="maintainability_improvement",
                file_content=content,
                file_path=file_path,
                user_requirements="Improve the maintainability of this code by applying best practices"
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_refactoring_prompt(
                context, refactoring_type="maintainability"
            )
            
            # Get AI improvement
            improvement_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': prompt_result.user_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse improvement
            improvement = self._parse_maintainability_improvement(improvement_response, file_path, content)
            
            return improvement
            
        except Exception as e:
            logger.error(f"Error improving maintainability for {file_path}: {e}")
            return {
                'error': str(e),
                'status': 'failed',
                'file_path': file_path
            }
    
    async def _optimize_all_files_performance(self, converted_files: Dict[str, Any]) -> Dict[str, Any]:
        """Optimize performance of all converted files."""
        performance_optimizations = {}
        
        for file_path, file_data in converted_files.items():
            if file_data.get('status') == 'success':
                try:
                    content = file_data.get('converted_content', '')
                    optimization = await self._optimize_single_file_performance(file_path, content)
                    performance_optimizations[file_path] = optimization
                except Exception as e:
                    logger.warning(f"Failed to optimize performance for {file_path}: {e}")
                    performance_optimizations[file_path] = {'error': str(e), 'status': 'failed'}
        
        return performance_optimizations
    
    async def _optimize_single_file_performance(self, file_path: str, content: str) -> Dict[str, Any]:
        """Optimize performance of a single file."""
        try:
            # Build context for performance optimization
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="performance_optimization",
                file_content=content,
                file_path=file_path,
                user_requirements="Optimize the performance of this code while maintaining functionality"
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_refactoring_prompt(
                context, refactoring_type="performance"
            )
            
            # Get AI optimization
            optimization_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': prompt_result.user_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse optimization
            optimization = self._parse_performance_optimization(optimization_response, file_path, content)
            
            return optimization
            
        except Exception as e:
            logger.error(f"Error optimizing performance for {file_path}: {e}")
            return {
                'error': str(e),
                'status': 'failed',
                'file_path': file_path
            }
    
    async def _apply_best_practices(self, converted_files: Dict[str, Any]) -> Dict[str, Any]:
        """Apply best practices to all converted files."""
        best_practices = {}
        
        for file_path, file_data in converted_files.items():
            if file_data.get('status') == 'success':
                try:
                    content = file_data.get('converted_content', '')
                    practices = await self._apply_best_practices_to_file(file_path, content)
                    best_practices[file_path] = practices
                except Exception as e:
                    logger.warning(f"Failed to apply best practices for {file_path}: {e}")
                    best_practices[file_path] = {'error': str(e), 'status': 'failed'}
        
        return best_practices
    
    async def _apply_best_practices_to_file(self, file_path: str, content: str) -> Dict[str, Any]:
        """Apply best practices to a single file."""
        try:
            # Build context for best practices
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="best_practices",
                file_content=content,
                file_path=file_path,
                user_requirements="Apply modern best practices to this code"
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_refactoring_prompt(
                context, refactoring_type="best_practices"
            )
            
            # Get AI best practices application
            practices_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': prompt_result.user_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse best practices
            practices = self._parse_best_practices(practices_response, file_path, content)
            
            return practices
            
        except Exception as e:
            logger.error(f"Error applying best practices for {file_path}: {e}")
            return {
                'error': str(e),
                'status': 'failed',
                'file_path': file_path
            }
    
    async def _merge_refactoring_improvements(self, file_path: str, maintainability: Dict, 
                                           performance: Dict, practices: Dict) -> Dict[str, Any]:
        """Merge all refactoring improvements for a file."""
        try:
            # Get original content
            modernization_result = await self.memory.get('modernization_result', {})
            converted_files = modernization_result.get('converted_files', {})
            original_content = converted_files.get(file_path, {}).get('converted_content', '')
            
            # Start with original content
            final_content = original_content
            
            # Apply maintainability improvements
            if file_path in maintainability and maintainability[file_path].get('status') == 'success':
                final_content = maintainability[file_path].get('improved_content', final_content)
            
            # Apply performance optimizations
            if file_path in performance and performance[file_path].get('status') == 'success':
                final_content = performance[file_path].get('optimized_content', final_content)
            
            # Apply best practices
            if file_path in practices and practices[file_path].get('status') == 'success':
                final_content = practices[file_path].get('practices_content', final_content)
            
            return {
                'status': 'success',
                'file_path': file_path,
                'original_content': original_content,
                'refactored_content': final_content,
                'improvements_applied': {
                    'maintainability': file_path in maintainability and maintainability[file_path].get('status') == 'success',
                    'performance': file_path in performance and performance[file_path].get('status') == 'success',
                    'best_practices': file_path in practices and practices[file_path].get('status') == 'success'
                }
            }
            
        except Exception as e:
            logger.error(f"Error merging refactoring improvements for {file_path}: {e}")
            return {
                'status': 'failed',
                'error': str(e),
                'file_path': file_path
            }
    
    def _parse_quality_analysis(self, response: str, file_path: str, content: str) -> Dict[str, Any]:
        """Parse quality analysis response."""
        try:
            analysis = {
                'status': 'success',
                'file_path': file_path,
                'analysis': response,
                'quality_score': self._extract_quality_score(response),
                'issues_found': self._extract_issues_from_response(response),
                'suggestions': self._extract_suggestions_from_response(response)
            }
            
            return analysis
            
        except Exception as e:
            logger.error(f"Error parsing quality analysis: {e}")
            return {
                'status': 'failed',
                'error': str(e),
                'file_path': file_path
            }
    
    def _parse_maintainability_improvement(self, response: str, file_path: str, content: str) -> Dict[str, Any]:
        """Parse maintainability improvement response."""
        try:
            improvement = {
                'status': 'success',
                'file_path': file_path,
                'improvement': response,
                'improved_content': self._extract_code_from_response(response),
                'changes_made': self._extract_changes_from_response(response)
            }
            
            return improvement
            
        except Exception as e:
            logger.error(f"Error parsing maintainability improvement: {e}")
            return {
                'status': 'failed',
                'error': str(e),
                'file_path': file_path
            }
    
    def _parse_performance_optimization(self, response: str, file_path: str, content: str) -> Dict[str, Any]:
        """Parse performance optimization response."""
        try:
            optimization = {
                'status': 'success',
                'file_path': file_path,
                'optimization': response,
                'optimized_content': self._extract_code_from_response(response),
                'performance_improvements': self._extract_performance_improvements(response)
            }
            
            return optimization
            
        except Exception as e:
            logger.error(f"Error parsing performance optimization: {e}")
            return {
                'status': 'failed',
                'error': str(e),
                'file_path': file_path
            }
    
    def _parse_best_practices(self, response: str, file_path: str, content: str) -> Dict[str, Any]:
        """Parse best practices response."""
        try:
            practices = {
                'status': 'success',
                'file_path': file_path,
                'practices': response,
                'practices_content': self._extract_code_from_response(response),
                'practices_applied': self._extract_practices_applied(response)
            }
            
            return practices
            
        except Exception as e:
            logger.error(f"Error parsing best practices: {e}")
            return {
                'status': 'failed',
                'error': str(e),
                'file_path': file_path
            }
    
    def _extract_quality_score(self, response: str) -> float:
        """Extract quality score from response."""
        # This is a simplified implementation
        # In a real system, you'd use more sophisticated parsing
        if 'high quality' in response.lower():
            return 0.9
        elif 'good quality' in response.lower():
            return 0.7
        elif 'medium quality' in response.lower():
            return 0.5
        else:
            return 0.3
    
    def _extract_issues_from_response(self, response: str) -> List[str]:
        """Extract issues from response."""
        # This is a simplified implementation
        issues = []
        
        # Basic extraction logic
        if 'issue' in response.lower():
            issues.append('Issues detected')
        if 'problem' in response.lower():
            issues.append('Problems identified')
        
        return issues
    
    def _extract_suggestions_from_response(self, response: str) -> List[str]:
        """Extract suggestions from response."""
        # This is a simplified implementation
        suggestions = []
        
        # Basic extraction logic
        if 'suggest' in response.lower():
            suggestions.append('Suggestions provided')
        if 'recommend' in response.lower():
            suggestions.append('Recommendations made')
        
        return suggestions
    
    def _extract_code_from_response(self, response: str) -> str:
        """Extract code from response."""
        # This is a simplified implementation
        # In a real system, you'd use more sophisticated parsing
        return response
    
    def _extract_changes_from_response(self, response: str) -> List[str]:
        """Extract changes from response."""
        # This is a simplified implementation
        changes = []
        
        # Basic extraction logic
        if 'refactor' in response.lower():
            changes.append('Code refactored')
        if 'improve' in response.lower():
            changes.append('Code improved')
        
        return changes
    
    def _extract_performance_improvements(self, response: str) -> List[str]:
        """Extract performance improvements from response."""
        # This is a simplified implementation
        improvements = []
        
        # Basic extraction logic
        if 'optimize' in response.lower():
            improvements.append('Performance optimized')
        if 'faster' in response.lower():
            improvements.append('Speed improved')
        
        return improvements
    
    def _extract_practices_applied(self, response: str) -> List[str]:
        """Extract practices applied from response."""
        # This is a simplified implementation
        practices = []
        
        # Basic extraction logic
        if 'best practice' in response.lower():
            practices.append('Best practices applied')
        if 'modern' in response.lower():
            practices.append('Modern patterns applied')
        
        return practices
    
    async def _refactor_code(self, file_path: str, content: str) -> Dict[str, Any]:
        """Refactor code for general improvements."""
        return await self._improve_single_file_maintainability(file_path, content)
    
    async def _optimize_performance(self, file_path: str, content: str) -> Dict[str, Any]:
        """Optimize performance of code."""
        return await self._optimize_single_file_performance(file_path, content)
    
    async def _improve_maintainability(self, file_path: str, content: str) -> Dict[str, Any]:
        """Improve maintainability of code."""
        return await self._improve_single_file_maintainability(file_path, content)
    
    async def _analyze_code_quality(self, file_path: str, content: str) -> Dict[str, Any]:
        """Analyze code quality."""
        return await self._analyze_single_file_quality(file_path, content)
    
    async def _optimize_performance_task(self, task: Dict) -> Dict[str, Any]:
        """Optimize performance task."""
        file_path = task.get('file_path', '')
        content = task.get('content', '')
        return await self._optimize_single_file_performance(file_path, content)
    
    async def _improve_maintainability_task(self, task: Dict) -> Dict[str, Any]:
        """Improve maintainability task."""
        file_path = task.get('file_path', '')
        content = task.get('content', '')
        return await self._improve_single_file_maintainability(file_path, content)
    
    async def _analyze_quality_task(self, task: Dict) -> Dict[str, Any]:
        """Analyze quality task."""
        file_path = task.get('file_path', '')
        content = task.get('content', '')
        return await self._analyze_single_file_quality(file_path, content) 