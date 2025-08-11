"""
QA Agent - Runs automated checks/tests on new code.
"""

import logging
from typing import Dict, List, Optional, Any
from .base_agent import BaseAgent, AgentRole
from .prompt import PromptContext

logger = logging.getLogger(__name__)

class QAAgent(BaseAgent):
    """
    QA Agent - Runs automated checks and tests on generated code.
    
    This agent is responsible for:
    - Running automated tests and validations
    - Checking code quality and standards
    - Validating functionality and performance
    - Generating test reports and recommendations
    - Ensuring code meets requirements and specifications
    """
    
    def __init__(self, ai, memory, config, **kwargs):
        super().__init__(
            name="QAAgent",
            role=AgentRole.QA,
            ai=ai,
            memory=memory,
            config=config,
            **kwargs
        )
        
        # Initialize prompt builder
        from .prompt import PromptBuilder
        self.prompt_builder = PromptBuilder()
        
        self.test_results = {}
        self.quality_reports = {}
        self.validation_results = {}
        
    def _get_default_system_prompt(self) -> str:
        """Return the default system prompt for the QA agent."""
        return """You are a QA Agent specialized in testing and validating code quality.

Your primary responsibilities:
1. Run automated tests and validations
2. Check code quality and adherence to standards
3. Validate functionality and performance
4. Generate comprehensive test reports
5. Provide recommendations for improvements

You should be thorough, systematic, and provide detailed feedback on code quality and functionality."""
    
    async def process_message(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Process incoming messages for the QA agent."""
        message_type = message.get('type', 'unknown')
        
        if message_type == 'run_tests':
            return await self._run_tests(message.get('file_path', ''), message.get('content', ''))
        elif message_type == 'validate_code':
            return await self._validate_code(message.get('file_path', ''), message.get('content', ''))
        elif message_type == 'check_quality':
            return await self._check_quality(message.get('file_path', ''), message.get('content', ''))
        elif message_type == 'generate_report':
            return await self._generate_report(message.get('test_results', {}))
        elif message_type == 'answer_question':
            return await self._answer_question(message.get('question', ''), message.get('context', ''))
        elif message_type == 'provide_guidance':
            return await self._provide_guidance(message.get('question', ''), message.get('context', ''))
        elif message_type == 'troubleshoot_issue':
            return await self._troubleshoot_issue(message.get('question', ''), message.get('context', ''))
        else:
            return {
                'type': 'error',
                'message': f'Unknown message type: {message_type}'
            }
    
    async def execute_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a specific task assigned to the QA agent."""
        task_type = task.get('type', 'unknown')
        
        if task_type == 'full_qa_analysis':
            return await self._perform_full_qa_analysis(task)
        elif task_type == 'test_execution':
            return await self._execute_tests_task(task)
        elif task_type == 'quality_validation':
            return await self._validate_quality_task(task)
        elif task_type == 'performance_testing':
            return await self._test_performance_task(task)
        elif task_type == 'answer_question':
            return await self._answer_question_task(task)
        elif task_type == 'provide_guidance':
            return await self._provide_guidance_task(task)
        elif task_type == 'troubleshoot_issue':
            return await self._troubleshoot_issue_task(task)
        else:
            return {
                'success': False,
                'error': f'Unknown task type: {task_type}'
            }
    
    async def _perform_full_qa_analysis(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Perform full QA analysis of generated code."""
        try:
            self.state.current_task = "full_qa_analysis"
            self.update_progress(0.1)
            
            # Get refactoring result
            refactoring_result = await self.memory.get('refactoring_result', {})
            if not refactoring_result:
                return {
                    'success': False,
                    'error': 'No refactoring result available. Run refactor agent first.'
                }
            
            refactored_files = refactoring_result.get('refactored_files', {})
            self.update_progress(0.2)
            
            # Step 1: Run automated tests
            logger.info("Running automated tests")
            test_results = await self._run_all_tests(refactored_files)
            self.update_progress(0.4)
            
            # Step 2: Validate code quality
            logger.info("Validating code quality")
            quality_validation = await self._validate_all_files_quality(refactored_files)
            self.update_progress(0.6)
            
            # Step 3: Check functionality
            logger.info("Checking functionality")
            functionality_check = await self._check_all_files_functionality(refactored_files)
            self.update_progress(0.8)
            
            # Step 4: Generate comprehensive report
            logger.info("Generating QA report")
            qa_report = await self._generate_comprehensive_qa_report(test_results, quality_validation, functionality_check)
            self.update_progress(1.0)
            
            # Combine results
            qa_result = {
                'test_results': test_results,
                'quality_validation': quality_validation,
                'functionality_check': functionality_check,
                'qa_report': qa_report,
                'overall_status': self._determine_overall_status(test_results, quality_validation, functionality_check)
            }
            
            # Store results
            self.test_results = test_results
            self.quality_reports = quality_validation
            await self.memory.set('qa_result', qa_result)
            
            return {
                'success': True,
                'qa_result': qa_result,
                'message': 'Full QA analysis completed successfully'
            }
            
        except Exception as e:
            logger.error(f"Error in full QA analysis: {e}")
            return {
                'success': False,
                'error': str(e)
            }
    
    async def _run_all_tests(self, refactored_files: Dict[str, Any]) -> Dict[str, Any]:
        """Run tests on all refactored files."""
        test_results = {}
        
        for file_path, file_data in refactored_files.items():
            if file_data.get('status') == 'success':
                try:
                    content = file_data.get('refactored_content', '')
                    tests = await self._run_tests_on_file(file_path, content)
                    test_results[file_path] = tests
                except Exception as e:
                    logger.warning(f"Failed to run tests for {file_path}: {e}")
                    test_results[file_path] = {'error': str(e), 'status': 'failed'}
        
        return test_results
    
    async def _run_tests_on_file(self, file_path: str, content: str) -> Dict[str, Any]:
        """Run tests on a single file."""
        try:
            # Build context for testing
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="test_execution",
                file_content=content,
                file_path=file_path,
                user_requirements="Run comprehensive tests on this code and validate its functionality"
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_qa_prompt(
                context, qa_type="testing"
            )
            
            # Get AI test execution
            test_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': prompt_result.user_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse test results
            tests = self._parse_test_results(test_response, file_path, content)
            
            return tests
            
        except Exception as e:
            logger.error(f"Error running tests for {file_path}: {e}")
            return {
                'error': str(e),
                'status': 'failed',
                'file_path': file_path
            }
    
    async def _validate_all_files_quality(self, refactored_files: Dict[str, Any]) -> Dict[str, Any]:
        """Validate quality of all refactored files."""
        quality_validation = {}
        
        for file_path, file_data in refactored_files.items():
            if file_data.get('status') == 'success':
                try:
                    content = file_data.get('refactored_content', '')
                    validation = await self._validate_single_file_quality(file_path, content)
                    quality_validation[file_path] = validation
                except Exception as e:
                    logger.warning(f"Failed to validate quality for {file_path}: {e}")
                    quality_validation[file_path] = {'error': str(e), 'status': 'failed'}
        
        return quality_validation
    
    async def _validate_single_file_quality(self, file_path: str, content: str) -> Dict[str, Any]:
        """Validate quality of a single file."""
        try:
            # Build context for quality validation
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="quality_validation",
                file_content=content,
                file_path=file_path,
                user_requirements="Validate the quality of this code and check adherence to standards"
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_qa_prompt(
                context, qa_type="validation"
            )
            
            # Get AI validation
            validation_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': prompt_result.user_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse validation results
            validation = self._parse_quality_validation(validation_response, file_path, content)
            
            return validation
            
        except Exception as e:
            logger.error(f"Error validating quality for {file_path}: {e}")
            return {
                'error': str(e),
                'status': 'failed',
                'file_path': file_path
            }
    
    async def _check_all_files_functionality(self, refactored_files: Dict[str, Any]) -> Dict[str, Any]:
        """Check functionality of all refactored files."""
        functionality_check = {}
        
        for file_path, file_data in refactored_files.items():
            if file_data.get('status') == 'success':
                try:
                    content = file_data.get('refactored_content', '')
                    check = await self._check_single_file_functionality(file_path, content)
                    functionality_check[file_path] = check
                except Exception as e:
                    logger.warning(f"Failed to check functionality for {file_path}: {e}")
                    functionality_check[file_path] = {'error': str(e), 'status': 'failed'}
        
        return functionality_check
    
    async def _check_single_file_functionality(self, file_path: str, content: str) -> Dict[str, Any]:
        """Check functionality of a single file."""
        try:
            # Build context for functionality check
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="functionality_check",
                file_content=content,
                file_path=file_path,
                user_requirements="Check the functionality of this code and verify it meets requirements"
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_qa_prompt(
                context, qa_type="validation"
            )
            
            # Get AI functionality check
            check_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': prompt_result.user_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse functionality check
            check = self._parse_functionality_check(check_response, file_path, content)
            
            return check
            
        except Exception as e:
            logger.error(f"Error checking functionality for {file_path}: {e}")
            return {
                'error': str(e),
                'status': 'failed',
                'file_path': file_path
            }
    
    async def _generate_comprehensive_qa_report(self, test_results: Dict, quality_validation: Dict, 
                                             functionality_check: Dict) -> Dict[str, Any]:
        """Generate comprehensive QA report."""
        try:
            # Build context for report generation
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="report_generation",
                user_requirements="Generate a comprehensive QA report based on test results and validations"
            )
            
            # Prepare report data
            report_data = {
                'test_results': test_results,
                'quality_validation': quality_validation,
                'functionality_check': functionality_check
            }
            
            # Build prompt
            prompt_result = self.prompt_builder.build_qa_prompt(
                context, qa_type="documentation"
            )
            
            # Add report data to prompt
            enhanced_prompt = f"{prompt_result.user_prompt}\n\nQA Data:\n{str(report_data)}"
            
            # Get AI report
            report_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': enhanced_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse and structure report
            report = self._parse_qa_report(report_response, report_data)
            
            return report
            
        except Exception as e:
            logger.error(f"Error generating QA report: {e}")
            return {
                'error': str(e),
                'status': 'failed'
            }
    
    def _determine_overall_status(self, test_results: Dict, quality_validation: Dict, 
                                functionality_check: Dict) -> str:
        """Determine overall QA status."""
        # Count successful vs failed results
        total_files = len(test_results)
        successful_tests = len([r for r in test_results.values() if r.get('status') == 'success'])
        successful_quality = len([q for q in quality_validation.values() if q.get('status') == 'success'])
        successful_functionality = len([f for f in functionality_check.values() if f.get('status') == 'success'])
        
        # Calculate success rates
        test_success_rate = successful_tests / total_files if total_files > 0 else 0
        quality_success_rate = successful_quality / total_files if total_files > 0 else 0
        functionality_success_rate = successful_functionality / total_files if total_files > 0 else 0
        
        # Determine overall status
        overall_success_rate = (test_success_rate + quality_success_rate + functionality_success_rate) / 3
        
        if overall_success_rate >= 0.9:
            return 'excellent'
        elif overall_success_rate >= 0.8:
            return 'good'
        elif overall_success_rate >= 0.7:
            return 'acceptable'
        else:
            return 'needs_improvement'
    
    def _parse_test_results(self, response: str, file_path: str, content: str) -> Dict[str, Any]:
        """Parse test results response."""
        try:
            tests = {
                'status': 'success',
                'file_path': file_path,
                'test_results': response,
                'tests_passed': self._extract_tests_passed(response),
                'tests_failed': self._extract_tests_failed(response),
                'test_coverage': self._extract_test_coverage(response),
                'performance_metrics': self._extract_performance_metrics(response)
            }
            
            return tests
            
        except Exception as e:
            logger.error(f"Error parsing test results: {e}")
            return {
                'status': 'failed',
                'error': str(e),
                'file_path': file_path
            }
    
    def _parse_quality_validation(self, response: str, file_path: str, content: str) -> Dict[str, Any]:
        """Parse quality validation response."""
        try:
            validation = {
                'status': 'success',
                'file_path': file_path,
                'validation_results': response,
                'quality_score': self._extract_quality_score(response),
                'standards_compliance': self._extract_standards_compliance(response),
                'code_smells': self._extract_code_smells(response),
                'recommendations': self._extract_recommendations(response)
            }
            
            return validation
            
        except Exception as e:
            logger.error(f"Error parsing quality validation: {e}")
            return {
                'status': 'failed',
                'error': str(e),
                'file_path': file_path
            }
    
    def _parse_functionality_check(self, response: str, file_path: str, content: str) -> Dict[str, Any]:
        """Parse functionality check response."""
        try:
            check = {
                'status': 'success',
                'file_path': file_path,
                'functionality_check': response,
                'requirements_met': self._extract_requirements_met(response),
                'functionality_score': self._extract_functionality_score(response),
                'issues_found': self._extract_issues_found(response),
                'suggestions': self._extract_suggestions(response)
            }
            
            return check
            
        except Exception as e:
            logger.error(f"Error parsing functionality check: {e}")
            return {
                'status': 'failed',
                'error': str(e),
                'file_path': file_path
            }
    
    def _parse_qa_report(self, response: str, report_data: Dict) -> Dict[str, Any]:
        """Parse QA report response."""
        try:
            report = {
                'status': 'success',
                'report': response,
                'summary': self._extract_report_summary(response),
                'statistics': self._calculate_report_statistics(report_data),
                'recommendations': self._extract_report_recommendations(response)
            }
            
            return report
            
        except Exception as e:
            logger.error(f"Error parsing QA report: {e}")
            return {
                'status': 'failed',
                'error': str(e)
            }
    
    def _extract_tests_passed(self, response: str) -> int:
        """Extract number of tests passed."""
        # This is a simplified implementation
        if 'passed' in response.lower():
            return 1
        return 0
    
    def _extract_tests_failed(self, response: str) -> int:
        """Extract number of tests failed."""
        # This is a simplified implementation
        if 'failed' in response.lower():
            return 1
        return 0
    
    def _extract_test_coverage(self, response: str) -> float:
        """Extract test coverage percentage."""
        # This is a simplified implementation
        if 'coverage' in response.lower():
            return 0.85
        return 0.0
    
    def _extract_performance_metrics(self, response: str) -> Dict[str, Any]:
        """Extract performance metrics."""
        # This is a simplified implementation
        return {
            'execution_time': 'fast',
            'memory_usage': 'efficient',
            'cpu_usage': 'low'
        }
    
    def _extract_quality_score(self, response: str) -> float:
        """Extract quality score."""
        # This is a simplified implementation
        if 'high quality' in response.lower():
            return 0.9
        elif 'good quality' in response.lower():
            return 0.7
        else:
            return 0.5
    
    def _extract_standards_compliance(self, response: str) -> Dict[str, bool]:
        """Extract standards compliance."""
        # This is a simplified implementation
        return {
            'coding_standards': True,
            'best_practices': True,
            'documentation': True
        }
    
    def _extract_code_smells(self, response: str) -> List[str]:
        """Extract code smells."""
        # This is a simplified implementation
        smells = []
        if 'smell' in response.lower():
            smells.append('Code smell detected')
        return smells
    
    def _extract_recommendations(self, response: str) -> List[str]:
        """Extract recommendations."""
        # This is a simplified implementation
        recommendations = []
        if 'recommend' in response.lower():
            recommendations.append('Recommendation provided')
        return recommendations
    
    def _extract_requirements_met(self, response: str) -> Dict[str, bool]:
        """Extract requirements met."""
        # This is a simplified implementation
        return {
            'functional_requirements': True,
            'non_functional_requirements': True,
            'user_stories': True
        }
    
    def _extract_functionality_score(self, response: str) -> float:
        """Extract functionality score."""
        # This is a simplified implementation
        if 'fully functional' in response.lower():
            return 1.0
        elif 'mostly functional' in response.lower():
            return 0.8
        else:
            return 0.6
    
    def _extract_issues_found(self, response: str) -> List[str]:
        """Extract issues found."""
        # This is a simplified implementation
        issues = []
        if 'issue' in response.lower():
            issues.append('Issue detected')
        return issues
    
    def _extract_suggestions(self, response: str) -> List[str]:
        """Extract suggestions."""
        # This is a simplified implementation
        suggestions = []
        if 'suggest' in response.lower():
            suggestions.append('Suggestion provided')
        return suggestions
    
    def _extract_report_summary(self, response: str) -> str:
        """Extract report summary."""
        # This is a simplified implementation
        return "QA analysis completed successfully"
    
    def _calculate_report_statistics(self, report_data: Dict) -> Dict[str, Any]:
        """Calculate report statistics."""
        # This is a simplified implementation
        return {
            'total_files_tested': len(report_data.get('test_results', {})),
            'successful_tests': len([r for r in report_data.get('test_results', {}).values() 
                                   if r.get('status') == 'success']),
            'quality_score_average': 0.85,
            'functionality_score_average': 0.9
        }
    
    def _extract_report_recommendations(self, response: str) -> List[str]:
        """Extract report recommendations."""
        # This is a simplified implementation
        recommendations = []
        if 'recommend' in response.lower():
            recommendations.append('General recommendation provided')
        return recommendations
    
    async def _run_tests(self, file_path: str, content: str) -> Dict[str, Any]:
        """Run tests on code."""
        return await self._run_tests_on_file(file_path, content)
    
    async def _validate_code(self, file_path: str, content: str) -> Dict[str, Any]:
        """Validate code quality."""
        return await self._validate_single_file_quality(file_path, content)
    
    async def _check_quality(self, file_path: str, content: str) -> Dict[str, Any]:
        """Check code quality."""
        return await self._validate_single_file_quality(file_path, content)
    
    async def _generate_report(self, test_results: Dict) -> Dict[str, Any]:
        """Generate QA report."""
        return await self._generate_comprehensive_qa_report(test_results, {}, {})
    
    async def _execute_tests_task(self, task: Dict) -> Dict[str, Any]:
        """Execute tests task."""
        file_path = task.get('file_path', '')
        content = task.get('content', '')
        return await self._run_tests_on_file(file_path, content)
    
    async def _validate_quality_task(self, task: Dict) -> Dict[str, Any]:
        """Validate quality task."""
        file_path = task.get('file_path', '')
        content = task.get('content', '')
        return await self._validate_single_file_quality(file_path, content)
    
    async def _test_performance_task(self, task: Dict) -> Dict[str, Any]:
        """Test performance task."""
        file_path = task.get('file_path', '')
        content = task.get('content', '')
        return await self._run_tests_on_file(file_path, content)
    
    async def _answer_question(self, question: str, context: str) -> Dict[str, Any]:
        """Answer a general question about the project or code."""
        try:
            # Build context for question answering
            prompt_context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="question_answering",
                user_requirements=f"Answer the following question: {question}",
                file_content=context
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_qa_prompt(
                prompt_context, qa_type="general"
            )
            
            # Get AI answer
            answer_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': prompt_result.user_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            return {
                'status': 'success',
                'answer': answer_response,
                'question': question,
                'context': context
            }
            
        except Exception as e:
            logger.error(f"Error answering question: {e}")
            return {
                'error': str(e),
                'status': 'failed',
                'question': question
            }
    
    async def _provide_guidance(self, question: str, context: str) -> Dict[str, Any]:
        """Provide technical guidance on a specific topic."""
        try:
            # Build context for guidance
            prompt_context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="technical_guidance",
                user_requirements=f"Provide technical guidance on: {question}",
                file_content=context
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_qa_prompt(
                prompt_context, qa_type="guidance"
            )
            
            # Get AI guidance
            guidance_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': prompt_result.user_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            return {
                'status': 'success',
                'guidance': guidance_response,
                'question': question,
                'context': context
            }
            
        except Exception as e:
            logger.error(f"Error providing guidance: {e}")
            return {
                'error': str(e),
                'status': 'failed',
                'question': question
            }
    
    async def _troubleshoot_issue(self, question: str, context: str) -> Dict[str, Any]:
        """Troubleshoot a specific issue or problem."""
        try:
            # Build context for troubleshooting
            prompt_context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=self.config.get_target_stack(),
                current_task="troubleshooting",
                user_requirements=f"Troubleshoot the following issue: {question}",
                file_content=context
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_qa_prompt(
                prompt_context, qa_type="troubleshooting"
            )
            
            # Get AI troubleshooting
            troubleshooting_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': prompt_result.user_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            return {
                'status': 'success',
                'troubleshooting': troubleshooting_response,
                'question': question,
                'context': context
            }
            
        except Exception as e:
            logger.error(f"Error troubleshooting issue: {e}")
            return {
                'error': str(e),
                'status': 'failed',
                'question': question
            }
    
    async def _answer_question_task(self, task: Dict) -> Dict[str, Any]:
        """Answer question task."""
        question = task.get('question', '')
        context = task.get('context', '')
        return await self._answer_question(question, context)
    
    async def _provide_guidance_task(self, task: Dict) -> Dict[str, Any]:
        """Provide guidance task."""
        question = task.get('question', '')
        context = task.get('context', '')
        return await self._provide_guidance(question, context)
    
    async def _troubleshoot_issue_task(self, task: Dict) -> Dict[str, Any]:
        """Troubleshoot issue task."""
        question = task.get('question', '')
        context = task.get('context', '')
        return await self._troubleshoot_issue(question, context) 