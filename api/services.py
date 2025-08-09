"""
Service layer for the Legacy2Modern API

This module provides service functions that wrap the existing CLI functionality
and make it available through the API.
"""

import os
import logging
import tempfile
from pathlib import Path
from typing import Dict, Any, Optional, List
from datetime import datetime

# Add the project root to sys.path
import sys
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from engine.modernizers.cobol_system.transpilers.hybrid_transpiler import HybridTranspiler
from engine.modernizers.cobol_system.transpilers.llm_augmentor import LLMConfig
from engine.modernizers.static_site.transpilers.transpiler import StaticSiteTranspiler
from engine.modernizers.static_site.transpilers.agent import WebsiteAgent
from engine.agents.agent import LLMAgent


class Legacy2ModernService:
    """Service class that wraps the Legacy2Modern functionality."""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.cobol_transpiler = None
        self.website_transpiler = None
        self.llm_agent = None
        self._initialize_components()
    
    def _initialize_components(self):
        """Initialize the transpilation components."""
        try:
            # Initialize with default LLM config
            default_llm_config = LLMConfig.from_env()
            self.cobol_transpiler = HybridTranspiler(default_llm_config)
            self.website_transpiler = StaticSiteTranspiler()
            self.llm_agent = LLMAgent(default_llm_config)
            self.logger.info("All components initialized successfully")
        except Exception as e:
            self.logger.error(f"Failed to initialize components: {e}")
            # Initialize without LLM config if there's an issue
            try:
                self.cobol_transpiler = HybridTranspiler()
                self.website_transpiler = StaticSiteTranspiler()
                self.llm_agent = LLMAgent()
                self.logger.info("Components initialized without LLM config")
            except Exception as e2:
                self.logger.error(f"Failed to initialize components without LLM: {e2}")
                raise
    
    def transpile_cobol(self, source_code: str = None, source_file_path: str = None, 
                       output_file: str = None, llm_config: Dict[str, Any] = None) -> Dict[str, Any]:
        """
        Transpile COBOL code to Python.
        
        Args:
            source_code: COBOL source code as string
            source_file_path: Path to COBOL file
            output_file: Output file path for generated Python code
            llm_config: LLM configuration for AI-assisted translation
            
        Returns:
            Dictionary containing transpilation results
        """
        try:
            # Configure LLM if provided
            if llm_config:
                try:
                    llm_config_obj = LLMConfig(**llm_config)
                    self.cobol_transpiler = HybridTranspiler(llm_config_obj)
                except Exception as e:
                    self.logger.warning(f"Failed to configure LLM, using default: {e}")
            
            # Handle source code input
            if source_code:
                # Create temporary file for source code
                with tempfile.NamedTemporaryFile(mode='w', suffix='.cobol', delete=False) as temp_file:
                    temp_file.write(source_code)
                    temp_file_path = temp_file.name
                
                try:
                    result = self.cobol_transpiler.transpile_file(temp_file_path)
                    
                    # Write output if specified
                    if output_file:
                        os.makedirs(os.path.dirname(output_file), exist_ok=True)
                        with open(output_file, 'w') as f:
                            f.write(result)
                    
                    return {
                        'success': True,
                        'message': 'COBOL transpilation completed successfully',
                        'generated_code': result,
                        'metadata': {
                            'source_type': 'string',
                            'output_file': output_file
                        }
                    }
                finally:
                    # Clean up temporary file
                    os.unlink(temp_file_path)
            
            elif source_file_path:
                if not os.path.exists(source_file_path):
                    return {
                        'success': False,
                        'message': f'Source file not found: {source_file_path}',
                        'errors': [f'File {source_file_path} does not exist']
                    }
                
                result = self.cobol_transpiler.transpile_file(source_file_path)
                
                # Write output if specified
                if output_file:
                    os.makedirs(os.path.dirname(output_file), exist_ok=True)
                    with open(output_file, 'w') as f:
                        f.write(result)
                
                return {
                    'success': True,
                    'message': 'COBOL transpilation completed successfully',
                    'generated_code': result,
                    'metadata': {
                        'source_type': 'file',
                        'source_file': source_file_path,
                        'output_file': output_file
                    }
                }
            else:
                return {
                    'success': False,
                    'message': 'Either source_code or source_file_path must be provided',
                    'errors': ['Missing source input']
                }
                
        except Exception as e:
            self.logger.error(f"COBOL transpilation failed: {e}")
            return {
                'success': False,
                'message': f'COBOL transpilation failed: {str(e)}',
                'errors': [str(e)],
                'error_type': type(e).__name__
            }
    
    def modernize_website(self, input_path: str, output_dir: str, 
                         target_framework: str = 'react', analyze_only: bool = False) -> Dict[str, Any]:
        """
        Modernize a legacy website to a modern framework.
        
        Args:
            input_path: Path to HTML file or ZIP archive
            output_dir: Output directory for modern website
            target_framework: Target framework ('react', 'astro', 'nextjs')
            analyze_only: Only analyze without generating code
            
        Returns:
            Dictionary containing modernization results
        """
        try:
            if not os.path.exists(input_path):
                return {
                    'success': False,
                    'message': f'Input file not found: {input_path}',
                    'errors': [f'File {input_path} does not exist']
                }
            
            # Create output directory if it doesn't exist
            os.makedirs(output_dir, exist_ok=True)
            
            result = self.website_transpiler.transpile_website(
                input_path=input_path,
                output_dir=output_dir,
                target_framework=target_framework,
                analyze_only=analyze_only
            )
            
            if result['success']:
                return {
                    'success': True,
                    'message': f'Website modernization to {target_framework} completed successfully',
                    'analysis': result.get('analysis'),
                    'parsed_data': result.get('parsed_data'),
                    'metadata': {
                        'target_framework': target_framework,
                        'output_directory': output_dir,
                        'analyze_only': analyze_only
                    }
                }
            else:
                return {
                    'success': False,
                    'message': 'Website modernization failed',
                    'errors': [result.get('error', 'Unknown error')],
                    'error_type': result.get('error_type', 'Unknown')
                }
                
        except Exception as e:
            self.logger.error(f"Website modernization failed: {e}")
            return {
                'success': False,
                'message': f'Website modernization failed: {str(e)}',
                'errors': [str(e)],
                'error_type': type(e).__name__
            }
    
    def analyze_website(self, input_path: str) -> Dict[str, Any]:
        """
        Analyze a legacy website without modernizing it.
        
        Args:
            input_path: Path to HTML file or ZIP archive
            
        Returns:
            Dictionary containing analysis results
        """
        try:
            if not os.path.exists(input_path):
                return {
                    'success': False,
                    'message': f'Input file not found: {input_path}',
                    'errors': [f'File {input_path} does not exist']
                }
            
            result = self.website_transpiler.analyze_website(input_path)
            
            if result['success']:
                return {
                    'success': True,
                    'message': 'Website analysis completed successfully',
                    'analysis': result.get('analysis'),
                    'parsed_data': result.get('parsed_data'),
                    'metadata': {
                        'input_path': input_path,
                        'analysis_timestamp': datetime.now().isoformat()
                    }
                }
            else:
                return {
                    'success': False,
                    'message': 'Website analysis failed',
                    'errors': [result.get('error', 'Unknown error')],
                    'error_type': result.get('error_type', 'Unknown')
                }
                
        except Exception as e:
            self.logger.error(f"Website analysis failed: {e}")
            return {
                'success': False,
                'message': f'Website analysis failed: {str(e)}',
                'errors': [str(e)],
                'error_type': type(e).__name__
            }
    
    def analyze_code(self, source_code: str, target_code: str = None) -> Dict[str, Any]:
        """
        Analyze source code and optionally compare with target code.
        
        Args:
            source_code: Source code to analyze
            target_code: Target code for comparison analysis
            
        Returns:
            Dictionary containing analysis results
        """
        try:
            # Basic code analysis
            analysis_result = {
                'source_code_length': len(source_code),
                'source_code_lines': len(source_code.splitlines()),
                'source_code_complexity': self._calculate_complexity(source_code)
            }
            
            if target_code:
                analysis_result.update({
                    'target_code_length': len(target_code),
                    'target_code_lines': len(target_code.splitlines()),
                    'target_code_complexity': self._calculate_complexity(target_code),
                    'code_difference': len(source_code) - len(target_code)
                })
            
            return {
                'success': True,
                'message': 'Code analysis completed successfully',
                'analysis': analysis_result,
                'metadata': {
                    'analysis_timestamp': datetime.now().isoformat()
                }
            }
            
        except Exception as e:
            self.logger.error(f"Code analysis failed: {e}")
            return {
                'success': False,
                'message': f'Code analysis failed: {str(e)}',
                'errors': [str(e)],
                'error_type': type(e).__name__
            }
    
    def _calculate_complexity(self, code: str) -> Dict[str, Any]:
        """Calculate basic code complexity metrics."""
        lines = code.splitlines()
        non_empty_lines = [line for line in lines if line.strip()]
        
        return {
            'total_lines': len(lines),
            'non_empty_lines': len(non_empty_lines),
            'comment_lines': len([line for line in lines if line.strip().startswith(('#', '//', '/*', '*'))]),
            'avg_line_length': sum(len(line) for line in non_empty_lines) / len(non_empty_lines) if non_empty_lines else 0
        }
    
    def get_health_status(self) -> Dict[str, Any]:
        """Get the health status of all components."""
        components = {}
        
        try:
            if self.cobol_transpiler:
                components['cobol_transpiler'] = 'healthy'
            else:
                components['cobol_transpiler'] = 'unhealthy'
        except:
            components['cobol_transpiler'] = 'unhealthy'
        
        try:
            if self.website_transpiler:
                components['website_transpiler'] = 'healthy'
            else:
                components['website_transpiler'] = 'unhealthy'
        except:
            components['website_transpiler'] = 'unhealthy'
        
        try:
            if self.llm_agent:
                components['llm_agent'] = 'healthy'
            else:
                components['llm_agent'] = 'unhealthy'
        except:
            components['llm_agent'] = 'unhealthy'
        
        return {
            'status': 'healthy' if all(status == 'healthy' for status in components.values()) else 'degraded',
            'components': components,
            'timestamp': datetime.now().isoformat()
        } 