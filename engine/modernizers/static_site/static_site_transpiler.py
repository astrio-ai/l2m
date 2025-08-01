"""
Static Site Transpiler

Converts legacy static websites (HTML, CSS, Bootstrap, jQuery) into modern web applications.
Supports React, Next.js, and Astro with Tailwind CSS.
"""

import os
import json
from pathlib import Path
from typing import Dict, List, Any, Optional

from .parser.html.html_parser import HTMLParser
from .parser.html.html_analyzer import HTMLAnalyzer
from .transformers.bootstrap_rules import BootstrapRules
from .transformers.jquery_rules import JQueryRules
from .templates.react.react_generator import ReactTemplateGenerator
from .templates.astro.astro_generator import AstroTemplateGenerator
from .templates.nextjs.nextjs_generator import NextJSTemplateGenerator


class StaticSiteTranspiler:
    """
    Transpiler for converting legacy static websites to modern frameworks.
    
    Supports:
    - HTML + Bootstrap + jQuery + PHP → React + Tailwind
    - HTML + Bootstrap + jQuery + PHP → Astro + Tailwind  
    - HTML + Bootstrap + jQuery + PHP → Next.js + Tailwind
    """
    
    def __init__(self):
        self.parser = HTMLParser()
        self.analyzer = HTMLAnalyzer()
        self.bootstrap_rules = BootstrapRules()
        self.jquery_rules = JQueryRules()
        self.template_generators = {
            'react': ReactTemplateGenerator(),
            'astro': AstroTemplateGenerator(),
            'nextjs': NextJSTemplateGenerator()
        }
    
    def transpile_website(
        self,
        input_path: str,
        output_dir: str,
        target_framework: str = 'react',
        analyze_only: bool = False
    ) -> Dict[str, Any]:
        """
        Transpile legacy website to modern framework.
        
        Args:
            input_path: Path to HTML file or ZIP archive
            output_dir: Directory to generate modern website
            target_framework: Target framework ('react', 'astro', 'nextjs')
            analyze_only: Only analyze without generating code
            
        Returns:
            Transpilation results
        """
        try:
            # Step 1: Parse the legacy website
            print("🔍 Parsing legacy website...")
            parsed_data = self.parser.parse_input(input_path)
            
            # Step 2: Analyze the website structure
            print("📊 Analyzing website structure...")
            analysis = self.analyzer.analyze_website(parsed_data)
            
            # Step 3: Apply transformation rules
            print("🔄 Applying transformation rules...")
            transformed_data = self._apply_transformation_rules(parsed_data, analysis)
            
            # Step 4: Generate modern project (if not analyze_only)
            generation_results = None
            if not analyze_only:
                print("🚀 Generating modern website...")
                template_generator = self.template_generators.get(target_framework)
                if template_generator:
                    generation_results = template_generator.generate_project(transformed_data, output_dir)
                else:
                    raise ValueError(f"Unsupported target framework: {target_framework}")
            
            return {
                'success': True,
                'parsed_data': parsed_data,
                'analysis': analysis,
                'transformed_data': transformed_data,
                'generation': generation_results,
                'summary': self._generate_summary(parsed_data, analysis, generation_results)
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'error_type': type(e).__name__
            }
    
    def analyze_website(self, input_path: str) -> Dict[str, Any]:
        """
        Analyze a legacy website without generating code.
        
        Args:
            input_path: Path to HTML file or ZIP archive
            
        Returns:
            Analysis results
        """
        try:
            parsed_data = self.parser.parse_input(input_path)
            analysis = self.analyzer.analyze_website(parsed_data)
            
            return {
                'success': True,
                'parsed_data': parsed_data,
                'analysis': analysis,
                'summary': self._generate_summary(parsed_data, analysis, None)
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'error_type': type(e).__name__
            }
    
    def _apply_transformation_rules(self, parsed_data: Dict[str, Any], analysis: Dict[str, Any]) -> Dict[str, Any]:
        """
        Apply transformation rules to convert legacy code to modern equivalents.
        """
        transformed_data = parsed_data.copy()
        
        # Apply Bootstrap to Tailwind transformations
        if analysis.get('bootstrap_detected', False):
            print("  🔄 Converting Bootstrap to Tailwind...")
            bootstrap_transformed = self.bootstrap_rules.transform_bootstrap_to_tailwind(parsed_data)
            transformed_data.update(bootstrap_transformed)
        
        # Apply jQuery to React transformations
        if analysis.get('jquery_detected', False):
            print("  🔄 Converting jQuery to React...")
            jquery_transformed = self.jquery_rules.transform_jquery_to_react(parsed_data)
            transformed_data.update(jquery_transformed)
        
        return transformed_data
    
    def _generate_page_components(self, file_data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Generate React/Astro components from HTML structure.
        """
        components = []
        
        # Extract main sections
        if 'body' in file_data:
            body_content = file_data['body']
            
            # Split into logical sections
            sections = self._split_into_sections(body_content)
            
            for i, section in enumerate(sections):
                component = {
                    'name': f'Section_{i}',
                    'content': section,
                    'type': 'component'
                }
                components.append(component)
        
        return components
    
    def _split_into_sections(self, content: str) -> List[str]:
        """
        Split HTML content into logical sections.
        """
        # Simple section splitting based on div tags
        sections = []
        current_section = ""
        
        lines = content.split('\n')
        for line in lines:
            if '<div' in line and 'class=' in line:
                if current_section:
                    sections.append(current_section.strip())
                current_section = line
            else:
                current_section += line + '\n'
        
        if current_section:
            sections.append(current_section.strip())
        
        return sections if sections else [content]
    
    def _generate_page_content(self, structure: Dict[str, Any]) -> str:
        """
        Generate page content from structure.
        """
        content = []
        
        # Add header
        if 'title' in structure:
            content.append(f"<title>{structure['title']}</title>")
        
        # Add meta tags
        if 'meta' in structure:
            for meta in structure['meta']:
                content.append(f"<meta {meta}>")
        
        # Add CSS
        if 'css' in structure:
            content.append(f"<style>{structure['css']}</style>")
        
        # Add body content
        if 'body' in structure:
            content.append(f"<body>{structure['body']}</body>")
        
        return '\n'.join(content)
    
    def _generate_summary(self, parsed_data: Dict, analysis: Dict, generation: Optional[Dict]) -> Dict[str, Any]:
        """
        Generate a summary of the transpilation process.
        """
        summary = {
            'input_files': len(parsed_data.get('files', [])),
            'bootstrap_detected': analysis.get('bootstrap_detected', False),
            'jquery_detected': analysis.get('jquery_detected', False),
            'php_detected': analysis.get('php_detected', False),
            'total_components': len(analysis.get('components', [])),
            'total_styles': len(analysis.get('styles', [])),
            'total_scripts': len(analysis.get('scripts', []))
        }
        
        if generation:
            summary['output_files'] = len(generation.get('files', []))
            summary['generation_success'] = generation.get('success', False)
        
        return summary
    
    def get_supported_frameworks(self) -> List[str]:
        """
        Get list of supported target frameworks.
        """
        return list(self.template_generators.keys())
    
    def validate_input(self, input_path: str) -> Dict[str, Any]:
        """
        Validate input file/directory.
        """
        try:
            if not os.path.exists(input_path):
                return {
                    'valid': False,
                    'error': f"Input path does not exist: {input_path}"
                }
            
            if os.path.isfile(input_path):
                if not input_path.endswith(('.html', '.htm')):
                    return {
                        'valid': False,
                        'error': f"Input file must be HTML: {input_path}"
                    }
            elif os.path.isdir(input_path):
                html_files = list(Path(input_path).glob('*.html')) + list(Path(input_path).glob('*.htm'))
                if not html_files:
                    return {
                        'valid': False,
                        'error': f"No HTML files found in directory: {input_path}"
                    }
            
            return {'valid': True}
            
        except Exception as e:
            return {
                'valid': False,
                'error': str(e)
            } 