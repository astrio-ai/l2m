"""
Hybrid COBOL to Python Transpiler

This module combines rule-based translation with AI assistance
to handle both simple and complex COBOL constructs.
"""

import logging
from typing import Dict, Any, List, Optional
from .transpiler import CobolTranspiler
from .edge_case_detector import EdgeCaseDetector
from .llm_augmentor import LLMAugmentor, LLMConfig


class HybridTranspiler:
    """
    Hybrid transpiler that combines rule-based and AI-assisted translation.
    """
    
    def __init__(self, llm_config: Optional[LLMConfig] = None):
        self.rule_based_transpiler = CobolTranspiler()
        self.edge_case_detector = EdgeCaseDetector()
        self.llm_augmentor = LLMAugmentor(llm_config)
        self.logger = logging.getLogger(__name__)
        
        # Store AI translations for integration
        self.ai_translations: Dict[str, str] = {}
        self.edge_cases: List[Dict[str, Any]] = []
    
    def transpile_file(self, cobol_file_path: str) -> str:
        """
        Transpile a COBOL file using hybrid approach.
        
        Args:
            cobol_file_path: Path to the COBOL file
            
        Returns:
            Generated Python code
        """
        with open(cobol_file_path, 'r') as f:
            cobol_source = f.read()
        
        return self.transpile_source(cobol_source, cobol_file_path)
    
    def transpile_source(self, cobol_source: str, file_path: str = "") -> str:
        """
        Transpile COBOL source using hybrid approach.
        
        Args:
            cobol_source: COBOL source code
            file_path: Path to the source file (for logging)
            
        Returns:
            Generated Python code
        """
        self.logger.info(f"Starting hybrid transpilation for {file_path}")
        
        # Step 1: Detect edge cases
        self.logger.info("Step 1: Detecting edge cases...")
        edge_cases = self.detect_edge_cases(cobol_source)
        
        # Step 2: Apply rule-based translation
        self.logger.info("Step 2: Applying rule-based translation...")
        rule_based_code = self.apply_rule_based_translation(cobol_source, file_path)
        
        # Step 3: Apply AI-assisted translation for complex cases
        self.logger.info("Step 3: Applying AI-assisted translation...")
        ai_translations = self.apply_ai_translation(edge_cases)
        
        # Step 4: Integrate results
        self.logger.info("Step 4: Integrating results...")
        final_code = self.integrate_translations(rule_based_code, ai_translations)
        
        self.logger.info("Hybrid transpilation completed")
        return final_code
    
    def detect_edge_cases(self, cobol_source: str) -> List[Dict[str, Any]]:
        """
        Detect edge cases in COBOL source.
        
        Args:
            cobol_source: COBOL source code
            
        Returns:
            List of detected edge cases
        """
        from ..parsers.cobol_lst import parse_cobol_source
        
        # Parse COBOL into LST
        lst, tokens = parse_cobol_source(cobol_source)
        
        # Detect edge cases
        edge_cases = self.edge_case_detector.detect_edge_cases(lst, "root")
        self.edge_case_detector.log_edge_cases(edge_cases, "source")
        
        # Store edge cases for later use
        self.edge_cases = edge_cases
        
        return edge_cases
    
    def apply_rule_based_translation(self, cobol_source: str, file_path: str) -> str:
        """
        Apply rule-based translation to COBOL source.
        
        Args:
            cobol_source: COBOL source code
            file_path: Path to the source file
            
        Returns:
            Rule-based translated code
        """
        return self.rule_based_transpiler.transpile_source(cobol_source, file_path)
    
    def apply_ai_translation(self, edge_cases: List[Dict[str, Any]]) -> Dict[str, str]:
        """
        Apply AI-assisted translation to complex edge cases.
        
        Args:
            edge_cases: List of edge cases to translate
            
        Returns:
            Dictionary of AI translations
        """
        if not self.llm_augmentor.can_augment():
            self.logger.warning("LLM augmentation not available, skipping AI translation")
            return {}
        
        # Filter for high-priority edge cases
        high_priority_cases = [
            ec for ec in edge_cases 
            if ec.get('severity') == 'high' or ec.get('type') in ['complex_construct', 'unknown_tokens']
        ]
        
        if not high_priority_cases:
            self.logger.info("No high-priority edge cases found for AI translation")
            return {}
        
        self.logger.info(f"Applying AI translation to {len(high_priority_cases)} edge cases")
        
        # Translate edge cases using AI
        translations = self.llm_augmentor.batch_translate_edge_cases(high_priority_cases)
        
        # Store translations for integration
        self.ai_translations.update(translations)
        
        return translations
    
    def integrate_translations(self, rule_based_code: str, ai_translations: Dict[str, str]) -> str:
        """
        Integrate rule-based and AI translations.
        
        Args:
            rule_based_code: Code from rule-based translation
            ai_translations: AI-generated translations
            
        Returns:
            Integrated Python code
        """
        if not ai_translations:
            return rule_based_code
        
        # Generate integration code
        integration_code = self.llm_augmentor.generate_integration_code(ai_translations)
        
        # Insert AI translations before the main function
        lines = rule_based_code.split('\n')
        
        # Find the main function
        main_func_index = -1
        for i, line in enumerate(lines):
            if line.strip() == "def main():":
                main_func_index = i
                break
        
        if main_func_index != -1:
            # Insert AI translations before main function
            integration_lines = integration_code.split('\n')
            lines.insert(main_func_index, '')
            lines.insert(main_func_index, '# AI-Generated Code Snippets')
            lines.insert(main_func_index, '# ========================')
            lines.insert(main_func_index, '')
            
            for line in reversed(integration_lines):
                lines.insert(main_func_index, line)
        else:
            # Append at the end if main function not found
            lines.append('')
            lines.append('# AI-Generated Code Snippets')
            lines.append('# ========================')
            lines.append('')
            lines.extend(integration_code.split('\n'))
        
        return '\n'.join(lines)
    
    def generate_translation_report(self, output_file: str = "translation_report.txt"):
        """
        Generate a comprehensive translation report.
        
        Args:
            output_file: Path to the output report file
        """
        with open(output_file, 'w') as f:
            f.write("COBOL to Python Translation Report\n")
            f.write("=" * 50 + "\n\n")
            
            # Edge case summary
            f.write("Edge Cases Detected:\n")
            f.write("-" * 20 + "\n")
            f.write(f"Total edge cases: {len(self.edge_cases)}\n")
            
            by_type = {}
            by_severity = {}
            
            for ec in self.edge_cases:
                edge_type = ec.get('type', 'unknown')
                severity = ec.get('severity', 'unknown')
                
                by_type[edge_type] = by_type.get(edge_type, 0) + 1
                by_severity[severity] = by_severity.get(severity, 0) + 1
            
            f.write("\nBy Type:\n")
            for edge_type, count in by_type.items():
                f.write(f"  {edge_type}: {count}\n")
            
            f.write("\nBy Severity:\n")
            for severity, count in by_severity.items():
                f.write(f"  {severity}: {count}\n")
            
            # AI translation summary
            f.write("\nAI Translations:\n")
            f.write("-" * 20 + "\n")
            f.write(f"Total AI translations: {len(self.ai_translations)}\n")
            
            for edge_case_id, translated_code in self.ai_translations.items():
                f.write(f"\n{edge_case_id}:\n")
                f.write("-" * 30 + "\n")
                f.write(translated_code)
                f.write("\n")
        
        self.logger.info(f"Translation report generated: {output_file}")
    
    def get_translation_stats(self) -> Dict[str, Any]:
        """
        Get statistics about the translation process.
        
        Returns:
            Dictionary with translation statistics
        """
        return {
            'total_edge_cases': len(self.edge_cases),
            'ai_translations': len(self.ai_translations),
            'llm_available': self.llm_augmentor.can_augment(),
            'edge_case_types': list(set(ec.get('type', 'unknown') for ec in self.edge_cases)),
            'severity_distribution': {
                severity: len([ec for ec in self.edge_cases if ec.get('severity') == severity])
                for severity in set(ec.get('severity', 'unknown') for ec in self.edge_cases)
            }
        } 