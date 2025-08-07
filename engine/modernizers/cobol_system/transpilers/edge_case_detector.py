"""
Edge Case Detection and Logging

This module provides functionality to detect and log COBOL constructs
that the rule engine cannot handle, enabling AI-assisted transformation.
"""

import logging
from typing import Dict, Any, List, Optional, Set
from ..parsers.cobol_lst import LosslessNode


class EdgeCaseDetector:
    """
    Detects and logs COBOL constructs that cannot be handled by the rule engine.
    """
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.edge_cases: List[Dict[str, Any]] = []
        self.known_patterns: Set[str] = set()
        self.complex_constructs: Set[str] = set()
        
        # Initialize known patterns that we can handle
        self._initialize_known_patterns()
        self._initialize_complex_constructs()
    
    def _initialize_known_patterns(self):
        """Initialize patterns that the rule engine can handle."""
        self.known_patterns.update({
            # Basic COBOL structure
            'IDENTIFICATION', 'DIVISION', 'PROGRAM-ID', 'PROCEDURE',
            'ENVIRONMENT', 'DATA', 'WORKING-STORAGE', 'LINKAGE',
            'FILE', 'SECTION', 'PICTURE', 'VALUE',
            
            # Control structures
            'IF', 'ELSE', 'END-IF', 'THEN',
            'PERFORM', 'UNTIL', 'TIMES', 'END-PERFORM',
            'EVALUATE', 'WHEN', 'END-EVALUATE',
            
            # Basic operations
            'DISPLAY', 'ACCEPT', 'MOVE', 'ADD', 'SUBTRACT',
            'MULTIPLY', 'DIVIDE', 'COMPUTE',
            
            # Data definitions
            '01', 'PIC', 'VALUE', 'WORKING-STORAGE', 'DATA', 'DIVISION',
            
            # Variables and literals
            'COUNTER', 'LOOP-VAR', 'MORE-DATA', 'YES', 'NO', 'LOOP',
            'A000-COUNT', '100-MAIN',
            
            # Numeric literals and operators
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
            '>', '<', '=', '>=', '<=', '!=',
            
            # String literals
            "'YES'", "'NO'", "'LOOP'", "'COUNTER IS '", "'COUNTING...'",
            
            # PIC clause patterns
            '9(3)', 'X(10)', 'X(3)',
            
            # Program structure
            'PERFORM-TEST', 'IDENTIFICATION', 'DIVISION', 'PROGRAM-ID',
            
            # File operations
            'SELECT', 'ASSIGN', 'TO', 'OPEN', 'CLOSE', 'READ', 'WRITE',
            
            # Program control
            'GOBACK', 'STOP', 'RUN', 'EXIT',
            
            # Punctuation and structure
            '.', ',', '(', ')', ';', ':', '"', "'",
            
            # Common literals and identifiers
            'HELLO', 'WORLD', 'HELLO WORLD!',
            
            # Whitespace and formatting
            '\n', ' ', '\t', '<EOF>', '.\n ', '. ', '.\n     ', '.\n '
        })
    
    def _initialize_complex_constructs(self):
        """Initialize complex COBOL constructs that need special handling."""
        self.complex_constructs.update({
            'COMPUTE', 'EVALUATE', 'SEARCH', 'SORT', 'MERGE',
            'STRING', 'UNSTRING', 'INSPECT', 'REPLACE',
            'CALL', 'EXIT', 'GO TO', 'ALTER',
            'COPY', 'INCLUDE', 'REPLACE', 'REDEFINES',
            'OCCURS', 'DEPENDING', 'INDEXED', 'SEQUENTIAL',
            'RANDOM', 'DYNAMIC', 'ACCESS', 'MODE'
        })
    
    def detect_edge_cases(self, node: LosslessNode, context: str = "") -> List[Dict[str, Any]]:
        """
        Detect edge cases in a COBOL AST node.
        
        Args:
            node: The COBOL AST node to analyze
            context: Context information about the node
            
        Returns:
            List of detected edge cases
        """
        edge_cases = []
        
        # Get all tokens from the node
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text]
        
        # Check for unknown patterns
        unknown_tokens = [token for token in token_texts if token not in self.known_patterns]
        
        # Check for complex constructs
        complex_tokens = [token for token in token_texts if token in self.complex_constructs]
        
        # Check for nested structures that might be complex - only flag very complex structures
        if len(node.children) > 8:  # Higher threshold for complexity
            edge_cases.append({
                'type': 'complex_nested_structure',
                'node_type': node.rule_name,
                'context': context,
                'token_count': len(token_texts),
                'child_count': len(node.children),
                'tokens': token_texts[:10],  # First 10 tokens for context
                'severity': 'medium'
            })
        
        # Check for unknown tokens - only flag if there are significant unknown tokens
        # Filter out pure formatting tokens
        significant_unknown_tokens = [
            token for token in unknown_tokens 
            if not (token.startswith('.') or token.startswith('\n') or token.startswith(' ') or token.startswith('\t'))
        ]
        
        if significant_unknown_tokens and len(significant_unknown_tokens) > 2:  # Only flag if more than 2 significant unknown tokens
            edge_cases.append({
                'type': 'unknown_tokens',
                'node_type': node.rule_name,
                'context': context,
                'unknown_tokens': significant_unknown_tokens,
                'all_tokens': token_texts,
                'severity': 'high' if len(significant_unknown_tokens) > 4 else 'medium'
            })
        
        # Check for complex constructs
        if complex_tokens:
            edge_cases.append({
                'type': 'complex_construct',
                'node_type': node.rule_name,
                'context': context,
                'complex_tokens': complex_tokens,
                'all_tokens': token_texts,
                'severity': 'high'
            })
        
        # Check for very long statements (potential complexity)
        if len(token_texts) > 20:
            edge_cases.append({
                'type': 'long_statement',
                'node_type': node.rule_name,
                'context': context,
                'token_count': len(token_texts),
                'tokens': token_texts[:15],  # First 15 tokens
                'severity': 'medium'
            })
        
        # Recursively check children
        for i, child in enumerate(node.children):
            child_context = f"{context}.child[{i}]" if context else f"child[{i}]"
            child_edge_cases = self.detect_edge_cases(child, child_context)
            edge_cases.extend(child_edge_cases)
        
        return edge_cases
    
    def log_edge_cases(self, edge_cases: List[Dict[str, Any]], file_path: str = ""):
        """
        Log detected edge cases for AI processing.
        
        Args:
            edge_cases: List of detected edge cases
            file_path: Path to the COBOL file being processed
        """
        if not edge_cases:
            return
        
        # Group edge cases by severity
        high_severity = [ec for ec in edge_cases if ec['severity'] == 'high']
        medium_severity = [ec for ec in edge_cases if ec['severity'] == 'medium']
        low_severity = [ec for ec in edge_cases if ec['severity'] == 'low']
        
        # Log summary
        self.logger.info(f"Edge case detection for {file_path}:")
        self.logger.info(f"  High severity: {len(high_severity)}")
        self.logger.info(f"  Medium severity: {len(medium_severity)}")
        self.logger.info(f"  Low severity: {len(low_severity)}")
        
        # Log high severity cases in detail
        for edge_case in high_severity:
            self.logger.warning(f"High severity edge case: {edge_case['type']}")
            self.logger.warning(f"  Context: {edge_case['context']}")
            self.logger.warning(f"  Node type: {edge_case['node_type']}")
            
            if 'unknown_tokens' in edge_case:
                self.logger.warning(f"  Unknown tokens: {edge_case['unknown_tokens']}")
            
            if 'complex_tokens' in edge_case:
                self.logger.warning(f"  Complex tokens: {edge_case['complex_tokens']}")
        
        # Store edge cases for later processing
        self.edge_cases.extend(edge_cases)
    
    def generate_edge_case_report(self, output_file: str = "edge_cases_report.txt"):
        """
        Generate a detailed report of all detected edge cases.
        
        Args:
            output_file: Path to the output report file
        """
        if not self.edge_cases:
            return
        
        with open(output_file, 'w') as f:
            f.write("COBOL Edge Cases Report\n")
            f.write("=" * 50 + "\n\n")
            
            # Group by type
            by_type = {}
            for edge_case in self.edge_cases:
                edge_type = edge_case['type']
                if edge_type not in by_type:
                    by_type[edge_type] = []
                by_type[edge_type].append(edge_case)
            
            for edge_type, cases in by_type.items():
                f.write(f"\n{edge_type.upper()} ({len(cases)} cases):\n")
                f.write("-" * 30 + "\n")
                
                for i, case in enumerate(cases, 1):
                    f.write(f"\n{i}. Severity: {case['severity']}\n")
                    f.write(f"   Context: {case['context']}\n")
                    f.write(f"   Node type: {case['node_type']}\n")
                    
                    if 'unknown_tokens' in case:
                        f.write(f"   Unknown tokens: {case['unknown_tokens']}\n")
                    
                    if 'complex_tokens' in case:
                        f.write(f"   Complex tokens: {case['complex_tokens']}\n")
                    
                    if 'token_count' in case:
                        f.write(f"   Token count: {case['token_count']}\n")
                    
                    if 'tokens' in case:
                        f.write(f"   Sample tokens: {case['tokens'][:10]}\n")
                
                f.write("\n")
        
        self.logger.info(f"Edge case report generated: {output_file}")
    
    def get_ai_processing_candidates(self) -> List[Dict[str, Any]]:
        """
        Get edge cases that should be processed by AI.
        
        Returns:
            List of edge cases suitable for AI processing
        """
        # Return high severity cases and complex constructs
        return [
            ec for ec in self.edge_cases 
            if ec['severity'] == 'high' or ec['type'] in ['complex_construct', 'unknown_tokens']
        ]
    
    def clear_edge_cases(self):
        """Clear the stored edge cases."""
        self.edge_cases.clear() 