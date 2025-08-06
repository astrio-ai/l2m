"""
COBOL to Lossless Syntax Tree (LST) Parser

This module parses COBOL source code into a lossless syntax tree representation
that preserves all original structure and information.
"""

import os
import sys
from typing import List, Dict, Any, Optional, Tuple
from dataclasses import dataclass, field
from .cobol85.Cobol85Lexer import Cobol85Lexer
from .cobol85.Cobol85Parser import Cobol85Parser
from .cobol85.Cobol85Listener import Cobol85Listener
from antlr4 import CommonTokenStream, InputStream, ParseTreeWalker

@dataclass
class LosslessNode:
    """Lossless Syntax Tree node that preserves all original information."""
    rule_name: str
    text: str = ""
    children: List['LosslessNode'] = field(default_factory=list)
    tokens: List[Any] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def add_child(self, child: 'LosslessNode'):
        """Add a child node."""
        self.children.append(child)
    
    def get_tokens(self) -> List[Any]:
        """Get all tokens in this node and its children."""
        tokens = []
        tokens.extend(self.tokens)
        for child in self.children:
            tokens.extend(child.get_tokens())
        return tokens
    
    def get_text(self) -> str:
        """Get the text content of this node."""
        if self.text:
            return self.text
        return " ".join(token.text for token in self.get_tokens() if hasattr(token, 'text'))
    
    def find_nodes_by_rule(self, rule_name: str) -> List['LosslessNode']:
        """Find all nodes with the specified rule name."""
        nodes = []
        if self.rule_name == rule_name:
            nodes.append(self)
        for child in self.children:
            nodes.extend(child.find_nodes_by_rule(rule_name))
        return nodes
    
    def __repr__(self):
        return f"{self.rule_name}({self.get_text()[:50]}{'...' if len(self.get_text()) > 50 else ''})"

class CobolLSTListener(Cobol85Listener):
    """ANTLR listener that builds a lossless syntax tree."""
    
    def __init__(self):
        self.root: Optional[LosslessNode] = None
        self.current_node: Optional[LosslessNode] = None
        self.node_stack: List[LosslessNode] = []
    
    def enterEveryRule(self, ctx):
        """Called when entering any rule."""
        rule_name = type(ctx).__name__
        
        # Create new node
        node = LosslessNode(rule_name=rule_name)
        
        # Add tokens for this rule
        if hasattr(ctx, 'getTokens'):
            try:
                node.tokens = list(ctx.getTokens())
            except TypeError:
                # getTokens() requires a token type argument
                node.tokens = []
        
        # Set text content
        if hasattr(ctx, 'getText'):
            node.text = ctx.getText()
        
        # Add to tree
        if self.current_node is None:
            self.root = node
        else:
            self.current_node.add_child(node)
        
        # Push to stack
        self.node_stack.append(node)
        self.current_node = node
    
    def exitEveryRule(self, ctx):
        """Called when exiting any rule."""
        if self.node_stack:
            self.node_stack.pop()
            self.current_node = self.node_stack[-1] if self.node_stack else None

class CobolSemanticAnalyzer:
    """Semantic analyzer for COBOL LST."""
    
    def __init__(self, lst_root: LosslessNode, tokens: List[Any]):
        self.lst_root = lst_root
        self.tokens = tokens
        self.symbol_table_root = LosslessNode("symbol_table")
        self.variables: Dict[str, Dict[str, Any]] = {}
        self.paragraphs: List[str] = []
        self.sections: List[str] = []
    
    def analyze(self):
        """Perform semantic analysis on the LST."""
        self._extract_variables()
        self._extract_paragraphs()
        self._extract_sections()
        self._build_symbol_table()
    
    def _extract_variables(self):
        """Extract variable declarations from the LST."""
        data_division_nodes = self.lst_root.find_nodes_by_rule("DataDivisionContext")
        
        for data_div in data_division_nodes:
            working_storage_nodes = data_div.find_nodes_by_rule("WorkingStorageSectionContext")
            
            for ws_section in working_storage_nodes:
                data_desc_nodes = ws_section.find_nodes_by_rule("DataDescriptionEntryContext")
                
                for data_desc in data_desc_nodes:
                    self._process_data_description(data_desc)
    
    def _process_data_description(self, node: LosslessNode):
        """Process a data description entry."""
        tokens = node.get_tokens()
        token_texts = [t.text for t in tokens if hasattr(t, 'text') and t.text.strip()]
        
        if not token_texts:
            return
        
        # Extract level number
        level = None
        name = None
        pic = None
        
        # Find level number (first numeric token)
        for i, token in enumerate(token_texts):
            if token.isdigit() and i == 0:
                level = token
                break
        
        # Find variable name (first non-numeric token after level)
        for i, token in enumerate(token_texts):
            if i > 0 and token and not token.isdigit() and not token.startswith('PIC') and token not in ['(', ')', '.', 'TO', 'FROM', 'GIVING']:
                if not name:
                    name = token
                break
        
        # Find PIC clause
        for i, token in enumerate(token_texts):
            if token == 'PIC' and i + 1 < len(token_texts):
                pic_parts = []
                for j in range(i + 1, len(token_texts)):
                    if token_texts[j] == '.' or token_texts[j] == 'VALUE':
                        break
                    pic_parts.append(token_texts[j])
                pic = ''.join(pic_parts).strip().rstrip('.')
                break
        
        if name and level and name != level and not name.isdigit():
            self.variables[name] = {
                'kind': 'variable',
                'level': level,
                'pic': pic,
                'python_type': self._get_python_type_from_pic(pic) if pic else 'str'
            }
    
    def _get_python_type_from_pic(self, pic: str) -> str:
        """Convert COBOL PIC clause to Python type."""
        if not pic:
            return 'str'
        
        pic = pic.upper()
        
        if '9' in pic and 'V' in pic:
            return 'float'
        elif '9' in pic and 'S' in pic:
            return 'int'
        elif '9' in pic:
            return 'int'
        elif 'X' in pic:
            return 'str'
        elif 'A' in pic:
            return 'str'
        else:
            return 'str'
    
    def _extract_paragraphs(self):
        """Extract paragraph names from the LST."""
        procedure_division_nodes = self.lst_root.find_nodes_by_rule("ProcedureDivisionContext")
        
        for proc_div in procedure_division_nodes:
            paragraph_nodes = proc_div.find_nodes_by_rule("ParagraphContext")
            
            for para_node in paragraph_nodes:
                para_name_nodes = para_node.find_nodes_by_rule("ParagraphNameContext")
                
                for name_node in para_name_nodes:
                    para_name = name_node.get_text().strip()
                    if para_name and para_name not in self.paragraphs:
                        self.paragraphs.append(para_name)
    
    def _extract_sections(self):
        """Extract section names from the LST."""
        procedure_division_nodes = self.lst_root.find_nodes_by_rule("ProcedureDivisionContext")
        
        for proc_div in procedure_division_nodes:
            section_nodes = proc_div.find_nodes_by_rule("SectionContext")
            
            for section_node in section_nodes:
                section_name_nodes = section_node.find_nodes_by_rule("SectionNameContext")
                
                for name_node in section_name_nodes:
                    section_name = name_node.get_text().strip()
                    if section_name and section_name not in self.sections:
                        self.sections.append(section_name)
    
    def _build_symbol_table(self):
        """Build a symbol table from the analysis."""
        # Create root symbol table node
        self.symbol_table_root = LosslessNode("symbol_table")
        
        # Add variables to symbol table
        for var_name, var_info in self.variables.items():
            var_node = LosslessNode(
                rule_name="variable",
                text=var_name,
                metadata=var_info
            )
            self.symbol_table_root.add_child(var_node)
        
        # Add paragraphs to symbol table
        for para_name in self.paragraphs:
            para_node = LosslessNode(
                rule_name="paragraph",
                text=para_name,
                metadata={'kind': 'paragraph'}
            )
            self.symbol_table_root.add_child(para_node)
        
        # Add sections to symbol table
        for section_name in self.sections:
            section_node = LosslessNode(
                rule_name="section",
                text=section_name,
                metadata={'kind': 'section'}
            )
            self.symbol_table_root.add_child(section_node)

def preprocess_cobol_source(cobol_source: str) -> str:
    """
    Preprocess COBOL source to remove comments and clean up formatting.
    
    Args:
        cobol_source: Raw COBOL source code
        
    Returns:
        Cleaned COBOL source code
    """
    lines = cobol_source.split('\n')
    cleaned_lines = []
    
    for line in lines:
        # Remove COBOL comments (lines starting with * in columns 7-72)
        stripped = line.strip()
        if stripped.startswith('*'):
            continue
        
        # Remove inline comments (everything after * in the line)
        if '*' in line:
            # Find the first * that's not in a string literal
            in_string = False
            for i, char in enumerate(line):
                if char == "'" and (i == 0 or line[i-1] != "'"):
                    in_string = not in_string
                elif char == '*' and not in_string:
                    line = line[:i].rstrip()
                    break
        
        # Only add non-empty lines
        if line.strip():
            cleaned_lines.append(line)
    
    return '\n'.join(cleaned_lines)

def parse_cobol_source(cobol_source: str) -> Tuple[LosslessNode, List[Any]]:
    """
    Parse COBOL source code into a lossless syntax tree.
    
    Args:
        cobol_source: COBOL source code as string
        
    Returns:
        Tuple of (LST root node, list of tokens)
    """
    # Preprocess to remove comments
    cleaned_source = preprocess_cobol_source(cobol_source)
    
    # Create input stream
    input_stream = InputStream(cleaned_source)
    
    # Create lexer
    lexer = Cobol85Lexer(input_stream)
    stream = CommonTokenStream(lexer)
    
    # Create parser
    parser = Cobol85Parser(stream)
    
    # Parse the input
    tree = parser.startRule()
    
    # Create listener and walk the tree
    listener = CobolLSTListener()
    walker = ParseTreeWalker()
    walker.walk(listener, tree)
    
    return listener.root, stream.tokens

def parse_cobol_file(file_path: str) -> Tuple[LosslessNode, List[Any]]:
    """
    Parse a COBOL file into a lossless syntax tree.
    
    Args:
        file_path: Path to COBOL file
        
    Returns:
        Tuple of (LST root node, list of tokens)
    """
    with open(file_path, 'r', encoding='utf-8') as f:
        cobol_source = f.read()
    
    return parse_cobol_source(cobol_source) 