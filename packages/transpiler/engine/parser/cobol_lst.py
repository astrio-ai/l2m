"""
COBOL Lossless Semantic Tree (LST) Parser and Analyzer

This module provides functionality to parse COBOL source code into a lossless semantic tree (LST)
and perform semantic analysis, using the ANTLR-generated parser for COBOL85.
"""

from antlr4 import InputStream, CommonTokenStream
from ...grammars.cobol85.Cobol85Lexer import Cobol85Lexer
from ...grammars.cobol85.Cobol85Parser import Cobol85Parser
from typing import Optional, List, Tuple, Any, Dict
from rich.tree import Tree
from rich.console import Console

class LosslessNode:
    """
    Node in the Lossless Semantic Tree (LST).
    Retains all tokens and parse tree structure.
    """
    def __init__(self, rule_name: str, children: Optional[List['LosslessNode']] = None, token=None, start=None, stop=None):
        self.rule_name = rule_name
        self.children = children or []
        self.token = token  # For leaf nodes
        self.start = start  # Start token index
        self.stop = stop    # Stop token index

    def __repr__(self):
        if self.token:
            return f"Token({getattr(self.token, 'text', repr(self.token))!r})"
        return f"{self.rule_name}({self.children})"

    # --- LST Navigation Utilities ---
    def find_all(self, rule_name: str) -> List['LosslessNode']:
        """Recursively find all nodes with the given rule name."""
        found = []
        if self.rule_name == rule_name:
            found.append(self)
        for child in self.children:
            found.extend(child.find_all(rule_name))
        return found

    def find_first(self, rule_name: str) -> Optional['LosslessNode']:
        """Find the first node with the given rule name."""
        if self.rule_name == rule_name:
            return self
        for child in self.children:
            result = child.find_first(rule_name)
            if result:
                return result
        return None

    def walk(self, func):
        """Apply func(self) and recursively to all children."""
        func(self)
        for child in self.children:
            child.walk(func)

    def get_tokens(self) -> List[Any]:
        """Return all tokens in this subtree (including whitespace/comments if present)."""
        tokens = []
        if self.token:
            tokens.append(self.token)
        for child in self.children:
            tokens.extend(child.get_tokens())
        return tokens

def build_lossless_tree(ctx, tokens: CommonTokenStream) -> LosslessNode:
    """
    Recursively build a lossless tree from the ANTLR parse tree.
    Retains all tokens, including whitespace and comments if present in the token stream.
    """
    if ctx.getChildCount() == 0:
        # Leaf node: get the token
        token = ctx.getPayload()
        return LosslessNode(rule_name=type(ctx).__name__, token=token)
    else:
        children = [build_lossless_tree(child, tokens) for child in ctx.getChildren()]
        return LosslessNode(rule_name=type(ctx).__name__, children=children, start=getattr(ctx, 'start', None), stop=getattr(ctx, 'stop', None))

def preprocess_cobol_source(source_code: str) -> str:
    """
    Preprocess COBOL source code for ANTLR parsing:
    - Remove comment lines (col 7 is '*')
    - Remove debug lines (col 7 is 'D' or 'd')
    - Strip sequence numbers (cols 1-6)
    - Truncate lines to 72 columns
    - Keep blank/short lines as-is
    """
    processed_lines = []
    for line in source_code.splitlines():
        if len(line) >= 7:
            if line[6] == '*' or line[6] in ('D', 'd'):
                continue  # Skip comment or debug line
            # Strip sequence numbers (cols 1-6), keep cols 7-72
            code = line[6:72]
            processed_lines.append(code.rstrip())
        else:
            # Keep short/blank lines as-is
            processed_lines.append(line.rstrip())
    return '\n'.join(processed_lines)

def parse_cobol_source(source_code: str) -> Tuple[LosslessNode, CommonTokenStream]:
    """
    Parse COBOL source code into a Lossless Semantic Tree (LST).
    Returns the root node and the token stream.
    """
    source_code = preprocess_cobol_source(source_code)
    input_stream = InputStream(source_code)
    lexer = Cobol85Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Cobol85Parser(token_stream)
    tree = parser.startRule()  # Entry point as per Cobol85.g4
    lossless_tree = build_lossless_tree(tree, token_stream)
    return lossless_tree, token_stream

# --- Semantic Analysis ---
class SymbolTableNode:
    """
    Node in the hierarchical symbol table.
    """
    def __init__(self, name, kind, metadata=None, parent=None):
        self.name = name
        self.kind = kind  # e.g., variable, constant, section, paragraph, procedure, file, program, group, screen, report, comm
        self.metadata = metadata or {}
        self.parent = parent  # SymbolTableNode or None
        self.children = []  # List[SymbolTableNode]

    def add_child(self, child: 'SymbolTableNode'):
        self.children.append(child)
        child.parent = self

    def __repr__(self):
        return f"SymbolTableNode(name={self.name!r}, kind={self.kind!r}, metadata={self.metadata}, children={len(self.children)})"

    def pretty(self, indent=0):
        s = '  ' * indent + f"{self.kind}: {self.name} {self.metadata}\n"
        for child in self.children:
            s += child.pretty(indent + 1)
        return s

    def find_all_kind(self, kind: str):
        """Return all nodes of a given kind in the subtree."""
        found = []
        if self.kind == kind:
            found.append(self)
        for child in self.children:
            found.extend(child.find_all_kind(kind))
        return found

    def find_by_name(self, name: str):
        """Return all nodes with the given name in the subtree."""
        found = []
        if self.name == name:
            found.append(self)
        for child in self.children:
            found.extend(child.find_by_name(name))
        return found

class CobolSemanticAnalyzer:
    """
    Performs semantic analysis on the COBOL LST.
    Now builds a hierarchical symbol table with metadata and scope.
    """
    def __init__(self, lst_root: LosslessNode, token_stream: CommonTokenStream):
        self.lst_root = lst_root
        self.token_stream = token_stream
        self.symbol_table_root = SymbolTableNode(name="<root>", kind="root")
        self.type_info: Dict[str, str] = {}
        self.control_flow_graph = None  # Placeholder
        self.data_flow_graph = None     # Placeholder

    def analyze(self):
        """
        Perform all semantic analysis passes.
        """
        self.build_symbol_table()
        self.resolve_types()
        self.analyze_control_flow()
        self.analyze_data_flow()

    def build_symbol_table(self):
        """
        Walk the LST and collect all symbols, building a hierarchical symbol table.
        Now extracts more metadata (level, type, PIC/USAGE, etc.) and supports more symbol types.
        """
        def get_token_info(node: LosslessNode):
            tokens = node.get_tokens()
            if tokens:
                tok = tokens[0]
                return {
                    "line": getattr(tok, 'line', None),
                    "column": getattr(tok, 'column', None),
                    "text": getattr(tok, 'text', None)
                }
            return {}

        def extract_name(node: LosslessNode):
            # Try to get the name from the first token
            tokens = node.get_tokens()
            if tokens:
                name = getattr(tokens[0], 'text', None)
                if isinstance(name, str) and name:
                    return name
            return None

        def extract_level(node: LosslessNode):
            # Look for a child with rule_name 'LEVEL_NUMBER_XX' or 'INTEGERLITERAL'
            for child in node.children:
                if child.rule_name in ("LEVEL_NUMBER_66Context", "LEVEL_NUMBER_77Context", "LEVEL_NUMBER_88Context", "IntegerLiteralContext"):
                    tokens = child.get_tokens()
                    if tokens:
                        return getattr(tokens[0], 'text', None)
            return None

        def extract_pic_usage(node: LosslessNode):
            # Look for children with rule_name 'DataPictureClauseContext' or 'DataUsageClauseContext'
            pic = None
            usage = None
            for child in node.children:
                if child.rule_name == "DataPictureClauseContext":
                    tokens = child.get_tokens()
                    if tokens:
                        pic = getattr(tokens[0], 'text', None)
                if child.rule_name == "DataUsageClauseContext":
                    tokens = child.get_tokens()
                    if tokens:
                        usage = getattr(tokens[0], 'text', None)
            return pic, usage

        def walk(node: LosslessNode, parent_sym: SymbolTableNode):
            # Extended mapping for more COBOL symbol types
            rule_to_kind = {
                "DataDescriptionEntryContext": "variable",
                "DataDescriptionEntryFormat1Context": "variable",
                "DataDescriptionEntryFormat2Context": "group",
                "DataDescriptionEntryFormat3Context": "constant",
                "FileDescriptionEntryContext": "file",
                "ParagraphContext": "paragraph",
                "ProcedureSectionContext": "section",
                "ProgramUnitContext": "program",
                "ProcedureNameContext": "procedure",
                "SectionNameContext": "section",
                "FileNameContext": "file",
                "ProgramNameContext": "program",
                "ScreenDescriptionEntryContext": "screen",
                "ReportDescriptionEntryContext": "report",
                "ReportGroupDescriptionEntryContext": "report_group",
                "CommunicationDescriptionEntryContext": "comm",
                "LibraryDescriptionEntryContext": "library",
                # Add more as needed
            }
            kind = rule_to_kind.get(node.rule_name)
            name = extract_name(node)
            metadata = get_token_info(node)
            # Add more metadata: level, type, PIC/USAGE, parent info
            if kind in ("variable", "group", "constant"):
                metadata["level"] = extract_level(node)
                pic, usage = extract_pic_usage(node)
                if pic:
                    metadata["pic"] = pic
                if usage:
                    metadata["usage"] = usage
            # For all, add parent kind/name for context
            if parent_sym and parent_sym.kind != "root":
                metadata["parent_kind"] = parent_sym.kind
                metadata["parent_name"] = parent_sym.name
            if kind and name:
                sym = SymbolTableNode(name=name, kind=kind, metadata=metadata)
                parent_sym.add_child(sym)
                for child in node.children:
                    walk(child, sym)
            else:
                for child in node.children:
                    walk(child, parent_sym)

        walk(self.lst_root, self.symbol_table_root)

    def resolve_types(self):
        """
        Walk the LST and resolve types for variables and literals.
        """
        def collect_types(node: LosslessNode):
            # Example: collect type info for dataName nodes
            if node.rule_name == "DataNameContext":
                tokens = node.get_tokens()
                if tokens:
                    name = getattr(tokens[0], 'text', None)
                    if isinstance(name, str) and name:
                        # Placeholder: in real code, extract type from context
                        self.type_info[name] = "UNKNOWN_TYPE"
        self.lst_root.walk(collect_types)

    def analyze_control_flow(self):
        """
        Stub for control flow analysis (e.g., build a control flow graph).
        """
        # TODO: Implement control flow analysis
        self.control_flow_graph = None

    def analyze_data_flow(self):
        """
        Stub for data flow analysis (e.g., build a data flow graph).
        """
        # TODO: Implement data flow analysis
        self.data_flow_graph = None

def symbol_table_to_rich_tree(node, rich_tree=None):
    label = f"[bold]{node.kind}[/bold]: {node.name} [dim]{node.metadata}[/dim]"
    if rich_tree is None:
        rich_tree = Tree(label)
    else:
        rich_tree = rich_tree.add(label)
    for child in node.children:
        symbol_table_to_rich_tree(child, rich_tree)
    return rich_tree

def symbol_table_to_dot(node, dot=None, parent_id=None, node_id=0):
    """
    Recursively output the symbol table as a DOT graph.
    """
    if dot is None:
        dot = ["digraph SymbolTable {", "  node [shape=box, fontname=monospace];"]
    this_id = f"n{node_id}"
    label = f"{node.kind}: {node.name}\\n{node.metadata}"
    dot.append(f'  {this_id} [label="{label}"];')
    if parent_id is not None:
        dot.append(f"  {parent_id} -> {this_id};")
    next_id = node_id + 1
    for child in node.children:
        dot, next_id = symbol_table_to_dot(child, dot, this_id, next_id)
    if parent_id is None:
        dot.append("}")
    return dot, next_id

# Example usage
if __name__ == "__main__":
    import sys
    if len(sys.argv) < 2:
        print("Usage: python -m packages.transpiler.engine.cobol_lst <source_file.cob>")
        sys.exit(1)
    with open(sys.argv[1], "r") as f:
        source = f.read()
    lst, tokens = parse_cobol_source(source)
    print(lst)
    analyzer = CobolSemanticAnalyzer(lst, tokens)
    analyzer.analyze()
    console = Console()
    tree = symbol_table_to_rich_tree(analyzer.symbol_table_root)
    console.print(tree)
    print("Symbol Table (hierarchical):\n", analyzer.symbol_table_root.pretty())
    print("Type Info:", analyzer.type_info)
    print("Semantic analysis complete.")
    # Output DOT file for Graphviz
    dot_lines, _ = symbol_table_to_dot(analyzer.symbol_table_root)
    with open("symbol_table.dot", "w") as f:
        f.write("\n".join(dot_lines))
    print("DOT file written to symbol_table.dot")