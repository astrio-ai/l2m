"""
Language-Agnostic Intermediate Representation (IR)

This module defines a simple, minimal, and uniform IR that can represent
programs from various source languages before converting them to target languages.
"""

from typing import List, Dict, Any, Optional, Union
from dataclasses import dataclass, field
from enum import Enum

class IRType(Enum):
    """IR node types"""
    PROGRAM = "program"
    FUNCTION = "function"
    VARIABLE = "variable"
    ASSIGNMENT = "assignment"
    EXPRESSION = "expression"
    STATEMENT = "statement"
    LOOP = "loop"
    CONDITIONAL = "conditional"
    CALL = "call"
    RETURN = "return"
    INPUT = "input"
    OUTPUT = "output"
    OPERATION = "operation"
    LITERAL = "literal"
    IDENTIFIER = "identifier"

class IROperator(Enum):
    """IR operators"""
    ADD = "+"
    SUBTRACT = "-"
    MULTIPLY = "*"
    DIVIDE = "/"
    EQUAL = "=="
    NOT_EQUAL = "!="
    GREATER_THAN = ">"
    LESS_THAN = "<"
    GREATER_EQUAL = ">="
    LESS_EQUAL = "<="
    AND = "and"
    OR = "or"
    NOT = "not"
    ASSIGN = "="

@dataclass
class IRNode:
    """Base IR node"""
    type: IRType
    value: Optional[str] = None
    children: List['IRNode'] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def add_child(self, child: 'IRNode'):
        """Add a child node"""
        self.children.append(child)
    
    def __repr__(self):
        return f"{self.type.value}({self.value or ''})"

@dataclass
class IRProgram(IRNode):
    """IR program node"""
    def __init__(self, name: str = "main"):
        super().__init__(IRType.PROGRAM, name)
        self.functions: List[IRFunction] = []
        self.variables: List[IRVariable] = []
    
    def add_function(self, func: 'IRFunction'):
        """Add a function to the program"""
        self.functions.append(func)
        self.add_child(func)
    
    def add_variable(self, var: 'IRVariable'):
        """Add a variable to the program"""
        self.variables.append(var)
        self.add_child(var)

@dataclass
class IRFunction(IRNode):
    """IR function node"""
    def __init__(self, name: str, params: List[str] = None):
        super().__init__(IRType.FUNCTION, name)
        self.params = params or []
        self.body: List[IRNode] = []

@dataclass
class IRVariable(IRNode):
    """IR variable node"""
    def __init__(self, name: str, var_type: str = "str", initial_value: Any = None):
        super().__init__(IRType.VARIABLE, name)
        self.var_type = var_type
        self.initial_value = initial_value

@dataclass
class IRAssignment(IRNode):
    """IR assignment node"""
    def __init__(self, target: str, value: IRNode):
        super().__init__(IRType.ASSIGNMENT, target)
        self.target = target
        self.value = value
        self.add_child(value)

@dataclass
class IROperation(IRNode):
    """IR operation node"""
    def __init__(self, operator: IROperator, left: IRNode, right: IRNode = None):
        super().__init__(IRType.OPERATION, operator.value)
        self.operator = operator
        self.left = left
        self.right = right
        self.add_child(left)
        if right:
            self.add_child(right)

@dataclass
class IRLiteral(IRNode):
    """IR literal node"""
    def __init__(self, value: Any, literal_type: str = "str"):
        super().__init__(IRType.LITERAL, str(value))
        self.literal_type = literal_type
        self.actual_value = value

@dataclass
class IRIdentifier(IRNode):
    """IR identifier node"""
    def __init__(self, name: str):
        super().__init__(IRType.IDENTIFIER, name)

@dataclass
class IRLoop(IRNode):
    """IR loop node"""
    def __init__(self, condition: IRNode, body: List[IRNode]):
        super().__init__(IRType.LOOP, "while")
        self.condition = condition
        self.body = body
        self.add_child(condition)
        for stmt in body:
            self.add_child(stmt)

@dataclass
class IRConditional(IRNode):
    """IR conditional node"""
    def __init__(self, condition: IRNode, then_body: List[IRNode], else_body: List[IRNode] = None):
        super().__init__(IRType.CONDITIONAL, "if")
        self.condition = condition
        self.then_body = then_body
        self.else_body = else_body
        self.add_child(condition)
        for stmt in then_body:
            self.add_child(stmt)
        if else_body:
            for stmt in else_body:
                self.add_child(stmt)

@dataclass
class IRCall(IRNode):
    """IR function call node"""
    def __init__(self, function_name: str, args: List[IRNode] = None):
        super().__init__(IRType.CALL, function_name)
        self.args = args or []
        for arg in self.args:
            self.add_child(arg)

@dataclass
class IRInput(IRNode):
    """IR input node"""
    def __init__(self, target: str, input_type: str = "str"):
        super().__init__(IRType.INPUT, target)
        self.target = target
        self.input_type = input_type

@dataclass
class IROutput(IRNode):
    """IR output node"""
    def __init__(self, values: List[IRNode]):
        super().__init__(IRType.OUTPUT, "print")
        self.values = values
        for value in values:
            self.add_child(value)

@dataclass
class IRReturn(IRNode):
    """IR return node"""
    def __init__(self, value: IRNode = None):
        super().__init__(IRType.RETURN, "return")
        self.value = value
        if value:
            self.add_child(value)

class IRBuilder:
    """Builder for creating IR nodes"""
    
    @staticmethod
    def program(name: str = "main") -> IRProgram:
        """Create a program node"""
        return IRProgram(name)
    
    @staticmethod
    def function(name: str, params: List[str] = None) -> IRFunction:
        """Create a function node"""
        return IRFunction(name, params)
    
    @staticmethod
    def variable(name: str, var_type: str = "str", initial_value: Any = None) -> IRVariable:
        """Create a variable node"""
        return IRVariable(name, var_type, initial_value)
    
    @staticmethod
    def assignment(target: str, value: IRNode) -> IRAssignment:
        """Create an assignment node"""
        return IRAssignment(target, value)
    
    @staticmethod
    def operation(operator: IROperator, left: IRNode, right: IRNode = None) -> IROperation:
        """Create an operation node"""
        return IROperation(operator, left, right)
    
    @staticmethod
    def literal(value: Any, literal_type: str = "str") -> IRLiteral:
        """Create a literal node"""
        return IRLiteral(value, literal_type)
    
    @staticmethod
    def identifier(name: str) -> IRIdentifier:
        """Create an identifier node"""
        return IRIdentifier(name)
    
    @staticmethod
    def loop(condition: IRNode, body: List[IRNode]) -> IRLoop:
        """Create a loop node"""
        return IRLoop(condition, body)
    
    @staticmethod
    def conditional(condition: IRNode, then_body: List[IRNode], else_body: List[IRNode] = None) -> IRConditional:
        """Create a conditional node"""
        return IRConditional(condition, then_body, else_body)
    
    @staticmethod
    def call(function_name: str, args: List[IRNode] = None) -> IRCall:
        """Create a function call node"""
        return IRCall(function_name, args)
    
    @staticmethod
    def input(target: str, input_type: str = "str") -> IRInput:
        """Create an input node"""
        return IRInput(target, input_type)
    
    @staticmethod
    def output(values: List[IRNode]) -> IROutput:
        """Create an output node"""
        return IROutput(values)
    
    @staticmethod
    def return_stmt(value: IRNode = None) -> IRReturn:
        """Create a return node"""
        return IRReturn(value)

class IRVisitor:
    """Base visitor for IR traversal"""
    
    def visit(self, node: IRNode) -> Any:
        """Visit an IR node"""
        method_name = f"visit_{node.type.value}"
        if hasattr(self, method_name):
            return getattr(self, method_name)(node)
        else:
            return self.visit_default(node)
    
    def visit_default(self, node: IRNode) -> Any:
        """Default visitor method"""
        for child in node.children:
            self.visit(child)
        return None 