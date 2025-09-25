"""
Graph type definitions.

This module contains type definitions for LangGraph workflows
and graph state management.
"""

from typing import Dict, Any, List, Optional, Union
from enum import Enum
from dataclasses import dataclass
from abc import ABC, abstractmethod


class GraphNodeType(str, Enum):
    """Graph node type enumeration."""
    AGENT = "agent"
    TOOL = "tool"
    CONDITION = "condition"
    MERGE = "merge"
    SPLIT = "split"


class GraphEdgeType(str, Enum):
    """Graph edge type enumeration."""
    SEQUENTIAL = "sequential"
    CONDITIONAL = "conditional"
    PARALLEL = "parallel"
    MERGE = "merge"


class GraphStatus(str, Enum):
    """Graph execution status enumeration."""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class GraphNode:
    """Graph node definition."""
    node_id: str
    node_type: GraphNodeType
    name: str
    description: str
    agent_id: Optional[str] = None
    tool_id: Optional[str] = None
    condition: Optional[str] = None
    options: Optional[Dict[str, Any]] = None


@dataclass
class GraphEdge:
    """Graph edge definition."""
    edge_id: str
    source_node: str
    target_node: str
    edge_type: GraphEdgeType
    condition: Optional[str] = None
    weight: Optional[float] = None
    options: Optional[Dict[str, Any]] = None


@dataclass
class GraphState:
    """Graph state definition."""
    state_id: str
    current_node: Optional[str] = None
    visited_nodes: List[str] = None
    execution_data: Dict[str, Any] = None
    status: GraphStatus = GraphStatus.PENDING
    start_time: Optional[str] = None
    end_time: Optional[str] = None
    duration: Optional[float] = None
    error: Optional[str] = None
    
    def __post_init__(self):
        """Initialize default values."""
        if self.visited_nodes is None:
            self.visited_nodes = []
        if self.execution_data is None:
            self.execution_data = {}


class GraphInterface(ABC):
    """Interface for all graph implementations."""
    
    @abstractmethod
    async def run(self, initial_state: Dict[str, Any]) -> Dict[str, Any]:
        """Run the graph with the given initial state."""
        pass
    
    @abstractmethod
    def get_nodes(self) -> List[GraphNode]:
        """Get all nodes in the graph."""
        pass
    
    @abstractmethod
    def get_edges(self) -> List[GraphEdge]:
        """Get all edges in the graph."""
        pass
    
    @abstractmethod
    def get_entry_point(self) -> str:
        """Get the entry point node."""
        pass
    
    @abstractmethod
    def get_exit_points(self) -> List[str]:
        """Get all exit point nodes."""
        pass


class GraphExecution:
    """Graph execution tracking."""
    
    def __init__(self, graph_id: str, execution_id: str):
        """Initialize the graph execution."""
        self.graph_id = graph_id
        self.execution_id = execution_id
        self.status = GraphStatus.PENDING
        self.current_node = None
        self.visited_nodes = []
        self.execution_data = {}
        self.start_time = None
        self.end_time = None
        self.duration = None
        self.error = None
    
    def start(self):
        """Start the graph execution."""
        from datetime import datetime
        self.start_time = datetime.now()
        self.status = GraphStatus.RUNNING
    
    def end(self, success: bool = True, error: Optional[str] = None):
        """End the graph execution."""
        from datetime import datetime
        self.end_time = datetime.now()
        if self.start_time:
            self.duration = (self.end_time - self.start_time).total_seconds()
        
        if success:
            self.status = GraphStatus.COMPLETED
        else:
            self.status = GraphStatus.FAILED
            self.error = error
    
    def visit_node(self, node_id: str):
        """Mark a node as visited."""
        if node_id not in self.visited_nodes:
            self.visited_nodes.append(node_id)
    
    def set_current_node(self, node_id: str):
        """Set the current node."""
        self.current_node = node_id
        self.visit_node(node_id)
    
    def add_execution_data(self, key: str, value: Any):
        """Add data to the execution."""
        self.execution_data[key] = value
    
    def get_execution_data(self, key: str, default: Any = None) -> Any:
        """Get data from the execution."""
        return self.execution_data.get(key, default)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert execution to dictionary."""
        return {
            "graph_id": self.graph_id,
            "execution_id": self.execution_id,
            "status": self.status.value,
            "current_node": self.current_node,
            "visited_nodes": self.visited_nodes,
            "execution_data": self.execution_data,
            "start_time": self.start_time.isoformat() if self.start_time else None,
            "end_time": self.end_time.isoformat() if self.end_time else None,
            "duration": self.duration,
            "error": self.error
        }
