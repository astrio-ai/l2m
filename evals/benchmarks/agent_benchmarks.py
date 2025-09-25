"""
Agent performance benchmarks.

This module contains benchmarks for evaluating individual agent
performance and accuracy.
"""

import time
import asyncio
from typing import Dict, List, Any
from pathlib import Path

from src.core.agents.analyzer_agent import AnalyzerAgent
from src.core.agents.planner_agent import PlannerAgent
from src.core.agents.executor_agent import ExecutorAgent
from src.core.agents.reviewer_agent import ReviewerAgent
from src.core.agents.tester_agent import TesterAgent
from src.core.agents.validator_agent import ValidatorAgent
from src.config.settings import Settings
from src.utils.logger import get_logger

logger = get_logger(__name__)


class AgentBenchmark:
    """Benchmark for individual agent performance."""
    
    def __init__(self, settings: Settings):
        """Initialize the agent benchmark."""
        self.settings = settings
        self.results = {}
    
    async def benchmark_analyzer_agent(self, test_cases: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Benchmark the analyzer agent."""
        logger.info("Starting analyzer agent benchmark")
        
        agent = AnalyzerAgent(self.settings)
        results = {
            "total_cases": len(test_cases),
            "successful": 0,
            "failed": 0,
            "avg_time": 0.0,
            "errors": []
        }
        
        total_time = 0.0
        
        for i, test_case in enumerate(test_cases):
            try:
                start_time = time.time()
                
                # Run analyzer agent
                agent_state = {
                    "agent_id": "analyzer",
                    "agent_type": "analyzer",
                    "codebase_path": test_case["codebase_path"],
                    "target_language": test_case.get("target_language", "python"),
                    "modernization_goals": test_case.get("goals", []),
                    "messages": []
                }
                
                result = agent.run(agent_state)
                
                end_time = time.time()
                execution_time = end_time - start_time
                total_time += execution_time
                
                if result.get("analysis_results"):
                    results["successful"] += 1
                else:
                    results["failed"] += 1
                    results["errors"].append(f"Case {i+1}: No analysis results")
                
            except Exception as e:
                results["failed"] += 1
                results["errors"].append(f"Case {i+1}: {str(e)}")
                logger.error(f"Error in analyzer benchmark case {i+1}: {e}")
        
        results["avg_time"] = total_time / len(test_cases) if test_cases else 0.0
        self.results["analyzer"] = results
        
        logger.info(f"Analyzer benchmark completed: {results['successful']}/{results['total_cases']} successful")
        return results
    
    async def benchmark_planner_agent(self, test_cases: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Benchmark the planner agent."""
        logger.info("Starting planner agent benchmark")
        
        agent = PlannerAgent(self.settings)
        results = {
            "total_cases": len(test_cases),
            "successful": 0,
            "failed": 0,
            "avg_time": 0.0,
            "errors": []
        }
        
        total_time = 0.0
        
        for i, test_case in enumerate(test_cases):
            try:
                start_time = time.time()
                
                # Run planner agent
                agent_state = {
                    "agent_id": "planner",
                    "agent_type": "planner",
                    "codebase_path": test_case["codebase_path"],
                    "target_language": test_case.get("target_language", "python"),
                    "modernization_goals": test_case.get("goals", []),
                    "analysis_results": test_case.get("analysis_results", {}),
                    "messages": []
                }
                
                result = agent.run(agent_state)
                
                end_time = time.time()
                execution_time = end_time - start_time
                total_time += execution_time
                
                if result.get("modernization_plan"):
                    results["successful"] += 1
                else:
                    results["failed"] += 1
                    results["errors"].append(f"Case {i+1}: No modernization plan")
                
            except Exception as e:
                results["failed"] += 1
                results["errors"].append(f"Case {i+1}: {str(e)}")
                logger.error(f"Error in planner benchmark case {i+1}: {e}")
        
        results["avg_time"] = total_time / len(test_cases) if test_cases else 0.0
        self.results["planner"] = results
        
        logger.info(f"Planner benchmark completed: {results['successful']}/{results['total_cases']} successful")
        return results
    
    async def run_all_benchmarks(self, test_cases: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Run all agent benchmarks."""
        logger.info("Starting comprehensive agent benchmarks")
        
        # Run individual agent benchmarks
        await self.benchmark_analyzer_agent(test_cases)
        await self.benchmark_planner_agent(test_cases)
        
        # Calculate overall metrics
        overall_results = {
            "total_agents": len(self.results),
            "total_cases": test_cases,
            "overall_success_rate": 0.0,
            "avg_execution_time": 0.0,
            "agent_results": self.results
        }
        
        # Calculate overall success rate
        total_successful = sum(result["successful"] for result in self.results.values())
        total_attempts = sum(result["total_cases"] for result in self.results.values())
        overall_results["overall_success_rate"] = total_successful / total_attempts if total_attempts > 0 else 0.0
        
        # Calculate average execution time
        total_time = sum(result["avg_time"] for result in self.results.values())
        overall_results["avg_execution_time"] = total_time / len(self.results) if self.results else 0.0
        
        logger.info(f"All benchmarks completed. Overall success rate: {overall_results['overall_success_rate']:.2%}")
        return overall_results
