"""
Workflow performance benchmarks.

This module contains benchmarks for evaluating workflow
performance and end-to-end system efficiency.
"""

import time
import asyncio
from typing import Dict, List, Any
from pathlib import Path

from src.core.graph.main_graph import MainWorkflowGraph
from src.core.graph.legacy_analysis import LegacyAnalysisWorkflow
from src.config.settings import Settings
from src.utils.logger import get_logger

logger = get_logger(__name__)


class WorkflowBenchmark:
    """Benchmark for workflow performance."""
    
    def __init__(self, settings: Settings):
        """Initialize the workflow benchmark."""
        self.settings = settings
        self.results = {}
    
    async def benchmark_main_workflow(self, test_cases: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Benchmark the main workflow."""
        logger.info("Starting main workflow benchmark")
        
        workflow = MainWorkflowGraph(self.settings)
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
                
                # Run main workflow
                initial_state = {
                    "codebase_path": test_case["codebase_path"],
                    "target_language": test_case.get("target_language", "python"),
                    "modernization_goals": test_case.get("goals", []),
                    "backup_location": test_case.get("backup_location"),
                    "test_framework": test_case.get("test_framework", "pytest"),
                    "messages": []
                }
                
                result = await workflow.run(initial_state)
                
                end_time = time.time()
                execution_time = end_time - start_time
                total_time += execution_time
                
                if result.get("modernization_success"):
                    results["successful"] += 1
                else:
                    results["failed"] += 1
                    results["errors"].append(f"Case {i+1}: Workflow failed")
                
            except Exception as e:
                results["failed"] += 1
                results["errors"].append(f"Case {i+1}: {str(e)}")
                logger.error(f"Error in main workflow benchmark case {i+1}: {e}")
        
        results["avg_time"] = total_time / len(test_cases) if test_cases else 0.0
        self.results["main_workflow"] = results
        
        logger.info(f"Main workflow benchmark completed: {results['successful']}/{results['total_cases']} successful")
        return results
    
    async def benchmark_analysis_workflow(self, test_cases: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Benchmark the analysis workflow."""
        logger.info("Starting analysis workflow benchmark")
        
        workflow = LegacyAnalysisWorkflow(self.settings)
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
                
                # Run analysis workflow
                initial_state = {
                    "codebase_path": test_case["codebase_path"],
                    "target_language": test_case.get("target_language", "python"),
                    "modernization_goals": test_case.get("goals", []),
                    "messages": []
                }
                
                result = await workflow.run(initial_state)
                
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
                logger.error(f"Error in analysis workflow benchmark case {i+1}: {e}")
        
        results["avg_time"] = total_time / len(test_cases) if test_cases else 0.0
        self.results["analysis_workflow"] = results
        
        logger.info(f"Analysis workflow benchmark completed: {results['successful']}/{results['total_cases']} successful")
        return results
    
    async def run_all_workflow_benchmarks(self, test_cases: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Run all workflow benchmarks."""
        logger.info("Starting comprehensive workflow benchmarks")
        
        # Run individual workflow benchmarks
        await self.benchmark_main_workflow(test_cases)
        await self.benchmark_analysis_workflow(test_cases)
        
        # Calculate overall metrics
        overall_results = {
            "total_workflows": len(self.results),
            "total_cases": test_cases,
            "overall_success_rate": 0.0,
            "avg_execution_time": 0.0,
            "workflow_results": self.results
        }
        
        # Calculate overall success rate
        total_successful = sum(result["successful"] for result in self.results.values())
        total_attempts = sum(result["total_cases"] for result in self.results.values())
        overall_results["overall_success_rate"] = total_successful / total_attempts if total_attempts > 0 else 0.0
        
        # Calculate average execution time
        total_time = sum(result["avg_time"] for result in self.results.values())
        overall_results["avg_execution_time"] = total_time / len(self.results) if self.results else 0.0
        
        logger.info(f"All workflow benchmarks completed. Overall success rate: {overall_results['overall_success_rate']:.2%}")
        return overall_results
