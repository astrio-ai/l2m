#!/usr/bin/env python3
"""
Run all evaluations for the Legacy2Modern system.

This script runs comprehensive evaluations including agent benchmarks,
workflow benchmarks, and quality assessments.
"""

import asyncio
import sys
import json
from pathlib import Path
from datetime import datetime

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from evals.benchmarks.agent_benchmarks import AgentBenchmark
from evals.benchmarks.workflow_benchmarks import WorkflowBenchmark
from src.config.settings import Settings
from src.utils.logger import get_logger

logger = get_logger(__name__)


async def run_evaluations():
    """Run all evaluations."""
    logger.info("Starting comprehensive evaluations")
    
    # Initialize settings
    settings = Settings()
    
    # Sample test cases (in a real scenario, these would come from datasets)
    test_cases = [
        {
            "codebase_path": "examples/cobol/HELLO.cobol",
            "target_language": "python",
            "goals": ["maintainability", "readability"],
            "backup_location": "data/backup",
            "test_framework": "pytest"
        },
        {
            "codebase_path": "examples/cobol/CBL0001.cobol",
            "target_language": "python",
            "goals": ["performance", "maintainability"],
            "backup_location": "data/backup",
            "test_framework": "pytest"
        }
    ]
    
    # Run agent benchmarks
    logger.info("Running agent benchmarks...")
    agent_benchmark = AgentBenchmark(settings)
    agent_results = await agent_benchmark.run_all_benchmarks(test_cases)
    
    # Run workflow benchmarks
    logger.info("Running workflow benchmarks...")
    workflow_benchmark = WorkflowBenchmark(settings)
    workflow_results = await workflow_benchmark.run_all_workflow_benchmarks(test_cases)
    
    # Compile results
    evaluation_results = {
        "timestamp": datetime.now().isoformat(),
        "test_cases": len(test_cases),
        "agent_benchmarks": agent_results,
        "workflow_benchmarks": workflow_results,
        "overall_summary": {
            "total_agents_tested": len(agent_results.get("agent_results", {})),
            "total_workflows_tested": len(workflow_results.get("workflow_results", {})),
            "agent_success_rate": agent_results.get("overall_success_rate", 0.0),
            "workflow_success_rate": workflow_results.get("overall_success_rate", 0.0)
        }
    }
    
    # Save results
    results_file = Path("evals/reports") / f"evaluation_results_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    results_file.parent.mkdir(parents=True, exist_ok=True)
    
    with open(results_file, 'w') as f:
        json.dump(evaluation_results, f, indent=2)
    
    logger.info(f"Evaluation results saved to {results_file}")
    
    # Print summary
    print("\n" + "="*60)
    print("EVALUATION SUMMARY")
    print("="*60)
    print(f"Test Cases: {evaluation_results['test_cases']}")
    print(f"Agents Tested: {evaluation_results['overall_summary']['total_agents_tested']}")
    print(f"Workflows Tested: {evaluation_results['overall_summary']['total_workflows_tested']}")
    print(f"Agent Success Rate: {evaluation_results['overall_summary']['agent_success_rate']:.2%}")
    print(f"Workflow Success Rate: {evaluation_results['overall_summary']['workflow_success_rate']:.2%}")
    print("="*60)
    
    return evaluation_results


if __name__ == "__main__":
    asyncio.run(run_evaluations())
