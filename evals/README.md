# Legacy2Modern Evaluations

This directory contains evaluation and benchmarking tools for the Legacy2Modern multi-agent system.

## Directory Structure

```
evals/
├── benchmarks/          # Benchmark test suites
├── datasets/           # Evaluation datasets
├── metrics/            # Performance metrics and analysis
├── reports/            # Evaluation reports and results
└── scripts/            # Evaluation scripts and tools
```

## Purpose

The evaluation system provides comprehensive testing and benchmarking capabilities for:

- **Agent Performance**: Individual agent effectiveness and accuracy
- **Workflow Efficiency**: End-to-end workflow performance
- **Code Quality**: Modernized code quality assessment
- **Transformation Accuracy**: Legacy-to-modern code transformation fidelity
- **System Performance**: Overall system performance and scalability

## Usage

Run evaluations using the evaluation scripts:

```bash
# Run all evaluations
python evals/scripts/run_all_evals.py

# Run specific evaluation
python evals/scripts/run_agent_evals.py
python evals/scripts/run_workflow_evals.py
python evals/scripts/run_quality_evals.py
```

## Metrics

The evaluation system tracks:

- **Accuracy**: Transformation correctness and fidelity
- **Performance**: Speed and resource utilization
- **Quality**: Code quality metrics and standards compliance
- **Coverage**: Test coverage and validation completeness
- **Scalability**: Performance under different load conditions
