"""
CLI Interface for LLM-Enhanced COBOL Transpiler

This module provides a comprehensive command-line interface that showcases
all the LLM integration capabilities including analysis, optimization,
and code review.
"""

import argparse
import logging
import os
import sys
import json
from pathlib import Path
from typing import Optional

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")))

from packages.transpiler.engine.hybrid_transpiler import HybridTranspiler
from packages.transpiler.engine.llm_augmentor import LLMConfig
from packages.llm_agent import LLMAgent


def setup_logging(verbose: bool = False):
    """Set up logging configuration."""
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )


def create_llm_config() -> Optional[LLMConfig]:
    """Create LLM configuration from environment variables."""
    try:
        return LLMConfig.from_env()
    except Exception as e:
        logging.warning(f"Failed to create LLM config: {e}")
        return None


def transpile_with_analysis(input_file: str, output_file: Optional[str] = None,
                          verbose: bool = False, generate_report: bool = False) -> bool:
    """
    Transpile COBOL file with comprehensive LLM analysis.
    
    Args:
        input_file: Path to input COBOL file
        output_file: Path to output Python file (optional)
        verbose: Enable verbose logging
        generate_report: Generate detailed analysis report
        
    Returns:
        True if successful, False otherwise
    """
    setup_logging(verbose)
    logger = logging.getLogger(__name__)
    
    # Check if input file exists
    if not os.path.exists(input_file):
        logger.error(f"Input file not found: {input_file}")
        return False
    
    # Create output file path if not provided
    if output_file is None:
        input_path = Path(input_file)
        output_file = input_path.with_suffix('.py').name
    
    try:
        # Create LLM configuration and agents
        llm_config = create_llm_config()
        hybrid_transpiler = HybridTranspiler(llm_config)
        llm_agent = LLMAgent(llm_config)
        
        logger.info(f"Starting transpilation: {input_file} -> {output_file}")
        
        # Read source code
        with open(input_file, 'r') as f:
            source_code = f.read()
        
        # Step 1: Transpile the code
        logger.info("Step 1: Transpiling COBOL to Python...")
        target_code = hybrid_transpiler.transpile_source(source_code, input_file)
        
        # Write output file
        with open(output_file, 'w') as f:
            f.write(target_code)
        
        logger.info(f"Transpilation completed: {output_file}")
        
        # Step 2: Analyze the transformation
        logger.info("Step 2: Analyzing code transformation...")
        analysis_result = llm_agent.analyze_code(source_code, target_code, "cobol-python")
        
        # Step 3: Review the generated code
        logger.info("Step 3: Reviewing generated code...")
        review_result = llm_agent.review_code(target_code, "python")
        
        # Step 4: Optimize the code
        logger.info("Step 4: Optimizing generated code...")
        optimization_result = llm_agent.optimize_code(target_code, "python")
        
        # Step 5: Generate documentation
        logger.info("Step 5: Generating documentation...")
        documentation = llm_agent.generate_documentation(target_code, "python")
        
        # Print results
        print_results(analysis_result, review_result, optimization_result, documentation)
        
        # Generate comprehensive report if requested
        if generate_report:
            generate_comprehensive_report(
                input_file, output_file, source_code, target_code,
                analysis_result, review_result, optimization_result,
                documentation, hybrid_transpiler, llm_agent
            )
        
        return True
        
    except Exception as e:
        logger.error(f"Enhanced transpilation failed: {e}")
        return False


def print_results(analysis_result, review_result, optimization_result, documentation):
    """Print analysis results in a formatted way."""
    print("\n" + "="*60)
    print("ENHANCED TRANSPILATION RESULTS")
    print("="*60)
    
    # Analysis Results
    print("\n📊 CODE ANALYSIS:")
    print(f"  Complexity Score: {analysis_result.complexity_score:.2f}")
    print(f"  Maintainability Score: {analysis_result.maintainability_score:.2f}")
    print(f"  Confidence: {analysis_result.confidence:.2f}")
    
    if analysis_result.performance_issues:
        print("  Performance Issues:")
        for issue in analysis_result.performance_issues:
            print(f"    - {issue}")
    
    if analysis_result.suggestions:
        print("  Suggestions:")
        for suggestion in analysis_result.suggestions:
            print(f"    - {suggestion}")
    
    # Review Results
    print(f"\n🔍 CODE REVIEW (Severity: {review_result.severity.upper()}):")
    print(f"  Confidence: {review_result.confidence:.2f}")
    
    if review_result.issues:
        print("  Issues Found:")
        for issue in review_result.issues:
            print(f"    - {issue}")
    
    if review_result.suggestions:
        print("  Improvement Suggestions:")
        for suggestion in review_result.suggestions:
            print(f"    - {suggestion}")
    
    # Optimization Results
    print(f"\n⚡ OPTIMIZATION RESULTS:")
    print(f"  Confidence: {optimization_result.confidence:.2f}")
    
    if optimization_result.improvements:
        print("  Improvements Applied:")
        for improvement in optimization_result.improvements:
            print(f"    - {improvement}")
    
    if optimization_result.performance_gains:
        print("  Performance Gains:")
        for metric, gain in optimization_result.performance_gains.items():
            print(f"    - {metric}: {gain}")
    
    # Documentation
    print(f"\n📚 GENERATED DOCUMENTATION:")
    print("  (See documentation section in report)")


def generate_comprehensive_report(input_file: str, output_file: str, source_code: str,
                               target_code: str, analysis_result, review_result,
                               optimization_result, documentation, hybrid_transpiler,
                               llm_agent):
    """Generate a comprehensive analysis report."""
    report_file = f"{Path(output_file).stem}_comprehensive_report.md"
    
    with open(report_file, 'w') as f:
        f.write("# Enhanced COBOL to Python Transpilation Report\n\n")
        
        # Basic Information
        f.write("## Basic Information\n")
        f.write(f"- **Input File**: {input_file}\n")
        f.write(f"- **Output File**: {output_file}\n")
        f.write(f"- **Transpilation Date**: {Path(input_file).stat().st_mtime}\n\n")
        
        # Source Code
        f.write("## Source COBOL Code\n")
        f.write("```cobol\n")
        f.write(source_code)
        f.write("\n```\n\n")
        
        # Target Code
        f.write("## Generated Python Code\n")
        f.write("```python\n")
        f.write(target_code)
        f.write("\n```\n\n")
        
        # Analysis Results
        f.write("## Code Analysis\n")
        f.write(f"- **Complexity Score**: {analysis_result.complexity_score:.2f}\n")
        f.write(f"- **Maintainability Score**: {analysis_result.maintainability_score:.2f}\n")
        f.write(f"- **Confidence**: {analysis_result.confidence:.2f}\n\n")
        
        if analysis_result.performance_issues:
            f.write("### Performance Issues\n")
            for issue in analysis_result.performance_issues:
                f.write(f"- {issue}\n")
            f.write("\n")
        
        if analysis_result.suggestions:
            f.write("### Suggestions\n")
            for suggestion in analysis_result.suggestions:
                f.write(f"- {suggestion}\n")
            f.write("\n")
        
        # Review Results
        f.write("## Code Review\n")
        f.write(f"- **Overall Severity**: {review_result.severity.upper()}\n")
        f.write(f"- **Confidence**: {review_result.confidence:.2f}\n\n")
        
        if review_result.issues:
            f.write("### Issues Found\n")
            for issue in review_result.issues:
                f.write(f"- {issue}\n")
            f.write("\n")
        
        if review_result.suggestions:
            f.write("### Improvement Suggestions\n")
            for suggestion in review_result.suggestions:
                f.write(f"- {suggestion}\n")
            f.write("\n")
        
        # Optimization Results
        f.write("## Code Optimization\n")
        f.write(f"- **Confidence**: {optimization_result.confidence:.2f}\n\n")
        
        if optimization_result.improvements:
            f.write("### Improvements Applied\n")
            for improvement in optimization_result.improvements:
                f.write(f"- {improvement}\n")
            f.write("\n")
        
        if optimization_result.performance_gains:
            f.write("### Performance Gains\n")
            for metric, gain in optimization_result.performance_gains.items():
                f.write(f"- **{metric}**: {gain}\n")
            f.write("\n")
        
        # Documentation
        f.write("## Generated Documentation\n")
        f.write(documentation)
        f.write("\n\n")
        
        # Statistics
        f.write("## Transpilation Statistics\n")
        stats = hybrid_transpiler.get_translation_stats()
        f.write(f"- **Total Edge Cases**: {stats['total_edge_cases']}\n")
        f.write(f"- **AI Translations**: {stats['ai_translations']}\n")
        f.write(f"- **LLM Available**: {stats['llm_available']}\n\n")
        
        # Agent Capabilities
        f.write("## LLM Agent Capabilities\n")
        capabilities = llm_agent.get_agent_capabilities()
        f.write(f"- **LLM Available**: {capabilities['llm_available']}\n")
        f.write(f"- **Provider**: {capabilities['provider']}\n")
        f.write(f"- **Model**: {capabilities['model']}\n")
        f.write(f"- **Cache Enabled**: {capabilities['cache_enabled']}\n")
        f.write("- **Capabilities**:\n")
        for capability in capabilities['capabilities']:
            f.write(f"  - {capability}\n")
    
    print(f"\n📄 Comprehensive report generated: {report_file}")


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description="Enhanced COBOL to Python Transpiler with LLM Analysis",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python cli.py input.cobol
python cli.py input.cobol -o output.py
python cli.py input.cobol --verbose --report
python cli.py input.cobol --analyze-only
        """
    )
    
    parser.add_argument(
        'input_file',
        help='Input COBOL file to transpile'
    )
    
    parser.add_argument(
        '-o', '--output',
        help='Output Python file (default: input_file.py)'
    )
    
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Enable verbose logging'
    )
    
    parser.add_argument(
        '-r', '--report',
        action='store_true',
        help='Generate comprehensive analysis report'
    )
    
    parser.add_argument(
        '--analyze-only',
        action='store_true',
        help='Only analyze existing Python file (skip transpilation)'
    )
    
    parser.add_argument(
        '--check-llm',
        action='store_true',
        help='Check LLM configuration and capabilities'
    )
    
    args = parser.parse_args()
    
    # Check LLM configuration if requested
    if args.check_llm:
        llm_config = create_llm_config()
        if llm_config and llm_config.api_key:
            print("✅ LLM configuration is valid")
            print(f"   Provider: {llm_config.provider}")
            print(f"   Model: {llm_config.model}")
            print(f"   Temperature: {llm_config.temperature}")
            print(f"   Cache Enabled: {llm_config.cache_enabled}")
            print(f"   Retry Attempts: {llm_config.retry_attempts}")
            
            # Test agent capabilities
            try:
                llm_agent = LLMAgent(llm_config)
                capabilities = llm_agent.get_agent_capabilities()
                print(f"   Available Capabilities: {', '.join(capabilities['capabilities'])}")
            except Exception as e:
                print(f"   ❌ Agent initialization failed: {e}")
        else:
            print("❌ LLM configuration is missing or invalid")
            print("   Please set LLM_API_KEY, LLM_MODEL, and other environment variables")
        return
    
    # Perform enhanced transpilation
    success = transpile_with_analysis(
        input_file=args.input_file,
        output_file=args.output,
        verbose=args.verbose,
        generate_report=args.report
    )
    
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main() 