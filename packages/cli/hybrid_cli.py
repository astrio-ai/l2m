"""
CLI Interface for Hybrid COBOL Transpiler

This module provides a command-line interface for the hybrid transpiler
that combines rule-based and AI-assisted translation.
"""

import argparse
import logging
import os
import sys
from pathlib import Path
from typing import Optional

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")))

from packages.transpiler.engine.hybrid_transpiler import HybridTranspiler
from packages.transpiler.engine.llm_augmentor import LLMConfig


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


def transpile_file(input_file: str, output_file: Optional[str] = None, 
                  verbose: bool = False, generate_report: bool = False) -> bool:
    """
    Transpile a COBOL file using the hybrid transpiler.
    
    Args:
        input_file: Path to input COBOL file
        output_file: Path to output Python file (optional)
        verbose: Enable verbose logging
        generate_report: Generate translation report
        
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
        # Create hybrid transpiler
        llm_config = create_llm_config()
        transpiler = HybridTranspiler(llm_config)
        
        logger.info(f"Starting hybrid transpilation: {input_file} -> {output_file}")
        
        # Transpile the file
        python_code = transpiler.transpile_file(input_file)
        
        # Write output file
        with open(output_file, 'w') as f:
            f.write(python_code)
        
        logger.info(f"Transpilation completed successfully: {output_file}")
        
        # Generate report if requested
        if generate_report:
            report_file = f"{Path(output_file).stem}_translation_report.txt"
            transpiler.generate_translation_report(report_file)
            logger.info(f"Translation report generated: {report_file}")
        
        # Print statistics
        stats = transpiler.get_translation_stats()
        logger.info("Translation Statistics:")
        logger.info(f"  Total edge cases: {stats['total_edge_cases']}")
        logger.info(f"  AI translations: {stats['ai_translations']}")
        logger.info(f"  LLM available: {stats['llm_available']}")
        
        return True
        
    except Exception as e:
        logger.error(f"Transpilation failed: {e}")
        return False


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description="Hybrid COBOL to Python Transpiler",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python hybrid_cli.py input.cobol
  python hybrid_cli.py input.cobol -o output.py
  python hybrid_cli.py input.cobol --verbose --report
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
        help='Generate translation report'
    )
    
    parser.add_argument(
        '--check-llm',
        action='store_true',
        help='Check LLM configuration and exit'
    )
    
    args = parser.parse_args()
    
    # Check LLM configuration if requested
    if args.check_llm:
        llm_config = create_llm_config()
        if llm_config and llm_config.api_key:
            print("✅ LLM configuration is valid")
            print(f"   Model: {llm_config.model}")
            print(f"   Temperature: {llm_config.temperature}")
        else:
            print("❌ LLM configuration is missing or invalid")
            print("   Please set LLM_API_KEY, LLM_MODEL, and DEFAULT_LLM_TEMPERATURE environment variables")
        return
    
    # Perform transpilation
    success = transpile_file(
        input_file=args.input_file,
        output_file=args.output,
        verbose=args.verbose,
        generate_report=args.report
    )
    
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main() 