"""
Entry point for CLI / API execution.
"""

import argparse
import asyncio
import sys
import re
from pathlib import Path
from src.config import get_settings
from src.workflows.modernization_pipeline import ModernizationPipeline
from src.workflows.batch_pipeline import BatchModernizationPipeline, discover_cobol_files
from src.utils.logger import get_logger

logger = get_logger(__name__)


def extract_python_code(text: str) -> str:
    """Extract Python code from text, handling markdown code blocks."""
    if not text:
        return ""
    
    # Check for markdown code blocks
    python_block_match = re.search(r'```python\s*\n(.*?)```', text, re.DOTALL)
    if python_block_match:
        return python_block_match.group(1).strip()
    
    # Check for generic code blocks
    code_block_match = re.search(r'```\s*\n(.*?)```', text, re.DOTALL)
    if code_block_match:
        return code_block_match.group(1).strip()
    
    # If no code blocks, return the text as-is (might already be Python code)
    return text.strip()


def save_python_file(output_dir: Path, cobol_file_path: str, python_code: str) -> Path:
    """Save Python code to a file in the output directory.
    
    Args:
        output_dir: Directory to save the file
        cobol_file_path: Path to the original COBOL file
        python_code: Python code to save
        
    Returns:
        Path to the saved Python file
    """
    # Create output directory if it doesn't exist
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Generate output filename based on COBOL filename
    cobol_path = Path(cobol_file_path)
    output_filename = cobol_path.stem + ".py"
    output_file = output_dir / output_filename
    
    # Write Python code to file
    output_file.write_text(python_code, encoding="utf-8")
    
    return output_file


def parse_arguments():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Legacy2Modern: COBOL to Python modernization tool",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Single file
  python -m src.main data/hello/HELLO.cbl
  
  # Multiple files
  python -m src.main file1.cbl file2.cbl file3.cbl
  
  # Directory (batch mode)
  python -m src.main --directory data/aws-samples/
  
  # Glob pattern
  python -m src.main --pattern "**/*.cbl"
  
  # File list
  python -m src.main --file-list files.txt
  
  # Batch mode with custom delay
  python -m src.main --directory data/aws-samples/ --batch-delay 10.0
        """
    )
    
    # Input options
    parser.add_argument(
        'files',
        nargs='*',
        default=[],
        help='COBOL file(s) to process (use --directory, --pattern, or --file-list for batch mode)'
    )
    parser.add_argument(
        '--directory', '-d',
        type=str,
        help='Directory containing COBOL files (recursive, batch mode)'
    )
    parser.add_argument(
        '--pattern', '-p',
        type=str,
        help='Glob pattern to find COBOL files (e.g., "**/*.cbl", batch mode)'
    )
    parser.add_argument(
        '--file-list', '-f',
        type=str,
        help='Text file containing list of COBOL file paths (one per line, batch mode)'
    )
    
    # Batch options
    parser.add_argument(
        '--batch',
        action='store_true',
        help='Enable batch mode (auto-enabled for multiple files/directories)'
    )
    parser.add_argument(
        '--batch-delay',
        type=float,
        help='Delay between files in batch mode (seconds)'
    )
    parser.add_argument(
        '--max-concurrent',
        type=int,
        help='Maximum concurrent files (default: 1 for rate limits)'
    )
    parser.add_argument(
        '--no-continue-on-error',
        action='store_true',
        help='Stop batch processing on first error (default: continue)'
    )
    
    return parser.parse_args()


async def process_single_file(cobol_file: str):
    """Process a single COBOL file."""
    if not Path(cobol_file).exists():
        print(f"Error: File not found: {cobol_file}")
        sys.exit(1)
    
    settings = get_settings()
    logger.info(f"Starting modernization for {cobol_file}")
    
    pipeline = ModernizationPipeline()
    results = await pipeline.run(cobol_file)
    
    print("\n" + "="*70)
    print("Modernization Results")
    print("="*70)
    print(f"\nCOBOL File: {results.get('cobol_file')}")
    
    if "error" in results:
        print(f"\n❌ Error: {results['error']}")
        sys.exit(1)
    
    # Print analysis results
    if results.get("analysis"):
        print("\n" + "-"*70)
        print("📊 ANALYSIS")
        print("-"*70)
        analysis_text = str(results["analysis"])
        print(analysis_text[:1000] + "..." if len(analysis_text) > 1000 else analysis_text)
    
    # Print translation results and save to file
    if results.get("translation"):
        print("\n" + "-"*70)
        print("🐍 TRANSLATION (Python Code)")
        print("-"*70)
        translation_text = str(results["translation"])
        print(translation_text[:2000] + "..." if len(translation_text) > 2000 else translation_text)
        
        # Extract and save Python code
        python_code = extract_python_code(translation_text)
        if python_code:
            output_file = save_python_file(settings.output_path, cobol_file, python_code)
            print(f"\n💾 Saved Python code to: {output_file}")
    elif results.get("orchestrator_output") and not results.get("refactored"):
        # Try to extract Python code from orchestrator output if translation not available
        # (but only if we don't have refactored code, which will be saved later)
        orchestrator_text = str(results["orchestrator_output"])
        python_code = extract_python_code(orchestrator_text)
        if python_code and ("def " in python_code or "print(" in python_code):
            output_file = save_python_file(settings.output_path, cobol_file, python_code)
            print(f"\n💾 Saved Python code to: {output_file}")
    
    # Print review results
    if results.get("review"):
        print("\n" + "-"*70)
        print("📝 REVIEW")
        print("-"*70)
        review_text = str(results["review"])
        print(review_text[:1000] + "..." if len(review_text) > 1000 else review_text)
    
    # Print test results
    if results.get("tests"):
        print("\n" + "-"*70)
        print("🧪 TESTS")
        print("-"*70)
        tests_text = str(results["tests"])
        print(tests_text[:1000] + "..." if len(tests_text) > 1000 else tests_text)
    
    # Print refactored code and save to file (prefer refactored over translation)
    if results.get("refactored"):
        print("\n" + "-"*70)
        print("🔧 REFACTORED CODE")
        print("-"*70)
        refactored_text = str(results["refactored"])
        print(refactored_text[:2000] + "..." if len(refactored_text) > 2000 else refactored_text)
        
        # Extract and save refactored Python code
        python_code = extract_python_code(refactored_text)
        if python_code:
            output_file = save_python_file(settings.output_path, cobol_file, python_code)
            print(f"\n💾 Saved refactored Python code to: {output_file}")
    
    # Print orchestrator output if using handoffs
    if results.get("orchestrator_output"):
        print("\n" + "-"*70)
        print("🎯 ORCHESTRATOR OUTPUT")
        print("-"*70)
        orchestrator_text = str(results["orchestrator_output"])
        print(orchestrator_text[:2000] + "..." if len(orchestrator_text) > 2000 else orchestrator_text)
    
    print("\n" + "="*70)
    print("✅ Modernization Complete!")
    print("="*70)


async def process_batch(args, settings):
    """Process multiple COBOL files in batch mode."""
    # Discover files based on input type
    cobol_files = []
    
    if args.directory:
        # Directory provided
        cobol_files = discover_cobol_files(Path(args.directory))
        if not cobol_files:
            print(f"Error: No COBOL files found in directory: {args.directory}")
            sys.exit(1)
    elif args.pattern:
        # Glob pattern provided
        cobol_files = discover_cobol_files(Path("."), pattern=args.pattern)
        if not cobol_files:
            print(f"Error: No COBOL files found matching pattern: {args.pattern}")
            sys.exit(1)
    elif args.file_list:
        # File list provided
        cobol_files = discover_cobol_files(Path(args.file_list))
        if not cobol_files:
            print(f"Error: No valid COBOL files found in list: {args.file_list}")
            sys.exit(1)
    elif args.files:
        # Multiple files provided
        cobol_files = [Path(f) for f in args.files if Path(f).exists()]
        if not cobol_files:
            print("Error: No valid COBOL files found")
            sys.exit(1)
    else:
        print("Error: No input files specified. Use files, --directory, --pattern, or --file-list")
        sys.exit(1)
    
    # Update settings based on CLI arguments
    if args.batch_delay is not None:
        settings.batch_file_delay_seconds = args.batch_delay
    if args.max_concurrent is not None:
        settings.batch_max_concurrent = args.max_concurrent
    if args.no_continue_on_error:
        settings.batch_continue_on_error = False
    
    # Enable batch mode
    settings.batch_mode = True
    
    print(f"Found {len(cobol_files)} COBOL file(s) to process")
    
    # Create batch pipeline and run
    batch_pipeline = BatchModernizationPipeline(settings)
    batch_result = await batch_pipeline.run_batch(cobol_files)
    
    # Exit with error code if any files failed
    if batch_result.failed > 0:
        sys.exit(1)


async def main():
    """Main entry point for CLI."""
    args = parse_arguments()
    settings = get_settings()
    
    # Check if any input was provided
    has_input = (
        (args.files and len(args.files) > 0) or
        args.directory or 
        args.pattern or 
        args.file_list
    )
    
    if not has_input:
        print("Error: No input files specified")
        print("Usage: python -m src.main <cobol_file_path>")
        print("       python -m src.main --directory <dir>")
        print("       python -m src.main --pattern <pattern>")
        print("       python -m src.main --file-list <file>")
        print("       python -m src.main --help  # for more options")
        sys.exit(1)
    
    # Determine if batch mode should be used
    use_batch = (
        args.batch or 
        args.directory or 
        args.pattern or 
        args.file_list or 
        (args.files and len(args.files) > 1)
    )
    
    if use_batch:
        # Batch processing
        await process_batch(args, settings)
    else:
        # Single file processing
        if not args.files or len(args.files) != 1:
            print("Error: Single file mode requires exactly one file")
            print("Usage: python -m src.main <cobol_file_path>")
            sys.exit(1)
        await process_single_file(args.files[0])


if __name__ == "__main__":
    asyncio.run(main())

