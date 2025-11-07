"""
Batch Modernization Pipeline - Process multiple COBOL files.

Handles batch processing of multiple COBOL files with progress tracking,
error handling, and comprehensive reporting.
"""

import asyncio
import json
import logging
import time
from dataclasses import dataclass, asdict
from datetime import datetime, timedelta
from pathlib import Path
from typing import List, Optional, Dict, Any
from src.config import get_settings
from src.utils.logger import get_logger
from src.workflows.modernization_pipeline import ModernizationPipeline

logger = get_logger(__name__)


@dataclass
class FileResult:
    """Result of processing a single file."""
    file_path: Path
    success: bool
    output_file: Optional[Path] = None
    test_file: Optional[Path] = None
    error: Optional[str] = None
    duration_seconds: float = 0.0
    steps_completed: List[str] = None
    file_size_lines: int = 0
    
    def __post_init__(self):
        if self.steps_completed is None:
            self.steps_completed = []


@dataclass
class BatchResult:
    """Result of batch processing."""
    total_files: int
    successful: int
    failed: int
    results: List[FileResult]
    total_duration_seconds: float
    report_file: Optional[Path] = None
    start_time: Optional[datetime] = None
    end_time: Optional[datetime] = None


class StepTrackingHandler(logging.Handler):
    """Logging handler that captures step information for progress tracking."""
    
    def __init__(self, progress_tracker):
        super().__init__()
        self.progress_tracker = progress_tracker
        self.setLevel(logging.INFO)
    
    def emit(self, record):
        """Extract step information from log messages."""
        msg = record.getMessage()
        
        # Look for step patterns and update progress
        step_mapping = {
            "Step 1:": "Analyzing...",
            "Step 2:": "Translating...",
            "Step 3:": "Reviewing...",
            "Step 4:": "Generating tests...",
            "Step 5:": "Refactoring...",
        }
        
        for pattern, step_name in step_mapping.items():
            if pattern in msg:
                # Update progress tracker with new step
                if self.progress_tracker and self.progress_tracker.current_file_index > 0:
                    self.progress_tracker.update_step(step_name)
                break


class ProgressTracker:
    """Tracks and displays progress for batch processing."""
    
    def __init__(self, total_files: int):
        self.total_files = total_files
        self.current_file_index = 0
        self.current_filename = ""
        self.current_file_lines = 0
        self.start_time = time.time()
        self.file_times: List[float] = []
        self.current_step = "Initializing"
        self.step_handler = None
        
    def update(self, file_index: int, filename: str, file_lines: int, step: str):
        """Update progress display."""
        self.current_file_index = file_index
        self.current_filename = filename
        self.current_file_lines = file_lines
        self.current_step = step
        self._display()
    
    def update_step(self, step: str):
        """Update just the step without changing file info."""
        self.current_step = step
        self._display()
    
    def _display(self):
        """Internal method to display progress."""
        # Get progress bar and ETA
        progress_bar = self.get_progress_bar()
        eta = self.get_eta()
        
        # Format filename (truncate if too long)
        display_filename = self.current_filename[:35] + "..." if len(self.current_filename) > 35 else self.current_filename
        
        # Clear previous line and print new progress
        # Format: [████████░░░░░░░░] 20% (3/15) | ETA: 45 minutes | [3/15] Processing COACTUPC.cbl (4237 lines)... Analyzing...
        display = f"\r{progress_bar} | ETA: {eta} | [{self.current_file_index}/{self.total_files}] Processing {display_filename} ({self.current_file_lines} lines)... {self.current_step}"
        # Truncate to terminal width if needed (assume 120 chars max)
        if len(display) > 120:
            display = display[:117] + "..."
        print(display, end="", flush=True)
    
    def get_progress_bar(self, width: int = 20) -> str:
        """Generate progress bar string."""
        if self.total_files == 0:
            return "[" + "░" * width + "]"
        
        progress = self.current_file_index / self.total_files
        filled = int(progress * width)
        bar = "█" * filled + "░" * (width - filled)
        percentage = int(progress * 100)
        return f"[{bar}] {percentage}% ({self.current_file_index}/{self.total_files})"
    
    def get_eta(self) -> str:
        """Calculate and return estimated time remaining."""
        if self.current_file_index == 0:
            return "Calculating..."
        
        elapsed = time.time() - self.start_time
        avg_time_per_file = elapsed / self.current_file_index
        remaining_files = self.total_files - self.current_file_index
        eta_seconds = avg_time_per_file * remaining_files
        
        if eta_seconds < 60:
            return f"{int(eta_seconds)} seconds"
        elif eta_seconds < 3600:
            minutes = int(eta_seconds / 60)
            return f"{minutes} minute{'s' if minutes != 1 else ''}"
        else:
            hours = int(eta_seconds / 3600)
            minutes = int((eta_seconds % 3600) / 60)
            return f"{hours}h {minutes}m"
    
    def record_file_completion(self, duration: float):
        """Record time taken for a file."""
        self.file_times.append(duration)
    
    def get_display(self, filename: str, file_lines: int) -> str:
        """Get full progress display string."""
        progress_bar = self.get_progress_bar()
        eta = self.get_eta()
        return f"\r{progress_bar} | ETA: {eta} | [{self.current_file_index}/{self.total_files}] Processing {filename} ({file_lines} lines)... {self.current_step}"


def discover_cobol_files(input_path: str, pattern: Optional[str] = None) -> List[Path]:
    """Discover COBOL files from various input types.
    
    Args:
        input_path: File path, directory path, or file list path
        pattern: Optional glob pattern (e.g., "**/*.cbl")
        
    Returns:
        List of COBOL file paths
    """
    path = Path(input_path)
    cobol_files = []
    
    # COBOL file extensions
    cobol_extensions = {'.cbl', '.cob', '.COBOL', '.CBL', '.COB'}
    
    if path.is_file():
        # Single file
        if path.suffix in cobol_extensions:
            cobol_files.append(path)
        elif path.suffix == '.txt':
            # File list - read paths from file
            with open(path, 'r') as f:
                for line in f:
                    line = line.strip()
                    if line and not line.startswith('#'):
                        file_path = Path(line)
                        if file_path.exists() and file_path.suffix in cobol_extensions:
                            cobol_files.append(file_path)
    elif path.is_dir():
        # Directory - recursively find COBOL files
        if pattern:
            cobol_files = list(path.glob(pattern))
        else:
            for ext in cobol_extensions:
                cobol_files.extend(path.rglob(f'*{ext}'))
    elif pattern:
        # Glob pattern
        cobol_files = list(Path('.').glob(pattern))
    
    # Sort by size (smallest first) for better progress visibility
    cobol_files.sort(key=lambda p: p.stat().st_size if p.exists() else 0)
    
    return cobol_files


def count_lines(file_path: Path) -> int:
    """Count lines in a file."""
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            return sum(1 for _ in f)
    except:
        return 0


class BatchModernizationPipeline:
    """Handles batch processing of multiple COBOL files."""
    
    def __init__(self, settings=None):
        """Initialize batch pipeline.
        
        Args:
            settings: Optional settings instance (uses get_settings() if None)
        """
        self.settings = settings or get_settings()
        self.progress_tracker: Optional[ProgressTracker] = None
        
    async def _process_file(
        self, 
        cobol_file: Path, 
        index: int, 
        total: int,
        output_dir: Path
    ) -> FileResult:
        """Process a single COBOL file.
        
        Args:
            cobol_file: Path to COBOL file
            index: Current file index (1-based)
            total: Total number of files
            output_dir: Output directory for generated files
            
        Returns:
            FileResult with processing outcome
        """
        start_time = time.time()
        file_lines = count_lines(cobol_file)
        steps_completed = []
        
        # Update progress
        if self.progress_tracker:
            self.progress_tracker.update(index, cobol_file.name, file_lines, "Starting...")
        
        try:
            # Create pipeline for this file
            pipeline = ModernizationPipeline()
            
            # Set up step tracking via logger
            if self.progress_tracker:
                self.progress_tracker.update(index, cobol_file.name, file_lines, "Initializing...")
                
                # Create and attach step tracking handler
                step_handler = StepTrackingHandler(self.progress_tracker)
                workflow_logger = logging.getLogger("src.workflows.modernization_pipeline")
                workflow_logger.addHandler(step_handler)
                self.progress_tracker.step_handler = step_handler
            
            # Run pipeline (it handles all steps internally)
            # The step handler will capture "Step X:" log messages and update progress
            result = await pipeline.run(str(cobol_file), save_files=True, output_dir=output_dir)
            
            # Remove step handler after processing
            if self.progress_tracker and self.progress_tracker.step_handler:
                workflow_logger = logging.getLogger("src.workflows.modernization_pipeline")
                workflow_logger.removeHandler(self.progress_tracker.step_handler)
                self.progress_tracker.step_handler = None
            
            if "error" in result:
                error_msg = str(result.get("error", "Unknown error"))
                logger.error(f"Error processing {cobol_file}: {error_msg}")
                duration = time.time() - start_time
                if self.progress_tracker:
                    self.progress_tracker.record_file_completion(duration)
                return FileResult(
                    file_path=cobol_file,
                    success=False,
                    error=error_msg,
                    duration_seconds=duration,
                    steps_completed=steps_completed,
                    file_size_lines=file_lines
                )
            
            # Check if files were saved (from saved_files in result)
            output_file = None
            test_file = None
            
            if "saved_files" in result:
                saved_files = result["saved_files"]
                if isinstance(saved_files, dict):
                    # Handle dict format: {"python_file": path, "test_file": path}
                    if "python_file" in saved_files:
                        output_file = Path(saved_files["python_file"])
                    if "test_file" in saved_files:
                        test_file = Path(saved_files["test_file"])
                elif isinstance(saved_files, list):
                    # Handle list format: [Path, Path, ...]
                    for saved_file in saved_files:
                        if isinstance(saved_file, (str, Path)):
                            saved_path = Path(saved_file)
                            if saved_path.suffix == '.py':
                                if saved_path.name.startswith('test_'):
                                    test_file = saved_path
                                else:
                                    output_file = saved_path
            
            # Track completed steps based on what's in result
            if result.get("analysis"):
                steps_completed.append("analysis")
            if result.get("translation"):
                steps_completed.append("translation")
            if result.get("review"):
                steps_completed.append("review")
            if result.get("tests"):
                steps_completed.append("tests")
            if result.get("refactored"):
                steps_completed.append("refactored")
            
            duration = time.time() - start_time
            if self.progress_tracker:
                self.progress_tracker.record_file_completion(duration)
                self.progress_tracker.update(index, cobol_file.name, file_lines, "Complete ✓")
            
            return FileResult(
                file_path=cobol_file,
                success=True,
                output_file=output_file,
                test_file=test_file,
                duration_seconds=duration,
                steps_completed=steps_completed,
                file_size_lines=file_lines
            )
            
        except Exception as e:
            error_msg = str(e)
            logger.exception(f"Exception processing {cobol_file}: {error_msg}")
            duration = time.time() - start_time
            if self.progress_tracker:
                self.progress_tracker.record_file_completion(duration)
            return FileResult(
                file_path=cobol_file,
                success=False,
                error=error_msg,
                duration_seconds=duration,
                steps_completed=steps_completed,
                file_size_lines=file_lines
            )
    
    async def _process_files_sequential(
        self, 
        files: List[Path], 
        output_dir: Path
    ) -> List[FileResult]:
        """Process files sequentially with delays.
        
        Args:
            files: List of COBOL file paths
            output_dir: Output directory for generated files
            
        Returns:
            List of FileResult objects
        """
        results = []
        total = len(files)
        
        for index, cobol_file in enumerate(files, start=1):
            # Process file
            result = await self._process_file(cobol_file, index, total, output_dir)
            results.append(result)
            
            # Check if we should stop on error
            if not result.success and not self.settings.batch_continue_on_error:
                logger.error(f"Stopping batch processing due to error in {cobol_file.name}")
                # Mark remaining files as not processed
                for remaining_file in files[index:]:
                    results.append(FileResult(
                        file_path=remaining_file,
                        success=False,
                        error="Processing stopped due to previous error",
                        duration_seconds=0.0,
                        file_size_lines=count_lines(remaining_file)
                    ))
                break
            
            # Add delay between files (except after last file)
            if index < total and self.settings.batch_file_delay_seconds > 0:
                if self.progress_tracker:
                    # Clear progress line and show delay message
                    print(f"\r{' ' * 120}\r", end="", flush=True)
                    print(f"Waiting {self.settings.batch_file_delay_seconds}s before next file...", end="", flush=True)
                await asyncio.sleep(self.settings.batch_file_delay_seconds)
                # Clear delay message
                if self.progress_tracker:
                    print(f"\r{' ' * 120}\r", end="", flush=True)
        
        return results
    
    async def _save_batch_report(
        self, 
        results: List[FileResult], 
        output_path: Path,
        start_time: datetime,
        end_time: datetime
    ) -> Path:
        """Save batch processing report to JSON file.
        
        Args:
            results: List of FileResult objects
            output_path: Directory to save report
            start_time: Batch start time
            end_time: Batch end time
            
        Returns:
            Path to saved report file
        """
        report_data = {
            "batch_info": {
                "start_time": start_time.isoformat(),
                "end_time": end_time.isoformat(),
                "total_duration_seconds": (end_time - start_time).total_seconds(),
                "total_files": len(results),
                "successful": sum(1 for r in results if r.success),
                "failed": sum(1 for r in results if not r.success),
            },
            "configuration": {
                "batch_file_delay_seconds": self.settings.batch_file_delay_seconds,
                "batch_max_concurrent": self.settings.batch_max_concurrent,
                "batch_continue_on_error": self.settings.batch_continue_on_error,
                "agent_delay_seconds": self.settings.agent_delay_seconds,
                "openai_model": self.settings.openai_model,
            },
            "results": [asdict(result) for result in results]
        }
        
        # Generate report filename
        timestamp = start_time.strftime("%Y%m%d_%H%M%S")
        report_file = output_path / f"batch_report_{timestamp}.json"
        
        # Save report
        with open(report_file, 'w', encoding='utf-8') as f:
            json.dump(report_data, f, indent=2, default=str)
        
        return report_file
    
    def _print_summary(self, batch_result: BatchResult):
        """Print batch processing summary to console.
        
        Args:
            batch_result: BatchResult object
        """
        print("\n" + "="*70)
        print("BATCH MODERNIZATION SUMMARY")
        print("="*70)
        print(f"Total files processed: {batch_result.total_files}")
        print(f"Successful: {batch_result.successful} ({batch_result.successful/batch_result.total_files*100:.1f}%)")
        print(f"Failed: {batch_result.failed} ({batch_result.failed/batch_result.total_files*100:.1f}%)")
        
        # Format duration
        duration = timedelta(seconds=int(batch_result.total_duration_seconds))
        hours, remainder = divmod(duration.seconds, 3600)
        minutes, seconds = divmod(remainder, 60)
        if hours > 0:
            duration_str = f"{hours}h {minutes}m {seconds}s"
        elif minutes > 0:
            duration_str = f"{minutes}m {seconds}s"
        else:
            duration_str = f"{seconds}s"
        
        print(f"Total duration: {duration_str}")
        
        # Show failed files
        failed_files = [r for r in batch_result.results if not r.success]
        if failed_files:
            print("\nFailed files:")
            for i, result in enumerate(failed_files, 1):
                error_preview = result.error[:60] + "..." if result.error and len(result.error) > 60 else (result.error or "Unknown error")
                print(f"  {i}. {result.file_path.name} - {error_preview}")
        
        # Show successful files with timing
        successful_files = [r for r in batch_result.results if r.success]
        if successful_files:
            avg_time = sum(r.duration_seconds for r in successful_files) / len(successful_files)
            print(f"\nAverage processing time per file: {avg_time:.1f}s")
        
        if batch_result.report_file:
            print(f"\nDetailed report saved to: {batch_result.report_file}")
        
        print("="*70)
    
    async def run_batch(
        self, 
        cobol_files: List[Path], 
        output_dir: Optional[Path] = None
    ) -> BatchResult:
        """Run batch modernization on multiple COBOL files.
        
        Args:
            cobol_files: List of COBOL file paths to process
            output_dir: Optional output directory (uses settings.output_path if None)
            
        Returns:
            BatchResult with processing outcomes
        """
        if not cobol_files:
            raise ValueError("No COBOL files provided for batch processing")
        
        output_dir = output_dir or self.settings.output_path
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Initialize progress tracker
        self.progress_tracker = ProgressTracker(len(cobol_files))
        
        start_time = datetime.now()
        print(f"\n{'='*70}")
        print(f"Starting batch modernization of {len(cobol_files)} file(s)")
        print(f"{'='*70}")
        print(f"Output directory: {output_dir}")
        print(f"Delay between files: {self.settings.batch_file_delay_seconds}s")
        print(f"Continue on error: {self.settings.batch_continue_on_error}")
        print("-" * 70)
        print()  # Empty line before progress starts
        
        # Process files
        try:
            results = await self._process_files_sequential(cobol_files, output_dir)
        except KeyboardInterrupt:
            print("\n\nBatch processing interrupted by user.")
            results = []
        
        end_time = datetime.now()
        total_duration = (end_time - start_time).total_seconds()
        
        # Clear progress line and print completion
        print("\r" + " " * 120 + "\r", end="")
        print("Batch processing complete!")
        
        # Generate report
        report_file = None
        if self.settings.batch_report_file or True:  # Always generate report
            report_file = await self._save_batch_report(
                results, 
                output_dir,
                start_time,
                end_time
            )
        
        # Create batch result
        batch_result = BatchResult(
            total_files=len(cobol_files),
            successful=sum(1 for r in results if r.success),
            failed=sum(1 for r in results if not r.success),
            results=results,
            total_duration_seconds=total_duration,
            report_file=report_file,
            start_time=start_time,
            end_time=end_time
        )
        
        # Print summary
        self._print_summary(batch_result)
        
        return batch_result

