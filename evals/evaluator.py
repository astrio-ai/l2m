"""
Evaluation System for L2M Agents

Evaluates generated Python code using test harness templates.
This approach is focused on functional correctness.
"""

import ast
import json
import re
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass, asdict
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
import resource


@dataclass
class TestCase:
    """A single test case with inputs and expected output."""
    inputs: Tuple[Any, ...]
    expected_output: Any
    description: Optional[str] = None


@dataclass
class ExecutionResult:
    """Result of executing a test harness."""
    program_name: str
    success: bool
    n_success: int
    total_tests: int
    execution_time: float
    error_type: Optional[str] = None  # "compilation_error", "runtime_error", "timeout", "logical_error"
    error_message: Optional[str] = None
    stdout: Optional[str] = None
    stderr: Optional[str] = None
    validation_type: Optional[str] = None  # "ground_truth" or "basic_validation"


@dataclass
class EvaluationMetrics:
    """Evaluation metrics for all programs."""
    timestamp: str
    total_programs: int
    ca_at_1: float  # Correctness at 1 (percentage)
    compilation_error_rate: float
    runtime_error_rate: float
    timeout_rate: float
    logical_error_rate: float
    avg_execution_time: float
    avg_test_cases_passed: float
    program_results: List[Dict[str, Any]]


class Evaluator:
    """Evaluates generated code using test harnesses."""
    
    def __init__(
        self,
        data_dir: Path = Path("data"),
        output_dir: Path = Path("data/output"),
        results_dir: Path = Path("evals/results"),
        timeout_seconds: int = 30,
        memory_limit_mb: int = 512
    ):
        """Initialize evaluator.
        
        Args:
            data_dir: Directory containing ground truth files
            output_dir: Directory containing generated Python files
            results_dir: Directory to store evaluation results
            timeout_seconds: Timeout for test execution
            memory_limit_mb: Memory limit in MB
        """
        self.data_dir = Path(data_dir)
        self.output_dir = Path(output_dir)
        self.results_dir = Path(results_dir)
        self.timeout_seconds = timeout_seconds
        self.memory_limit_mb = memory_limit_mb
        self.results_dir.mkdir(parents=True, exist_ok=True)
    
    def extract_main_function(self, code: str) -> Optional[Tuple[str, str]]:
        """Extract the main entry point function from generated code.
        
        Args:
            code: Python code string
            
        Returns:
            Tuple of (function_name, function_code) or None if not found
        """
        try:
            tree = ast.parse(code)
            
            # Look for main() function or run() method in main class
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef):
                    if node.name == "main":
                        # Extract the function code
                        func_code = ast.get_source_segment(code, node)
                        return ("main", func_code)
                
                # Look for class with run() method (like MarblesCostProgram)
                if isinstance(node, ast.ClassDef):
                    for item in node.body:
                        if isinstance(item, ast.FunctionDef) and item.name == "run":
                            # Get the class and method
                            class_code = ast.get_source_segment(code, node)
                            return (f"{node.name}.run", class_code)
            
            # If no main() or run() found, look for any top-level function
            # that might be the entry point
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef) and not any(
                    isinstance(parent, ast.ClassDef) for parent in ast.walk(tree)
                    if hasattr(parent, 'body') and node in getattr(parent, 'body', [])
                ):
                    # Top-level function - use the first substantial one
                    func_code = ast.get_source_segment(code, node)
                    if func_code and len(func_code) > 50:  # Substantial function
                        return (node.name, func_code)
            
            return None
        except SyntaxError as e:
            return None
    
    def extract_test_cases_from_ground_truth(
        self, 
        ground_truth_test_file: Path,
        ground_truth_python_file: Path
    ) -> List[TestCase]:
        """Extract test cases from ground truth test file.
        
        Args:
            ground_truth_test_file: Path to ground truth test file
            ground_truth_python_file: Path to ground truth Python file
            
        Returns:
            List of TestCase objects
        """
        test_cases = []
        
        try:
            test_content = ground_truth_test_file.read_text(encoding="utf-8")
            ground_truth_code = ground_truth_python_file.read_text(encoding="utf-8")
            
            # Parse pytest test file to extract test cases
            # Look for patterns like:
            # 1. program.run(["file.py", "arg1", "arg2", ...])
            # 2. main(filea_filename=..., mock_request_callback=...)
            # 3. result = main(...)
            
            # Pattern 1: program.run([...])
            run_pattern = r'program\.run\(\[(.*?)\]\)'
            
            # Pattern 2: main(...) with keyword arguments
            main_kwargs_pattern = r'main\s*\((.*?)\)'
            
            # Pattern 3: Any function call that might be the entry point
            # Look for common function names: hello, execute, run, etc.
            # This is a fallback for programs that don't use main() or program.run()
            function_call_pattern = r'(\w+)\s*\((.*?)\)'
            
            # Split by test functions to get context
            test_function_pattern = r'def\s+(test_\w+)\([^)]*\):'
            test_functions = list(re.finditer(test_function_pattern, test_content))
            
            for i, test_func_match in enumerate(test_functions):
                # Get the content of this test function
                start_pos = test_func_match.end()
                end_pos = test_functions[i + 1].start() if i + 1 < len(test_functions) else len(test_content)
                test_func_content = test_content[start_pos:end_pos]
                
                # Find all program.run() calls in this test
                run_matches = list(re.finditer(run_pattern, test_func_content, re.DOTALL))
                
                # Find all main() calls with keyword arguments
                main_matches = list(re.finditer(main_kwargs_pattern, test_func_content, re.DOTALL))
                
                for run_match in run_matches:
                    args_str = run_match.group(1)
                    # Parse arguments - handle both single and double quotes, and multiline
                    args_str = args_str.strip()
                    # Remove newlines and extra whitespace
                    args_str = re.sub(r'\s+', ' ', args_str)
                    
                    # Parse arguments more carefully
                    args = []
                    current_arg = ""
                    in_quotes = False
                    quote_char = None
                    
                    for char in args_str:
                        if char in ['"', "'"] and (not in_quotes or char == quote_char):
                            if in_quotes:
                                args.append(current_arg)
                                current_arg = ""
                                in_quotes = False
                                quote_char = None
                            else:
                                in_quotes = True
                                quote_char = char
                        elif char == ',' and not in_quotes:
                            if current_arg.strip():
                                args.append(current_arg.strip())
                                current_arg = ""
                        else:
                            current_arg += char
                    
                    if current_arg.strip():
                        args.append(current_arg.strip())
                    
                    # Clean up arguments
                    args = [arg.strip().strip('"').strip("'") for arg in args if arg.strip()]
                    
                    # Look for expected output in the lines after this run() call
                    run_end = run_match.end()
                    next_lines = test_func_content[run_end:run_end+300].split('\n')
                    
                    expected_output = None
                    for line in next_lines[:15]:  # Check next 15 lines
                        # Look for assert statements
                        if 'assert' in line:
                            # Check for CONST_SUCCESS
                            if 'CONST_SUCCESS' in line or 'result == CONST_SUCCESS' in line or '== CONST_SUCCESS' in line:
                                expected_output = 'SUCCESS'
                                break
                            # Check for ERROR constants
                            elif 'ERROR_MARBLE_DNE' in line:
                                expected_output = 'MRBC001E'
                                break
                            elif 'ERROR_MARBLE_EXISTS' in line:
                                expected_output = 'MRBC002E'
                                break
                            # Try to extract string literal from assert
                            str_matches = re.findall(r"['\"]([^'\"]+)['\"]", line)
                            if str_matches:
                                # Take the last string (usually the expected value)
                                expected_output = str_matches[-1]
                                break
                        
                        # Also check for f.getvalue() patterns
                        if 'f.getvalue()' in line or 'result' in line.lower():
                            # Look for comparison with string
                            str_match = re.search(r"==\s*['\"]([^'\"]+)['\"]", line)
                            if str_match:
                                expected_output = str_match.group(1)
                                break
                    
                    if args:
                        test_cases.append(TestCase(
                            inputs=(args,),
                            expected_output=expected_output,
                            description=f"{test_func_match.group(1)}: {args[1:] if len(args) > 1 else args}"
                        ))
                
                # Process main() calls - extract ALL main() calls, not just specific ones
                for main_match in main_matches:
                    kwargs_str = main_match.group(1).strip()
                    
                    # Check if this is a main() call (with or without args)
                    if kwargs_str == "":
                        # main() with no arguments
                        test_cases.append(TestCase(
                            inputs=(),
                            expected_output=None,
                            description=f"{test_func_match.group(1)}: main()"
                        ))
                    elif '=' in kwargs_str:
                        # This is a keyword argument call - extract all keyword arguments
                        # Store as a special format to indicate this is a keyword call
                        test_cases.append(TestCase(
                            inputs=({"main_kwargs": kwargs_str},),  # Store kwargs as string
                            expected_output=None,  # Don't assume return value
                            description=f"{test_func_match.group(1)}: main() with kwargs"
                        ))
                    else:
                        # Positional arguments (less common, but handle it)
                        # Parse as positional args
                        args = [arg.strip().strip('"').strip("'") for arg in kwargs_str.split(',') if arg.strip()]
                        test_cases.append(TestCase(
                            inputs=(args,),
                            expected_output=None,
                            description=f"{test_func_match.group(1)}: main() with args"
                        ))
            
            # Pattern 3: Extract function calls like hello(), execute(), etc.
            # This handles programs that don't use main() or program.run()
            if not test_cases:
                # Try to find function calls in test functions
                for test_func_match in test_functions:
                    start_pos = test_func_match.end()
                    end_pos = test_functions[test_functions.index(test_func_match) + 1].start() if test_functions.index(test_func_match) + 1 < len(test_functions) else len(test_content)
                    test_func_content = test_content[start_pos:end_pos]
                    
                    # Look for function calls (but skip main() and program.run() as we already handled those)
                    func_matches = list(re.finditer(function_call_pattern, test_func_content, re.DOTALL))
                    for func_match in func_matches:
                        func_name = func_match.group(1)
                        func_args = func_match.group(2).strip()
                        
                        # Skip if it's main() or program.run() (already handled)
                        if func_name == 'main' or func_name == 'run':
                            continue
                        
                        # Check if this function exists in the ground truth code
                        if f'def {func_name}(' in ground_truth_code or f'def {func_name}(' in test_content:
                            # This looks like an entry point function
                            if func_args == "":
                                # Function call with no arguments
                                test_cases.append(TestCase(
                                    inputs=(),
                                    expected_output=None,
                                    description=f"{test_func_match.group(1)}: {func_name}()"
                                ))
                            elif '=' in func_args:
                                # Keyword arguments
                                test_cases.append(TestCase(
                                    inputs=({f"{func_name}_kwargs": func_args},),
                                    expected_output=None,
                                    description=f"{test_func_match.group(1)}: {func_name}() with kwargs"
                                ))
            
            # If no test cases found, return empty list
            # Don't create default test cases - let the caller handle it
            # This prevents using wrong test cases for different programs
            
        except Exception as e:
            # Log warning but return empty list - let caller decide what to do
            # Don't create default test cases that might be wrong for this program
            print(f"Warning: Could not extract test cases from {ground_truth_test_file}: {e}")
            return []
        
        return test_cases
    
    def create_test_harness(
        self,
        program_name: str,
        ground_truth_code: str,
        generated_code: str,
        test_cases: List[TestCase],
        data_dir: Optional[Path] = None
    ) -> str:
        """Create a test harness template with f_gold and #TOFILL marker.
        
        Args:
            program_name: Name of the program
            ground_truth_code: Ground truth Python code
            generated_code: Generated Python code
            test_cases: List of test cases
            
        Returns:
            Test harness code as string
        """
        # Remove if __name__ blocks from ground truth code to prevent execution
        import re
        gt_lines = ground_truth_code.split('\n')
        gt_filtered = []
        for gt_line in gt_lines:
            if re.match(r'\s*if\s+__name__\s*==\s*["\']__main__["\']\s*:', gt_line):
                break
            gt_filtered.append(gt_line)
        ground_truth_code = '\n'.join(gt_filtered)
        # Check if code uses class-based structure with run() method
        has_class_run = False
        class_name = None
        
        try:
            tree = ast.parse(ground_truth_code)
            for node in ast.walk(tree):
                if isinstance(node, ast.ClassDef):
                    for item in node.body:
                        if isinstance(item, ast.FunctionDef) and item.name == "run":
                            has_class_run = True
                            class_name = node.name
                            break
                    if has_class_run:
                        break
        except:
            pass
        
        # Get absolute path to data directory
        if data_dir is None:
            data_dir = self.data_dir
        data_dir_abs = str(data_dir.resolve())
        
        # Create harness based on structure
        if has_class_run and class_name:
            # Class-based program with run() method
            class_name_var = class_name  # Store in variable to use in f-string
            harness = f'''# Test harness for {program_name}
import sys
import io
import tempfile
import os
from pathlib import Path
from contextlib import redirect_stdout

# Add data directories to Python path to resolve module imports
# This allows ground truth code to import modules like CSCVDLTI, etc.
data_dir_path = r"{data_dir_abs}"
if os.path.exists(data_dir_path):
    sys.path.insert(0, data_dir_path)
    # Also add subdirectories
    for item in os.listdir(data_dir_path):
        subdir = os.path.join(data_dir_path, item)
        if os.path.isdir(subdir):
            sys.path.insert(0, subdir)

# Ground truth reference implementation
{ground_truth_code}

def f_gold(*args):
    """Ground truth implementation."""
    args_list = args[0] if args else ["program.py"]
    # Create temporary database for each test
    import tempfile
    with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
        db_path = f.name
    try:
        # Try to instantiate with db_path, fallback to default
        try:
            import inspect
            sig = inspect.signature({class_name_var}.__init__)
            if len(sig.parameters) > 1:  # More than just 'self'
                program = {class_name_var}(db_path)
            else:
                program = {class_name_var}()
                # Update db path if program has db attribute
                if hasattr(program, 'db') and hasattr(program.db, 'db_path'):
                    program.db.close()
                    import sqlite3
                    program.db.db_path = db_path
                    program.db.conn = sqlite3.connect(db_path)
        except:
            program = {class_name_var}()
        f = io.StringIO()
        with redirect_stdout(f):
            program.run(args_list)
        result = f.getvalue().strip()
        # Clean up
        if hasattr(program, 'db') and hasattr(program.db, 'close'):
            program.db.close()
        if os.path.exists(db_path):
            os.unlink(db_path)
        return result
    except Exception as e:
        if os.path.exists(db_path):
            os.unlink(db_path)
        raise

# Placeholder for generated code
#TOFILL

# Test cases
if __name__ == '__main__':
    test_cases = [
'''
        else:
            # Function-based program
            # Check if test cases use keyword arguments (main_kwargs format)
            uses_kwargs = any(
                tc.inputs and len(tc.inputs) > 0 and isinstance(tc.inputs[0], dict) and "main_kwargs" in tc.inputs[0]
                for tc in test_cases
            ) if test_cases else False
            
            if uses_kwargs:
                # Handle main() with keyword arguments
                harness = f'''# Test harness for {program_name}
import sys
import tempfile
import os
from pathlib import Path

# Add data directories to Python path to resolve module imports
# This allows ground truth code to import modules like CSCVDLTI, etc.
import os
data_dir_path = r"{data_dir_abs}"
if os.path.exists(data_dir_path):
    sys.path.insert(0, data_dir_path)
    # Also add subdirectories
    for item in os.listdir(data_dir_path):
        subdir = os.path.join(data_dir_path, item)
        if os.path.isdir(subdir):
            sys.path.insert(0, subdir)

# Ground truth reference implementation
{ground_truth_code}

def f_gold(*args):
    """Ground truth implementation."""
    if not args or not isinstance(args[0], dict) or "main_kwargs" not in args[0]:
        return ""
    
    kwargs_str = args[0]["main_kwargs"]
    # Parse keyword arguments string into a dict
    # Simple parsing: extract key=value pairs
    kwargs = {{}}
    temp_file = None
    # Use eval to parse the kwargs string (safe in this context as it's from test file)
    try:
        # Replace variable names with their values from the test context
        # For tempfile cases, we need to create a temp file
        if "temp_filename" in kwargs_str:
            # Create a temp file for this test
            with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
                temp_file = f.name
            kwargs_str = kwargs_str.replace("temp_filename", "'" + temp_file + "'")
        
        # Parse the kwargs string
        # This is a simplified parser - in production you'd want something more robust
        import re
        # Extract key=value pairs
        pattern = r'(\w+)\s*=\s*([^,)]+)'  # noqa: W605
        for match in re.finditer(pattern, kwargs_str):
            key = match.group(1)
            value_str = match.group(2).strip()
            # Try to evaluate the value
            try:
                if value_str.startswith("'") or value_str.startswith('"'):
                    # Remove quotes from both ends
                    value = value_str.strip('"').strip("'")
                elif value_str == "False":
                    value = False
                elif value_str == "True":
                    value = True
                elif value_str == "None":
                    value = None
                elif value_str.isdigit():
                    value = int(value_str)
                else:
                    # It's a variable or function - we'll need to handle this differently
                    # For now, skip complex expressions
                    continue
                kwargs[key] = value
            except:
                pass
        
        # Call main with kwargs
        # Check if main accepts keyword arguments
        import inspect
        try:
            sig = inspect.signature(main)
            # If main accepts kwargs, call with kwargs
            if any(p.kind == inspect.Parameter.VAR_KEYWORD for p in sig.parameters.values()):
                result = main(**kwargs)
            elif len(sig.parameters) > 0:
                # Try to match parameters
                result = main(**kwargs)
            else:
                # main() doesn't accept arguments - this is a mismatch
                return "ERROR: Generated main() does not accept keyword arguments"
        except TypeError:
            # main() doesn't accept these kwargs
            return "ERROR: Generated main() signature does not match expected keyword arguments"
        except Exception as e:
            raise
        
        # Clean up temp file if created
        if temp_file and os.path.exists(temp_file):
            os.unlink(temp_file)
        
        return str(result) if result is not None else ""
    except Exception as e:
        if temp_file and os.path.exists(temp_file):
            os.unlink(temp_file)
        return f"ERROR: {{str(e)}}"

# Placeholder for generated code
#TOFILL

# Test cases
if __name__ == '__main__':
    test_cases = [
'''
            else:
                # Standard function-based (uses sys.argv)
                harness = f'''# Test harness for {program_name}
import sys
from pathlib import Path

# Add data directories to Python path to resolve module imports
# This allows ground truth code to import modules like CSCVDLTI, etc.
import os
data_dir_path = r"{data_dir_abs}"
if os.path.exists(data_dir_path):
    sys.path.insert(0, data_dir_path)
    # Also add subdirectories
    for item in os.listdir(data_dir_path):
        subdir = os.path.join(data_dir_path, item)
        if os.path.isdir(subdir):
            sys.path.insert(0, subdir)

# Ground truth reference implementation
{ground_truth_code}

def f_gold(*args):
    """Ground truth implementation."""
    # Try to find and call main function
    if 'main' in globals():
        import sys
        old_argv = sys.argv
        try:
            sys.argv = list(args[0]) if args else ["program.py"]
            result = main()
            return result if result is not None else ""
        finally:
            sys.argv = old_argv
    else:
        return ""

# Placeholder for generated code
#TOFILL

# Test cases
if __name__ == '__main__':
    test_cases = [
'''
        
        # Add test cases
        for i, test_case in enumerate(test_cases):
            # Handle empty inputs tuple
            if test_case.inputs and len(test_case.inputs) > 0:
                inputs_repr = repr(test_case.inputs[0]) if len(test_case.inputs) == 1 else repr(test_case.inputs)
            else:
                inputs_repr = "()"
            expected_repr = repr(test_case.expected_output) if test_case.expected_output else "None"
            harness += f"        ({inputs_repr}, {expected_repr}),  # {test_case.description or f'Test {i+1}'}\n"
        
        harness += '''    ]
    
    n_success = 0
    for inputs, expected_output in test_cases:
        try:
            generated_output = f_filled(*inputs)
            gold_output = f_gold(*inputs)
            
            # Compare outputs (handle None and empty string)
            if generated_output == gold_output:
                n_success += 1
        except Exception as e:
            pass  # Execution error
    
    print(f"#Results: {n_success}, {len(test_cases)}")
'''
        
        return harness
    
    def inject_generated_code(self, harness: str, generated_code: str) -> str:
        """Inject generated code into test harness at #TOFILL marker.
        
        Args:
            harness: Test harness code
            generated_code: Generated Python code to inject
            
        Returns:
            Harness with injected code
        """
        # Remove if __name__ == "__main__" blocks from generated code
        # These will execute when the harness runs, which we don't want
        import re
        # Simple approach: find the if __name__ line and remove it and everything after it
        # (since it's typically at the end of the file)
        lines = generated_code.split('\n')
        filtered_lines = []
        found_main_block = False
        
        for i, line in enumerate(lines):
            # Check if this line starts an if __name__ == "__main__" block
            if re.match(r'\s*if\s+__name__\s*==\s*["\']__main__["\']\s*:', line):
                found_main_block = True
                # Skip this line and everything after it
                break
            else:
                filtered_lines.append(line)
        
        generated_code = '\n'.join(filtered_lines)
        
        # Check if generated code uses class-based structure
        has_class_run = False
        class_name = None
        
        try:
            tree = ast.parse(generated_code)
            for node in ast.walk(tree):
                if isinstance(node, ast.ClassDef):
                    for item in node.body:
                        if isinstance(item, ast.FunctionDef) and item.name == "run":
                            has_class_run = True
                            class_name = node.name
                            break
                    if has_class_run:
                        break
        except:
            pass
        
        # Create f_filled function
        if has_class_run and class_name:
            # Include the entire generated code and create wrapper
            class_name_var = class_name  # Store in variable to use in f-string
            injected_code = f'''# Generated code
{generated_code}

def f_filled(*args):
    """Generated implementation."""
    args_list = args[0] if args else ["program.py"]
    import io
    import tempfile
    import os
    from contextlib import redirect_stdout
    
    # Create temporary database for each test
    with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
        db_path = f.name
    
    try:
        # Check if class constructor takes db_path
        import inspect
        sig = inspect.signature({class_name_var}.__init__)
        if len(sig.parameters) > 1:  # More than just 'self'
            program = {class_name_var}(db_path)
        else:
            program = {class_name_var}()
            # If program has db attribute, update its path
            if hasattr(program, 'db') and hasattr(program.db, 'db_path'):
                program.db.db_path = db_path
                program.db.close()
                import sqlite3
                program.db.conn = sqlite3.connect(db_path)
        
        f = io.StringIO()
        with redirect_stdout(f):
            program.run(args_list)
        result = f.getvalue().strip()
        
        # Clean up
        if hasattr(program, 'db') and hasattr(program.db, 'close'):
            program.db.close()
        if os.path.exists(db_path):
            os.unlink(db_path)
        return result
    except Exception as e:
        if os.path.exists(db_path):
            os.unlink(db_path)
        raise
'''
        else:
            # Function-based - include code and create wrapper
            # Check if harness uses keyword arguments
            uses_kwargs = "main_kwargs" in harness
            
            if uses_kwargs:
                # Handle main() with keyword arguments (same logic as f_gold)
                injected_code = f'''# Generated code
{generated_code}

def f_filled(*args):
    """Generated implementation."""
    if not args or not isinstance(args[0], dict) or "main_kwargs" not in args[0]:
        return ""
    
    kwargs_str = args[0]["main_kwargs"]
    # Parse keyword arguments string into a dict
    kwargs = {{}}
    import tempfile
    import os
    import re
    temp_file = None
    
    try:
        # Handle tempfile cases
        if "temp_filename" in kwargs_str:
            with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
                temp_file = f.name
            kwargs_str = kwargs_str.replace("temp_filename", "'" + temp_file + "'")
        
        # Parse key=value pairs
        pattern = r'(\w+)\s*=\s*([^,)]+)'  # noqa: W605
        for match in re.finditer(pattern, kwargs_str):
            key = match.group(1)
            value_str = match.group(2).strip()
            try:
                if value_str.startswith("'") or value_str.startswith('"'):
                    # Remove quotes from both ends
                    value = value_str.strip('"').strip("'")
                elif value_str == "False":
                    value = False
                elif value_str == "True":
                    value = True
                elif value_str == "None":
                    value = None
                elif value_str.isdigit():
                    value = int(value_str)
                else:
                    continue
                kwargs[key] = value
            except:
                pass
        
        # Call main with kwargs
        # Check if main accepts keyword arguments
        import inspect
        try:
            sig = inspect.signature(main)
            # If main accepts kwargs, call with kwargs
            if any(p.kind == inspect.Parameter.VAR_KEYWORD for p in sig.parameters.values()):
                result = main(**kwargs)
            elif len(sig.parameters) > 0:
                # Try to match parameters
                result = main(**kwargs)
            else:
                # main() doesn't accept arguments - this is a mismatch
                return "ERROR: Generated main() does not accept keyword arguments"
        except TypeError:
            # main() doesn't accept these kwargs
            return "ERROR: Generated main() signature does not match expected keyword arguments"
        except Exception as e:
            raise
        
        # Clean up temp file if created
        if temp_file and os.path.exists(temp_file):
            os.unlink(temp_file)
        
        return str(result) if result is not None else ""
    except Exception as e:
        if temp_file and os.path.exists(temp_file):
            os.unlink(temp_file)
        return f"ERROR: {{str(e)}}"
'''
            else:
                # Standard function-based (uses sys.argv)
                injected_code = f'''# Generated code
{generated_code}

def f_filled(*args):
    """Generated implementation."""
    if 'main' in globals():
        import sys
        old_argv = sys.argv
        try:
            sys.argv = list(args[0]) if args else ["program.py"]
            result = main()
            return result if result is not None else ""
        finally:
            sys.argv = old_argv
    else:
        return ""
'''
        
        # Replace #TOFILL marker
        if "#TOFILL" in harness:
            harness = harness.replace("#TOFILL", injected_code)
        else:
            # Append if marker not found
            harness = harness.replace("# Placeholder for generated code", f"# Generated code\n{injected_code}")
        
        return harness
    
    def execute_harness(
        self,
        harness_code: str,
        program_name: str
    ) -> ExecutionResult:
        """Execute test harness with timeout and memory limits.
        
        Args:
            harness_code: Complete test harness code
            program_name: Name of the program
            
        Returns:
            ExecutionResult
        """
        start_time = time.time()
        
        # Create temporary file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            f.write(harness_code)
            temp_file = Path(f.name)
        
        try:
            # Set memory limit (only on Unix-like systems, and handle errors gracefully)
            def set_memory_limit():
                try:
                    limit = self.memory_limit_mb * 1024 * 1024  # Convert to bytes
                    resource.setrlimit(resource.RLIMIT_AS, (limit, limit))
                except (ValueError, OSError) as e:
                    # On macOS, RLIMIT_AS might not be supported or might fail
                    # Just continue without setting the limit
                    pass
            
            # Execute with timeout
            # Only use preexec_fn on Unix-like systems (not Windows or macOS with issues)
            use_preexec = sys.platform not in ('win32', 'darwin')
            result = subprocess.run(
                [sys.executable, str(temp_file)],
                capture_output=True,
                text=True,
                timeout=self.timeout_seconds,
                preexec_fn=set_memory_limit if use_preexec else None
            )
            
            execution_time = time.time() - start_time
            
            # Parse output
            stdout = result.stdout
            stderr = result.stderr
            
            # Look for #Results: pattern
            results_match = re.search(r'#Results:\s*(\d+),\s*(\d+)', stdout)
            
            if results_match:
                n_success = int(results_match.group(1))
                total_tests = int(results_match.group(2))
                success = n_success == total_tests
                
                return ExecutionResult(
                    program_name=program_name,
                    success=success,
                    n_success=n_success,
                    total_tests=total_tests,
                    execution_time=execution_time,
                    error_type=None if success else "logical_error",
                    stdout=stdout,
                    stderr=stderr
                )
            else:
                # Check for compilation/runtime errors
                if result.returncode != 0:
                    if "SyntaxError" in stderr or "IndentationError" in stderr:
                        error_type = "compilation_error"
                    else:
                        error_type = "runtime_error"
                    
                    return ExecutionResult(
                        program_name=program_name,
                        success=False,
                        n_success=0,
                        total_tests=0,
                        execution_time=execution_time,
                        error_type=error_type,
                        error_message=stderr[:500],
                        stdout=stdout,
                        stderr=stderr
                    )
                else:
                    # No results pattern found but execution succeeded
                    return ExecutionResult(
                        program_name=program_name,
                        success=False,
                        n_success=0,
                        total_tests=0,
                        execution_time=execution_time,
                        error_type="logical_error",
                        error_message="No #Results pattern found in output",
                        stdout=stdout,
                        stderr=stderr
                    )
        
        except subprocess.TimeoutExpired:
            return ExecutionResult(
                program_name=program_name,
                success=False,
                n_success=0,
                total_tests=0,
                execution_time=self.timeout_seconds,
                error_type="timeout",
                error_message=f"Execution timeout after {self.timeout_seconds} seconds"
            )
        except MemoryError:
            return ExecutionResult(
                program_name=program_name,
                success=False,
                n_success=0,
                total_tests=0,
                execution_time=time.time() - start_time,
                error_type="runtime_error",
                error_message="Memory limit exceeded"
            )
        except Exception as e:
            return ExecutionResult(
                program_name=program_name,
                success=False,
                n_success=0,
                total_tests=0,
                execution_time=time.time() - start_time,
                error_type="runtime_error",
                error_message=str(e)
            )
        finally:
            # Clean up temp file
            if temp_file.exists():
                temp_file.unlink()
    
    def evaluate_program(
        self,
        program_name: str,
        ground_truth_python: Path,
        ground_truth_test: Optional[Path],
        generated_python: Path
    ) -> ExecutionResult:
        """Evaluate a single program.
        
        Args:
            program_name: Name of the program
            ground_truth_python: Path to ground truth Python file
            ground_truth_test: Optional path to ground truth test file
            generated_python: Path to generated Python file
            
        Returns:
            ExecutionResult
        """
        # Read files
        ground_truth_code = ground_truth_python.read_text(encoding="utf-8")
        generated_code = generated_python.read_text(encoding="utf-8")
        
        # Extract test cases
        test_cases = []
        if ground_truth_test and ground_truth_test.exists():
            test_cases = self.extract_test_cases_from_ground_truth(
                ground_truth_test,
                ground_truth_python
            )
        
        # If no test cases extracted, try to create a minimal generic test case
        # This handles cases where test extraction fails but we still want to evaluate
        if not test_cases:
            # Try to infer a simple test case from the code structure
            # Check if it's a class-based program with run() method
            has_run_method = 'def run(' in ground_truth_code or '.run(' in ground_truth_code
            has_main_function = 'def main(' in ground_truth_code
            
            # Try to find any function that might be the entry point
            # Look for common patterns: main, run, or any function defined at module level
            import ast
            entry_function = None
            try:
                tree = ast.parse(ground_truth_code)
                for node in ast.walk(tree):
                    if isinstance(node, ast.FunctionDef) and not any(isinstance(p, ast.ClassDef) for p in ast.walk(tree) if hasattr(p, 'body') and node in getattr(p, 'body', [])):
                        # Found a top-level function
                        if node.name in ['main', 'run', 'hello', 'execute'] or node.name.lower().startswith('main'):
                            entry_function = node.name
                            break
            except:
                pass
            
            if has_run_method:
                # Class-based program - try with minimal args
                test_cases = [TestCase(
                    inputs=(["program.py"],),
                    expected_output=None,
                    description="Minimal test case (no specific test cases found)"
                )]
            elif has_main_function:
                # Function-based program - try calling main with no args
                test_cases = [TestCase(
                    inputs=(),
                    expected_output=None,
                    description="Minimal test case (no specific test cases found)"
                )]
            elif entry_function:
                # Found an entry function - try calling it with no args
                test_cases = [TestCase(
                    inputs=(),
                    expected_output=None,
                    description=f"Minimal test case calling {entry_function}() (no specific test cases found)"
                )]
            else:
                # Last resort: create a minimal test case that tries to call main or run
                # This will at least attempt to execute the code
                test_cases = [TestCase(
                    inputs=(),
                    expected_output=None,
                    description="Minimal test case (no specific test cases found, attempting generic execution)"
                )]
        
        # Create test harness
        # Get the data directory from the ground truth file path
        data_dir_for_harness = ground_truth_python.parent.parent if ground_truth_python.parent.name != "data" else ground_truth_python.parent
        harness = self.create_test_harness(
            program_name,
            ground_truth_code,
            generated_code,
            test_cases,
            data_dir=data_dir_for_harness
        )
        
        # Inject generated code
        harness = self.inject_generated_code(harness, generated_code)
        
        # Execute harness
        result = self.execute_harness(harness, program_name)
        result.validation_type = "ground_truth"
        
        return result
    
    def validate_without_ground_truth(
        self,
        program_name: str,
        generated_file: Path
    ) -> ExecutionResult:
        """Validate generated code without ground truth using basic checks.
        
        This performs:
        1. Syntax/compilation check
        2. Import test
        3. Basic execution test (if main function exists)
        4. Code quality checks (type hints, docstrings)
        
        Args:
            program_name: Name of the program
            generated_file: Path to generated Python file
            
        Returns:
            ExecutionResult with validation results
        """
        start_time = time.time()
        tests_passed = 0
        total_tests = 0
        error_type = None
        error_message = None
        
        try:
            code = generated_file.read_text(encoding="utf-8")
            
            # Test 1: Syntax/Compilation Check
            total_tests += 1
            try:
                ast.parse(code)
                tests_passed += 1
            except SyntaxError as e:
                error_type = "compilation_error"
                error_message = f"Syntax error: {str(e)}"
                return ExecutionResult(
                    program_name=program_name,
                    success=False,
                    n_success=tests_passed,
                    total_tests=total_tests,
                    execution_time=time.time() - start_time,
                    error_type=error_type,
                    error_message=error_message,
                    validation_type="basic_validation"
                )
            
            # Test 2: Import Test
            total_tests += 1
            try:
                # Create a temporary module to test import
                import tempfile
                import importlib.util
                import sys
                
                # Create temp file
                with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
                    f.write(code)
                    temp_path = f.name
                
                try:
                    # Try to load as module
                    spec = importlib.util.spec_from_file_location(f"test_{program_name}", temp_path)
                    if spec and spec.loader:
                        module = importlib.util.module_from_spec(spec)
                        spec.loader.exec_module(module)
                        tests_passed += 1
                    else:
                        error_type = "compilation_error"
                        error_message = "Could not create module spec"
                finally:
                    import os
                    if os.path.exists(temp_path):
                        os.unlink(temp_path)
            except Exception as e:
                error_type = "compilation_error"
                error_message = f"Import error: {str(e)}"
                # Don't return yet - continue with other tests
            
            # Test 3: Basic Execution Test (if main function exists)
            if 'def main' in code or 'if __name__' in code:
                total_tests += 1
                try:
                    # Try to execute with minimal input
                    import subprocess
                    import tempfile
                    import os
                    
                    with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
                        # Remove if __name__ block to prevent direct execution
                        lines = code.split('\n')
                        filtered_lines = []
                        skip_block = False
                        for line in lines:
                            if 'if __name__' in line:
                                skip_block = True
                            if not skip_block:
                                filtered_lines.append(line)
                            elif line.strip() and not line.strip().startswith(' ') and not line.strip().startswith('\t'):
                                skip_block = False
                                if 'if __name__' not in line:
                                    filtered_lines.append(line)
                        
                        f.write('\n'.join(filtered_lines))
                        temp_path = f.name
                    
                    try:
                        # Try to import and call main if it exists
                        result = subprocess.run(
                            [sys.executable, "-c", f"import sys; sys.path.insert(0, '{os.path.dirname(temp_path)}'); exec(open('{temp_path}').read())"],
                            capture_output=True,
                            timeout=5,
                            text=True
                        )
                        if result.returncode == 0:
                            tests_passed += 1
                        else:
                            if not error_type:
                                error_type = "runtime_error"
                                error_message = result.stderr[:200] if result.stderr else "Execution failed"
                    finally:
                        if os.path.exists(temp_path):
                            os.unlink(temp_path)
                except subprocess.TimeoutExpired:
                    if not error_type:
                        error_type = "timeout"
                        error_message = "Execution timed out"
                except Exception as e:
                    if not error_type:
                        error_type = "runtime_error"
                        error_message = f"Execution error: {str(e)[:200]}"
            
            # Test 4: Code Quality Checks
            total_tests += 1
            quality_score = 0
            quality_checks = []
            
            # Check for type hints
            if '->' in code or ': str' in code or ': int' in code or ': bool' in code:
                quality_score += 1
                quality_checks.append("type_hints")
            
            # Check for docstrings
            if '"""' in code or "'''" in code:
                quality_score += 1
                quality_checks.append("docstrings")
            
            # Check for class definitions (structured code)
            if 'class ' in code:
                quality_score += 1
                quality_checks.append("classes")
            
            # Pass if at least 2 quality checks pass
            if quality_score >= 2:
                tests_passed += 1
            
            # Determine success
            success = tests_passed == total_tests and error_type is None
            
            if not success and not error_type:
                error_type = "validation_failed"
                error_message = f"Only {tests_passed}/{total_tests} validation checks passed"
            
            return ExecutionResult(
                program_name=program_name,
                success=success,
                n_success=tests_passed,
                total_tests=total_tests,
                execution_time=time.time() - start_time,
                error_type=error_type,
                error_message=error_message,
                validation_type="basic_validation"
            )
            
        except Exception as e:
            return ExecutionResult(
                program_name=program_name,
                success=False,
                n_success=0,
                total_tests=max(total_tests, 1),
                execution_time=time.time() - start_time,
                error_type="runtime_error",
                error_message=f"Validation error: {str(e)[:200]}",
                validation_type="basic_validation"
            )
    
    def evaluate_all(self) -> EvaluationMetrics:
        """Evaluate all programs in output directory.
        
        Returns:
            EvaluationMetrics
        """
        # Find all generated Python files
        generated_files = list(self.output_dir.glob("*.py"))
        
        # Filter out test files
        generated_files = [f for f in generated_files if not f.name.startswith("test_")]
        
        print(f"Found {len(generated_files)} generated Python files")
        
        results = []
        
        for generated_file in generated_files:
            program_name = generated_file.stem
            
            # Find ground truth files
            # Look in data directory
            ground_truth_python = None
            ground_truth_test = None
            
            for data_subdir in self.data_dir.iterdir():
                if data_subdir.is_dir():
                    # Skip output directory - it contains generated files, not ground truth
                    if data_subdir.name == "output" or data_subdir.name.startswith("output"):
                        continue
                    
                    # Look for ground truth Python file
                    possible_names = [
                        program_name + ".py",
                        program_name.lower() + ".py",
                        program_name.capitalize() + ".py",
                    ]
                    
                    for name in possible_names:
                        candidate = data_subdir / name
                        if candidate.exists() and candidate != generated_file:
                            ground_truth_python = candidate
                            break
                    
                    # Look for test file
                    test_candidate = data_subdir / f"test_{program_name}.py"
                    if test_candidate.exists():
                        ground_truth_test = test_candidate
                    
                    if ground_truth_python:
                        break
            
            # Check if we have ground truth
            has_ground_truth = ground_truth_python and ground_truth_test and ground_truth_test.exists()
            
            if has_ground_truth:
                # Full evaluation with ground truth
                print(f"Evaluating {program_name}...", end=" ", flush=True)
                
                result = self.evaluate_program(
                    program_name,
                    ground_truth_python,
                    ground_truth_test,
                    generated_file
                )
                # validation_type is already set in evaluate_program
            else:
                # Basic validation for programs without ground truth
                print(f"Validating {program_name} (no ground truth)...", end=" ", flush=True)
                
                result = self.validate_without_ground_truth(
                    program_name,
                    generated_file
                )
                result.validation_type = "basic_validation"
            
            results.append(result)
            
            status = "✓" if result.success else "✗"
            validation_label = "[GT]" if result.validation_type == "ground_truth" else "[BV]"
            print(f"{status} ({result.n_success}/{result.total_tests}) {validation_label}")
        
        # Calculate metrics
        total = len(results)
        if total == 0:
            return EvaluationMetrics(
                timestamp=datetime.now().isoformat(),
                total_programs=0,
                ca_at_1=0.0,
                compilation_error_rate=0.0,
                runtime_error_rate=0.0,
                timeout_rate=0.0,
                logical_error_rate=0.0,
                avg_execution_time=0.0,
                avg_test_cases_passed=0.0,
                program_results=[]
            )
        
        success_count = sum(1 for r in results if r.success)
        compilation_errors = sum(1 for r in results if r.error_type == "compilation_error")
        runtime_errors = sum(1 for r in results if r.error_type == "runtime_error")
        timeouts = sum(1 for r in results if r.error_type == "timeout")
        logical_errors = sum(1 for r in results if r.error_type == "logical_error")
        
        execution_times = [r.execution_time for r in results if r.execution_time > 0]
        test_cases_passed = [r.n_success for r in results if r.total_tests > 0]
        
        metrics = EvaluationMetrics(
            timestamp=datetime.now().isoformat(),
            total_programs=total,
            ca_at_1=(success_count / total * 100) if total > 0 else 0.0,
            compilation_error_rate=(compilation_errors / total * 100) if total > 0 else 0.0,
            runtime_error_rate=(runtime_errors / total * 100) if total > 0 else 0.0,
            timeout_rate=(timeouts / total * 100) if total > 0 else 0.0,
            logical_error_rate=(logical_errors / total * 100) if total > 0 else 0.0,
            avg_execution_time=(sum(execution_times) / len(execution_times)) if execution_times else 0.0,
            avg_test_cases_passed=(sum(test_cases_passed) / len(test_cases_passed)) if test_cases_passed else 0.0,
            program_results=[asdict(r) for r in results]
        )
        
        return metrics
    
    def save_results(self, metrics: EvaluationMetrics, filename: Optional[str] = None) -> Path:
        """Save evaluation results to JSON file.
        
        Args:
            metrics: EvaluationMetrics to save
            filename: Optional filename
            
        Returns:
            Path to saved file
        """
        if filename is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            filename = f"evaluation_{timestamp}.json"
        
        filepath = self.results_dir / filename
        
        with open(filepath, 'w') as f:
            json.dump(asdict(metrics), f, indent=2)
        
        return filepath
    
    def print_summary(self, metrics: EvaluationMetrics):
        """Print evaluation summary.
        
        Args:
            metrics: EvaluationMetrics to summarize
        """
        print("\n" + "=" * 80)
        print("EVALUATION SUMMARY")
        print("=" * 80)
        print(f"Timestamp: {metrics.timestamp}")
        print(f"Total Programs: {metrics.total_programs}")
        print()
        
        # Separate ground truth vs basic validation
        gt_results = [r for r in metrics.program_results if r.get('validation_type') == 'ground_truth']
        bv_results = [r for r in metrics.program_results if r.get('validation_type') == 'basic_validation']
        
        if gt_results:
            gt_success = sum(1 for r in gt_results if r['success'])
            print(f"Ground Truth Evaluation: {len(gt_results)} programs")
            print(f"  Success Rate: {gt_success}/{len(gt_results)} ({gt_success/len(gt_results)*100:.1f}%)")
            print()
        
        if bv_results:
            bv_success = sum(1 for r in bv_results if r['success'])
            print(f"Basic Validation: {len(bv_results)} programs (no ground truth)")
            print(f"  Validation Pass Rate: {bv_success}/{len(bv_results)} ({bv_success/len(bv_results)*100:.1f}%)")
            print()
        
        print("OVERALL METRICS:")
        print(f"  CA@1 (Correctness at 1): {metrics.ca_at_1:.2f}%")
        print(f"  Compilation Error Rate: {metrics.compilation_error_rate:.2f}%")
        print(f"  Runtime Error Rate: {metrics.runtime_error_rate:.2f}%")
        print(f"  Timeout Rate: {metrics.timeout_rate:.2f}%")
        print(f"  Logical Error Rate: {metrics.logical_error_rate:.2f}%")
        print(f"  Avg Execution Time: {metrics.avg_execution_time:.3f}s")
        print(f"  Avg Test Cases Passed: {metrics.avg_test_cases_passed:.2f}")
        print()
        
        # Show program-level results
        if metrics.program_results:
            print("PROGRAM RESULTS:")
            for result in metrics.program_results:
                status = "✓" if result['success'] else "✗"
                validation_label = "[GT]" if result.get('validation_type') == 'ground_truth' else "[BV]"
                print(f"  {status} {result['program_name']}: "
                      f"{result['n_success']}/{result['total_tests']} tests passed {validation_label}")
                if result['error_type']:
                    print(f"    Error: {result['error_type']}")
        print("=" * 80)


def main():
    """Main entry point."""
    import argparse
    
    parser = argparse.ArgumentParser(description="Evaluation")
    parser.add_argument("--data-dir", type=Path, default=Path("data"),
                       help="Directory containing ground truth files")
    parser.add_argument("--output-dir", type=Path, default=Path("data/output"),
                       help="Directory containing generated Python files")
    parser.add_argument("--results-dir", type=Path, default=Path("evals/results"),
                       help="Directory to store results")
    parser.add_argument("--timeout", type=int, default=30,
                       help="Timeout in seconds")
    parser.add_argument("--memory-limit", type=int, default=512,
                       help="Memory limit in MB")
    parser.add_argument("--save", action="store_true",
                       help="Save results to file")
    
    args = parser.parse_args()
    
    evaluator = Evaluator(
        data_dir=args.data_dir,
        output_dir=args.output_dir,
        results_dir=args.results_dir,
        timeout_seconds=args.timeout,
        memory_limit_mb=args.memory_limit
    )
    
    metrics = evaluator.evaluate_all()
    evaluator.print_summary(metrics)
    
    if args.save:
        filepath = evaluator.save_results(metrics)
        print(f"\nResults saved to: {filepath}")


if __name__ == "__main__":
    main()

