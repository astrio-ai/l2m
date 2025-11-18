"""
L2M (Legacy2Modern) agent for Harbor (aka Terminal) benchmarking.

L2M is an installed agent that specializes in modernizing legacy COBOL code to Python.
"""

from pathlib import Path
from typing import TYPE_CHECKING

from pydantic import BaseModel

# Import Harbor modules - these will be available when Harbor loads this module
if TYPE_CHECKING:
    from harbor.agents.installed.base import BaseInstalledAgent
    from harbor.models.agent.context import AgentContext
else:
    # Runtime imports - Harbor must be installed
    from harbor.agents.installed.base import BaseInstalledAgent
    from harbor.models.agent.context import AgentContext


class ExecInput(BaseModel):
    """Command execution input."""
    command: str
    cwd: str | None = None
    env: dict[str, str] | None = None
    timeout_sec: int | None = None


class L2MAgent(BaseInstalledAgent):
    """
    L2M (Legacy2Modern) Agent for Harbor.
    
    L2M is a specialized coding agent that modernizes legacy COBOL code to Python.
    It can run in headless mode using the --message flag.
    """
    
    @staticmethod
    def name() -> str:
        """The name of the agent."""
        return "l2m"
    
    def version(self) -> str | None:
        """The version of the agent."""
        try:
            import subprocess
            result = subprocess.run(
                ["l2m", "--version"],
                capture_output=True,
                text=True,
                timeout=5,
            )
            if result.returncode == 0:
                # Extract version from output
                version_line = result.stdout.split("\n")[0]
                return version_line.strip()
        except Exception:
            pass
        return None
    
    @property
    def _install_agent_template_path(self) -> Path:
        """
        Path to the jinja template script for installing L2M in the container.
        """
        # This should point to a Jinja template that installs L2M
        # The template will be rendered and executed in the container
        template_path = Path(__file__).parent / "templates" / "install_l2m.sh.j2"
        
        # Return absolute path (Harbor may need absolute paths)
        if not template_path.is_absolute():
            template_path = template_path.resolve()
        
        # Verify template exists
        if not template_path.exists():
            raise FileNotFoundError(
                f"Installation template not found at {template_path}. "
                "Please ensure the template file exists."
            )
        
        return template_path
    
    def create_run_agent_commands(self, instruction: str) -> list[ExecInput]:
        """
        Create the commands to run L2M in the container.
        
        L2M runs in headless mode using --message flag.
        The instruction is passed as the message to modernize the COBOL code.
        """
        # Escape instruction for shell safety
        import shlex
        escaped_instruction = shlex.quote(instruction)
        
        # Harbor might need API keys in the environment
        env = {}
        import os
        for key in ['OPENAI_API_KEY', 'ANTHROPIC_API_KEY', 'GOOGLE_API_KEY']:
            value = os.getenv(key)
            if value:
                env[key] = value
        
        commands = []
        
        # Format the instruction as an L2M command
        # L2M expects either a direct message or can work with files in the repo
        # Use --yes-always to disable approval prompts (required for headless Harbor execution)
        # L2M should handle dependency installation as part of the task (e.g., "Install R if not already available")
        l2m_command = f'l2m --message {escaped_instruction} --yes-always'
        commands.append(ExecInput(
            command=l2m_command,
            cwd=None,  # Run from current directory (repo root)
            env=env if env else None,  # Pass environment variables if available
            timeout_sec=600,  # 10 minute timeout (adjust as needed)
        ))
        
        # Post-command: Run test functions if they exist (to generate sample files, etc.)
        # This is a general helper - L2M should handle most of the work, but we can help
        # by running test functions that might generate required output files
        # Check if instruction mentions test function or sample files
        instruction_lower = instruction.lower()
        if 'test' in instruction_lower or 'sample' in instruction_lower:
            # Generic post-command: Try to find and run test files/scripts
            # This is language-agnostic and lets the task requirements guide what to run
            post_commands = []
            
            # Look for common test file patterns and run them
            # R: Look for .R files with test functions
            post_commands.append(
                'for rfile in /app/*.R; do '
                'if [ -f "$rfile" ] && command -v Rscript > /dev/null 2>&1; then '
                'Rscript -e "source(\\"$rfile\\"); if(exists(\\"test\\")) test()" 2>&1 || true; '
                'fi; done'
            )
            
            # Python: Look for test files
            post_commands.append(
                'for pyfile in /app/test*.py /app/*_test.py; do '
                'if [ -f "$pyfile" ]; then python3 "$pyfile" 2>&1 || true; fi; done'
            )
            
            # Shell scripts
            post_commands.append(
                'for shfile in /app/test*.sh /app/run*.sh; do '
                'if [ -f "$shfile" ] && [ -x "$shfile" ]; then bash "$shfile" 2>&1 || true; fi; done'
            )
            
            if post_commands:
                commands.append(ExecInput(
                    command='bash -c ' + shlex.quote('; '.join(post_commands)),
                    cwd=None,
                    env=env if env else None,
                    timeout_sec=60,
                ))
        
        return commands
    
    def populate_context_post_run(self, context: AgentContext) -> None:
        """
        Populate the context with the results of L2M execution.
        
        L2M generates Python files from COBOL files. We need to:
        1. Find generated Python files
        2. Check if any tests passed/failed
        3. Extract any error messages or logs
        
        Note: This method should use Harbor's AgentContext API. Adjust method names
        (e.g., add_message, add_file_modification) based on actual Harbor API.
        """
        try:
            # Get the working directory (repo root)
            # Harbor's context API may vary - adjust based on actual implementation
            working_dir = getattr(context, 'working_directory', None)
            
            if not working_dir:
                # Try alternative attribute names
                working_dir = getattr(context, 'work_dir', None) or getattr(context, 'cwd', None)
            
            if not working_dir:
                # If we can't get working directory, return silently
                # Harbor may handle this differently
                return
            
            working_path = Path(working_dir)
            
            # Look for generated Python files (new or recently modified)
            generated_files = []
            
            # Check for .py files corresponding to .cbl files
            for cobol_file in working_path.rglob("*.cbl"):
                python_file = cobol_file.with_suffix(".py")
                if python_file.exists():
                    generated_files.append(str(python_file.relative_to(working_path)))
            
            # Try to add information to context using Harbor's API
            # Adjust method names based on actual Harbor AgentContext implementation
            if hasattr(context, 'add_message'):
                if generated_files:
                    context.add_message(f"L2M generated {len(generated_files)} Python file(s):")
                    for file in generated_files:
                        context.add_message(f"  - {file}")
                else:
                    context.add_message("Warning: No Python files were generated by L2M")
                
                # Check for test files
                test_files = list(working_path.rglob("test_*.py"))
                if test_files:
                    context.add_message(f"Found {len(test_files)} test file(s)")
                    for test_file in test_files:
                        test_result = self._run_test_file(test_file, working_path)
                        if test_result:
                            context.add_message(f"Test {test_file.name}: {test_result}")
            
            # Try to mark files as modified/created if API exists
            if hasattr(context, 'add_file_modification') and generated_files:
                for file_path in generated_files:
                    full_path = working_path / file_path
                    try:
                        context.add_file_modification(
                            file_path=file_path,
                            modification_type="created" if full_path.exists() else "modified",
                        )
                    except Exception:
                        # If API signature is different, skip
                        pass
                        
        except Exception as e:
            # Log error but don't fail - Harbor may handle errors differently
            # In production, you might want to log this to Harbor's logging system
            pass
    
    def _run_test_file(self, test_file: Path, working_dir: Path) -> str | None:
        """Run a test file and return the result."""
        try:
            import subprocess
            result = subprocess.run(
                ["python", "-m", "pytest", str(test_file), "-v"],
                cwd=working_dir,
                capture_output=True,
                text=True,
                timeout=60,
            )
            if result.returncode == 0:
                return "PASSED"
            else:
                return f"FAILED: {result.stderr[:200]}"
        except Exception as e:
            return f"ERROR: {str(e)}"

