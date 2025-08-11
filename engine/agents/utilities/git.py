"""
Git operations for version snapshots during modernization.
"""

import asyncio
import logging
import os
import subprocess
from typing import Dict, List, Optional, Any, Tuple
from pathlib import Path
from datetime import datetime

logger = logging.getLogger(__name__)

class GitManager:
    """
    Manages Git operations for version control during modernization.
    
    Provides methods for creating snapshots, managing branches,
    and tracking changes throughout the modernization process.
    """
    
    def __init__(self, repo_path: str = "."):
        self.repo_path = Path(repo_path).resolve()
        self._ensure_git_repo()
    
    def _ensure_git_repo(self):
        """Ensure the directory is a Git repository."""
        git_dir = self.repo_path / ".git"
        if not git_dir.exists():
            logger.info(f"Initializing Git repository in {self.repo_path}")
            self._run_git_command(["init"])
    
    async def _run_git_command(self, args: List[str], capture_output: bool = True) -> Dict[str, Any]:
        """
        Run a Git command and return the result.
        
        Args:
            args: Git command arguments
            capture_output: Whether to capture command output
            
        Returns:
            Dictionary with command results
        """
        try:
            cmd = ["git"] + args
            process = await asyncio.create_subprocess_exec(
                *cmd,
                cwd=self.repo_path,
                stdout=asyncio.subprocess.PIPE if capture_output else None,
                stderr=asyncio.subprocess.PIPE if capture_output else None
            )
            
            if capture_output:
                stdout, stderr = await process.communicate()
                return {
                    'success': process.returncode == 0,
                    'stdout': stdout.decode('utf-8', errors='ignore'),
                    'stderr': stderr.decode('utf-8', errors='ignore'),
                    'return_code': process.returncode
                }
            else:
                await process.wait()
                return {
                    'success': process.returncode == 0,
                    'return_code': process.returncode
                }
                
        except Exception as e:
            logger.error(f"Git command failed: {' '.join(args)} - {e}")
            return {
                'success': False,
                'stdout': '',
                'stderr': str(e),
                'return_code': -1
            }
    
    async def get_status(self) -> Dict[str, Any]:
        """Get current Git repository status."""
        result = await self._run_git_command(["status", "--porcelain"])
        
        if not result['success']:
            return {'error': result['stderr']}
        
        # Parse status output
        status_lines = result['stdout'].strip().split('\n') if result['stdout'].strip() else []
        
        files = {
            'modified': [],
            'added': [],
            'deleted': [],
            'untracked': []
        }
        
        for line in status_lines:
            if len(line) >= 2:
                status = line[:2]
                filename = line[3:]
                
                if status == 'M ':
                    files['modified'].append(filename)
                elif status == 'A ':
                    files['added'].append(filename)
                elif status == 'D ':
                    files['deleted'].append(filename)
                elif status == '??':
                    files['untracked'].append(filename)
        
        return {
            'has_changes': any(files.values()),
            'files': files,
            'total_changes': sum(len(f) for f in files.values())
        }
    
    async def create_snapshot(self, message: str, include_untracked: bool = True) -> Dict[str, Any]:
        """
        Create a Git snapshot (commit) of current changes.
        
        Args:
            message: Commit message
            include_untracked: Whether to include untracked files
            
        Returns:
            Dictionary with commit information
        """
        # Add all changes
        if include_untracked:
            add_result = await self._run_git_command(["add", "."])
            if not add_result['success']:
                return {'error': f"Failed to add files: {add_result['stderr']}"}
        else:
            add_result = await self._run_git_command(["add", "-u"])
            if not add_result['success']:
                return {'error': f"Failed to add tracked files: {add_result['stderr']}"}
        
        # Check if there are changes to commit
        status = await self.get_status()
        if not status.get('has_changes', False):
            return {'message': 'No changes to commit'}
        
        # Create commit
        commit_result = await self._run_git_command([
            "commit", "-m", message,
            "--author", "Legacy2Modern <modernization@legacy2modern.com>"
        ])
        
        if not commit_result['success']:
            return {'error': f"Failed to create commit: {commit_result['stderr']}"}
        
        # Get commit hash
        hash_result = await self._run_git_command(["rev-parse", "HEAD"])
        commit_hash = hash_result['stdout'].strip() if hash_result['success'] else None
        
        return {
            'success': True,
            'commit_hash': commit_hash,
            'message': message,
            'timestamp': datetime.now().isoformat()
        }
    
    async def create_branch(self, branch_name: str, start_point: Optional[str] = None) -> Dict[str, Any]:
        """
        Create a new branch.
        
        Args:
            branch_name: Name of the new branch
            start_point: Optional starting point (commit, branch, etc.)
            
        Returns:
            Dictionary with branch creation result
        """
        args = ["checkout", "-b", branch_name]
        if start_point:
            args.append(start_point)
        
        result = await self._run_git_command(args)
        
        return {
            'success': result['success'],
            'branch_name': branch_name,
            'error': result['stderr'] if not result['success'] else None
        }
    
    async def switch_branch(self, branch_name: str) -> Dict[str, Any]:
        """Switch to an existing branch."""
        result = await self._run_git_command(["checkout", branch_name])
        
        return {
            'success': result['success'],
            'branch_name': branch_name,
            'error': result['stderr'] if not result['success'] else None
        }
    
    async def get_current_branch(self) -> str:
        """Get the name of the current branch."""
        result = await self._run_git_command(["branch", "--show-current"])
        
        if result['success']:
            return result['stdout'].strip()
        else:
            return "unknown"
    
    async def list_branches(self) -> List[str]:
        """List all branches."""
        result = await self._run_git_command(["branch", "--list"])
        
        if result['success']:
            branches = []
            for line in result['stdout'].strip().split('\n'):
                if line.strip():
                    # Remove the current branch indicator (*)
                    branch_name = line.strip().lstrip('* ')
                    branches.append(branch_name)
            return branches
        else:
            return []
    
    async def get_commit_history(self, limit: int = 10) -> List[Dict[str, Any]]:
        """
        Get recent commit history.
        
        Args:
            limit: Maximum number of commits to return
            
        Returns:
            List of commit information dictionaries
        """
        result = await self._run_git_command([
            "log", f"--max-count={limit}",
            "--pretty=format:%H|%an|%ae|%ad|%s",
            "--date=iso"
        ])
        
        if not result['success']:
            return []
        
        commits = []
        for line in result['stdout'].strip().split('\n'):
            if line.strip():
                parts = line.split('|')
                if len(parts) >= 5:
                    commits.append({
                        'hash': parts[0],
                        'author_name': parts[1],
                        'author_email': parts[2],
                        'date': parts[3],
                        'message': parts[4]
                    })
        
        return commits
    
    async def create_tag(self, tag_name: str, message: Optional[str] = None) -> Dict[str, Any]:
        """
        Create a tag for the current commit.
        
        Args:
            tag_name: Name of the tag
            message: Optional tag message
            
        Returns:
            Dictionary with tag creation result
        """
        args = ["tag"]
        if message:
            args.extend(["-a", tag_name, "-m", message])
        else:
            args.append(tag_name)
        
        result = await self._run_git_command(args)
        
        return {
            'success': result['success'],
            'tag_name': tag_name,
            'error': result['stderr'] if not result['success'] else None
        }
    
    async def list_tags(self) -> List[str]:
        """List all tags."""
        result = await self._run_git_command(["tag", "--list"])
        
        if result['success']:
            return [tag.strip() for tag in result['stdout'].strip().split('\n') if tag.strip()]
        else:
            return []
    
    async def get_diff(self, commit1: Optional[str] = None, commit2: Optional[str] = None) -> str:
        """
        Get diff between commits or current state.
        
        Args:
            commit1: First commit (or None for current state)
            commit2: Second commit (or None for current state)
            
        Returns:
            Diff text
        """
        args = ["diff"]
        
        if commit1 and commit2:
            args.extend([commit1, commit2])
        elif commit1:
            args.append(commit1)
        
        result = await self._run_git_command(args)
        
        if result['success']:
            return result['stdout']
        else:
            return ""
    
    async def reset_to_commit(self, commit_hash: str, hard: bool = False) -> Dict[str, Any]:
        """
        Reset repository to a specific commit.
        
        Args:
            commit_hash: Commit hash to reset to
            hard: Whether to perform a hard reset (discard changes)
            
        Returns:
            Dictionary with reset result
        """
        args = ["reset"]
        if hard:
            args.append("--hard")
        args.append(commit_hash)
        
        result = await self._run_git_command(args)
        
        return {
            'success': result['success'],
            'commit_hash': commit_hash,
            'error': result['stderr'] if not result['success'] else None
        }
    
    async def stash_changes(self, message: Optional[str] = None) -> Dict[str, Any]:
        """
        Stash current changes.
        
        Args:
            message: Optional stash message
            
        Returns:
            Dictionary with stash result
        """
        args = ["stash", "push"]
        if message:
            args.extend(["-m", message])
        
        result = await self._run_git_command(args)
        
        return {
            'success': result['success'],
            'stash_id': result['stdout'].strip() if result['success'] else None,
            'error': result['stderr'] if not result['success'] else None
        }
    
    async def pop_stash(self, stash_id: Optional[str] = None) -> Dict[str, Any]:
        """
        Pop (apply and remove) a stash.
        
        Args:
            stash_id: Optional stash ID (defaults to most recent)
            
        Returns:
            Dictionary with pop result
        """
        args = ["stash", "pop"]
        if stash_id:
            args.append(stash_id)
        
        result = await self._run_git_command(args)
        
        return {
            'success': result['success'],
            'error': result['stderr'] if not result['success'] else None
        }
    
    async def list_stashes(self) -> List[Dict[str, Any]]:
        """List all stashes."""
        result = await self._run_git_command(["stash", "list", "--format=format:%gd|%an|%ae|%ad|%s", "--date=iso"])
        
        if not result['success']:
            return []
        
        stashes = []
        for line in result['stdout'].strip().split('\n'):
            if line.strip():
                parts = line.split('|')
                if len(parts) >= 5:
                    stashes.append({
                        'id': parts[0],
                        'author_name': parts[1],
                        'author_email': parts[2],
                        'date': parts[3],
                        'message': parts[4]
                    })
        
        return stashes
    
    async def create_modernization_snapshot(self, stage: str, description: str) -> Dict[str, Any]:
        """
        Create a snapshot specifically for modernization stages.
        
        Args:
            stage: Modernization stage (e.g., 'parsing', 'generation', 'refactoring')
            description: Description of what was accomplished
            
        Returns:
            Dictionary with snapshot information
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        branch_name = f"modernization/{stage}/{timestamp}"
        
        # Create branch
        branch_result = await self.create_branch(branch_name)
        if not branch_result['success']:
            return branch_result
        
        # Create snapshot
        message = f"[{stage.upper()}] {description}\n\nTimestamp: {datetime.now().isoformat()}"
        snapshot_result = await self.create_snapshot(message)
        
        if not snapshot_result.get('success', False):
            return snapshot_result
        
        # Create tag
        tag_name = f"stage-{stage}-{timestamp}"
        tag_result = await self.create_tag(tag_name, f"Modernization stage: {stage}")
        
        return {
            'success': True,
            'branch_name': branch_name,
            'commit_hash': snapshot_result.get('commit_hash'),
            'tag_name': tag_name if tag_result['success'] else None,
            'stage': stage,
            'timestamp': timestamp,
            'description': description
        } 