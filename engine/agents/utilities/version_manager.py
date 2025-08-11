"""
Snapshot/version the modernization steps.
"""

import json
import logging
import os
import shutil
from typing import Dict, List, Optional, Any, Union
from pathlib import Path
from dataclasses import dataclass, field
from datetime import datetime
import hashlib

logger = logging.getLogger(__name__)

@dataclass
class VersionSnapshot:
    """Represents a version snapshot of the modernization process."""
    version_id: str
    timestamp: datetime
    description: str
    stage: str
    files_changed: List[str]
    metadata: Dict[str, Any] = field(default_factory=dict)
    parent_version: Optional[str] = None
    tags: List[str] = field(default_factory=list)

@dataclass
class FileChange:
    """Represents a change to a file in a version."""
    file_path: str
    change_type: str  # 'added', 'modified', 'deleted'
    content_hash: Optional[str] = None
    size: Optional[int] = None
    metadata: Dict[str, Any] = field(default_factory=dict)

class VersionManager:
    """
    Manages version snapshots of the modernization process.
    
    Provides methods for creating, managing, and restoring snapshots
    of the project at different stages of modernization.
    """
    
    def __init__(self, project_root: str = ".", versions_dir: Optional[str] = None):
        self.project_root = Path(project_root).resolve()
        self.versions_dir = Path(versions_dir) if versions_dir else self.project_root / ".l2m_versions"
        self.snapshots_file = self.versions_dir / "snapshots.json"
        self.snapshots: Dict[str, VersionSnapshot] = {}
        
        # Ensure versions directory exists
        self.versions_dir.mkdir(exist_ok=True)
        
        # Load existing snapshots
        self._load_snapshots()
    
    def _load_snapshots(self):
        """Load existing snapshots from disk."""
        try:
            if self.snapshots_file.exists():
                with open(self.snapshots_file, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    
                    for snapshot_data in data.get('snapshots', []):
                        snapshot = VersionSnapshot(
                            version_id=snapshot_data['version_id'],
                            timestamp=datetime.fromisoformat(snapshot_data['timestamp']),
                            description=snapshot_data['description'],
                            stage=snapshot_data['stage'],
                            files_changed=snapshot_data['files_changed'],
                            metadata=snapshot_data.get('metadata', {}),
                            parent_version=snapshot_data.get('parent_version'),
                            tags=snapshot_data.get('tags', [])
                        )
                        self.snapshots[snapshot.version_id] = snapshot
                    
                    logger.info(f"Loaded {len(self.snapshots)} snapshots")
                    
        except Exception as e:
            logger.error(f"Failed to load snapshots: {e}")
    
    def _save_snapshots(self):
        """Save snapshots to disk."""
        try:
            data = {
                'snapshots': [
                    {
                        'version_id': snapshot.version_id,
                        'timestamp': snapshot.timestamp.isoformat(),
                        'description': snapshot.description,
                        'stage': snapshot.stage,
                        'files_changed': snapshot.files_changed,
                        'metadata': snapshot.metadata,
                        'parent_version': snapshot.parent_version,
                        'tags': snapshot.tags
                    }
                    for snapshot in self.snapshots.values()
                ]
            }
            
            with open(self.snapshots_file, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=2, default=str)
                
        except Exception as e:
            logger.error(f"Failed to save snapshots: {e}")
    
    def create_snapshot(
        self,
        description: str,
        stage: str,
        files_to_track: Optional[List[str]] = None,
        parent_version: Optional[str] = None,
        tags: Optional[List[str]] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        Create a new version snapshot.
        
        Args:
            description: Description of the snapshot
            stage: Current modernization stage
            files_to_track: List of files to track (defaults to all)
            parent_version: Parent version ID
            tags: List of tags for the snapshot
            metadata: Additional metadata
            
        Returns:
            Version ID of the created snapshot
        """
        # Generate version ID
        timestamp = datetime.now()
        version_id = self._generate_version_id(description, timestamp)
        
        # Determine files to track
        if files_to_track is None:
            files_to_track = self._get_all_project_files()
        
        # Create snapshot directory
        snapshot_dir = self.versions_dir / version_id
        snapshot_dir.mkdir(exist_ok=True)
        
        # Copy files to snapshot
        files_changed = []
        for file_path in files_to_track:
            if self._should_track_file(file_path):
                try:
                    self._copy_file_to_snapshot(file_path, snapshot_dir)
                    files_changed.append(file_path)
                except Exception as e:
                    logger.warning(f"Failed to copy {file_path} to snapshot: {e}")
        
        # Create snapshot object
        snapshot = VersionSnapshot(
            version_id=version_id,
            timestamp=timestamp,
            description=description,
            stage=stage,
            files_changed=files_changed,
            metadata=metadata or {},
            parent_version=parent_version,
            tags=tags or []
        )
        
        # Save snapshot metadata
        self.snapshots[version_id] = snapshot
        self._save_snapshots()
        
        # Save snapshot details
        self._save_snapshot_details(snapshot, snapshot_dir)
        
        logger.info(f"Created snapshot {version_id}: {description}")
        return version_id
    
    def restore_snapshot(self, version_id: str, target_dir: Optional[str] = None) -> bool:
        """
        Restore a snapshot to the project directory.
        
        Args:
            version_id: Version ID to restore
            target_dir: Target directory (defaults to project root)
            
        Returns:
            True if restoration was successful
        """
        if version_id not in self.snapshots:
            logger.error(f"Snapshot {version_id} not found")
            return False
        
        snapshot = self.snapshots[version_id]
        snapshot_dir = self.versions_dir / version_id
        
        if not snapshot_dir.exists():
            logger.error(f"Snapshot directory {snapshot_dir} not found")
            return False
        
        target_path = Path(target_dir) if target_dir else self.project_root
        
        try:
            # Restore files from snapshot
            for file_path in snapshot.files_changed:
                snapshot_file = snapshot_dir / file_path
                target_file = target_path / file_path
                
                if snapshot_file.exists():
                    # Ensure target directory exists
                    target_file.parent.mkdir(parents=True, exist_ok=True)
                    
                    # Copy file
                    shutil.copy2(snapshot_file, target_file)
                    logger.debug(f"Restored {file_path}")
                else:
                    logger.warning(f"Snapshot file {snapshot_file} not found")
            
            logger.info(f"Restored snapshot {version_id} to {target_path}")
            return True
            
        except Exception as e:
            logger.error(f"Failed to restore snapshot {version_id}: {e}")
            return False
    
    def list_snapshots(self, stage: Optional[str] = None, tag: Optional[str] = None) -> List[VersionSnapshot]:
        """List available snapshots with optional filtering."""
        snapshots = list(self.snapshots.values())
        
        if stage:
            snapshots = [s for s in snapshots if s.stage == stage]
        
        if tag:
            snapshots = [s for s in snapshots if tag in s.tags]
        
        # Sort by timestamp (newest first)
        snapshots.sort(key=lambda s: s.timestamp, reverse=True)
        
        return snapshots
    
    def get_snapshot(self, version_id: str) -> Optional[VersionSnapshot]:
        """Get a specific snapshot by version ID."""
        return self.snapshots.get(version_id)
    
    def delete_snapshot(self, version_id: str) -> bool:
        """Delete a snapshot and its files."""
        if version_id not in self.snapshots:
            logger.error(f"Snapshot {version_id} not found")
            return False
        
        try:
            # Remove snapshot directory
            snapshot_dir = self.versions_dir / version_id
            if snapshot_dir.exists():
                shutil.rmtree(snapshot_dir)
            
            # Remove from snapshots
            del self.snapshots[version_id]
            self._save_snapshots()
            
            logger.info(f"Deleted snapshot {version_id}")
            return True
            
        except Exception as e:
            logger.error(f"Failed to delete snapshot {version_id}: {e}")
            return False
    
    def compare_snapshots(self, version1: str, version2: str) -> Dict[str, Any]:
        """
        Compare two snapshots and return differences.
        
        Args:
            version1: First version ID
            version2: Second version ID
            
        Returns:
            Dictionary with comparison results
        """
        if version1 not in self.snapshots or version2 not in self.snapshots:
            logger.error("One or both snapshots not found")
            return {}
        
        snapshot1 = self.snapshots[version1]
        snapshot2 = self.snapshots[version2]
        
        # Get file sets
        files1 = set(snapshot1.files_changed)
        files2 = set(snapshot2.files_changed)
        
        # Calculate differences
        added_files = files2 - files1
        removed_files = files1 - files2
        common_files = files1 & files2
        
        # Compare common files
        modified_files = []
        for file_path in common_files:
            if self._files_differ(file_path, version1, version2):
                modified_files.append(file_path)
        
        return {
            'version1': version1,
            'version2': version2,
            'added_files': list(added_files),
            'removed_files': list(removed_files),
            'modified_files': modified_files,
            'unchanged_files': list(common_files - set(modified_files)),
            'timestamp1': snapshot1.timestamp.isoformat(),
            'timestamp2': snapshot2.timestamp.isoformat(),
            'stage1': snapshot1.stage,
            'stage2': snapshot2.stage
        }
    
    def get_snapshot_tree(self) -> Dict[str, Any]:
        """Get a tree representation of snapshots showing parent-child relationships."""
        tree = {}
        
        # Find root snapshots (no parent)
        root_snapshots = [
            snapshot for snapshot in self.snapshots.values()
            if not snapshot.parent_version
        ]
        
        for root in root_snapshots:
            tree[root.version_id] = self._build_snapshot_branch(root.version_id)
        
        return tree
    
    def tag_snapshot(self, version_id: str, tags: List[str]) -> bool:
        """Add tags to a snapshot."""
        if version_id not in self.snapshots:
            logger.error(f"Snapshot {version_id} not found")
            return False
        
        snapshot = self.snapshots[version_id]
        snapshot.tags.extend(tags)
        snapshot.tags = list(set(snapshot.tags))  # Remove duplicates
        
        self._save_snapshots()
        logger.info(f"Added tags {tags} to snapshot {version_id}")
        return True
    
    def get_snapshot_statistics(self) -> Dict[str, Any]:
        """Get statistics about all snapshots."""
        if not self.snapshots:
            return {
                'total_snapshots': 0,
                'stages': {},
                'tags': {},
                'date_range': None,
                'total_size': 0
            }
        
        # Stage distribution
        stages = {}
        for snapshot in self.snapshots.values():
            stages[snapshot.stage] = stages.get(snapshot.stage, 0) + 1
        
        # Tag distribution
        tags = {}
        for snapshot in self.snapshots.values():
            for tag in snapshot.tags:
                tags[tag] = tags.get(tag, 0) + 1
        
        # Date range
        timestamps = [s.timestamp for s in self.snapshots.values()]
        date_range = {
            'earliest': min(timestamps).isoformat(),
            'latest': max(timestamps).isoformat()
        }
        
        # Calculate total size
        total_size = 0
        for snapshot in self.snapshots.values():
            snapshot_dir = self.versions_dir / snapshot.version_id
            if snapshot_dir.exists():
                total_size += self._get_directory_size(snapshot_dir)
        
        return {
            'total_snapshots': len(self.snapshots),
            'stages': stages,
            'tags': tags,
            'date_range': date_range,
            'total_size': total_size
        }
    
    def _generate_version_id(self, description: str, timestamp: datetime) -> str:
        """Generate a unique version ID."""
        # Create a hash from description and timestamp
        content = f"{description}{timestamp.isoformat()}"
        hash_obj = hashlib.md5(content.encode())
        return hash_obj.hexdigest()[:8]
    
    def _get_all_project_files(self) -> List[str]:
        """Get all files in the project directory."""
        files = []
        
        for root, dirs, filenames in os.walk(self.project_root):
            # Skip version directory and other ignored paths
            dirs[:] = [d for d in dirs if not d.startswith('.') and d != 'node_modules']
            
            for filename in filenames:
                if not filename.startswith('.'):
                    file_path = Path(root) / filename
                    rel_path = file_path.relative_to(self.project_root)
                    files.append(str(rel_path))
        
        return files
    
    def _should_track_file(self, file_path: str) -> bool:
        """Determine if a file should be tracked in snapshots."""
        # Skip certain file types and directories
        skip_patterns = [
            '.git', '.l2m_versions', 'node_modules', '__pycache__',
            '.pyc', '.log', '.tmp', '.cache'
        ]
        
        file_path_lower = file_path.lower()
        return not any(pattern in file_path_lower for pattern in skip_patterns)
    
    def _copy_file_to_snapshot(self, file_path: str, snapshot_dir: Path):
        """Copy a file to the snapshot directory."""
        source_file = self.project_root / file_path
        target_file = snapshot_dir / file_path
        
        if source_file.exists():
            # Ensure target directory exists
            target_file.parent.mkdir(parents=True, exist_ok=True)
            
            # Copy file
            shutil.copy2(source_file, target_file)
    
    def _save_snapshot_details(self, snapshot: VersionSnapshot, snapshot_dir: Path):
        """Save detailed information about a snapshot."""
        details_file = snapshot_dir / "snapshot_details.json"
        
        details = {
            'version_id': snapshot.version_id,
            'timestamp': snapshot.timestamp.isoformat(),
            'description': snapshot.description,
            'stage': snapshot.stage,
            'files_changed': snapshot.files_changed,
            'metadata': snapshot.metadata,
            'parent_version': snapshot.parent_version,
            'tags': snapshot.tags
        }
        
        with open(details_file, 'w', encoding='utf-8') as f:
            json.dump(details, f, indent=2, default=str)
    
    def _files_differ(self, file_path: str, version1: str, version2: str) -> bool:
        """Check if a file differs between two versions."""
        file1 = self.versions_dir / version1 / file_path
        file2 = self.versions_dir / version2 / file_path
        
        if not file1.exists() or not file2.exists():
            return True
        
        try:
            with open(file1, 'rb') as f1, open(file2, 'rb') as f2:
                return f1.read() != f2.read()
        except Exception:
            return True
    
    def _build_snapshot_branch(self, version_id: str) -> Dict[str, Any]:
        """Build a branch of snapshots starting from a given version."""
        snapshot = self.snapshots[version_id]
        
        # Find children
        children = [
            child for child in self.snapshots.values()
            if child.parent_version == version_id
        ]
        
        branch = {
            'version_id': version_id,
            'description': snapshot.description,
            'stage': snapshot.stage,
            'timestamp': snapshot.timestamp.isoformat(),
            'children': [self._build_snapshot_branch(child.version_id) for child in children]
        }
        
        return branch
    
    def _get_directory_size(self, directory: Path) -> int:
        """Calculate the total size of a directory."""
        total_size = 0
        
        try:
            for file_path in directory.rglob('*'):
                if file_path.is_file():
                    total_size += file_path.stat().st_size
        except Exception:
            pass
        
        return total_size 