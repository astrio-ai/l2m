"""
Base memory system for persistent storage of agent state and user instructions.
"""

import asyncio
import json
import logging
import os
import pickle
import sqlite3
import time
from abc import ABC, abstractmethod
from typing import Dict, List, Optional, Any, Union
from dataclasses import dataclass, field
from pathlib import Path

logger = logging.getLogger(__name__)

@dataclass
class MemoryItem:
    """A single item stored in memory."""
    key: str
    value: Any
    timestamp: float
    ttl: Optional[float] = None  # Time to live in seconds
    metadata: Dict[str, Any] = field(default_factory=dict)

class BaseMemory(ABC):
    """
    Abstract base class for memory systems.
    
    Provides persistent storage for agent state, user instructions,
    and other data that needs to persist across sessions.
    """
    
    def __init__(self, storage_path: Optional[str] = None):
        self.storage_path = storage_path or "memory"
        self._ensure_storage_path()
    
    @abstractmethod
    async def set(self, key: str, value: Any, ttl: Optional[float] = None, metadata: Optional[Dict[str, Any]] = None):
        """Store a value with optional TTL and metadata."""
        pass
    
    @abstractmethod
    async def get(self, key: str, default: Any = None) -> Any:
        """Retrieve a value by key."""
        pass
    
    @abstractmethod
    async def delete(self, key: str):
        """Delete a value by key."""
        pass
    
    @abstractmethod
    async def exists(self, key: str) -> bool:
        """Check if a key exists."""
        pass
    
    @abstractmethod
    async def list_keys(self, pattern: Optional[str] = None) -> List[str]:
        """List all keys, optionally filtered by pattern."""
        pass
    
    @abstractmethod
    async def clear(self):
        """Clear all stored data."""
        pass
    
    def _ensure_storage_path(self):
        """Ensure the storage directory exists."""
        os.makedirs(self.storage_path, exist_ok=True)

class FileMemory(BaseMemory):
    """
    File-based memory implementation using JSON files.
    """
    
    def __init__(self, storage_path: Optional[str] = None):
        super().__init__(storage_path)
        self.data_file = os.path.join(self.storage_path, "memory.json")
        self._load_data()
    
    def _load_data(self):
        """Load data from file."""
        try:
            if os.path.exists(self.data_file):
                with open(self.data_file, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    self._data = {
                        key: MemoryItem(**item) for key, item in data.items()
                    }
            else:
                self._data = {}
        except Exception as e:
            logger.warning(f"Failed to load memory data: {e}")
            self._data = {}
    
    def _save_data(self):
        """Save data to file."""
        try:
            # Convert MemoryItem objects to dictionaries
            data = {}
            for key, item in self._data.items():
                if item.ttl is None or time.time() < item.timestamp + item.ttl:
                    data[key] = {
                        "key": item.key,
                        "value": item.value,
                        "timestamp": item.timestamp,
                        "ttl": item.ttl,
                        "metadata": item.metadata
                    }
            
            with open(self.data_file, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=2, default=str)
        except Exception as e:
            logger.error(f"Failed to save memory data: {e}")
    
    async def set(self, key: str, value: Any, ttl: Optional[float] = None, metadata: Optional[Dict[str, Any]] = None):
        """Store a value with optional TTL and metadata."""
        self._data[key] = MemoryItem(
            key=key,
            value=value,
            timestamp=time.time(),
            ttl=ttl,
            metadata=metadata or {}
        )
        self._save_data()
    
    async def get(self, key: str, default: Any = None) -> Any:
        """Retrieve a value by key."""
        if key not in self._data:
            return default
        
        item = self._data[key]
        
        # Check TTL
        if item.ttl is not None and time.time() > item.timestamp + item.ttl:
            await self.delete(key)
            return default
        
        return item.value
    
    async def delete(self, key: str):
        """Delete a value by key."""
        if key in self._data:
            del self._data[key]
            self._save_data()
    
    async def exists(self, key: str) -> bool:
        """Check if a key exists."""
        if key not in self._data:
            return False
        
        item = self._data[key]
        
        # Check TTL
        if item.ttl is not None and time.time() > item.timestamp + item.ttl:
            await self.delete(key)
            return False
        
        return True
    
    async def list_keys(self, pattern: Optional[str] = None) -> List[str]:
        """List all keys, optionally filtered by pattern."""
        keys = list(self._data.keys())
        
        # Filter by pattern if provided
        if pattern:
            import fnmatch
            keys = fnmatch.filter(keys, pattern)
        
        # Filter out expired items
        valid_keys = []
        for key in keys:
            if await self.exists(key):
                valid_keys.append(key)
        
        return valid_keys
    
    async def clear(self):
        """Clear all stored data."""
        self._data = {}
        self._save_data()

class SQLiteMemory(BaseMemory):
    """
    SQLite-based memory implementation for better performance and ACID compliance.
    """
    
    def __init__(self, storage_path: Optional[str] = None):
        super().__init__(storage_path)
        self.db_path = os.path.join(self.storage_path, "memory.db")
        self._init_database()
    
    def _init_database(self):
        """Initialize the SQLite database."""
        try:
            with sqlite3.connect(self.db_path) as conn:
                conn.execute("""
                    CREATE TABLE IF NOT EXISTS memory (
                        key TEXT PRIMARY KEY,
                        value TEXT,
                        timestamp REAL,
                        ttl REAL,
                        metadata TEXT
                    )
                """)
                conn.commit()
        except Exception as e:
            logger.error(f"Failed to initialize SQLite database: {e}")
    
    async def set(self, key: str, value: Any, ttl: Optional[float] = None, metadata: Optional[Dict[str, Any]] = None):
        """Store a value with optional TTL and metadata."""
        try:
            with sqlite3.connect(self.db_path) as conn:
                conn.execute("""
                    INSERT OR REPLACE INTO memory (key, value, timestamp, ttl, metadata)
                    VALUES (?, ?, ?, ?, ?)
                """, (
                    key,
                    json.dumps(value, default=str),
                    time.time(),
                    ttl,
                    json.dumps(metadata or {}, default=str)
                ))
                conn.commit()
        except Exception as e:
            logger.error(f"Failed to set value for key {key}: {e}")
    
    async def get(self, key: str, default: Any = None) -> Any:
        """Retrieve a value by key."""
        try:
            with sqlite3.connect(self.db_path) as conn:
                cursor = conn.execute("""
                    SELECT value, timestamp, ttl FROM memory WHERE key = ?
                """, (key,))
                row = cursor.fetchone()
                
                if row is None:
                    return default
                
                value, timestamp, ttl = row
                
                # Check TTL
                if ttl is not None and time.time() > timestamp + ttl:
                    await self.delete(key)
                    return default
                
                return json.loads(value)
        except Exception as e:
            logger.error(f"Failed to get value for key {key}: {e}")
            return default
    
    async def delete(self, key: str):
        """Delete a value by key."""
        try:
            with sqlite3.connect(self.db_path) as conn:
                conn.execute("DELETE FROM memory WHERE key = ?", (key,))
                conn.commit()
        except Exception as e:
            logger.error(f"Failed to delete key {key}: {e}")
    
    async def exists(self, key: str) -> bool:
        """Check if a key exists."""
        try:
            with sqlite3.connect(self.db_path) as conn:
                cursor = conn.execute("""
                    SELECT timestamp, ttl FROM memory WHERE key = ?
                """, (key,))
                row = cursor.fetchone()
                
                if row is None:
                    return False
                
                timestamp, ttl = row
                
                # Check TTL
                if ttl is not None and time.time() > timestamp + ttl:
                    await self.delete(key)
                    return False
                
                return True
        except Exception as e:
            logger.error(f"Failed to check existence for key {key}: {e}")
            return False
    
    async def list_keys(self, pattern: Optional[str] = None) -> List[str]:
        """List all keys, optionally filtered by pattern."""
        try:
            with sqlite3.connect(self.db_path) as conn:
                if pattern:
                    # SQLite doesn't have built-in pattern matching like fnmatch
                    # This is a simple implementation
                    cursor = conn.execute("SELECT key, timestamp, ttl FROM memory")
                else:
                    cursor = conn.execute("SELECT key, timestamp, ttl FROM memory")
                
                keys = []
                for row in cursor.fetchall():
                    key, timestamp, ttl = row
                    
                    # Check TTL
                    if ttl is not None and time.time() > timestamp + ttl:
                        await self.delete(key)
                        continue
                    
                    if pattern:
                        import fnmatch
                        if fnmatch.fnmatch(key, pattern):
                            keys.append(key)
                    else:
                        keys.append(key)
                
                return keys
        except Exception as e:
            logger.error(f"Failed to list keys: {e}")
            return []
    
    async def clear(self):
        """Clear all stored data."""
        try:
            with sqlite3.connect(self.db_path) as conn:
                conn.execute("DELETE FROM memory")
                conn.commit()
        except Exception as e:
            logger.error(f"Failed to clear memory: {e}")

# Default memory implementation
Memory = SQLiteMemory 