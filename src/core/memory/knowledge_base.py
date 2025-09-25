"""
Knowledge base management.

This module handles the knowledge base for legacy code patterns,
transformation rules, and best practices.
"""

from typing import Dict, Any, List, Optional
import json
from pathlib import Path

from src.utils.logger import get_logger

logger = get_logger(__name__)


class KnowledgeBase:
    """Manages knowledge base for legacy code modernization."""
    
    def __init__(self, knowledge_path: str = "knowledge"):
        """Initialize knowledge base."""
        self.knowledge_path = Path(knowledge_path)
        self.patterns: Dict[str, Any] = {}
        self.rules: Dict[str, Any] = {}
        self.best_practices: Dict[str, Any] = {}
        self.logger = get_logger(__name__)
    
    def add_pattern(self, language: str, pattern: Dict[str, Any]):
        """Add a code pattern to the knowledge base."""
        if language not in self.patterns:
            self.patterns[language] = []
        
        self.patterns[language].append(pattern)
        self.logger.debug(f"Added pattern for {language}: {pattern.get('name', 'unnamed')}")
    
    def get_patterns(self, language: str) -> List[Dict[str, Any]]:
        """Get patterns for a specific language."""
        return self.patterns.get(language, [])
    
    def add_rule(self, language: str, rule: Dict[str, Any]):
        """Add a transformation rule to the knowledge base."""
        if language not in self.rules:
            self.rules[language] = []
        
        self.rules[language].append(rule)
        self.logger.debug(f"Added rule for {language}: {rule.get('name', 'unnamed')}")
    
    def get_rules(self, language: str) -> List[Dict[str, Any]]:
        """Get rules for a specific language."""
        return self.rules.get(language, [])
    
    def add_best_practice(self, category: str, practice: Dict[str, Any]):
        """Add a best practice to the knowledge base."""
        if category not in self.best_practices:
            self.best_practices[category] = []
        
        self.best_practices[category].append(practice)
        self.logger.debug(f"Added best practice for {category}: {practice.get('name', 'unnamed')}")
    
    def get_best_practices(self, category: str) -> List[Dict[str, Any]]:
        """Get best practices for a specific category."""
        return self.best_practices.get(category, [])
    
    def search_knowledge(self, query: str, language: str = None) -> List[Dict[str, Any]]:
        """Search the knowledge base."""
        results = []
        
        # Search patterns
        if language:
            patterns = self.get_patterns(language)
        else:
            patterns = [p for patterns in self.patterns.values() for p in patterns]
        
        for pattern in patterns:
            if query.lower() in pattern.get("name", "").lower() or query.lower() in pattern.get("description", "").lower():
                results.append({"type": "pattern", "data": pattern})
        
        # Search rules
        if language:
            rules = self.get_rules(language)
        else:
            rules = [r for rules in self.rules.values() for r in rules]
        
        for rule in rules:
            if query.lower() in rule.get("name", "").lower() or query.lower() in rule.get("description", "").lower():
                results.append({"type": "rule", "data": rule})
        
        # Search best practices
        for practices in self.best_practices.values():
            for practice in practices:
                if query.lower() in practice.get("name", "").lower() or query.lower() in practice.get("description", "").lower():
                    results.append({"type": "best_practice", "data": practice})
        
        return results
    
    def save_knowledge(self, file_path: str = None):
        """Save knowledge base to file."""
        if file_path is None:
            file_path = self.knowledge_path / "knowledge_base.json"
        
        knowledge_data = {
            "patterns": self.patterns,
            "rules": self.rules,
            "best_practices": self.best_practices
        }
        
        try:
            with open(file_path, 'w') as f:
                json.dump(knowledge_data, f, indent=2)
            self.logger.info(f"Knowledge base saved to {file_path}")
        except Exception as e:
            self.logger.error(f"Error saving knowledge base: {e}")
    
    def load_knowledge(self, file_path: str = None):
        """Load knowledge base from file."""
        if file_path is None:
            file_path = self.knowledge_path / "knowledge_base.json"
        
        try:
            with open(file_path, 'r') as f:
                knowledge_data = json.load(f)
            
            self.patterns = knowledge_data.get("patterns", {})
            self.rules = knowledge_data.get("rules", {})
            self.best_practices = knowledge_data.get("best_practices", {})
            
            self.logger.info(f"Knowledge base loaded from {file_path}")
        except Exception as e:
            self.logger.error(f"Error loading knowledge base: {e}")
