"""
Functionality Mapping System for Software Modernization

This module provides a comprehensive system for mapping functionality between
source (old) and target (new) systems during software modernization or migration.
It ensures functionality equivalence across any modernization scenario.
"""

import logging
from typing import Dict, Any, List, Optional, Union, Tuple
from dataclasses import dataclass, field
from enum import Enum
from abc import ABC, abstractmethod
import json
import uuid
from datetime import datetime


class FunctionalityType(Enum):
    """Types of functionality that can be mapped."""
    PROGRAM = "program"  # For COBOL programs, Python modules, etc.
    FUNCTION = "function"  # For individual functions/methods
    COMPONENT = "component"  # For React components, UI elements
    API_ENDPOINT = "api_endpoint"  # For API functions
    BUSINESS_RULE = "business_rule"  # For business logic
    DATA_STRUCTURE = "data_structure"  # For data models
    WORKFLOW = "workflow"  # For process flows
    INTEGRATION = "integration"  # For external system connections


class EquivalenceLevel(Enum):
    """Levels of functionality equivalence."""
    EXACT = "exact"  # Perfect functional equivalence
    HIGH = "high"  # High similarity with minor differences
    MEDIUM = "medium"  # Moderate similarity with some differences
    LOW = "low"  # Basic similarity with significant differences
    PARTIAL = "partial"  # Partial functionality preserved


@dataclass
class InputOutputMapping:
    """Mapping of inputs and outputs between source and target systems."""
    source_inputs: Dict[str, Any] = field(default_factory=dict)
    target_inputs: Dict[str, Any] = field(default_factory=dict)
    source_outputs: Dict[str, Any] = field(default_factory=dict)
    target_outputs: Dict[str, Any] = field(default_factory=dict)
    data_transformations: Dict[str, str] = field(default_factory=dict)
    validation_rules: List[str] = field(default_factory=list)


@dataclass
class BusinessLogicMapping:
    """Mapping of business logic between source and target systems."""
    source_logic: str = ""
    target_logic: str = ""
    logic_transformations: List[str] = field(default_factory=list)
    business_rules: List[str] = field(default_factory=list)
    decision_points: List[str] = field(default_factory=list)
    error_handling: Dict[str, str] = field(default_factory=dict)


@dataclass
class FunctionalityMapping:
    """Complete mapping of a functionality between source and target systems."""
    functionality_id: str
    functionality_type: FunctionalityType
    source_name: str
    target_name: str
    source_language: str
    target_language: str
    equivalence_level: EquivalenceLevel
    input_output_mapping: InputOutputMapping
    business_logic_mapping: BusinessLogicMapping
    dependencies: List[str] = field(default_factory=list)
    constraints: List[str] = field(default_factory=list)
    notes: str = ""
    created_at: datetime = field(default_factory=datetime.now)
    updated_at: datetime = field(default_factory=datetime.now)
    confidence_score: float = 0.0
    validation_status: str = "pending"  # pending, validated, failed, needs_review


class FunctionalityMapper:
    """
    Comprehensive functionality mapping system for software modernization.
    
    This class provides methods to create, validate, and manage functionality
    mappings between source and target systems for any modernization scenario.
    """
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.mappings: Dict[str, FunctionalityMapping] = {}
        self.mapping_history: List[Dict[str, Any]] = []
        
    def create_functionality_mapping(
        self,
        functionality_type: FunctionalityType,
        source_name: str,
        target_name: str,
        source_language: str,
        target_language: str,
        source_code: Optional[str] = None,
        target_code: Optional[str] = None,
        custom_id: Optional[str] = None
    ) -> FunctionalityMapping:
        """
        Create a new functionality mapping.
        
        Args:
            functionality_type: Type of functionality being mapped
            source_name: Name/identifier in source system
            target_name: Name/identifier in target system
            source_language: Source programming language
            target_language: Target programming language
            source_code: Source code (optional)
            target_code: Target code (optional)
            custom_id: Custom functionality ID (optional)
            
        Returns:
            FunctionalityMapping object
        """
        # Generate functionality ID
        if custom_id:
            functionality_id = custom_id
        else:
            prefix = self._get_id_prefix(functionality_type)
            functionality_id = f"{prefix}-{str(uuid.uuid4())[:8].upper()}"
        
        # Create mapping
        mapping = FunctionalityMapping(
            functionality_id=functionality_id,
            functionality_type=functionality_type,
            source_name=source_name,
            target_name=target_name,
            source_language=source_language,
            target_language=target_language,
            equivalence_level=EquivalenceLevel.MEDIUM,  # Default
            input_output_mapping=InputOutputMapping(),
            business_logic_mapping=BusinessLogicMapping()
        )
        
        # Store mapping
        self.mappings[functionality_id] = mapping
        
        # Log creation
        self.logger.info(f"Created functionality mapping: {functionality_id}")
        
        return mapping
    
    def map_inputs_outputs(
        self,
        functionality_id: str,
        source_inputs: Dict[str, Any],
        target_inputs: Dict[str, Any],
        source_outputs: Dict[str, Any],
        target_outputs: Dict[str, Any],
        data_transformations: Optional[Dict[str, str]] = None,
        validation_rules: Optional[List[str]] = None
    ) -> InputOutputMapping:
        """
        Map inputs and outputs between source and target systems.
        
        Args:
            functionality_id: ID of the functionality mapping
            source_inputs: Inputs expected by source system
            target_inputs: Inputs expected by target system
            source_outputs: Outputs produced by source system
            target_outputs: Outputs produced by target system
            data_transformations: Rules for transforming data between systems
            validation_rules: Rules for validating input/output equivalence
            
        Returns:
            Updated InputOutputMapping
        """
        if functionality_id not in self.mappings:
            raise ValueError(f"Functionality mapping {functionality_id} not found")
        
        mapping = self.mappings[functionality_id]
        
        # Update input/output mapping
        mapping.input_output_mapping = InputOutputMapping(
            source_inputs=source_inputs,
            target_inputs=target_inputs,
            source_outputs=source_outputs,
            target_outputs=target_outputs,
            data_transformations=data_transformations or {},
            validation_rules=validation_rules or []
        )
        
        # Update timestamp
        mapping.updated_at = datetime.now()
        
        self.logger.info(f"Updated input/output mapping for {functionality_id}")
        return mapping.input_output_mapping
    
    def map_business_logic(
        self,
        functionality_id: str,
        source_logic: str,
        target_logic: str,
        logic_transformations: Optional[List[str]] = None,
        business_rules: Optional[List[str]] = None,
        decision_points: Optional[List[str]] = None,
        error_handling: Optional[Dict[str, str]] = None
    ) -> BusinessLogicMapping:
        """
        Map business logic between source and target systems.
        
        Args:
            functionality_id: ID of the functionality mapping
            source_logic: Business logic in source system
            target_logic: Business logic in target system
            logic_transformations: Rules for transforming logic
            business_rules: Business rules to be preserved
            decision_points: Key decision points in the logic
            error_handling: Error handling mappings
            
        Returns:
            Updated BusinessLogicMapping
        """
        if functionality_id not in self.mappings:
            raise ValueError(f"Functionality mapping {functionality_id} not found")
        
        mapping = self.mappings[functionality_id]
        
        # Update business logic mapping
        mapping.business_logic_mapping = BusinessLogicMapping(
            source_logic=source_logic,
            target_logic=target_logic,
            logic_transformations=logic_transformations or [],
            business_rules=business_rules or [],
            decision_points=decision_points or [],
            error_handling=error_handling or {}
        )
        
        # Update timestamp
        mapping.updated_at = datetime.now()
        
        self.logger.info(f"Updated business logic mapping for {functionality_id}")
        return mapping.business_logic_mapping
    
    def validate_equivalence(
        self,
        functionality_id: str,
        test_cases: Optional[List[Dict[str, Any]]] = None
    ) -> Dict[str, Any]:
        """
        Validate the equivalence between source and target functionality.
        
        Args:
            functionality_id: ID of the functionality mapping
            test_cases: Test cases to validate equivalence
            
        Returns:
            Validation result with confidence score and issues
        """
        if functionality_id not in self.mappings:
            raise ValueError(f"Functionality mapping {functionality_id} not found")
        
        mapping = self.mappings[functionality_id]
        
        # Perform validation
        validation_result = {
            "functionality_id": functionality_id,
            "equivalence_level": mapping.equivalence_level.value,
            "confidence_score": 0.0,
            "issues": [],
            "warnings": [],
            "test_results": []
        }
        
        # Validate input/output mapping
        io_validation = self._validate_input_output_mapping(mapping.input_output_mapping)
        validation_result["io_validation"] = io_validation
        
        # Validate business logic mapping
        logic_validation = self._validate_business_logic_mapping(mapping.business_logic_mapping)
        validation_result["logic_validation"] = logic_validation
        
        # Calculate overall confidence score
        confidence_score = self._calculate_confidence_score(io_validation, logic_validation)
        validation_result["confidence_score"] = confidence_score
        
        # Update mapping confidence
        mapping.confidence_score = confidence_score
        
        # Determine validation status
        if confidence_score >= 0.8:
            mapping.validation_status = "validated"
        elif confidence_score >= 0.6:
            mapping.validation_status = "needs_review"
        else:
            mapping.validation_status = "failed"
        
        # Run test cases if provided
        if test_cases:
            test_results = self._run_test_cases(mapping, test_cases)
            validation_result["test_results"] = test_results
        
        self.logger.info(f"Validated equivalence for {functionality_id}: {confidence_score}")
        return validation_result
    
    def get_mapping_summary(self) -> Dict[str, Any]:
        """
        Get a summary of all functionality mappings.
        
        Returns:
            Summary of mappings with statistics
        """
        total_mappings = len(self.mappings)
        validated_count = sum(1 for m in self.mappings.values() if m.validation_status == "validated")
        failed_count = sum(1 for m in self.mappings.values() if m.validation_status == "failed")
        needs_review_count = sum(1 for m in self.mappings.values() if m.validation_status == "needs_review")
        
        # Group by functionality type
        type_counts = {}
        for mapping in self.mappings.values():
            type_name = mapping.functionality_type.value
            type_counts[type_name] = type_counts.get(type_name, 0) + 1
        
        # Group by language pairs
        language_pairs = {}
        for mapping in self.mappings.values():
            pair = f"{mapping.source_language}→{mapping.target_language}"
            language_pairs[pair] = language_pairs.get(pair, 0) + 1
        
        return {
            "total_mappings": total_mappings,
            "validated_count": validated_count,
            "failed_count": failed_count,
            "needs_review_count": needs_review_count,
            "type_counts": type_counts,
            "language_pairs": language_pairs,
            "average_confidence": sum(m.confidence_score for m in self.mappings.values()) / total_mappings if total_mappings > 0 else 0
        }
    
    def export_mappings(self, format: str = "json") -> str:
        """
        Export all functionality mappings.
        
        Args:
            format: Export format (json, yaml, csv)
            
        Returns:
            Exported mappings as string
        """
        if format.lower() == "json":
            return json.dumps({
                "mappings": [self._mapping_to_dict(m) for m in self.mappings.values()],
                "summary": self.get_mapping_summary(),
                "exported_at": datetime.now().isoformat()
            }, indent=2, default=str)
        else:
            raise ValueError(f"Unsupported export format: {format}")
    
    def import_mappings(self, data: str, format: str = "json") -> int:
        """
        Import functionality mappings.
        
        Args:
            data: Mappings data as string
            format: Import format (json, yaml)
            
        Returns:
            Number of mappings imported
        """
        if format.lower() == "json":
            imported_data = json.loads(data)
            mappings_data = imported_data.get("mappings", [])
            
            imported_count = 0
            for mapping_data in mappings_data:
                mapping = self._dict_to_mapping(mapping_data)
                self.mappings[mapping.functionality_id] = mapping
                imported_count += 1
            
            self.logger.info(f"Imported {imported_count} functionality mappings")
            return imported_count
        else:
            raise ValueError(f"Unsupported import format: {format}")
    
    def _get_id_prefix(self, functionality_type: FunctionalityType) -> str:
        """Get ID prefix based on functionality type."""
        prefix_map = {
            FunctionalityType.PROGRAM: "PROG",
            FunctionalityType.FUNCTION: "FUNC",
            FunctionalityType.COMPONENT: "COMP",
            FunctionalityType.API_ENDPOINT: "API",
            FunctionalityType.BUSINESS_RULE: "RULE",
            FunctionalityType.DATA_STRUCTURE: "DATA",
            FunctionalityType.WORKFLOW: "WORK",
            FunctionalityType.INTEGRATION: "INTG"
        }
        return prefix_map.get(functionality_type, "FUNC")
    
    def _validate_input_output_mapping(self, io_mapping: InputOutputMapping) -> Dict[str, Any]:
        """Validate input/output mapping."""
        issues = []
        warnings = []
        
        # Check for missing input mappings
        if not io_mapping.source_inputs and not io_mapping.target_inputs:
            warnings.append("No input mappings defined")
        
        # Check for missing output mappings
        if not io_mapping.source_outputs and not io_mapping.target_outputs:
            warnings.append("No output mappings defined")
        
        # Check data transformations
        if io_mapping.data_transformations:
            for source_field, target_field in io_mapping.data_transformations.items():
                if source_field not in io_mapping.source_inputs:
                    issues.append(f"Data transformation references unknown source field: {source_field}")
        
        return {
            "valid": len(issues) == 0,
            "issues": issues,
            "warnings": warnings,
            "score": 1.0 - (len(issues) * 0.2) - (len(warnings) * 0.1)
        }
    
    def _validate_business_logic_mapping(self, logic_mapping: BusinessLogicMapping) -> Dict[str, Any]:
        """Validate business logic mapping."""
        issues = []
        warnings = []
        
        # Check for missing logic
        if not logic_mapping.source_logic and not logic_mapping.target_logic:
            warnings.append("No business logic defined")
        
        # Check business rules
        if not logic_mapping.business_rules:
            warnings.append("No business rules defined")
        
        # Check error handling
        if logic_mapping.error_handling:
            for source_error, target_error in logic_mapping.error_handling.items():
                if not source_error or not target_error:
                    issues.append("Invalid error handling mapping")
        
        return {
            "valid": len(issues) == 0,
            "issues": issues,
            "warnings": warnings,
            "score": 1.0 - (len(issues) * 0.2) - (len(warnings) * 0.1)
        }
    
    def _calculate_confidence_score(self, io_validation: Dict[str, Any], logic_validation: Dict[str, Any]) -> float:
        """Calculate overall confidence score."""
        io_score = io_validation.get("score", 0.0)
        logic_score = logic_validation.get("score", 0.0)
        
        # Weight: 60% input/output, 40% business logic
        return (io_score * 0.6) + (logic_score * 0.4)
    
    def _run_test_cases(self, mapping: FunctionalityMapping, test_cases: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Run test cases to validate functionality equivalence."""
        results = []
        
        for i, test_case in enumerate(test_cases):
            result = {
                "test_case_id": i + 1,
                "inputs": test_case.get("inputs", {}),
                "expected_outputs": test_case.get("expected_outputs", {}),
                "passed": False,
                "actual_outputs": {},
                "errors": []
            }
            
            # This would be implemented based on the specific language pair
            # For now, we'll just mark as not implemented
            result["errors"].append("Test case execution not implemented for this language pair")
            
            results.append(result)
        
        return results
    
    def _mapping_to_dict(self, mapping: FunctionalityMapping) -> Dict[str, Any]:
        """Convert mapping to dictionary for export."""
        return {
            "functionality_id": mapping.functionality_id,
            "functionality_type": mapping.functionality_type.value,
            "source_name": mapping.source_name,
            "target_name": mapping.target_name,
            "source_language": mapping.source_language,
            "target_language": mapping.target_language,
            "equivalence_level": mapping.equivalence_level.value,
            "input_output_mapping": {
                "source_inputs": mapping.input_output_mapping.source_inputs,
                "target_inputs": mapping.input_output_mapping.target_inputs,
                "source_outputs": mapping.input_output_mapping.source_outputs,
                "target_outputs": mapping.input_output_mapping.target_outputs,
                "data_transformations": mapping.input_output_mapping.data_transformations,
                "validation_rules": mapping.input_output_mapping.validation_rules
            },
            "business_logic_mapping": {
                "source_logic": mapping.business_logic_mapping.source_logic,
                "target_logic": mapping.business_logic_mapping.target_logic,
                "logic_transformations": mapping.business_logic_mapping.logic_transformations,
                "business_rules": mapping.business_logic_mapping.business_rules,
                "decision_points": mapping.business_logic_mapping.decision_points,
                "error_handling": mapping.business_logic_mapping.error_handling
            },
            "dependencies": mapping.dependencies,
            "constraints": mapping.constraints,
            "notes": mapping.notes,
            "created_at": mapping.created_at.isoformat(),
            "updated_at": mapping.updated_at.isoformat(),
            "confidence_score": mapping.confidence_score,
            "validation_status": mapping.validation_status
        }
    
    def _dict_to_mapping(self, mapping_dict: Dict[str, Any]) -> FunctionalityMapping:
        """Convert dictionary to mapping for import."""
        return FunctionalityMapping(
            functionality_id=mapping_dict["functionality_id"],
            functionality_type=FunctionalityType(mapping_dict["functionality_type"]),
            source_name=mapping_dict["source_name"],
            target_name=mapping_dict["target_name"],
            source_language=mapping_dict["source_language"],
            target_language=mapping_dict["target_language"],
            equivalence_level=EquivalenceLevel(mapping_dict["equivalence_level"]),
            input_output_mapping=InputOutputMapping(**mapping_dict["input_output_mapping"]),
            business_logic_mapping=BusinessLogicMapping(**mapping_dict["business_logic_mapping"]),
            dependencies=mapping_dict.get("dependencies", []),
            constraints=mapping_dict.get("constraints", []),
            notes=mapping_dict.get("notes", ""),
            created_at=datetime.fromisoformat(mapping_dict["created_at"]),
            updated_at=datetime.fromisoformat(mapping_dict["updated_at"]),
            confidence_score=mapping_dict.get("confidence_score", 0.0),
            validation_status=mapping_dict.get("validation_status", "pending")
        ) 