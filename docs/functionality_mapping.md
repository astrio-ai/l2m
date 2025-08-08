# Functionality Mapping System

## Overview

The Functionality Mapping System is a comprehensive framework for ensuring functionality equivalence during software modernization and migration projects. It provides detailed mapping between source (old) and target (new) systems, ensuring that business logic, inputs/outputs, and functionality are preserved throughout the transformation process.

## Core Concepts

### Functionality Mapping

The system creates detailed mappings between source and target systems with the following key components:

1. **Functionality ID**: A unique identifier for each mapped functionality (e.g., `PROG-001`, `FUNC-002`, `COMP-003`)
2. **Input/Output Equivalence**: Maps inputs and outputs between source and target systems
3. **Business Logic Mapping**: Preserves business rules and decision points
4. **Validation**: Ensures functional equivalence with confidence scoring

### Supported Functionality Types

- **PROGRAM**: Complete programs or modules (e.g., COBOL programs → Python modules)
- **FUNCTION**: Individual functions or methods
- **COMPONENT**: UI components (e.g., HTML elements → React components)
- **API_ENDPOINT**: API functions and endpoints
- **BUSINESS_RULE**: Business logic and rules
- **DATA_STRUCTURE**: Data models and structures
- **WORKFLOW**: Process flows and workflows
- **INTEGRATION**: External system connections

### Equivalence Levels

- **EXACT**: Perfect functional equivalence
- **HIGH**: High similarity with minor differences
- **MEDIUM**: Moderate similarity with some differences
- **LOW**: Basic similarity with significant differences
- **PARTIAL**: Partial functionality preserved

## Architecture

### Shared Infrastructure

The functionality mapping system is organized with clear separation of concerns:

```
engine/
├── functionality_mapper/           # Shared infrastructure
│   ├── __init__.py               # Exposes base classes
│   └── base_mapper.py            # Base FunctionalityMapper
├── modernizers/
│   ├── cobol_system/
│   │   ├── functionality_mapper/  # COBOL-specific mapping
│   │   │   ├── __init__.py       # Exposes COBOL classes
│   │   │   └── functionality_mapper.py
│   │   └── ...
│   └── static_site/
│       ├── functionality_mapper/  # Website-specific mapping
│       │   ├── __init__.py       # Exposes website classes
│       │   └── functionality_mapper.py
│       └── ...
└── agents/                       # LLM-related functionality only
    ├── agent.py
    ├── code_analyzer.py
    ├── optimizer.py
    └── reviewer.py
```

### Base Functionality Mapper

The `FunctionalityMapper` class provides the core functionality:

```python
from engine.functionality_mapper import FunctionalityMapper, FunctionalityType

# Create a new functionality mapping
mapper = FunctionalityMapper()
mapping = mapper.create_functionality_mapping(
    functionality_type=FunctionalityType.PROGRAM,
    source_name="LEGACY-PROG",
    target_name="modern_program",
    source_language="cobol",
    target_language="python"
)
```

### Specialized Mappers

The specialized mappers extend the base functionality mapper with domain-specific features. They are organized within their respective modernizer folders to maintain clear separation of concerns and domain ownership.

#### COBOL to Python Mapper

The `COBOLFunctionalityMapper` provides COBOL-specific features:

```python
from engine.modernizers.cobol_system.functionality_mapper import COBOLFunctionalityMapper

cobol_mapper = COBOLFunctionalityMapper()

# Create COBOL program mapping
functionality_mapping, cobol_mapping = cobol_mapper.create_cobol_program_mapping(
    "PAYROLL-PROGRAM",
    "payroll_program",
    source_code=cobol_source
)

# Map COBOL fields
field_mappings = cobol_mapper.map_cobol_fields(
    functionality_mapping.functionality_id,
    field_definitions
)

# Map COBOL paragraphs to Python functions
paragraph_mappings = {
    "MAIN-LOGIC": "main_logic",
    "CALCULATE-TAX": "calculate_tax"
}
cobol_mapper.map_cobol_paragraphs(
    functionality_mapping.functionality_id,
    paragraph_mappings
)
```

#### Website Modernization Mapper

The `WebsiteFunctionalityMapper` provides website-specific features:

```python
from engine.modernizers.static_site.functionality_mapper import (
    WebsiteFunctionalityMapper, WebsiteFramework
)

website_mapper = WebsiteFunctionalityMapper()

# Create website mapping
functionality_mapping, website_mapping = website_mapper.create_website_mapping(
    "https://legacy-site.com",
    "https://modern-site.com",
    WebsiteFramework.REACT
)

# Map UI components
component_mappings = [
    {
        "legacy_selector": "#navigation",
        "modern_component": "Navigation",
        "component_type": "navigation"
    }
]
ui_mappings = website_mapper.map_ui_components(
    functionality_mapping.functionality_id,
    component_mappings
)
```

## Usage Examples

### COBOL to Python Modernization

```python
# 1. Create COBOL program mapping
cobol_mapper = COBOLFunctionalityMapper()
functionality_mapping, cobol_mapping = cobol_mapper.create_cobol_program_mapping(
    "PAYROLL-PROGRAM",
    "payroll_program",
    source_code=cobol_source
)

# 2. Analyze COBOL structure
analysis = cobol_mapper.analyze_cobol_structure(
    functionality_mapping.functionality_id,
    cobol_source
)

# 3. Map inputs and outputs
cobol_mapper.map_inputs_outputs(
    functionality_mapping.functionality_id,
    source_inputs={"employee_data": "COBOL-RECORD"},
    target_inputs={"employee_data": "dict"},
    source_outputs={"payroll_result": "COBOL-RECORD"},
    target_outputs={"payroll_result": "dict"}
)

# 4. Map business logic
cobol_mapper.map_business_logic(
    functionality_mapping.functionality_id,
    source_logic="IF AMOUNT > 10000 THEN TAX = AMOUNT * 0.15",
    target_logic="if amount > 10000: tax = amount * 0.15",
    business_rules=["Tax rate is 15% for amounts over 10000"]
)

# 5. Validate equivalence
validation_result = cobol_mapper.validate_equivalence(
    functionality_mapping.functionality_id
)
```

### Website Modernization

```python
# 1. Create website mapping
website_mapper = WebsiteFunctionalityMapper()
functionality_mapping, website_mapping = website_mapper.create_website_mapping(
    "https://legacy-site.com",
    "https://modern-site.com",
    WebsiteFramework.REACT
)

# 2. Analyze legacy website
analysis = website_mapper.analyze_legacy_website(
    functionality_mapping.functionality_id,
    html_content
)

# 3. Map UI components
component_mappings = [
    {
        "legacy_selector": "#contact-form",
        "modern_component": "ContactForm",
        "component_type": "form",
        "props_mapping": {"action": "submitUrl"},
        "event_handlers": {"onSubmit": "handleSubmit"}
    }
]
ui_mappings = website_mapper.map_ui_components(
    functionality_mapping.functionality_id,
    component_mappings
)

# 4. Map API endpoints
api_mappings = [
    {
        "legacy_endpoint": "/api/contact",
        "modern_endpoint": "/api/contact",
        "http_method": "POST"
    }
]
api_mappings_list = website_mapper.map_api_endpoints(
    functionality_mapping.functionality_id,
    api_mappings
)

# 5. Generate modernization plan
plan = website_mapper.generate_modernization_plan(
    functionality_mapping.functionality_id
)
```

## Validation and Quality Assurance

### Equivalence Validation

The system provides comprehensive validation of functionality equivalence:

```python
# Validate a mapping
validation_result = mapper.validate_equivalence(functionality_id)

print(f"Confidence Score: {validation_result['confidence_score']}")
print(f"Validation Status: {validation_result['validation_status']}")
print(f"Issues: {validation_result['issues']}")
print(f"Warnings: {validation_result['warnings']}")
```

### Confidence Scoring

The system calculates confidence scores based on:
- Input/output mapping completeness (60% weight)
- Business logic mapping completeness (40% weight)
- Validation status: validated (≥0.8), needs_review (≥0.6), failed (<0.6)

### Test Case Generation

```python
# Generate test cases for COBOL to Python
test_cases = cobol_mapper.generate_python_equivalence_tests(
    functionality_mapping.functionality_id
)
```

## Export and Import

### Export Mappings

```python
# Export all mappings to JSON
exported_data = mapper.export_mappings("json")
print(exported_data)
```

### Import Mappings

```python
# Import mappings from JSON
new_mapper = FunctionalityMapper()
imported_count = new_mapper.import_mappings(exported_data, "json")
print(f"Imported {imported_count} mappings")
```

## Reporting and Analytics

### Mapping Summary

```python
summary = mapper.get_mapping_summary()
print(f"Total Mappings: {summary['total_mappings']}")
print(f"Validated: {summary['validated_count']}")
print(f"Average Confidence: {summary['average_confidence']:.2f}")
```

### Type and Language Pair Analysis

```python
# By functionality type
for type_name, count in summary['type_counts'].items():
    print(f"{type_name}: {count}")

# By language pair
for pair, count in summary['language_pairs'].items():
    print(f"{pair}: {count}")
```

## Best Practices

### 1. Comprehensive Mapping

- Map all inputs and outputs between source and target systems
- Preserve business rules and decision points
- Document data transformations and validation rules

### 2. Validation Strategy

- Validate mappings early and often
- Use confidence scores to prioritize review efforts
- Generate and run test cases to verify equivalence

### 3. Documentation

- Export mappings for documentation and audit trails
- Use meaningful functionality IDs for easy reference
- Document any deviations from exact equivalence

### 4. Iterative Improvement

- Start with high-level mappings and refine over time
- Use validation results to improve mappings
- Update mappings as requirements evolve

## Integration with Legacy2Modern CLI

The functionality mapping system integrates seamlessly with the Legacy2Modern CLI:

```bash
# Analyze functionality equivalence
legacy2modern analyze-equivalence source.cobol target.py

# Generate mapping report
legacy2modern generate-mapping-report project.json

# Validate modernization
legacy2modern validate-modernization source/ target/
```

## Extensibility

The system is designed to be extensible for new modernization scenarios:

1. **New Functionality Types**: Add new enum values to `FunctionalityType`
2. **Specialized Mappers**: Create new mapper classes for specific language pairs
3. **Validation Rules**: Implement custom validation logic for specific domains
4. **Export Formats**: Add support for new export/import formats

## Conclusion

The Functionality Mapping System provides a robust foundation for ensuring functionality equivalence during software modernization projects. By creating detailed mappings between source and target systems, it helps organizations:

- **Preserve Business Logic**: Ensure critical business rules are maintained
- **Validate Equivalence**: Verify that functionality is preserved with confidence scoring
- **Document Transformations**: Create audit trails of modernization decisions
- **Manage Complexity**: Handle complex modernization scenarios systematically
- **Ensure Quality**: Provide comprehensive validation and testing capabilities

This system makes software modernization more predictable, reliable, and maintainable across any technology stack or modernization scenario. 