# Functionality Mapper Architecture Organization

## Overview

This document explains the architectural organization of the functionality mapping system and why the base functionality mapper is located in a shared infrastructure folder and specialized mappers are located within their respective modernizer folders rather than in the general `engine/agents/` directory.

## Architectural Decision

### Original Placement (Not Recommended)
```
engine/agents/
├── functionality_mapper.py          # Base functionality mapper
├── cobol_functionality_mapper.py   # COBOL-specific mapper
├── website_functionality_mapper.py # Website-specific mapper
└── ...
```

### Current Placement (Recommended)
```
engine/
├── functionality_mapper/            # Shared infrastructure
│   ├── __init__.py                # Exposes base classes
│   └── base_mapper.py             # Base FunctionalityMapper
├── modernizers/
│   ├── cobol_system/
│   │   ├── functionality_mapper/   # COBOL-specific mapper
│   │   │   ├── __init__.py        # Exposes COBOL classes
│   │   │   └── functionality_mapper.py
│   │   ├── transpilers/
│   │   ├── parsers/
│   │   └── ...
│   ├── static_site/
│   │   ├── functionality_mapper/   # Website-specific mapper
│   │   │   ├── __init__.py        # Exposes website classes
│   │   │   └── functionality_mapper.py
│   │   ├── transpilers/
│   │   ├── parsers/
│   │   └── ...
│   └── ...
└── agents/                         # LLM-related functionality only
    ├── agent.py
    ├── code_analyzer.py
    ├── optimizer.py
    └── reviewer.py
```

## Rationale for Current Organization

### 1. **Clear Separation of Concerns**
- **Agents**: LLM-related functionality with prompts and AI capabilities
- **Functionality Mapper**: Pure business logic for system mapping (no LLM components)
- **Modernizers**: Domain-specific transformation logic with specialized mappers

### 2. **Shared Infrastructure**
- **Base Functionality Mapper**: Provides common infrastructure for all modernizers
- **Specialized Mappers**: Extend base functionality for specific domains
- **Reusability**: Base mapper can be used by any modernizer

### 3. **Domain-Specific Organization**
Each modernizer is responsible for its own domain-specific functionality:
- **COBOL System**: Handles COBOL-specific features (PIC clauses, level numbers, file I/O)
- **Static Site**: Handles website-specific features (UI components, API endpoints, routing)

### 2. **Cohesion and Coupling**
- **High Cohesion**: Related functionality is grouped together
- **Low Coupling**: Each modernizer is self-contained with minimal dependencies

### 3. **Maintainability**
- Easier to maintain when related code is co-located
- Changes to COBOL functionality don't affect website functionality
- Each modernizer can evolve independently

### 4. **Extensibility**
- New modernizers can include their own functionality mappers
- No need to modify the central agents package
- Each modernizer can have domain-specific features

### 5. **Clear Ownership**
- **Base Functionality Mapper**: Shared infrastructure team
- **COBOL Functionality Mapper**: COBOL team owns COBOL-specific mapping
- **Website Functionality Mapper**: Website team owns website-specific mapping
- **Agents**: AI/LLM team owns LLM-related functionality
- Clear boundaries and responsibilities

## Import Structure

### Base Functionality Mapper
```python
from engine.functionality_mapper import (
    FunctionalityMapper, FunctionalityType, EquivalenceLevel
)
```

### COBOL Functionality Mapper
```python
from engine.modernizers.cobol_system.functionality_mapper import (
    COBOLFunctionalityMapper, COBOLFieldMapping, COBOLProgramMapping
)
```

### Website Functionality Mapper
```python
from engine.modernizers.static_site.functionality_mapper import (
    WebsiteFunctionalityMapper, WebsiteFramework, UIComponentType
)
```

## Benefits of Current Organization

### 1. **Domain-Specific Features**
Each modernizer can implement features specific to its domain:
- **COBOL**: PIC clause parsing, level number handling, COBOL structure analysis
- **Website**: UI component mapping, API endpoint mapping, legacy website analysis

### 2. **Independent Evolution**
- COBOL functionality can evolve without affecting website functionality
- Each modernizer can have its own versioning and release cycles
- Domain experts can work on their specific modernizer

### 3. **Clear Dependencies**
- Base functionality mapper provides common infrastructure
- Specialized mappers extend base functionality for specific domains
- Dependencies are explicit and minimal

### 4. **Testing and Validation**
- Each modernizer can have its own test suite
- Domain-specific tests are co-located with domain-specific code
- Easier to test integration between related components

## Migration Path

The functionality mappers were reorganized to improve separation of concerns:

1. **Base Functionality Mapper**: `engine/agents/functionality_mapper.py` → `engine/functionality_mapper/base_mapper.py`
2. **COBOL Functionality Mapper**: `engine/agents/cobol_functionality_mapper.py` → `engine/modernizers/cobol_system/functionality_mapper/functionality_mapper.py`
3. **Website Functionality Mapper**: `engine/agents/website_functionality_mapper.py` → `engine/modernizers/static_site/functionality_mapper/functionality_mapper.py`

### Updated Imports
All import statements have been updated to reflect the new organization:
- Tests: `tests/test_functionality_mapping.py`
- Demo: `examples/functionality_mapping_demo.py`
- Documentation: `docs/functionality_mapping.md`
- Specialized Mappers: Updated to import from `engine.functionality_mapper`

## Future Considerations

### Adding New Modernizers
When adding new modernizers, follow the same pattern:

```python
# engine/modernizers/new_system/functionality_mapper/functionality_mapper.py
from engine.functionality_mapper import FunctionalityMapper

class NewSystemFunctionalityMapper(FunctionalityMapper):
    """Specialized mapper for new system modernization."""
    pass
```

### Extending Existing Modernizers
Each modernizer can extend its functionality mapper with domain-specific features:

```python
# engine/modernizers/cobol_system/functionality_mapper/functionality_mapper.py
class COBOLFunctionalityMapper(FunctionalityMapper):
    def analyze_cobol_structure(self, source_code: str) -> Dict[str, Any]:
        """COBOL-specific analysis."""
        pass
```

## Conclusion

The current architectural organization provides:
- **Better separation of concerns** (LLM agents vs. functionality mapping vs. modernizers)
- **Improved maintainability** with clear boundaries
- **Clear ownership and responsibilities** for each component
- **Independent evolution** of modernizers and shared infrastructure
- **Domain-specific functionality** with shared base infrastructure
- **Reusability** of base functionality mapper across all modernizers

This organization aligns with the principle that functionality mapping is a separate concern from LLM agents, and each modernizer should be self-contained with its own parser, transformers, templates, and specialized functionality mapping capabilities. 