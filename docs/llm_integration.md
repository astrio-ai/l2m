# Enhanced LLM Integration for Legacy2Modern

This document describes the enhanced LLM (Large Language Model) integration feature that provides comprehensive AI-assisted capabilities for the Legacy2Modern transpiler, supporting both COBOL transpilation and website modernization.

## Overview

The enhanced LLM integration goes beyond simple edge case translation to provide a complete AI-powered development experience. It includes intelligent code analysis, optimization, automated code review, and documentation generation for multiple language pairs and frameworks.

## Features

### 🤖 Multi-Provider LLM Support
- **Anthropic Claude**: Primary LLM for code analysis and generation (Claude-3.5-Sonnet, Claude-3-Haiku)
- **OpenAI GPT Models**: GPT-4, GPT-3.5-turbo, and other OpenAI models
- **Local LLMs**: Ollama, local model support for privacy and cost control
- **Provider Abstraction**: Easy switching between providers

### 🔧 Advanced Configuration
- **Caching**: In-memory response caching with TTL
- **Retry Logic**: Exponential backoff with configurable attempts
- **Temperature Control**: Adjustable creativity levels
- **Token Limits**: Configurable response length limits

### 📊 Intelligent Code Analysis
- **COBOL Transformation Quality**: Assess COBOL to Python transformation quality
- **Website Modernization Analysis**: Assess HTML to React/Next.js/Astro transformation quality
- **Complexity Scoring**: Evaluate code complexity and maintainability
- **Performance Analysis**: Identify performance bottlenecks and issues
- **Security Review**: Detect security vulnerabilities and concerns
- **Improvement Suggestions**: AI-powered recommendations for better code

### ⚡ Code Optimization
- **Performance Optimization**: AI-driven performance improvements
- **Readability Enhancement**: Code clarity and maintainability improvements
- **Best Practices**: Modern language feature adoption
- **Framework Optimization**: React/Next.js/Astro best practices
- **Benchmarking**: Performance comparison between original and optimized code

### 🔍 Automated Code Review
- **Comprehensive Review**: Full code review with issue detection
- **Security-Focused Review**: Specialized security vulnerability detection
- **Performance Review**: Performance-specific analysis and suggestions
- **Framework-Specific Review**: React/Next.js/Astro best practices review
- **Quality Metrics**: Code quality scoring and validation

### 📚 Documentation Generation
- **Auto-Documentation**: Generate comprehensive code documentation
- **Function Descriptions**: AI-generated function and class documentation
- **Usage Examples**: Code examples and usage patterns
- **Best Practices**: Documentation following industry standards
- **Component Documentation**: React component documentation and props

## Setup

### 1. Environment Configuration

Create a `.env` file in the project root:

```bash
# LLM API Configuration
LLM_API_KEY=your_api_key_here
LLM_MODEL=claude-3-5-sonnet-20241022
LLM_PROVIDER=anthropic  # anthropic, openai, local
DEFAULT_LLM_TEMPERATURE=0.1

# Advanced Configuration
LLM_CACHE_ENABLED=true
LLM_CACHE_TTL=3600
LLM_RETRY_ATTEMPTS=3
LLM_RETRY_DELAY=1.0

# Transpiler Configuration
LOG_LEVEL=INFO
EDGE_CASE_REPORT_PATH=edge_cases_report.txt
```

### 2. Install Dependencies

```bash
pip install -r requirements.txt
```

### 3. Verify Configuration

```bash
python run_cli.py
# Then in the CLI: /check-llm
```

## Usage

### Enhanced CLI Interface

```bash
# Start the interactive CLI
python run_cli.py

# In the CLI:
> transpile examples/cobol/HELLO.cobol
> modernize examples/website/legacy-site.html
> analyze the generated code
> /check-llm
```

### Programmatic Usage

```python
from engine.agents.agent import LLMAgent
from engine.modernizers.cobol_system.transpilers.llm_augmentor import LLMConfig

# Create LLM configuration
llm_config = LLMConfig.from_env()
agent = LLMAgent(llm_config)

# Analyze COBOL transformation
analysis = agent.analyze_code(source_code, target_code, "cobol-python")
print(f"Complexity Score: {analysis.complexity_score}")

# Review generated code
review = agent.review_code(target_code, "python")
print(f"Review Severity: {review.severity}")

# Optimize code
optimization = agent.optimize_code(target_code, "python")
print(f"Optimization Confidence: {optimization.confidence}")

# Generate documentation
documentation = agent.generate_documentation(target_code, "python")
print(f"Documentation: {len(documentation)} characters")
```

## Architecture

### Core Components

#### LLM Agent System (`engine/agents/`)
- **agent.py**: Main LLM agent orchestrator with multi-provider support
- **code_analyzer.py**: Intelligent code analysis and complexity assessment
- **optimizer.py**: AI-powered code optimization and improvement
- **reviewer.py**: Automated code review and quality assessment
- **results.py**: Result data classes for analysis, optimization, and review

#### COBOL System Integration (`engine/modernizers/cobol_system/`)
- **llm_augmentor.py**: Enhanced with multi-provider support, caching, and retry logic
- **hybrid_transpiler.py**: Combines traditional transpilation with LLM augmentation
- **edge_case_detector.py**: AI-powered detection of complex COBOL patterns

#### Website Modernization Integration (`engine/modernizers/static_site/`)
- **llm_augmentor.py**: LLM integration for website modernization
- **agent.py**: Website-specific LLM agent for analysis and optimization
- **transpiler.py**: LLM-powered website transpiler

### Multi-Provider Support

The system supports multiple LLM providers through a clean abstraction layer:

```python
class LLMProvider(ABC):
    @abstractmethod
    def generate_response(self, messages: List[Dict[str, str]], config: LLMConfig) -> str:
        pass

class AnthropicProvider(LLMProvider):
    # Anthropic Claude implementation

class OpenAIProvider(LLMProvider):
    # OpenAI implementation

class LocalProvider(LLMProvider):
    # Local LLM implementation
```

## Configuration

### Provider-Specific Setup

#### Anthropic (Recommended)
```bash
LLM_PROVIDER=anthropic
LLM_API_KEY=sk-ant-your-anthropic-key
LLM_MODEL=claude-3-5-sonnet-20241022
```

#### OpenAI
```bash
LLM_PROVIDER=openai
LLM_API_KEY=sk-your-openai-key
LLM_MODEL=gpt-4
```

#### Local (Ollama)
```bash
LLM_PROVIDER=local
LLM_MODEL=codellama:7b
```

### Advanced Configuration

#### Caching
```bash
LLM_CACHE_ENABLED=true
LLM_CACHE_TTL=3600  # 1 hour
```

#### Retry Logic
```bash
LLM_RETRY_ATTEMPTS=3
LLM_RETRY_DELAY=1.0  # seconds
```

#### Temperature Control
```bash
DEFAULT_LLM_TEMPERATURE=0.1  # Low for code generation, higher for analysis
```

## Use Cases

### COBOL Transpilation
- **Complex Logic Analysis**: AI-powered analysis of complex COBOL business logic
- **Edge Case Detection**: Identify and handle edge cases in COBOL constructs
- **Code Quality Assessment**: Evaluate the quality of COBOL to Python transformations
- **Performance Optimization**: Optimize generated Python code for better performance
- **Documentation Generation**: Generate comprehensive documentation for transpiled code

### Website Modernization
- **Component Analysis**: Analyze legacy HTML structure for component extraction
- **Style Modernization**: Convert Bootstrap to Tailwind CSS with AI guidance
- **JavaScript Transformation**: Convert jQuery to React hooks with best practices
- **Framework Selection**: AI-powered framework recommendation (React/Next.js/Astro)
- **Performance Optimization**: Optimize modernized websites for better performance

## Testing

### Test Coverage
```bash
# Run COBOL LLM integration tests
pytest tests/cobol_system/test_llm_integration.py -v

# Run website modernization LLM tests
pytest tests/static_site/test_llm_integration.py -v

# Run all LLM tests
pytest tests/ -k "llm" -v
```

### Quality Assurance
- **Code Quality Metrics**: Automated assessment of generated code quality
- **Performance Benchmarks**: Comparison of original vs. optimized code
- **Security Validation**: Automated security review of generated code
- **Best Practices Validation**: Ensure generated code follows modern best practices

## Performance

### Caching Benefits
- **Reduced API Calls**: Cache frequently requested translations
- **Faster Response Times**: Instant responses for cached content
- **Cost Savings**: Lower API usage costs
- **Configurable TTL**: Flexible cache expiration

### Retry Logic Benefits
- **Improved Reliability**: Handle transient API failures
- **Exponential Backoff**: Intelligent retry timing
- **Graceful Degradation**: Fallback to rule-based when AI unavailable
- **Configurable Attempts**: Adjust retry behavior per use case

## Security

### API Key Management
- **Environment Variables**: Secure key storage
- **Provider Isolation**: Separate keys for different providers
- **Key Rotation**: Support for key updates
- **Access Control**: Restrict API key access

### Code Safety
- **Response Validation**: Validate AI-generated code
- **Security Scanning**: Detect vulnerabilities
- **Input Sanitization**: Sanitize inputs and outputs
- **Code Review**: Human review recommendations

## Future Enhancements

### Planned Features
- **Multi-Language Support**: Extend LLM capabilities to other legacy languages
- **Advanced Caching**: Persistent caching with database storage
- **Batch Processing**: Efficient processing of multiple files
- **Custom Models**: Support for fine-tuned models for specific domains
- **Real-time Collaboration**: Multi-user collaboration features

### Performance Optimizations
- **Parallel Processing**: Concurrent LLM requests for faster processing
- **Smart Caching**: Intelligent cache invalidation and management
- **Resource Optimization**: Efficient memory and CPU usage
- **Network Optimization**: Optimized API calls and response handling 