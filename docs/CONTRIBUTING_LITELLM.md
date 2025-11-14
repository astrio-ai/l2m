# Contributing: LiteLLM Provider Support

## Overview

L2M uses [LiteLLM](https://github.com/BerriAI/litellm) as its LLM abstraction layer, which supports **100+ LLM providers**. While L2M works with any LiteLLM-supported provider, we can improve the experience by:

1. Adding provider-specific metadata
2. Creating model aliases
3. Adding provider-specific configuration
4. Testing and documenting provider usage

## Currently Supported Providers

Based on `src/resources/model-metadata.json`, we currently have metadata for:

- ✅ **OpenAI** - Full support
- ✅ **Anthropic** - Full support  
- ✅ **DeepSeek** - Supported
- ✅ **OpenRouter** - Supported (aggregator for many models)
- ✅ **Fireworks AI** - Supported
- ✅ **Gemini** (Google) - Supported
- ✅ **Vertex AI** (Google Cloud) - Supported

## LiteLLM Providers Available (Not Yet Fully Integrated)

LiteLLM supports many more providers that could be added:

### Major Providers
- **Cohere** (`cohere/command-r-plus`, `cohere/command-r`, etc.)
- **Mistral AI** (`mistral/mistral-large`, `mistral/mistral-medium`, etc.)
- **Meta** (`meta-llama/llama-3-70b-instruct`, etc.)
- **Perplexity** (`perplexity/llama-3.1-sonar-large-128k-online`, etc.)
- **Together AI** (`togethercomputer/llama-2-70b-chat`, etc.)
- **Hugging Face** (`huggingface/meta-llama/Llama-2-70b-chat-hf`, etc.)
- **Azure OpenAI** (`azure/gpt-4`, etc.)
- **AWS Bedrock** (`bedrock/anthropic.claude-3-sonnet-20240229-v1:0`, etc.)
- **Groq** (`groq/llama-3-70b-8192`, etc.)
- **Ollama** (`ollama/llama2`, etc.) - Local models!
- **vLLM** (`vllm/meta-llama/Llama-2-7b-chat-hf`) - Local models!
- **Anyscale** (`anyscale/meta-llama/Llama-2-7b-chat-hf`, etc.)
- **Replicate** (`replicate/meta/llama-2-70b-chat`, etc.)
- **Sagemaker** (`sagemaker/jumpstart-dft-meta-textgeneration-llama-2-7b`, etc.)

### Specialized Providers
- **AI21** (`ai21/j2-ultra`, `ai21/j2-grande`, etc.)
- **Aleph Alpha** (`aleph-alpha/luminous-base`, etc.)
- **NLP Cloud** (`nlp-cloud/dolphin`, etc.)
- **Petals** (`petals/meta-llama/Llama-2-70b-chat-hf`) - Distributed inference
- **Prem AI** (`prem/prem-chat`, etc.)
- **Baseten** (`baseten/qwen-72b-instruct`, etc.)
- **Cloudflare Workers AI** (`cloudflare/@cf/meta/llama-2-7b-chat-hf`, etc.)

## Contribution Opportunities

### 1. Add Model Metadata

**What**: Add metadata for new providers to `src/resources/model-metadata.json`

**Why**: Metadata includes:
- Token limits (input/output)
- Cost per token
- Supported features (prompt caching, tool choice, etc.)
- Provider information

**Example**:
```json
{
  "mistral/mistral-large-latest": {
    "max_tokens": 32000,
    "max_input_tokens": 128000,
    "max_output_tokens": 32000,
    "input_cost_per_token": 0.000002,
    "output_cost_per_token": 0.000006,
    "litellm_provider": "mistral",
    "mode": "chat",
    "supports_assistant_prefill": true,
    "supports_tool_choice": true,
    "supports_prompt_caching": false
  }
}
```

### 2. Add Model Aliases

**What**: Add convenient aliases to `MODEL_ALIASES` in `src/core/models.py`

**Why**: Makes it easier for users to use models with short names

**Example**:
```python
MODEL_ALIASES = {
    # ... existing aliases ...
    "mistral-large": "mistral/mistral-large-latest",
    "llama3": "meta-llama/llama-3-70b-instruct",
    "cohere": "cohere/command-r-plus",
}
```

### 3. Provider-Specific Configuration

**What**: Add provider-specific setup/configuration helpers

**Why**: Some providers need special setup (API keys, endpoints, etc.)

**Where**: Could add to `src/core/models.py` or create `src/core/providers/`

**Example**: Helper functions for:
- AWS Bedrock region configuration
- Azure OpenAI endpoint setup
- Ollama local server detection
- Hugging Face token management

### 4. Testing Provider Integration

**What**: Add tests for new providers

**Why**: Ensures providers work correctly with L2M's features

**Where**: `tests/basic/test_models.py` or create provider-specific test files

**Example**:
```python
def test_mistral_provider():
    """Test Mistral AI provider integration"""
    model = Model("mistral/mistral-large-latest")
    # Test model initialization, API calls, etc.
```

### 5. Documentation

**What**: Document how to use specific providers

**Why**: Helps users set up and use providers correctly

**Where**: `docs/` directory or README

**Example**: Create `docs/PROVIDERS.md` with:
- Setup instructions for each provider
- API key requirements
- Example configurations
- Known limitations

### 6. Provider-Specific Features

**What**: Implement provider-specific optimizations

**Why**: Some providers have unique features that could improve L2M

**Examples**:
- **Ollama**: Local model support, no API keys needed
- **vLLM**: High-performance local inference
- **Bedrock**: AWS integration, IAM-based auth
- **Azure**: Enterprise features, compliance

## How to Contribute

### Step 1: Choose a Provider

Pick a provider from the list above that interests you or that you use.

### Step 2: Test Basic Integration

LiteLLM should already support it! Test with:

```bash
# Set API key
export MISTRAL_API_KEY="your-key"

# Try using the model
l2m --model mistral/mistral-large-latest
```

### Step 3: Add Metadata

If it works, add metadata to `src/resources/model-metadata.json`:

1. Check LiteLLM docs for the model's specs
2. Add token limits, costs, and features
3. Test that it works correctly

### Step 4: Add Aliases (Optional)

Add convenient aliases to `MODEL_ALIASES` in `src/core/models.py`

### Step 5: Write Tests

Add tests to ensure the provider works with L2M's features.

### Step 6: Document

Update documentation with setup instructions and examples.

## Priority Providers

Based on popularity and usefulness:

1. **Mistral AI** - Popular, competitive models
2. **Cohere** - Strong reasoning models
3. **Ollama** - Local models, no API needed
4. **Groq** - Very fast inference
5. **Together AI** - Good for open-source models
6. **Azure OpenAI** - Enterprise users
7. **AWS Bedrock** - AWS ecosystem integration

## Getting Started

1. Fork the repository
2. Pick a provider from the list
3. Test it works with LiteLLM
4. Add metadata and aliases
5. Write tests
6. Submit a PR!

## Questions?

- Check LiteLLM docs: https://docs.litellm.ai/docs/providers
- Check existing provider implementations in `src/resources/model-metadata.json`
- Open an issue to discuss your contribution

## Example PR

**Title**: `feat: Add Mistral AI provider support`

**Changes**:
- Add Mistral models to `model-metadata.json`
- Add `mistral-large` alias
- Add tests for Mistral provider
- Update documentation

**Testing**:
- Tested with `mistral/mistral-large-latest`
- Verified token limits and costs
- Confirmed streaming works

