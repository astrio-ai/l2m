# Provider Setup Guide

This guide provides detailed setup instructions for each LLM provider supported by L2M. L2M uses [LiteLLM](https://github.com/BerriAI/litellm) under the hood, which enables support for 100+ LLM providers.

## Table of Contents

- [Quick Start](#quick-start)
- [Provider Priority](#provider-priority)
- [Supported Providers](#supported-providers)
  - [OpenAI](#openai)
  - [Anthropic (Claude)](#anthropic-claude)
  - [Google Gemini](#google-gemini)
  - [DeepSeek](#deepseek)
  - [OpenRouter](#openrouter)
  - [Groq](#groq)
  - [Fireworks AI](#fireworks-ai)
  - [GitHub Copilot](#github-copilot)
  - [AWS Bedrock](#aws-bedrock)
  - [Azure OpenAI](#azure-openai)
  - [Ollama (Local)](#ollama-local)
- [Model Selection](#model-selection)
- [Common Issues](#common-issues)

## Quick Start

1. Copy the example environment file:
   ```bash
   cp .env.example .env
   ```

2. Add your API key to `.env`:
   ```env
   # Example for OpenAI
   OPENAI_API_KEY=sk-...
   ```

3. Run L2M:
   ```bash
   l2m
   ```

## Provider Priority

L2M automatically selects the first available provider based on this priority order:

1. **OpenAI** (if `OPENAI_API_KEY` is set)
2. **Anthropic** (if `ANTHROPIC_API_KEY` is set)
3. **Gemini** (if `GEMINI_API_KEY` is set)
4. **DeepSeek** (if `DEEPSEEK_API_KEY` is set)
5. **OpenRouter** (if `OPENROUTER_API_KEY` is set)

You can override this by specifying a model explicitly:
```bash
l2m --model anthropic/claude-sonnet-4-20250514
```

## Supported Providers

### OpenAI

OpenAI provides access to GPT models including GPT-4o, GPT-4 Turbo, and GPT-3.5.

#### Setup

1. Get your API key from [OpenAI Platform](https://platform.openai.com/api-keys)

2. Add to `.env`:
   ```env
   OPENAI_API_KEY=sk-proj-...
   ```

#### Available Models

- `gpt-4o` (Default - recommended)
- `gpt-4o-mini` (Faster, cheaper)
- `gpt-4-turbo`
- `gpt-3.5-turbo`
- `o1` (Reasoning model)
- `o1-mini` (Faster reasoning)

#### Model Override

```env
OPENAI_MODEL=gpt-4o-mini
```

#### Example Usage

```bash
# Use default OpenAI model
l2m

# Use specific OpenAI model
l2m --model gpt-4o-mini
```

#### Cost

- GPT-4o: $2.50/$10.00 per 1M tokens (input/output)
- GPT-4o-mini: $0.15/$0.60 per 1M tokens
- GPT-3.5-turbo: $0.50/$1.50 per 1M tokens

See [OpenAI Pricing](https://openai.com/api/pricing/) for current rates.

---

### Anthropic (Claude)

Anthropic provides Claude models, known for excellent coding capabilities.

#### Setup

1. Get your API key from [Anthropic Console](https://console.anthropic.com/settings/keys)

2. Add to `.env`:
   ```env
   ANTHROPIC_API_KEY=sk-ant-...
   ```

#### Available Models

- `claude-sonnet-4-20250514` (Latest Sonnet - recommended)
- `claude-opus-4-20250514` (Most capable)
- `claude-3-5-sonnet-20241022` (Previous Sonnet)
- `claude-3-5-haiku-20241022` (Fastest)

#### Model Aliases

L2M provides convenient aliases:
- `sonnet` → `claude-sonnet-4-20250514`
- `opus` → `claude-opus-4-20250514`
- `haiku` → `claude-3-5-haiku-20241022`

#### Model Override

```env
ANTHROPIC_MODEL=claude-sonnet-4-20250514
```

#### Example Usage

```bash
# Use Claude Sonnet 4
l2m --model sonnet

# Use Claude Opus 4
l2m --model opus

# Use full model name
l2m --model anthropic/claude-sonnet-4-20250514
```

#### Cost

- Claude Sonnet 4: $3.00/$15.00 per 1M tokens (input/output)
- Claude Opus 4: $15.00/$75.00 per 1M tokens
- Claude Haiku 3.5: $0.80/$4.00 per 1M tokens

See [Anthropic Pricing](https://www.anthropic.com/pricing) for current rates.

---

### Google Gemini

Google's Gemini models offer multimodal capabilities and long context windows.

#### Setup

1. Get your API key from [Google AI Studio](https://aistudio.google.com/app/apikey)

2. Add to `.env`:
   ```env
   GEMINI_API_KEY=...
   ```

#### Available Models

- `gemini-2.5-pro` (Default - most capable)
- `gemini-2.5-flash` (Faster, cheaper)
- `gemini-2.5-flash-lite` (Lightest)

#### Model Aliases

- `gemini` → `gemini-2.5-pro`
- `flash` → `gemini-2.5-flash`
- `flash-lite` → `gemini-2.5-flash-lite`

#### Model Override

```env
GEMINI_MODEL=gemini-2.5-flash
```

#### Example Usage

```bash
# Use Gemini Pro
l2m --model gemini

# Use Gemini Flash
l2m --model flash
```

#### Cost

- Gemini 2.5 Pro: Free tier available, then $1.25/$5.00 per 1M tokens
- Gemini 2.5 Flash: Free tier available, then $0.075/$0.30 per 1M tokens

See [Google AI Pricing](https://ai.google.dev/pricing) for current rates.

---

### DeepSeek

DeepSeek provides cost-effective models with strong coding capabilities.

#### Setup

1. Get your API key from [DeepSeek Platform](https://platform.deepseek.com/api_keys)

2. Add to `.env`:
   ```env
   DEEPSEEK_API_KEY=...
   ```

#### Available Models

- `deepseek-chat` (General purpose)
- `deepseek-coder` (Code-optimized)
- `deepseek-reasoner` (R1 reasoning model)
- `deepseek-v3.2-exp` (Experimental v3.2)

#### Model Aliases

- `deepseek` → `deepseek-chat`
- `r1` → `deepseek-reasoner`

#### Model Override

```env
DEEPSEEK_MODEL=deepseek-coder
```

#### Example Usage

```bash
# Use DeepSeek Chat
l2m --model deepseek

# Use DeepSeek R1 (reasoning)
l2m --model r1
```

#### Cost

- DeepSeek is significantly cheaper than major providers
- Check [DeepSeek Pricing](https://platform.deepseek.com/pricing) for current rates

---

### OpenRouter

OpenRouter provides access to 100+ models from different providers through a single API.

#### Setup

1. Get your API key from [OpenRouter](https://openrouter.ai/settings/integrations)

2. Add to `.env`:
   ```env
   OPENROUTER_API_KEY=...
   ```

#### Features

- Access to 100+ models including GPT-4, Claude, Llama, and more
- Automatic fallback between models
- Free tier available (with strict rate limits)
- Pay-as-you-go pricing

#### Example Models

- `openrouter/openai/gpt-4o`
- `openrouter/anthropic/claude-sonnet-4-20250514`
- `openrouter/meta-llama/llama-3.3-70b-instruct`
- `openrouter/qwen/qwen-2.5-72b-instruct:free` (Free tier)

#### Example Usage

```bash
# Use a specific model via OpenRouter
l2m --model openrouter/anthropic/claude-sonnet-4-20250514

# Use free tier model
l2m --model openrouter/qwen/qwen-2.5-72b-instruct:free
```

#### Free Tier Limitations

The free tier has strict rate limits. For production use:
1. Add your OpenRouter API key for better limits
2. Consider using your own provider API keys
3. Switch to a different model if rate limited

#### Cost

Varies by model. See [OpenRouter Models](https://openrouter.ai/models) for pricing.

---

### Groq

Groq provides fast inference for open-source models.

#### Setup

1. Get your API key from [Groq Console](https://console.groq.com/keys)

2. Add to `.env`:
   ```env
   GROQ_API_KEY=...
   ```

#### Available Models

- `groq/llama-3.3-70b-versatile`
- `groq/llama-3.1-8b-instant`
- `groq/mixtral-8x7b-32768`

#### Example Usage

```bash
l2m --model groq/llama-3.3-70b-versatile
```

#### Features

- Extremely fast inference
- Free tier available
- Lower cost than major providers

---

### Fireworks AI

Fireworks AI offers fast inference for open and proprietary models.

#### Setup

1. Get your API key from [Fireworks AI](https://fireworks.ai/)

2. Add to `.env`:
   ```env
   FIREWORKS_API_KEY=...
   ```

#### Example Usage

```bash
l2m --model fireworks_ai/accounts/fireworks/models/llama-v3p1-70b-instruct
```

---

### GitHub Copilot

Use your GitHub Copilot subscription to access GPT models.

#### Setup

1. Get your GitHub token from [GitHub Settings](https://github.com/settings/tokens)
   - Requires Copilot access permissions

2. Add to `.env`:
   ```env
   GITHUB_COPILOT_TOKEN=ghp_...
   ```

#### How It Works

L2M automatically converts your GitHub Copilot token to an OpenAI-compatible token.

#### Example Usage

```bash
# L2M will automatically use Copilot credentials
l2m --model gpt-4o
```

#### Limitations

- Requires active GitHub Copilot subscription
- Token expires and needs periodic refresh (handled automatically)

---

### AWS Bedrock

Access Claude and other models via AWS Bedrock.

#### Prerequisites

```bash
pip install boto3
```

#### Setup

##### Option 1: AWS Profile (Recommended)

```env
AWS_PROFILE=default
```

##### Option 2: Access Keys

```env
AWS_ACCESS_KEY_ID=...
AWS_SECRET_ACCESS_KEY=...
AWS_REGION=us-east-1
```

#### Available Models

- `bedrock/anthropic.claude-sonnet-4-20250514-v1:0`
- `bedrock/anthropic.claude-opus-4-20250514-v1:0`
- `bedrock/anthropic.claude-3-5-sonnet-20241022-v2:0`

#### Example Usage

```bash
l2m --model bedrock/anthropic.claude-sonnet-4-20250514-v1:0
```

#### Cost

Bedrock pricing varies by region. See [AWS Bedrock Pricing](https://aws.amazon.com/bedrock/pricing/).

---

### Azure OpenAI

Use OpenAI models via Azure.

#### Setup

```env
OPENAI_API_KEY=...
OPENAI_API_BASE=https://YOUR_RESOURCE.openai.azure.com/
OPENAI_API_VERSION=2024-02-15-preview
OPENAI_API_TYPE=azure
```

#### Example Usage

```bash
l2m --model azure/gpt-4o
```

---

### Ollama (Local)

Run models locally using Ollama.

#### Prerequisites

1. Install Ollama from [ollama.com](https://ollama.com)

2. Pull a model:
   ```bash
   ollama pull llama3.3
   ```

#### Setup

No API key required. Ollama must be running locally.

#### Example Usage

```bash
# Use Llama 3.3 locally
l2m --model ollama/llama3.3

# Use other Ollama models
l2m --model ollama/codellama
l2m --model ollama/qwen2.5-coder
```

#### Benefits

- No API costs
- Complete privacy
- Works offline
- Full control over models

#### Limitations

- Requires local compute resources
- Slower than cloud providers (depending on hardware)
- Smaller context windows

---

## Model Selection

### Using Command Line

```bash
# Specify model with --model flag
l2m --model sonnet
l2m --model gpt-4o
l2m --model gemini
```

### Using Environment Variable

```env
L2M_MODEL=gpt-4o
```

### Switching Models Mid-Session

```bash
# Inside L2M CLI
/model sonnet
/model gpt-4o-mini
```

### List Available Models

```bash
l2m --list-models
```

## Common Issues

### Missing API Key Error

**Error**: `Warning: <model> expects these environment variables`

**Solution**:
1. Check your `.env` file exists in your project directory
2. Verify the API key is set correctly
3. Restart your terminal if you just set environment variables

### Model Not Found

**Error**: `Warning: Unknown context window size and costs`

**Solution**:
1. Check model name spelling
2. Use `l2m --list-models` to see available models
3. Try a suggested similar model name

### Rate Limit Errors

**Free Tier Models**:
- Switch to a different model
- Add your own API key for better rate limits
- Wait 5-10 minutes before retrying

**Paid Models**:
- Check your API quota/credits
- Wait a few minutes before retrying
- Consider upgrading your plan

### Bedrock/AWS Errors

**Error**: Missing AWS credentials

**Solutions**:
```bash
# Install boto3
pip install boto3

# Configure AWS CLI
aws configure

# Or use AWS_PROFILE
export AWS_PROFILE=default
```

### Azure OpenAI Errors

**Error**: Missing Azure configuration

**Solution**: Ensure all Azure-specific variables are set:
```env
OPENAI_API_KEY=<your-azure-key>
OPENAI_API_BASE=https://YOUR_RESOURCE.openai.azure.com/
OPENAI_API_VERSION=2024-02-15-preview
OPENAI_API_TYPE=azure
```

### Ollama Connection Error

**Error**: Cannot connect to Ollama

**Solutions**:
1. Start Ollama: `ollama serve`
2. Verify Ollama is running: `ollama list`
3. Check Ollama is on default port (11434)

## Best Practices

1. **Use `.env` files**: Keep API keys out of version control
2. **Start with cheaper models**: Use `gpt-4o-mini` or `flash` for testing
3. **Monitor usage**: Check your provider dashboards regularly
4. **Use model aliases**: Easier than remembering full model names
5. **Keep keys secure**: Never commit `.env` files to git

## Getting Help

- **GitHub Issues**: [Report bugs](https://github.com/astrio-ai/l2m/issues)
- **Discord**: [Join community](https://discord.gg/2BVwAUzW)
- **Email**: naingoolwin.astrio@gmail.com
- **LiteLLM Docs**: [LiteLLM Provider Docs](https://docs.litellm.ai/docs/providers)

## Contributing

Found an issue with this guide or want to add a provider? See [CONTRIBUTING.md](../CONTRIBUTING.md) for how to contribute.
