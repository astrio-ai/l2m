# Example: Adding a New Provider (Mistral AI)

This guide shows step-by-step how to add support for a new LiteLLM provider.

## Step 1: Test Basic Integration

First, verify the provider works with LiteLLM:

```bash
# Set API key
export MISTRAL_API_KEY="your-api-key-here"

# Test with L2M
l2m --model mistral/mistral-large-latest
```

If it works, proceed to add metadata!

## Step 2: Add Model Metadata

Edit `src/resources/model-metadata.json` and add:

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
  },
  "mistral/mistral-medium-latest": {
    "max_tokens": 32000,
    "max_input_tokens": 32000,
    "max_output_tokens": 32000,
    "input_cost_per_token": 0.0000006,
    "output_cost_per_token": 0.0000018,
    "litellm_provider": "mistral",
    "mode": "chat",
    "supports_assistant_prefill": true,
    "supports_tool_choice": true,
    "supports_prompt_caching": false
  }
}
```

**Where to find this info:**
- Check LiteLLM docs: https://docs.litellm.ai/docs/providers/mistral
- Check Mistral AI pricing page
- Test with LiteLLM directly to verify features

## Step 3: Add Model Aliases (Optional)

Edit `src/core/models.py` and add to `MODEL_ALIASES`:

```python
MODEL_ALIASES = {
    # ... existing aliases ...
    "mistral-large": "mistral/mistral-large-latest",
    "mistral-medium": "mistral/mistral-medium-latest",
    "mistral": "mistral/mistral-large-latest",  # Default
}
```

Now users can use: `l2m --model mistral-large`

## Step 4: Add Dependency Check (If Needed)

If the provider needs special dependencies, add to `check_for_dependencies()` in `src/core/models.py`:

```python
def check_for_dependencies(io, model_name):
    # ... existing checks ...
    
    # Check if this is a Mistral model
    if model_name.startswith("mistral/"):
        # Mistral uses standard LiteLLM, no extra deps needed
        # But you could check for API key here if needed
        pass
```

## Step 5: Write Tests

Add tests to `tests/basic/test_models.py`:

```python
def test_mistral_provider():
    """Test Mistral AI provider integration"""
    # Test model initialization
    model = Model("mistral/mistral-large-latest")
    assert model.name == "mistral/mistral-large-latest"
    
    # Test metadata loading
    assert model.max_input_tokens > 0
    assert model.max_output_tokens > 0
    
    # Test alias resolution
    model2 = Model("mistral-large")
    assert model2.name == "mistral/mistral-large-latest"
```

## Step 6: Update Documentation

Add to `docs/PROVIDERS.md` (create if doesn't exist):

```markdown
## Mistral AI

### Setup

1. Get API key from https://console.mistral.ai/
2. Set environment variable:
   ```bash
   export MISTRAL_API_KEY="your-key"
   ```

### Usage

```bash
l2m --model mistral-large
# or
l2m --model mistral/mistral-large-latest
```

### Available Models

- `mistral-large` - Best performance
- `mistral-medium` - Balanced
- `mistral-small` - Fast and efficient
```

## Step 7: Submit PR

1. Commit your changes
2. Push to your fork
3. Create PR with:
   - Title: `feat: Add Mistral AI provider support`
   - Description: What you added and tested
   - Link to this example if helpful!

## Quick Checklist

- [ ] Tested provider works with LiteLLM
- [ ] Added metadata to `model-metadata.json`
- [ ] Added aliases (optional)
- [ ] Added dependency checks (if needed)
- [ ] Wrote tests
- [ ] Updated documentation
- [ ] Tested end-to-end with L2M

That's it! The provider should now work seamlessly with L2M.

