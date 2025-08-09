# Legacy2Modern API Examples

This directory contains examples and tools for testing and using the Legacy2Modern API.

## Examples Overview

### 1. Python Client (`client_example.py`)

A comprehensive Python client that demonstrates all API endpoints.

**Features:**
- Health check and status monitoring
- COBOL transpilation (both code and file uploads)
- Website modernization and analysis
- Code analysis and complexity metrics
- Framework information retrieval

**Usage:**
```bash
# Install dependencies
pip install requests

# Run the example
python api/examples/client_example.py
```

**Example Output:**
```
🔍 Checking API health...
✅ API Status: healthy
📊 Components: {'cobol_transpiler': 'healthy', 'website_transpiler': 'healthy', 'llm_agent': 'healthy'}

==================================================
COBOL TRANSPILATION EXAMPLE
==================================================
🔄 Transpiling COBOL code...
✅ Transpilation successful!
📝 Generated Python code:
def main():
    print("Hello, World!")
```

### 2. Curl Examples (`curl_examples.sh`)

A comprehensive shell script with curl commands for testing all API endpoints.

**Features:**
- 10 different test scenarios
- Color-coded output for easy reading
- Error handling and validation
- Performance testing
- File upload examples

**Usage:**
```bash
# Make executable
chmod +x api/examples/curl_examples.sh

# Run all tests
./api/examples/curl_examples.sh

# Run individual tests (modify the script as needed)
curl -s http://localhost:8000/health | jq '.'
```

**Prerequisites:**
- `curl` - HTTP client
- `jq` - JSON processor (optional, for formatting)
- `bc` - Basic calculator (for performance tests)

## Testing Scenarios

### Basic Functionality
1. **Health Check** - Verify API is running and components are healthy
2. **Root Endpoint** - Test basic API accessibility
3. **Supported Frameworks** - Get list of available target frameworks

### COBOL Transpilation
4. **JSON Transpilation** - Send COBOL code as JSON payload
5. **File Upload** - Upload COBOL files for processing
6. **Output Generation** - Save transpiled Python code to files

### Website Modernization
7. **Website Analysis** - Analyze legacy HTML without modernization
8. **Framework Conversion** - Convert to React, Next.js, or Astro
9. **File Processing** - Handle HTML file uploads

### Advanced Features
10. **Code Analysis** - Analyze code complexity and structure
11. **Error Handling** - Test API error responses
12. **Performance Testing** - Measure response times

## Example API Calls

### Health Check
```bash
curl -s http://localhost:8000/health | jq '.'
```

### COBOL Transpilation
```bash
curl -X POST http://localhost:8000/transpile/cobol \
  -H "Content-Type: application/json" \
  -d '{
    "source_code": "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.\nPROCEDURE DIVISION.\n    DISPLAY \"Hello\".\n    STOP RUN.",
    "output_file": "/tmp/hello.py"
  }' | jq '.'
```

### File Upload
```bash
curl -X POST http://localhost:8000/transpile/cobol/file \
  -F "file=@examples/cobol/HELLO.cobol" \
  -F "output_file=/tmp/hello.py" \
  -F "llm_config={}" | jq '.'
```

### Website Analysis
```bash
curl -X POST http://localhost:8000/analyze/website \
  -H "Content-Type: application/json" \
  -d '{"input_path": "examples/website/legacy-site.html"}' | jq '.'
```

## Customization

### Environment Variables
```bash
# Customize API endpoint
export L2M_API_BASE="http://your-server:8000"

# Modify the scripts to use your endpoint
sed -i 's|http://localhost:8000|$L2M_API_BASE|g' curl_examples.sh
```

### Adding New Tests
1. **Python Client**: Add new methods to `Legacy2ModernClient` class
2. **Curl Script**: Add new test sections with appropriate curl commands
3. **Documentation**: Update this README with new examples

### Error Handling
Both examples include comprehensive error handling:
- HTTP status code checking
- Response validation
- Graceful fallbacks for missing files
- Detailed error messages

## Troubleshooting

### Common Issues

1. **API Not Running**
   ```bash
   # Check if server is running
   curl -s http://localhost:8000/health
   
   # Start the server if needed
   python api/run_server.py
   ```

2. **File Not Found**
   ```bash
   # Check if example files exist
   ls -la examples/cobol/HELLO.cobol
   ls -la examples/website/legacy-site.html
   ```

3. **Permission Denied**
   ```bash
   # Make scripts executable
   chmod +x api/examples/*.sh
   ```

4. **Dependencies Missing**
   ```bash
   # Install required tools
   pip install requests
   sudo apt-get install jq bc  # Ubuntu/Debian
   brew install jq bc          # macOS
   ```

### Debug Mode
Enable verbose output for debugging:
```bash
# Python client with debug logging
export L2M_API_LOG_LEVEL=debug
python api/examples/client_example.py

# Curl with verbose output
curl -v http://localhost:8000/health
```

## Integration Examples

### CI/CD Pipeline
```yaml
# GitHub Actions example
- name: Test API Health
  run: |
    curl -f http://localhost:8000/health
    python api/examples/client_example.py
```

### Docker Testing
```dockerfile
# Test API in container
COPY api/examples/ /app/examples/
RUN chmod +x /app/examples/*.sh
CMD ["/app/examples/curl_examples.sh"]
```

### Load Testing
```bash
# Simple load test with curl
for i in {1..100}; do
  curl -s http://localhost:8000/health > /dev/null &
done
wait
```

## Contributing

1. **Add New Examples**: Create new example files for specific use cases
2. **Improve Error Handling**: Enhance error messages and recovery
3. **Performance Testing**: Add more sophisticated performance benchmarks
4. **Documentation**: Update examples and README files

## License

These examples are provided under the same license as the main Legacy2Modern project. 