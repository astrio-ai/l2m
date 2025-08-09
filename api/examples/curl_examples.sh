#!/bin/bash
# cURL examples for the Legacy2Modern API
# Make sure the API server is running on http://localhost:8001

API_BASE="http://localhost:8001"

echo "🚀 Legacy2Modern API cURL Examples"
echo "=================================="

# Test health endpoint
echo -e "\n1. Health Check:"
curl -s "$API_BASE/health" | jq '.' 2>/dev/null || curl -s "$API_BASE/health"

# Get supported frameworks
echo -e "\n2. Get Supported Frameworks:"
curl -s "$API_BASE/frameworks" | jq '.' 2>/dev/null || curl -s "$API_BASE/frameworks"

# Transpile COBOL code
echo -e "\n3. Transpile COBOL Code:"
curl -s -X POST "$API_BASE/transpile/cobol" \
  -H "Content-Type: application/json" \
  -d '{
    "source_code": "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.\nPROCEDURE DIVISION.\nDISPLAY \"Hello, World!\".\nSTOP RUN.",
    "output_file": "hello.py"
  }' | jq '.' 2>/dev/null || curl -s -X POST "$API_BASE/transpile/cobol" \
  -H "Content-Type: application/json" \
  -d '{
    "source_code": "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.\nPROCEDURE DIVISION.\nDISPLAY \"Hello, World!\".\nSTOP RUN.",
    "output_file": "hello.py"
  }'

# Analyze Python code
echo -e "\n4. Analyze Python Code:"
curl -s -X POST "$API_BASE/analyze/code" \
  -H "Content-Type: application/json" \
  -d '{
    "source_code": "def hello():\n    print(\"Hello, World!\")\n    return \"success\""
  }' | jq '.' 2>/dev/null || curl -s -X POST "$API_BASE/analyze/code" \
  -H "Content-Type: application/json" \
  -d '{
    "source_code": "def hello():\n    print(\"Hello, World!\")\n    return \"success\""
  }'

# Test with LLM configuration
echo -e "\n5. Transpile COBOL with LLM Config:"
curl -s -X POST "$API_BASE/transpile/cobol" \
  -H "Content-Type: application/json" \
  -d '{
    "source_code": "IDENTIFICATION DIVISION.\nPROGRAM-ID. CALC.\nPROCEDURE DIVISION.\nADD 1 TO 2 GIVING RESULT.\nDISPLAY RESULT.\nSTOP RUN.",
    "llm_config": {
      "model": "claude-3-sonnet",
      "temperature": 0.1
    }
  }' | jq '.' 2>/dev/null || curl -s -X POST "$API_BASE/transpile/cobol" \
  -H "Content-Type: application/json" \
  -d '{
    "source_code": "IDENTIFICATION DIVISION.\nPROGRAM-ID. CALC.\nPROCEDURE DIVISION.\nADD 1 TO 2 GIVING RESULT.\nDISPLAY RESULT.\nSTOP RUN.",
    "llm_config": {
      "model": "claude-3-sonnet",
      "temperature": 0.1
    }
  }'

echo -e "\n🎉 cURL examples completed!"
echo -e "\n💡 To test file uploads, you can use:"
echo -e "   curl -X POST \"$API_BASE/transpile/cobol/file\" \\"
echo -e "     -F \"file=@your_file.cobol\" \\"
echo -e "     -F \"output_file=output.py\"" 