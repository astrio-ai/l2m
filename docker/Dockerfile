# Dockerfile for Legacy2Modern multi-agent system

FROM python:3.11-slim

# Set environment variables
ENV PYTHONPATH=/app/src
ENV PYTHONUNBUFFERED=1
ENV PYTHONDONTWRITEBYTECODE=1

# Set work directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements and install Python dependencies
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy application code
COPY src/ ./src/
COPY examples/ ./examples/
COPY tests/ ./tests/
COPY docs/ ./docs/
COPY scripts/ ./scripts/
COPY pyproject.toml .
COPY pytest.ini .

# Create necessary directories
RUN mkdir -p logs output backup temp

# Set permissions
RUN chmod +x scripts/*.py

# Expose port
EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8000/health || exit 1

# Default command
CMD ["python", "-m", "src.api.server", "--host", "0.0.0.0", "--port", "8000"]
