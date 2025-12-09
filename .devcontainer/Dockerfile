FROM python:3.14-slim-bookworm AS base

# Install system dependencies
RUN apt-get update && \
    apt-get install --no-install-recommends -y build-essential git libportaudio2 pandoc && \
    rm -rf /var/lib/apt/lists/*

# Create app user with UID 1000
RUN useradd -m -u 1000 -s /bin/bash appuser

WORKDIR /app

# Create virtual environment
RUN python -m venv /venv
ENV PATH="/venv/bin:$PATH"

# Playwright browser settings (optional, for browser automation features)
# ENV PLAYWRIGHT_BROWSERS_PATH=/home/appuser/pw-browsers
# ENV PLAYWRIGHT_SKIP_BROWSER_GC=1

# Create directories with proper permissions
RUN mkdir -p /home/appuser/.l2m /home/appuser/.cache && \
    chown -R appuser:appuser /home/appuser /app /venv && \
    chmod -R 777 /home/appuser/.l2m /home/appuser/.cache

# So git doesn't complain about unusual permissions
RUN git config --system --add safe.directory /app

# Set default git user config to avoid warnings (users can override with --global)
RUN git config --system user.name "L2M User" && \
    git config --system user.email "l2m@localhost"

# This puts the container's ~/.l2m into the host's project directory (usually host's cwd).
# That way caches, version checks, etc get stored in the host filesystem not
# simply discarded every time the container exits.
ENV HOME=/app

#########################
FROM base AS l2m-full

ENV L2M_DOCKER_IMAGE=astrio-ai/l2m-full

COPY . /tmp/l2m

# Install dependencies as root
RUN /venv/bin/python -m pip install --upgrade --no-cache-dir pip && \
    /venv/bin/python -m pip install --no-cache-dir /tmp/l2m boto3 && \
    rm -rf /tmp/l2m

# Note: Playwright not needed for basic L2M usage
# Uncomment if you need browser automation features:
# RUN /venv/bin/python -m playwright install --with-deps chromium

# Fix site-packages permissions
RUN find /venv/lib/python3.14/site-packages \( -type d -exec chmod a+rwx {} + \) -o \( -type f -exec chmod a+rw {} + \)

# Switch to appuser
USER appuser

ENTRYPOINT ["/venv/bin/l2m"]

#########################
FROM base AS l2m

ENV L2M_DOCKER_IMAGE=astrio-ai/l2m

COPY . /tmp/l2m

# Install dependencies as root
RUN /venv/bin/python -m pip install --upgrade --no-cache-dir pip && \
    /venv/bin/python -m pip install --no-cache-dir /tmp/l2m boto3 google-cloud-aiplatform && \
    rm -rf /tmp/l2m

# Note: Playwright not needed for basic L2M usage
# Uncomment if you need browser automation features:
# RUN /venv/bin/python -m playwright install --with-deps chromium

# Fix site-packages permissions
RUN find /venv/lib/python3.14/site-packages \( -type d -exec chmod a+rwx {} + \) -o \( -type f -exec chmod a+rw {} + \)

# Switch to appuser
USER appuser

ENTRYPOINT ["/venv/bin/l2m"]
