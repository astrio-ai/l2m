# Use Node.js 20 LTS as base image
FROM node:20-slim

# Set working directory
WORKDIR /workspace

# Install system dependencies and build tools
RUN apt-get update && apt-get install -y \
    git \
    curl \
    wget \
    unzip \
    build-essential \
    python3 \
    python3-pip \
    python3-venv \
    && rm -rf /var/lib/apt/lists/*

# Install global npm packages commonly used in development
RUN npm install -g \
    npm@latest \
    pnpm \
    typescript \
    @types/node \
    eslint \
    prettier \
    nodemon \
    concurrently \
    cross-env

# Install additional development tools
RUN npm install -g \
    create-react-app \
    create-next-app \
    @vue/cli \
    @angular/cli \
    vite \
    webpack-cli

# Set environment variables
ENV NODE_ENV=development
ENV PATH="/workspace/node_modules/.bin:${PATH}"

# Create a non-root user for better security
RUN useradd -m -s /bin/bash developer && \
    chown -R developer:developer /workspace

# Switch to non-root user
USER developer

# Expose common development ports
EXPOSE 3000 3001 8080 8000 5173 4173

# Default command
CMD ["/bin/bash"] 