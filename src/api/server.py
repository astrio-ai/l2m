"""
FastAPI server implementation.

This module provides the main FastAPI server for the multi-agent
modernization system.
"""

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.trustedhost import TrustedHostMiddleware
import uvicorn

from src.api.routes.agents import router as agents_router
from src.api.routes.workflows import router as workflows_router
from src.api.routes.analysis import router as analysis_router
from src.api.routes.modernization import router as modernization_router
from src.api.middleware.auth import AuthMiddleware
from src.api.middleware.rate_limit import RateLimitMiddleware
from src.api.middleware.logging import LoggingMiddleware
from src.config.settings import Settings
from src.utils.logger import get_logger

logger = get_logger(__name__)


def create_app() -> FastAPI:
    """Create and configure the FastAPI application."""
    settings = Settings()
    
    app = FastAPI(
        title="Legacy2Modern Multi-Agent System",
        description="Multi-agent system for legacy code modernization",
        version="2.0.0",
        docs_url="/docs",
        redoc_url="/redoc"
    )
    
    # Add CORS middleware
    app.add_middleware(
        CORSMiddleware,
        allow_origins=settings.allowed_origins,
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )
    
    # Add trusted host middleware
    app.add_middleware(
        TrustedHostMiddleware,
        allowed_hosts=settings.allowed_hosts
    )
    
    # Add custom middleware
    app.add_middleware(AuthMiddleware)
    app.add_middleware(RateLimitMiddleware)
    app.add_middleware(LoggingMiddleware)
    
    # Include routers
    app.include_router(agents_router, prefix="/api/v1/agents", tags=["agents"])
    app.include_router(workflows_router, prefix="/api/v1/workflows", tags=["workflows"])
    app.include_router(analysis_router, prefix="/api/v1/analysis", tags=["analysis"])
    app.include_router(modernization_router, prefix="/api/v1/modernization", tags=["modernization"])
    
    @app.get("/")
    async def root():
        """Root endpoint."""
        return {
            "message": "Legacy2Modern Multi-Agent System",
            "version": "2.0.0",
            "status": "running"
        }
    
    @app.get("/health")
    async def health_check():
        """Health check endpoint."""
        return {"status": "healthy"}
    
    return app


def run_server(host: str = "0.0.0.0", port: int = 8000, reload: bool = False):
    """Run the FastAPI server."""
    app = create_app()
    
    logger.info(f"Starting server on {host}:{port}")
    uvicorn.run(
        "src.api.server:app",
        host=host,
        port=port,
        reload=reload,
        log_level="info"
    )


if __name__ == "__main__":
    run_server()
