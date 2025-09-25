"""
Authentication middleware.

This module provides authentication middleware for the API,
including API key validation and user authentication.
"""

from fastapi import Request, HTTPException, status
from fastapi.responses import Response
from starlette.middleware.base import BaseHTTPMiddleware
from typing import Optional
import os

from src.utils.logger import get_logger

logger = get_logger(__name__)


class AuthMiddleware(BaseHTTPMiddleware):
    """Authentication middleware for API requests."""
    
    def __init__(self, app, api_key: Optional[str] = None):
        """Initialize the authentication middleware."""
        super().__init__(app)
        self.api_key = api_key or os.getenv("API_KEY")
        self.logger = get_logger(__name__)
    
    async def dispatch(self, request: Request, call_next):
        """Process the request and apply authentication."""
        # Skip authentication for health check and docs
        if request.url.path in ["/health", "/docs", "/redoc", "/openapi.json"]:
            return await call_next(request)
        
        # Check for API key in headers
        api_key = request.headers.get("X-API-Key")
        
        if not api_key:
            self.logger.warning("Missing API key in request")
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="API key required"
            )
        
        if self.api_key and api_key != self.api_key:
            self.logger.warning(f"Invalid API key: {api_key}")
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Invalid API key"
            )
        
        # Add user context to request state
        request.state.user_id = "api_user"
        request.state.api_key = api_key
        
        # Process the request
        response = await call_next(request)
        
        return response
