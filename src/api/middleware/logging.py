"""
Request logging middleware.

This module provides logging middleware for the API,
including request/response logging and performance monitoring.
"""

from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from typing import Optional
import time
import json

from src.utils.logger import get_logger

logger = get_logger(__name__)


class LoggingMiddleware(BaseHTTPMiddleware):
    """Logging middleware for API requests."""
    
    def __init__(self, app, log_requests: bool = True, log_responses: bool = True):
        """Initialize the logging middleware."""
        super().__init__(app)
        self.log_requests = log_requests
        self.log_responses = log_responses
        self.logger = get_logger(__name__)
    
    async def dispatch(self, request: Request, call_next):
        """Process the request and apply logging."""
        # Record start time
        start_time = time.time()
        
        # Log request
        if self.log_requests:
            self._log_request(request)
        
        # Process the request
        response = await call_next(request)
        
        # Calculate processing time
        process_time = time.time() - start_time
        
        # Log response
        if self.log_responses:
            self._log_response(request, response, process_time)
        
        return response
    
    def _log_request(self, request: Request):
        """Log incoming request."""
        request_data = {
            "method": request.method,
            "url": str(request.url),
            "headers": dict(request.headers),
            "client": request.client.host if request.client else "unknown"
        }
        
        self.logger.info(f"Request: {request.method} {request.url.path}", extra=request_data)
    
    def _log_response(self, request: Request, response: Response, process_time: float):
        """Log outgoing response."""
        response_data = {
            "method": request.method,
            "url": str(request.url),
            "status_code": response.status_code,
            "process_time": process_time,
            "client": request.client.host if request.client else "unknown"
        }
        
        self.logger.info(
            f"Response: {request.method} {request.url.path} - {response.status_code}",
            extra=response_data
        )
