"""
Rate limiting middleware.

This module provides rate limiting middleware for the API,
including request throttling and quota management.
"""

from fastapi import Request, HTTPException, status
from fastapi.responses import Response
from starlette.middleware.base import BaseHTTPMiddleware
from typing import Dict, Optional
import time
from collections import defaultdict, deque

from src.utils.logger import get_logger

logger = get_logger(__name__)


class RateLimitMiddleware(BaseHTTPMiddleware):
    """Rate limiting middleware for API requests."""
    
    def __init__(self, app, requests_per_minute: int = 60, requests_per_hour: int = 1000):
        """Initialize the rate limiting middleware."""
        super().__init__(app)
        self.requests_per_minute = requests_per_minute
        self.requests_per_hour = requests_per_hour
        self.request_counts: Dict[str, deque] = defaultdict(deque)
        self.logger = get_logger(__name__)
    
    async def dispatch(self, request: Request, call_next):
        """Process the request and apply rate limiting."""
        # Get client identifier
        client_id = self._get_client_id(request)
        
        # Check rate limits
        if not self._check_rate_limit(client_id):
            self.logger.warning(f"Rate limit exceeded for client {client_id}")
            raise HTTPException(
                status_code=status.HTTP_429_TOO_MANY_REQUESTS,
                detail="Rate limit exceeded"
            )
        
        # Record the request
        self._record_request(client_id)
        
        # Process the request
        response = await call_next(request)
        
        return response
    
    def _get_client_id(self, request: Request) -> str:
        """Get client identifier for rate limiting."""
        # Use IP address as client identifier
        client_ip = request.client.host if request.client else "unknown"
        return client_ip
    
    def _check_rate_limit(self, client_id: str) -> bool:
        """Check if client has exceeded rate limits."""
        now = time.time()
        requests = self.request_counts[client_id]
        
        # Remove old requests (older than 1 hour)
        while requests and requests[0] < now - 3600:
            requests.popleft()
        
        # Check hourly limit
        if len(requests) >= self.requests_per_hour:
            return False
        
        # Check minute limit (last 60 seconds)
        minute_requests = [req for req in requests if req > now - 60]
        if len(minute_requests) >= self.requests_per_minute:
            return False
        
        return True
    
    def _record_request(self, client_id: str):
        """Record a request for rate limiting."""
        now = time.time()
        self.request_counts[client_id].append(now)
