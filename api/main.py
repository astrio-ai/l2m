"""
FastAPI application for Legacy2Modern CLI

This module provides a REST API interface for the legacy2modern transpilation engine.
"""

import os
import logging
from datetime import datetime
from typing import Dict, Any
from contextlib import asynccontextmanager

from fastapi import FastAPI, HTTPException, UploadFile, File, Form
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
import uvicorn

from .models import (
    TranspilationRequest, WebsiteModernizationRequest, CobolTranspilationRequest,
    AnalysisRequest, TranspilationResponse, WebsiteAnalysisResponse,
    HealthResponse, ErrorResponse, FrameworkType
)
from .services import Legacy2ModernService

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize service
service = Legacy2ModernService()


@asynccontextmanager
async def lifespan(app: FastAPI):
    """Lifespan context manager for startup and shutdown events."""
    # Startup
    logger.info("Starting Legacy2Modern API server...")
    try:
        # Service is already initialized in constructor
        logger.info("All components initialized successfully")
    except Exception as e:
        logger.error(f"Failed to initialize components: {e}")
        raise
    yield
    # Shutdown
    logger.info("Shutting down Legacy2Modern API server...")


# Initialize FastAPI app
app = FastAPI(
    title="Legacy2Modern API",
    description="REST API for transpiling legacy code to modern languages and frameworks",
    version="1.0.0",
    docs_url="/docs",
    redoc_url="/redoc",
    lifespan=lifespan
)

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Configure this appropriately for production
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/", response_model=Dict[str, str])
async def root():
    """Root endpoint with API information."""
    return {
        "message": "Legacy2Modern API",
        "version": "1.0.0",
        "description": "REST API for transpiling legacy code to modern languages and frameworks",
        "docs": "/docs",
        "health": "/health"
    }


@app.get("/health", response_model=HealthResponse)
async def health_check():
    """Health check endpoint."""
    try:
        health_status = service.get_health_status()
        return HealthResponse(
            status=health_status['status'],
            version="1.0.0",
            timestamp=health_status['timestamp'],
            components=health_status['components']
        )
    except Exception as e:
        logger.error(f"Health check failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/transpile/cobol", response_model=TranspilationResponse)
async def transpile_cobol(request: CobolTranspilationRequest):
    """
    Transpile COBOL code to Python.
    
    This endpoint accepts either source code as a string or a file path,
    and optionally an output file path for the generated Python code.
    """
    try:
        result = service.transpile_cobol(
            source_code=request.source_code,
            source_file_path=request.source_file_path,
            output_file=request.output_file,
            llm_config=request.llm_config
        )
        
        if result['success']:
            return TranspilationResponse(
                success=True,
                message=result['message'],
                generated_code=result.get('generated_code'),
                analysis=result.get('analysis'),
                metadata=result.get('metadata')
            )
        else:
            return TranspilationResponse(
                success=False,
                message=result['message'],
                errors=result.get('errors', []),
                warnings=result.get('warnings', []),
                metadata=result.get('metadata')
            )
            
    except Exception as e:
        logger.error(f"COBOL transpilation failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/transpile/cobol/file", response_model=TranspilationResponse)
async def transpile_cobol_file(
    file: UploadFile = File(...),
    output_file: str = Form(None),
    llm_config: str = Form("{}")
):
    """
    Transpile COBOL file to Python.
    
    This endpoint accepts a COBOL file upload and optionally an output file path.
    """
    try:
        # Validate file type
        if not file.filename.endswith('.cobol') and not file.filename.endswith('.cob'):
            raise HTTPException(status_code=400, detail="File must be a COBOL file (.cobol or .cob)")
        
        # Read file content
        content = await file.read()
        cobol_source = content.decode('utf-8')
        
        # Parse LLM config if provided
        import json
        llm_config_dict = json.loads(llm_config) if llm_config else {}
        
        result = service.transpile_cobol(
            source_code=cobol_source,
            output_file=output_file,
            llm_config=llm_config_dict
        )
        
        if result['success']:
            return TranspilationResponse(
                success=True,
                message=result['message'],
                generated_code=result.get('generated_code'),
                analysis=result.get('analysis'),
                metadata=result.get('metadata')
            )
        else:
            return TranspilationResponse(
                success=False,
                message=result['message'],
                errors=result.get('errors', []),
                warnings=result.get('warnings', []),
                metadata=result.get('metadata')
            )
            
    except Exception as e:
        logger.error(f"COBOL file transpilation failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/modernize/website", response_model=TranspilationResponse)
async def modernize_website(request: WebsiteModernizationRequest):
    """
    Modernize a legacy website to a modern framework.
    
    This endpoint accepts an input path to an HTML file or ZIP archive,
    and generates a modern website in the specified framework.
    """
    try:
        result = service.modernize_website(
            input_path=request.input_path,
            output_dir=request.output_dir,
            target_framework=request.target_framework.value,
            analyze_only=request.analyze_only
        )
        
        if result['success']:
            return TranspilationResponse(
                success=True,
                message=result['message'],
                analysis=result.get('analysis'),
                metadata=result.get('metadata')
            )
        else:
            return TranspilationResponse(
                success=False,
                message=result['message'],
                errors=result.get('errors', []),
                warnings=result.get('warnings', []),
                metadata=result.get('metadata')
            )
            
    except Exception as e:
        logger.error(f"Website modernization failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/modernize/website/file", response_model=TranspilationResponse)
async def modernize_website_file(
    file: UploadFile = File(...),
    output_dir: str = Form(...),
    target_framework: FrameworkType = Form(FrameworkType.REACT),
    analyze_only: bool = Form(False)
):
    """
    Modernize a legacy website file to a modern framework.
    
    This endpoint accepts an HTML file upload and generates a modern website.
    """
    try:
        # Validate file type
        if not file.filename.endswith('.html') and not file.filename.endswith('.htm'):
            raise HTTPException(status_code=400, detail="File must be an HTML file (.html or .htm)")
        
        # Create temporary file
        import tempfile
        with tempfile.NamedTemporaryFile(mode='wb', suffix='.html', delete=False) as temp_file:
            content = await file.read()
            temp_file.write(content)
            temp_file_path = temp_file.name
        
        try:
            result = service.modernize_website(
                input_path=temp_file_path,
                output_dir=output_dir,
                target_framework=target_framework.value,
                analyze_only=analyze_only
            )
            
            if result['success']:
                return TranspilationResponse(
                    success=True,
                    message=result['message'],
                    analysis=result.get('analysis'),
                    metadata=result.get('metadata')
                )
            else:
                return TranspilationResponse(
                    success=False,
                    message=result['message'],
                    errors=result.get('errors', []),
                    warnings=result.get('warnings', []),
                    metadata=result.get('metadata')
                )
        finally:
            # Clean up temporary file
            os.unlink(temp_file_path)
            
    except Exception as e:
        logger.error(f"Website file modernization failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/analyze/website", response_model=WebsiteAnalysisResponse)
async def analyze_website(input_path: str):
    """
    Analyze a legacy website without modernizing it.
    
    This endpoint provides detailed analysis of the website structure and components.
    """
    try:
        result = service.analyze_website(input_path)
        
        if result['success']:
            return WebsiteAnalysisResponse(
                success=True,
                message=result['message'],
                analysis=result.get('analysis'),
                parsed_data=result.get('parsed_data'),
                metadata=result.get('metadata')
            )
        else:
            return WebsiteAnalysisResponse(
                success=False,
                message=result['message'],
                errors=result.get('errors', []),
                metadata=result.get('metadata')
            )
            
    except Exception as e:
        logger.error(f"Website analysis failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/analyze/code", response_model=TranspilationResponse)
async def analyze_code(request: AnalysisRequest):
    """
    Analyze source code and optionally compare with target code.
    
    This endpoint provides code complexity metrics and analysis.
    """
    try:
        result = service.analyze_code(
            source_code=request.source_code,
            target_code=request.target_code
        )
        
        if result['success']:
            return TranspilationResponse(
                success=True,
                message=result['message'],
                analysis=result.get('analysis'),
                metadata=result.get('metadata')
            )
        else:
            return TranspilationResponse(
                success=False,
                message=result['message'],
                errors=result.get('errors', []),
                warnings=result.get('warnings', []),
                metadata=result.get('metadata')
            )
            
    except Exception as e:
        logger.error(f"Code analysis failed: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/frameworks", response_model=Dict[str, Any])
async def get_supported_frameworks():
    """Get list of supported target frameworks for website modernization."""
    try:
        frameworks = service.website_transpiler.get_supported_frameworks()
        return {
            "supported_frameworks": frameworks,
            "description": "Target frameworks for website modernization"
        }
    except Exception as e:
        logger.error(f"Failed to get supported frameworks: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.exception_handler(Exception)
async def global_exception_handler(request, exc):
    """Global exception handler."""
    logger.error(f"Unhandled exception: {exc}")
    return JSONResponse(
        status_code=500,
        content=ErrorResponse(
            error="Internal server error",
            error_type="InternalError",
            details={"message": str(exc)}
        ).dict()
    )


if __name__ == "__main__":
    uvicorn.run(
        "api.main:app",
        host="0.0.0.0",
        port=8001,
        reload=True,
        log_level="info"
    ) 