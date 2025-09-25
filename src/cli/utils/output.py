"""
Output formatting utilities for CLI.

This module provides utilities for formatting command output
in various formats (JSON, YAML, HTML, etc.).
"""

import json
import yaml
from typing import Any, Dict
from pathlib import Path

from src.utils.logger import get_logger

logger = get_logger(__name__)


def format_output(data: Any, format_type: str) -> str:
    """Format data for output in the specified format."""
    try:
        if format_type == "json":
            return json.dumps(data, indent=2, default=str)
        elif format_type == "yaml":
            return yaml.dump(data, default_flow_style=False)
        elif format_type == "html":
            return _format_html(data)
        else:
            raise ValueError(f"Unsupported format type: {format_type}")
    except Exception as e:
        logger.error(f"Error formatting output: {e}")
        return str(data)


def _format_html(data: Any) -> str:
    """Format data as HTML."""
    html = """
    <!DOCTYPE html>
    <html>
    <head>
        <title>Legacy2Modern Analysis Results</title>
        <style>
            body { font-family: Arial, sans-serif; margin: 20px; }
            .header { background-color: #f0f0f0; padding: 10px; border-radius: 5px; }
            .content { margin-top: 20px; }
            .section { margin-bottom: 20px; }
            .section h3 { color: #333; border-bottom: 1px solid #ccc; }
            .code { background-color: #f5f5f5; padding: 10px; border-radius: 3px; font-family: monospace; }
        </style>
    </head>
    <body>
        <div class="header">
            <h1>Legacy2Modern Analysis Results</h1>
        </div>
        <div class="content">
            <div class="section">
                <h3>Results</h3>
                <div class="code">
                    <pre>{}</pre>
                </div>
            </div>
        </div>
    </body>
    </html>
    """.format(json.dumps(data, indent=2, default=str))
    
    return html
