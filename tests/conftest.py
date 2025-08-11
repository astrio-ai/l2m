"""
Pytest configuration and shared fixtures for the legacy2modern test suite.
"""

import os
import sys
import pytest
from unittest.mock import Mock

# Add the project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))


@pytest.fixture
def sample_website_files():
    """Fixture providing list of sample website files for testing."""
    examples_dir = os.path.join(os.path.dirname(__file__), "..", "examples", "website")
    return [
        os.path.join(examples_dir, "legacy-site.html"),
        os.path.join(examples_dir, "multi-page-example", "index.html"),
        os.path.join(examples_dir, "multi-page-example", "about.html"),
        os.path.join(examples_dir, "multi-page-example", "contact.html")
    ]


@pytest.fixture
def simple_html_source():
    """Fixture providing a simple HTML source for testing."""
    return """
    <!DOCTYPE html>
    <html>
    <head>
        <title>Test Page</title>
    </head>
    <body>
        <h1>Hello World</h1>
        <p>This is a test page.</p>
    </body>
    </html>
    """


@pytest.fixture
def complex_html_source():
    """Fixture providing complex HTML source with forms and styling."""
    return """
    <!DOCTYPE html>
    <html>
    <head>
        <title>Complex Test Page</title>
        <style>
            .container { max-width: 1200px; margin: 0 auto; }
            .header { background: #f0f0f0; padding: 20px; }
        </style>
    </head>
    <body>
        <div class="container">
            <header class="header">
                <h1>Welcome</h1>
                <nav>
                    <a href="index.html">Home</a>
                    <a href="about.html">About</a>
                    <a href="contact.html">Contact</a>
                </nav>
            </header>
            <main>
                <form action="/submit" method="post">
                    <input type="text" name="name" placeholder="Name">
                    <input type="email" name="email" placeholder="Email">
                    <button type="submit">Submit</button>
                </form>
            </main>
        </div>
    </body>
    </html>
    """


@pytest.fixture
def bootstrap_html_source():
    """Fixture providing HTML source with Bootstrap framework."""
    return """
    <!DOCTYPE html>
    <html>
    <head>
        <title>Bootstrap Test Page</title>
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">
    </head>
    <body>
        <div class="container">
            <div class="row">
                <div class="col-md-6">
                    <div class="card">
                        <div class="card-body">
                            <h5 class="card-title">Card Title</h5>
                            <p class="card-text">Some quick example text.</p>
                            <a href="#" class="btn btn-primary">Go somewhere</a>
                        </div>
                    </div>
                </div>
            </div>
        </div>
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"></script>
    </body>
    </html>
    """


@pytest.fixture
def test_variables():
    """Fixture providing test variables dictionary."""
    return {
        "page_title": {"type": "string", "default": "Test Page"},
        "navigation_items": {"type": "array", "items": ["Home", "About", "Contact"]},
        "form_fields": {"type": "object", "properties": ["name", "email", "message"]}
    }


@pytest.fixture
def mock_website_transpiler():
    """Fixture providing a mock website transpiler."""
    from engine.modernizers.static_site.transpilers.transpiler import StaticSiteTranspiler
    return Mock(spec=StaticSiteTranspiler)


@pytest.fixture
def mock_website_agent():
    """Fixture providing a mock website agent."""
    from engine.modernizers.static_site.transpilers.agent import WebsiteAgent
    return Mock(spec=WebsiteAgent) 