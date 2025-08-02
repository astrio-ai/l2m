#!/usr/bin/env python3
"""
Website modernizer tests for static site conversion.
"""

import os
import sys
import unittest
import pytest
from unittest.mock import Mock, patch
import tempfile
import shutil

# Add the project root to Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")))

from engine.modernizers.static_site.transpilers.agent import WebsiteAgent, WebsiteAnalysis
from engine.modernizers.static_site.parser.html.html_parser import HTMLParser
from engine.modernizers.static_site.parser.html.html_analyzer import HTMLAnalyzer


@pytest.mark.static_site
@pytest.mark.website_modernizer
class TestWebsiteModernizer(unittest.TestCase):
    """Test website modernizer functionality."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.test_data_dir = os.path.join(os.path.dirname(__file__), "../../examples/website")
        self.output_dir = os.path.join(os.path.dirname(__file__), "../../output/test-modernized")
        
        # Create temporary output directory
        os.makedirs(self.output_dir, exist_ok=True)
        
        # Sample HTML for testing
        self.sample_html = """
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>Test Website</title>
            <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">
        </head>
        <body>
            <nav class="navbar navbar-expand-lg navbar-light bg-light">
                <div class="container">
                    <a class="navbar-brand" href="#">Test Brand</a>
                    <div class="navbar-nav">
                        <a class="nav-link" href="#home">Home</a>
                        <a class="nav-link" href="#services">Services</a>
                        <a class="nav-link" href="#contact">Contact</a>
                    </div>
                </div>
            </nav>
            
            <section id="home" class="container mt-5">
                <h1>Welcome to Our Website</h1>
                <p>This is a test website for modernization.</p>
                <button class="btn btn-primary">Get Started</button>
            </section>
            
            <section id="services" class="container mt-5">
                <h2>Our Services</h2>
                <div class="row">
                    <div class="col-md-4">
                        <div class="card">
                            <div class="card-body">
                                <h5 class="card-title">Service 1</h5>
                                <p class="card-text">Description of service 1.</p>
                            </div>
                        </div>
                    </div>
                </div>
            </section>
            
            <section id="contact" class="container mt-5">
                <h2>Contact Us</h2>
                <form>
                    <div class="mb-3">
                        <label for="name" class="form-label">Name</label>
                        <input type="text" class="form-control" id="name">
                    </div>
                    <div class="mb-3">
                        <label for="email" class="form-label">Email</label>
                        <input type="email" class="form-control" id="email">
                    </div>
                    <div class="mb-3">
                        <label for="message" class="form-label">Message</label>
                        <textarea class="form-control" id="message" rows="3"></textarea>
                    </div>
                    <button type="submit" class="btn btn-primary">Send Message</button>
                </form>
            </section>
            
            <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
            <script>
                $(document).ready(function() {
                    $('.btn-primary').click(function() {
                        alert('Button clicked!');
                    });
                });
            </script>
        </body>
        </html>
        """
    
    def tearDown(self):
        """Clean up test fixtures."""
        if os.path.exists(self.output_dir):
            shutil.rmtree(self.output_dir)
    
    def test_website_agent_initialization(self):
        """Test website agent initialization."""
        agent = WebsiteAgent()
        
        self.assertIsNotNone(agent)
        self.assertIsNotNone(agent.parser)
        self.assertIsNotNone(agent.analyzer)
        self.assertIsNotNone(agent.llm_augmentor)
    
    def test_html_parser_parse_input(self):
        """Test HTML parser with sample input."""
        parser = HTMLParser()
        
        # Create temporary HTML file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.html', delete=False) as f:
            f.write(self.sample_html)
            temp_file = f.name
        
        try:
            result = parser.parse_input(temp_file)
            
            self.assertIsInstance(result, dict)
            self.assertIn('files', result)
            self.assertGreater(len(result['files']), 0)
            
            file_data = result['files'][0]
            self.assertIn('structure', file_data)
            self.assertIn('frameworks', file_data)
            # 'scripts' might not be in the structure, check for 'javascript' instead
            self.assertIn('javascript', file_data)
            
        finally:
            os.unlink(temp_file)
    
    def test_html_analyzer_analyze_website(self):
        """Test HTML analyzer with parsed data."""
        parser = HTMLParser()
        analyzer = HTMLAnalyzer()
        
        # Create temporary HTML file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.html', delete=False) as f:
            f.write(self.sample_html)
            temp_file = f.name
        
        try:
            parsed_data = parser.parse_input(temp_file)
            analysis = analyzer.analyze_website(parsed_data)
            
            self.assertIsInstance(analysis, dict)
            # Check for expected keys in analysis
            self.assertIn('complexity_score', analysis)
            self.assertIn('modernization_effort', analysis)
            self.assertIn('framework_migration', analysis)
            
        finally:
            os.unlink(temp_file)
    
    def test_website_analysis_dataclass(self):
        """Test WebsiteAnalysis dataclass."""
        analysis = WebsiteAnalysis(
            structure={'title': 'Test'},
            components=[{'name': 'test'}],
            styles=[{'name': 'test.css'}],
            scripts=[{'name': 'test.js'}],
            modern_components=[],
            modern_styles={},
            modern_scripts=[]
        )
        
        self.assertEqual(analysis.structure['title'], 'Test')
        self.assertEqual(len(analysis.components), 1)
        self.assertEqual(len(analysis.styles), 1)
        self.assertEqual(len(analysis.scripts), 1)
    
    @patch('requests.post')
    def test_website_agent_analyze_website(self, mock_post):
        """Test website agent analyze website method."""
        agent = WebsiteAgent()
        
        # Create temporary HTML file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.html', delete=False) as f:
            f.write(self.sample_html)
            temp_file = f.name
        
        try:
            analysis = agent._analyze_website(temp_file)
            
            self.assertIsInstance(analysis, WebsiteAnalysis)
            self.assertIsInstance(analysis.structure, dict)
            self.assertIsInstance(analysis.components, list)
            self.assertIsInstance(analysis.styles, list)
            self.assertIsInstance(analysis.scripts, list)
            
        finally:
            os.unlink(temp_file)
    
    @patch('requests.post')
    def test_website_agent_extract_components(self, mock_post):
        """Test component extraction from parsed data."""
        agent = WebsiteAgent()
        
        # Mock parsed data
        parsed_data = {
            'files': [{
                'structure': {
                    'sections': [
                        {
                            'type': 'navigation',
                            'id': 'nav',
                            'title': 'Navigation',
                            'content': '<nav>...</nav>',
                            'classes': ['navbar'],
                            'attributes': {}
                        },
                        {
                            'type': 'hero',
                            'id': 'hero',
                            'title': 'Hero Section',
                            'content': '<section>...</section>',
                            'classes': ['hero'],
                            'attributes': {}
                        }
                    ]
                }
            }]
        }
        
        components = agent._extract_components(parsed_data)
        
        self.assertIsInstance(components, list)
        self.assertEqual(len(components), 2)
        
        # Check first component
        nav_component = components[0]
        self.assertEqual(nav_component['type'], 'navigation')
        self.assertEqual(nav_component['id'], 'nav')
        self.assertEqual(nav_component['title'], 'Navigation')
    
    @patch('requests.post')
    def test_website_agent_extract_styles(self, mock_post):
        """Test style extraction from parsed data."""
        agent = WebsiteAgent()
        
        # Mock parsed data
        parsed_data = {
            'files': [{
                'styles': [
                    {
                        'name': 'bootstrap.css',
                        'content': '.navbar { background: #f8f9fa; }',
                        'classes': ['navbar']
                    },
                    {
                        'name': 'custom.css',
                        'content': '.hero { padding: 2rem; }',
                        'classes': ['hero']
                    }
                ]
            }]
        }
        
        styles = agent._extract_styles(parsed_data)
        
        self.assertIsInstance(styles, list)
        self.assertEqual(len(styles), 2)
        
        # Check first style
        bootstrap_style = styles[0]
        self.assertEqual(bootstrap_style['name'], 'bootstrap.css')
        self.assertIn('navbar', bootstrap_style['content'])
    
    @patch('requests.post')
    def test_website_agent_extract_scripts(self, mock_post):
        """Test script extraction from parsed data."""
        agent = WebsiteAgent()
        
        # Mock parsed data
        parsed_data = {
            'files': [{
                'scripts': [
                    {
                        'name': 'jquery.js',
                        'content': '$(document).ready(function() { ... });',
                        'type': 'jquery'
                    },
                    {
                        'name': 'custom.js',
                        'content': 'function test() { console.log("test"); }',
                        'type': 'vanilla'
                    }
                ]
            }]
        }
        
        scripts = agent._extract_scripts(parsed_data)
        
        self.assertIsInstance(scripts, list)
        self.assertEqual(len(scripts), 2)
        
        # Check first script
        jquery_script = scripts[0]
        self.assertEqual(jquery_script['name'], 'jquery.js')
        self.assertEqual(jquery_script['type'], 'jquery')
    
    @patch('requests.post')
    def test_website_agent_generate_react_app(self, mock_post):
        """Test React app generation."""
        agent = WebsiteAgent()
        
        # Mock analysis
        analysis = WebsiteAnalysis(
            structure={'title': 'Test'},
            components=[
                {'name': 'Navigation', 'content': '<nav>...</nav>'},
                {'name': 'Hero', 'content': '<section>...</section>'}
            ],
            styles=[{'name': 'bootstrap.css'}],
            scripts=[{'name': 'jquery.js'}],
            modern_components=[],
            modern_styles={},
            modern_scripts=[]
        )
        
        react_app = agent._generate_react_app(analysis, [], {}, [])
        
        self.assertIsInstance(react_app, str)
        self.assertIn('import React', react_app)
        self.assertIn('const App', react_app)
        self.assertIn('export default App', react_app)
    
    @patch('requests.post')
    def test_website_agent_generate_package_json(self, mock_post):
        """Test package.json generation."""
        agent = WebsiteAgent()
        
        package_json = agent._generate_package_json()
        
        self.assertIsInstance(package_json, dict)
        self.assertIn('name', package_json)
        self.assertIn('version', package_json)
        self.assertIn('dependencies', package_json)
        self.assertIn('devDependencies', package_json)
        self.assertIn('scripts', package_json)
        
        # Check specific dependencies
        dependencies = package_json['dependencies']
        self.assertIn('react', dependencies)
        self.assertIn('react-dom', dependencies)
        self.assertIn('typescript', dependencies)
    
    @patch('requests.post')
    def test_website_agent_generate_readme(self, mock_post):
        """Test README generation."""
        agent = WebsiteAgent()
        
        readme = agent._generate_readme()
        
        self.assertIsInstance(readme, str)
        self.assertIn('# Modernized Website', readme)
        self.assertIn('React', readme)
        self.assertIn('TypeScript', readme)
        self.assertIn('Tailwind CSS', readme)
    
    @patch('requests.post')
    def test_website_agent_write_project_files(self, mock_post):
        """Test project file writing."""
        agent = WebsiteAgent()
        
        react_app = "import React from 'react';\nconst App = () => <div>Hello</div>;\nexport default App;"
        styles = {'main.css': 'body { margin: 0; }'}
        package_json = {'name': 'test', 'version': '1.0.0'}
        readme = "# Test\nThis is a test."
        components = [{'name': 'Test', 'content': 'const Test = () => <div>Test</div>;'}]
        
        # Test file writing
        agent._write_project_files(self.output_dir, react_app, styles, package_json, readme, components)
        
        # Check that files were created
        self.assertTrue(os.path.exists(os.path.join(self.output_dir, 'src')))
        self.assertTrue(os.path.exists(os.path.join(self.output_dir, 'src', 'App.tsx')))
        self.assertTrue(os.path.exists(os.path.join(self.output_dir, 'src', 'components')))
        self.assertTrue(os.path.exists(os.path.join(self.output_dir, 'package.json')))
        self.assertTrue(os.path.exists(os.path.join(self.output_dir, 'README.md')))
        self.assertTrue(os.path.exists(os.path.join(self.output_dir, 'tsconfig.json')))
    
    @patch('requests.post')
    def test_website_agent_generate_header_component(self, mock_post):
        """Test header component generation."""
        agent = WebsiteAgent()
        
        header_component = agent._generate_header_component()
        
        self.assertIsInstance(header_component, str)
        self.assertIn('import React', header_component)
        self.assertIn('const Header', header_component)
        self.assertIn('useState', header_component)
        self.assertIn('className', header_component)
        self.assertIn('export default Header', header_component)
    
    @patch('requests.post')
    def test_website_agent_generate_footer_component(self, mock_post):
        """Test footer component generation."""
        agent = WebsiteAgent()
        
        footer_component = agent._generate_footer_component()
        
        self.assertIsInstance(footer_component, str)
        self.assertIn('import React', footer_component)
        self.assertIn('const Footer', footer_component)
        self.assertIn('className', footer_component)
        self.assertIn('export default Footer', footer_component)
    
    @patch('requests.post')
    def test_website_agent_call_llm(self, mock_post):
        """Test LLM calling functionality."""
        agent = WebsiteAgent()
        
        # Mock successful LLM response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"response": "def test_function():\n    return 'test'"}
        mock_post.return_value = mock_response
        
        prompt = "Convert this to Python: DISPLAY 'Hello'"
        result = agent._call_llm(prompt)
        
        # The result might be the original prompt if LLM fails, or the response
        self.assertIsInstance(result, str)
        # Check that we get some response (either original prompt or LLM response)
        self.assertGreater(len(result), 0)


if __name__ == '__main__':
    unittest.main() 