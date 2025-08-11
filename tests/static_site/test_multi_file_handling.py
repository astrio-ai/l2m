"""
Tests for enhanced multi-file website handling capabilities.
"""

import pytest
import tempfile
import zipfile
import os
import shutil
from pathlib import Path
from unittest.mock import patch, MagicMock

from engine.modernizers.static_site.parser.html.html_parser import HTMLParser
from engine.modernizers.static_site.parser.html.html_analyzer import HTMLAnalyzer
from engine.modernizers.static_site.transpilers.transpiler import StaticSiteTranspiler


class TestMultiFileWebsiteHandling:
    """Test enhanced multi-file website handling capabilities."""
    
    @pytest.fixture
    def sample_website_directory(self):
        """Create a sample multi-page website directory for testing."""
        temp_dir = tempfile.mkdtemp()
        
        # Create sample HTML files
        html_files = {
            'index.html': '''
                <!DOCTYPE html>
                <html>
                <head>
                    <title>Home - Sample Website</title>
                    <link rel="stylesheet" href="css/style.css">
                    <link rel="stylesheet" href="css/bootstrap.css">
                </head>
                <body>
                    <nav class="navbar">
                        <a href="index.html">Home</a>
                        <a href="about.html">About</a>
                        <a href="services.html">Services</a>
                        <a href="contact.html">Contact</a>
                    </nav>
                    <main>
                        <section class="hero">
                            <h1>Welcome to Our Website</h1>
                            <p>This is a sample multi-page website.</p>
                        </section>
                    </main>
                    <footer>
                        <p>&copy; 2024 Sample Website</p>
                    </footer>
                    <script src="js/jquery.js"></script>
                    <script src="js/main.js"></script>
                </body>
                </html>
            ''',
            'about.html': '''
                <!DOCTYPE html>
                <html>
                <head>
                    <title>About - Sample Website</title>
                    <link rel="stylesheet" href="css/style.css">
                    <link rel="stylesheet" href="css/bootstrap.css">
                </head>
                <body>
                    <nav class="navbar">
                        <a href="index.html">Home</a>
                        <a href="about.html">About</a>
                        <a href="services.html">Services</a>
                        <a href="contact.html">Contact</a>
                    </nav>
                    <main>
                        <section class="about">
                            <h1>About Us</h1>
                            <p>Learn more about our company.</p>
                        </section>
                    </main>
                    <footer>
                        <p>&copy; 2024 Sample Website</p>
                    </footer>
                    <script src="js/jquery.js"></script>
                    <script src="js/main.js"></script>
                </body>
                </html>
            ''',
            'services.html': '''
                <!DOCTYPE html>
                <html>
                <head>
                    <title>Services - Sample Website</title>
                    <link rel="stylesheet" href="css/style.css">
                    <link rel="stylesheet" href="css/bootstrap.css">
                </head>
                <body>
                    <nav class="navbar">
                        <a href="index.html">Home</a>
                        <a href="about.html">About</a>
                        <a href="services.html">Services</a>
                        <a href="contact.html">Contact</a>
                    </nav>
                    <main>
                        <section class="services">
                            <h1>Our Services</h1>
                            <div class="service-card">
                                <h3>Service 1</h3>
                                <p>Description of service 1.</p>
                            </div>
                            <div class="service-card">
                                <h3>Service 2</h3>
                                <p>Description of service 2.</p>
                            </div>
                        </section>
                    </main>
                    <footer>
                        <p>&copy; 2024 Sample Website</p>
                    </footer>
                    <script src="js/jquery.js"></script>
                    <script src="js/main.js"></script>
                </body>
                </html>
            ''',
            'contact.html': '''
                <!DOCTYPE html>
                <html>
                <head>
                    <title>Contact - Sample Website</title>
                    <link rel="stylesheet" href="css/style.css">
                    <link rel="stylesheet" href="css/bootstrap.css">
                </head>
                <body>
                    <nav class="navbar">
                        <a href="index.html">Home</a>
                        <a href="about.html">About</a>
                        <a href="services.html">Services</a>
                        <a href="contact.html">Contact</a>
                    </nav>
                    <main>
                        <section class="contact">
                            <h1>Contact Us</h1>
                            <form action="/submit" method="post">
                                <input type="text" name="name" placeholder="Your Name" required>
                                <input type="email" name="email" placeholder="Your Email" required>
                                <textarea name="message" placeholder="Your Message" required></textarea>
                                <button type="submit">Send Message</button>
                            </form>
                        </section>
                    </main>
                    <footer>
                        <p>&copy; 2024 Sample Website</p>
                    </footer>
                    <script src="js/jquery.js"></script>
                    <script src="js/main.js"></script>
                </body>
                </html>
            '''
        }
        
        # Create directory structure
        css_dir = Path(temp_dir) / 'css'
        js_dir = Path(temp_dir) / 'js'
        images_dir = Path(temp_dir) / 'images'
        
        css_dir.mkdir()
        js_dir.mkdir()
        images_dir.mkdir()
        
        # Create HTML files
        for filename, content in html_files.items():
            with open(Path(temp_dir) / filename, 'w', encoding='utf-8') as f:
                f.write(content)
        
        # Create sample CSS file
        with open(css_dir / 'style.css', 'w') as f:
            f.write('''
                body { font-family: Arial, sans-serif; }
                .navbar { background: #333; color: white; }
                .hero { text-align: center; padding: 50px; }
                .service-card { border: 1px solid #ddd; padding: 20px; margin: 10px; }
                .contact form { max-width: 500px; margin: 0 auto; }
            ''')
        
        # Create sample JS file
        with open(js_dir / 'main.js', 'w') as f:
            f.write('''
                $(document).ready(function() {
                    console.log("Website loaded");
                });
            ''')
        
        # Create sample image files
        (images_dir / 'logo.png').touch()
        (images_dir / 'hero-bg.jpg').touch()
        
        yield temp_dir
        
        # Cleanup
        shutil.rmtree(temp_dir)
    
    @pytest.fixture
    def sample_zip_archive(self, sample_website_directory):
        """Create a ZIP archive from the sample website directory."""
        zip_path = Path(sample_website_directory).parent / 'sample-website.zip'
        
        with zipfile.ZipFile(zip_path, 'w') as zip_file:
            for root, dirs, files in os.walk(sample_website_directory):
                for file in files:
                    file_path = Path(root) / file
                    arc_name = file_path.relative_to(sample_website_directory)
                    zip_file.write(file_path, arc_name)
        
        yield str(zip_path)
        
        # Cleanup
        zip_path.unlink()
    
    def test_directory_parsing(self, sample_website_directory):
        """Test parsing a directory containing multiple HTML files."""
        parser = HTMLParser()
        result = parser.parse_input(sample_website_directory)
        
        assert result['type'] == 'directory'
        assert len(result['files']) == 4  # 4 HTML files
        assert 'site_map' in result
        assert 'navigation_structure' in result
        assert 'shared_components' in result
        assert 'page_dependencies' in result
        assert 'asset_dependencies' in result
        assert 'directory_stats' in result
        
        # Check directory statistics
        stats = result['directory_stats']
        assert stats['total_files'] >= 4  # At least 4 HTML files
        assert stats['total_directories'] >= 3  # css, js, images directories
        assert '.html' in stats['file_types']
        assert '.css' in stats['file_types']
        assert '.js' in stats['file_types']
    
    def test_zip_archive_parsing(self, sample_zip_archive):
        """Test parsing a ZIP archive containing website files."""
        parser = HTMLParser()
        result = parser.parse_input(sample_zip_archive)
        
        assert result['type'] == 'archive'
        assert len(result['files']) == 4  # 4 HTML files
        assert 'archive_info' in result
        assert 'file_hierarchy' in result
        assert 'compression_analysis' in result
        
        # Check archive information
        archive_info = result['archive_info']
        assert archive_info['total_files'] >= 4
        assert archive_info['compressed_size'] > 0
        assert archive_info['uncompressed_size'] > 0
        assert archive_info['compression_ratio'] >= 0
        assert '.html' in archive_info['file_types']
        assert '.css' in archive_info['file_types']
        assert '.js' in archive_info['file_types']
    
    @patch('subprocess.run')
    def test_git_repository_parsing(self, mock_run, sample_website_directory):
        """Test parsing a git repository."""
        # Mock successful git clone
        mock_run.return_value.returncode = 0
        
        parser = HTMLParser()
        result = parser.parse_input('https://github.com/user/sample-website')
        
        assert result['type'] == 'directory'
        assert 'git_info' in result
        
        # Check git metadata
        git_info = result['git_info']
        assert 'remotes' in git_info
        assert 'current_branch' in git_info
        assert 'last_commit' in git_info
        assert 'total_files' in git_info
        assert 'repo_size' in git_info
    
    def test_site_map_generation(self, sample_website_directory):
        """Test site map generation from multiple files."""
        parser = HTMLParser()
        result = parser.parse_input(sample_website_directory)
        
        site_map = result['site_map']
        assert 'pages' in site_map
        assert 'navigation' in site_map
        assert 'breadcrumbs' in site_map
        assert 'sitemap' in site_map
        assert 'page_hierarchy' in site_map
        assert 'url_structure' in site_map
        
        # Check pages
        pages = site_map['pages']
        assert len(pages) == 4
        assert 'index.html' in pages
        assert 'about.html' in pages
        assert 'services.html' in pages
        assert 'contact.html' in pages
        
        # Check page data
        index_page = pages['index.html']
        assert 'title' in index_page
        assert 'meta' in index_page
        assert 'navigation_links' in index_page
        assert 'components' in index_page
        assert 'assets' in index_page
    
    def test_navigation_analysis(self, sample_website_directory):
        """Test navigation structure analysis."""
        parser = HTMLParser()
        result = parser.parse_input(sample_website_directory)
        
        navigation = result['navigation_structure']
        assert 'main_menu' in navigation
        assert 'footer_links' in navigation
        assert 'breadcrumbs' in navigation
        assert 'internal_links' in navigation
        assert 'external_links' in navigation
        assert 'navigation_patterns' in navigation
        assert 'menu_structure' in navigation
        assert 'link_analysis' in navigation
        
        # Check internal links
        internal_links = navigation['internal_links']
        assert len(internal_links) == 4  # One entry per page
        
        # Each page should have links to other pages
        for page_links in internal_links.values():
            assert isinstance(page_links, list)
    
    def test_shared_components_identification(self, sample_website_directory):
        """Test identification of shared components across pages."""
        parser = HTMLParser()
        result = parser.parse_input(sample_website_directory)
        
        shared_components = result['shared_components']
        assert 'headers' in shared_components
        assert 'footers' in shared_components
        assert 'navigation' in shared_components
        assert 'sidebars' in shared_components
        assert 'forms' in shared_components
        assert 'tables' in shared_components
        assert 'buttons' in shared_components
        assert 'modals' in shared_components
        assert 'common_elements' in shared_components
        
        # Should identify navigation as shared component
        navigation_components = shared_components['navigation']
        assert len(navigation_components) > 0
        
        # Should identify footer as shared component
        footer_components = shared_components['footers']
        assert len(footer_components) > 0
    
    def test_page_dependencies_analysis(self, sample_website_directory):
        """Test analysis of dependencies between pages."""
        parser = HTMLParser()
        result = parser.parse_input(sample_website_directory)
        
        dependencies = result['page_dependencies']
        assert 'shared_components' in dependencies
        assert 'shared_styles' in dependencies
        assert 'shared_scripts' in dependencies
        assert 'page_links' in dependencies
        assert 'dependency_graph' in dependencies
        assert 'asset_dependencies' in dependencies
        assert 'cross_page_references' in dependencies
        
        # Check dependency graph
        dependency_graph = dependencies['dependency_graph']
        assert len(dependency_graph) == 4  # One entry per page
        
        for page_deps in dependency_graph.values():
            assert 'depends_on' in page_deps
            assert 'depended_by' in page_deps
            assert 'shared_assets' in page_deps
    
    def test_asset_dependencies_analysis(self, sample_website_directory):
        """Test analysis of asset dependencies."""
        parser = HTMLParser()
        result = parser.parse_input(sample_website_directory)
        
        asset_dependencies = result['asset_dependencies']
        assert 'css_dependencies' in asset_dependencies
        assert 'js_dependencies' in asset_dependencies
        assert 'image_dependencies' in asset_dependencies
        assert 'font_dependencies' in asset_dependencies
        assert 'external_dependencies' in asset_dependencies
        assert 'dependency_tree' in asset_dependencies
        
        # Check CSS dependencies
        css_deps = asset_dependencies['css_dependencies']
        assert len(css_deps) == 4  # One entry per page
        
        # Each page should depend on CSS files
        for page_css in css_deps.values():
            assert isinstance(page_css, list)
            assert len(page_css) > 0  # Should have CSS dependencies
    
    def test_enhanced_website_analysis(self, sample_website_directory):
        """Test enhanced website analysis with multi-file capabilities."""
        parser = HTMLParser()
        analyzer = HTMLAnalyzer()
        
        # Parse the website
        parsed_data = parser.parse_input(sample_website_directory)
        
        # Analyze the website
        analysis = analyzer.analyze_website(parsed_data)
        
        # Check enhanced analysis features
        assert 'site_architecture' in analysis
        assert 'cross_page_dependencies' in analysis
        assert 'shared_assets_analysis' in analysis
        assert 'navigation_complexity' in analysis
        assert 'performance_analysis' in analysis
        assert 'modernization_strategy' in analysis
        assert 'component_reusability' in analysis
        assert 'asset_optimization' in analysis
        
        # Check site architecture analysis
        site_arch = analysis['site_architecture']
        assert 'total_pages' in site_arch
        assert 'navigation_depth' in site_arch
        assert 'page_types' in site_arch
        assert 'common_patterns' in site_arch
        assert 'architecture_score' in site_arch
        assert site_arch['total_pages'] == 4
        
        # Check modernization strategy
        strategy = analysis['modernization_strategy']
        assert 'phases' in strategy
        assert 'priorities' in strategy
        assert 'timeline' in strategy
        assert 'risk_mitigation' in strategy
        assert 'success_metrics' in strategy
        assert 'rollback_plan' in strategy
    
    def test_component_reusability_analysis(self, sample_website_directory):
        """Test component reusability analysis."""
        parser = HTMLParser()
        analyzer = HTMLAnalyzer()
        
        parsed_data = parser.parse_input(sample_website_directory)
        analysis = analyzer.analyze_website(parsed_data)
        
        reusability = analysis['component_reusability']
        assert 'reusable_components' in reusability
        assert 'component_patterns' in reusability
        assert 'extraction_opportunities' in reusability
        assert 'component_library' in reusability
        assert 'reusability_score' in reusability
        
        # Should identify navigation and footer as reusable components
        reusable_components = reusability['reusable_components']
        assert len(reusable_components) > 0
    
    def test_asset_optimization_analysis(self, sample_website_directory):
        """Test asset optimization analysis."""
        parser = HTMLParser()
        analyzer = HTMLAnalyzer()
        
        parsed_data = parser.parse_input(sample_website_directory)
        analysis = analyzer.analyze_website(parsed_data)
        
        optimization = analysis['asset_optimization']
        assert 'css_optimization' in optimization
        assert 'js_optimization' in optimization
        assert 'image_optimization' in optimization
        assert 'font_optimization' in optimization
        assert 'bundling_opportunities' in optimization
        assert 'minification_opportunities' in optimization
        assert 'compression_opportunities' in optimization
    
    def test_input_type_detection(self):
        """Test input type detection for different input formats."""
        # Test single file
        assert HTMLParser()._detect_input_type('index.html') == 'single_file'
        
        # Test directory
        with tempfile.TemporaryDirectory() as temp_dir:
            assert HTMLParser()._detect_input_type(temp_dir) == 'directory'
        
        # Test ZIP archive
        with tempfile.NamedTemporaryFile(suffix='.zip') as temp_file:
            assert HTMLParser()._detect_input_type(temp_file.name) == 'archive'
        
        # Test git repository
        assert HTMLParser()._detect_input_type('https://github.com/user/repo') == 'git'
        assert HTMLParser()._detect_input_type('git@github.com:user/repo.git') == 'git'
        
        # Test unknown type
        assert HTMLParser()._detect_input_type('unknown.txt') == 'unknown'
    
    def test_directory_statistics_calculation(self, sample_website_directory):
        """Test directory statistics calculation."""
        parser = HTMLParser()
        result = parser.parse_input(sample_website_directory)
        
        stats = result['directory_stats']
        assert stats['total_files'] >= 4
        assert stats['total_directories'] >= 3
        assert stats['directory_depth'] >= 1
        assert stats['total_size'] > 0
        assert len(stats['file_types']) >= 3  # html, css, js
        assert len(stats['largest_files']) <= 10  # Top 10 largest files
        
        # Check file types
        assert '.html' in stats['file_types']
        assert '.css' in stats['file_types']
        assert '.js' in stats['file_types']
    
    def test_archive_compression_analysis(self, sample_zip_archive):
        """Test ZIP archive compression analysis."""
        parser = HTMLParser()
        result = parser.parse_input(sample_zip_archive)
        
        archive_info = result['archive_info']
        assert archive_info['compression_ratio'] >= 0
        assert archive_info['compression_ratio'] <= 100
        assert archive_info['compressed_size'] <= archive_info['uncompressed_size']
        
        # Check file hierarchy
        file_hierarchy = result['file_hierarchy']
        assert isinstance(file_hierarchy, dict)
        assert len(file_hierarchy) > 0
    
    def test_git_metadata_extraction(self):
        """Test git metadata extraction."""
        parser = HTMLParser()
        
        with tempfile.TemporaryDirectory() as temp_dir:
            # Mock git commands
            with patch('subprocess.run') as mock_run:
                # Mock successful git commands
                mock_run.return_value.returncode = 0
                mock_run.return_value.stdout = "origin\thttps://github.com/user/repo.git (fetch)\norigin\thttps://github.com/user/repo.git (push)\n"
                
                # Mock branch command
                mock_run.side_effect = [
                    MagicMock(returncode=0, stdout="main\n"),
                    MagicMock(returncode=0, stdout="abc123|John Doe|john@example.com|Initial commit|2024-01-01 12:00:00\n"),
                    MagicMock(returncode=0, stdout="index.html\nabout.html\ncss/style.css\n"),
                    MagicMock(returncode=0, stdout="1.2M\t.\n")
                ]
                
                git_info = parser._extract_git_metadata(temp_dir)
                
                assert 'remotes' in git_info
                assert 'current_branch' in git_info
                assert 'last_commit' in git_info
                assert 'total_files' in git_info
                assert 'repo_size' in git_info
                
                # Check commit info
                commit = git_info['last_commit']
                assert commit['hash'] == 'abc123'
                assert commit['author'] == 'John Doe'
                assert commit['email'] == 'john@example.com'
                assert commit['message'] == 'Initial commit'
                assert commit['date'] == '2024-01-01 12:00:00'


if __name__ == '__main__':
    pytest.main([__file__]) 