"""
HTML Parser for legacy website analysis.
"""

import os
import zipfile
from pathlib import Path
from typing import Dict, List, Optional, Any
from bs4 import BeautifulSoup
import re


class HTMLParser:
    """
    Parser for analyzing legacy HTML websites.
    
    Supports:
    - Single HTML files
    - ZIP archives containing HTML files
    - Bootstrap 3/4 detection
    - jQuery detection
    - PHP code extraction
    """
    
    def __init__(self):
        self.supported_frameworks = {
            'bootstrap': {
                'versions': ['3', '4'],
                'patterns': [
                    r'bootstrap.*\.css',
                    r'bootstrap.*\.js',
                    r'class="[^"]*col-',
                    r'class="[^"]*btn-',
                    r'class="[^"]*alert-',
                    r'class="[^"]*navbar',
                    r'class="[^"]*container',
                    r'class="[^"]*row',
                    r'class="[^"]*form-'
                ]
            },
            'jquery': {
                'patterns': [
                    r'jquery.*\.js',
                    r'\$\(.*\)',
                    r'jQuery\(',
                    r'\.on\(',
                    r'\.click\(',
                    r'\.ajax\('
                ]
            },
            'php': {
                'patterns': [
                    r'<\?php',
                    r'<\?=',
                    r'include\s*\(',
                    r'require\s*\(',
                    r'function\s+\w+\s*\(',
                    r'\$\w+',
                    r'echo\s+',
                    r'print\s+'
                ]
            }
        }
    
    def parse_input(self, input_path: str) -> Dict[str, Any]:
        """
        Parse input file or ZIP archive.
        
        Args:
            input_path: Path to HTML file or ZIP archive
            
        Returns:
            Dictionary containing parsed website structure
        """
        input_path = Path(input_path)
        
        if not input_path.exists():
            raise FileNotFoundError(f"Input file not found: {input_path}")
        
        if input_path.suffix.lower() == '.zip':
            return self._parse_zip_archive(input_path)
        elif input_path.suffix.lower() in ['.html', '.htm']:
            return self._parse_single_file(input_path)
        else:
            raise ValueError(f"Unsupported file type: {input_path.suffix}")
    
    def _parse_zip_archive(self, zip_path: Path) -> Dict[str, Any]:
        """Parse ZIP archive containing HTML files."""
        result = {
            'type': 'archive',
            'files': [],
            'structure': {},
            'frameworks': {},
            'assets': {
                'css': [],
                'js': [],
                'images': [],
                'fonts': []
            }
        }
        
        with zipfile.ZipFile(zip_path, 'r') as zip_file:
            for file_info in zip_file.filelist:
                file_path = Path(file_info.filename)
                
                if file_path.suffix.lower() in ['.html', '.htm']:
                    content = zip_file.read(file_info.filename).decode('utf-8', errors='ignore')
                    parsed_file = self._parse_html_content(content, str(file_path))
                    result['files'].append(parsed_file)
                    
                    # Update frameworks detection
                    for framework, detection in parsed_file.get('frameworks', {}).items():
                        if framework not in result['frameworks']:
                            result['frameworks'][framework] = detection
                        else:
                            result['frameworks'][framework]['detected'] = (
                                result['frameworks'][framework]['detected'] or detection['detected']
                            )
                
                elif file_path.suffix.lower() in ['.css']:
                    result['assets']['css'].append(str(file_path))
                elif file_path.suffix.lower() in ['.js']:
                    result['assets']['js'].append(str(file_path))
                elif file_path.suffix.lower() in ['.jpg', '.jpeg', '.png', '.gif', '.svg', '.webp']:
                    result['assets']['images'].append(str(file_path))
                elif file_path.suffix.lower() in ['.woff', '.woff2', '.ttf', '.eot']:
                    result['assets']['fonts'].append(str(file_path))
        
        return result
    
    def _parse_single_file(self, file_path: Path) -> Dict[str, Any]:
        """Parse single HTML file."""
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
        
        parsed_file = self._parse_html_content(content, str(file_path))
        
        return {
            'type': 'single_file',
            'files': [parsed_file],
            'structure': parsed_file.get('structure', {}),
            'frameworks': parsed_file.get('frameworks', {}),
            'assets': parsed_file.get('assets', {})
        }
    
    def _parse_html_content(self, content: str, file_path: str) -> Dict[str, Any]:
        """Parse HTML content and extract structure and framework information."""
        soup = BeautifulSoup(content, 'html.parser')
        
        result = {
            'file_path': file_path,
            'structure': self._extract_structure(soup),
            'frameworks': self._detect_frameworks(content, soup),
            'assets': self._extract_assets(soup),
            'php_code': self._extract_php_code(content),
            'javascript': self._extract_javascript(soup),
            'styles': self._extract_styles(soup)
        }
        
        return result
    
    def _extract_structure(self, soup: BeautifulSoup) -> Dict[str, Any]:
        """Extract website structure from HTML."""
        structure = {
            'title': soup.title.string if soup.title else '',
            'meta': {},
            'navigation': [],
            'sections': [],
            'forms': [],
            'tables': [],
            'images': [],
            'links': []
        }
        
        # Extract meta tags
        for meta in soup.find_all('meta'):
            name = meta.get('name', meta.get('property', ''))
            content = meta.get('content', '')
            if name:
                structure['meta'][name] = content
        
        # Extract navigation
        nav_elements = soup.find_all(['nav', 'ul', 'ol'], class_=re.compile(r'nav|menu|navbar'))
        for nav in nav_elements:
            nav_items = []
            for link in nav.find_all('a'):
                nav_items.append({
                    'text': link.get_text(strip=True),
                    'href': link.get('href', ''),
                    'target': link.get('target', '')
                })
            structure['navigation'].append(nav_items)
        
        # Extract sections
        for section in soup.find_all(['section', 'div'], class_=re.compile(r'section|content|main')):
            structure['sections'].append({
                'tag': section.name,
                'classes': section.get('class', []),
                'id': section.get('id', ''),
                'content': section.get_text(strip=True)[:200] + '...' if len(section.get_text(strip=True)) > 200 else section.get_text(strip=True)
            })
        
        # Extract forms
        for form in soup.find_all('form'):
            form_data = {
                'action': form.get('action', ''),
                'method': form.get('method', 'get'),
                'inputs': []
            }
            for input_elem in form.find_all(['input', 'textarea', 'select']):
                form_data['inputs'].append({
                    'type': input_elem.get('type', input_elem.name),
                    'name': input_elem.get('name', ''),
                    'placeholder': input_elem.get('placeholder', ''),
                    'required': input_elem.get('required') is not None
                })
            structure['forms'].append(form_data)
        
        # Extract tables
        for table in soup.find_all('table'):
            table_data = {
                'headers': [],
                'rows': []
            }
            headers = table.find_all('th')
            for header in headers:
                table_data['headers'].append(header.get_text(strip=True))
            
            rows = table.find_all('tr')
            for row in rows[1:]:  # Skip header row
                cells = row.find_all(['td', 'th'])
                row_data = [cell.get_text(strip=True) for cell in cells]
                table_data['rows'].append(row_data)
            
            structure['tables'].append(table_data)
        
        # Extract images
        for img in soup.find_all('img'):
            structure['images'].append({
                'src': img.get('src', ''),
                'alt': img.get('alt', ''),
                'title': img.get('title', '')
            })
        
        # Extract links
        for link in soup.find_all('a'):
            structure['links'].append({
                'text': link.get_text(strip=True),
                'href': link.get('href', ''),
                'target': link.get('target', '')
            })
        
        return structure
    
    def _detect_frameworks(self, content: str, soup: BeautifulSoup) -> Dict[str, Any]:
        """Detect frameworks and libraries used in the website."""
        frameworks = {}
        
        for framework, config in self.supported_frameworks.items():
            detected = False
            version = None
            evidence = []
            
            # Check for framework patterns
            for pattern in config['patterns']:
                matches = re.findall(pattern, content, re.IGNORECASE)
                if matches:
                    detected = True
                    evidence.extend(matches[:5])  # Limit evidence to first 5 matches
            
            # Check for specific version patterns
            if framework == 'bootstrap':
                version_match = re.search(r'bootstrap[.-]?(\d+\.\d+)', content, re.IGNORECASE)
                if version_match:
                    version = version_match.group(1)
            
            frameworks[framework] = {
                'detected': detected,
                'version': version,
                'evidence': evidence
            }
        
        return frameworks
    
    def _extract_assets(self, soup: BeautifulSoup) -> Dict[str, List[str]]:
        """Extract asset references from HTML."""
        assets = {
            'css': [],
            'js': [],
            'images': [],
            'fonts': []
        }
        
        # Extract CSS files
        for link in soup.find_all('link', rel='stylesheet'):
            href = link.get('href', '')
            if href:
                assets['css'].append(href)
        
        # Extract JavaScript files
        for script in soup.find_all('script', src=True):
            src = script.get('src', '')
            if src:
                assets['js'].append(src)
        
        # Extract images
        for img in soup.find_all('img'):
            src = img.get('src', '')
            if src:
                assets['images'].append(src)
        
        return assets
    
    def _extract_php_code(self, content: str) -> List[str]:
        """Extract PHP code blocks from content."""
        php_patterns = [
            r'<\?php.*?\?>',
            r'<\?=.*?\?>',
            r'<\?.*?\?>'
        ]
        
        php_blocks = []
        for pattern in php_patterns:
            matches = re.findall(pattern, content, re.DOTALL | re.IGNORECASE)
            php_blocks.extend(matches)
        
        return php_blocks
    
    def _extract_javascript(self, soup: BeautifulSoup) -> List[str]:
        """Extract JavaScript code blocks."""
        scripts = []
        for script in soup.find_all('script'):
            if script.string:
                scripts.append(script.string.strip())
        return scripts
    
    def _extract_styles(self, soup: BeautifulSoup) -> List[str]:
        """Extract inline styles."""
        styles = []
        for style in soup.find_all('style'):
            if style.string:
                styles.append(style.string.strip())
        return styles 