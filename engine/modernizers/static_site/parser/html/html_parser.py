"""
HTML Parser for legacy website analysis.
"""

import os
import zipfile
import tempfile
import subprocess
import shutil
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
        Parse input file, ZIP archive, or git repository.
        
        Args:
            input_path: Path to HTML file, ZIP archive, or git repository URL
            
        Returns:
            Dictionary containing parsed website structure
        """
        input_path = Path(input_path)
        
        # Handle git repository URLs
        if str(input_path).startswith(('http://', 'https://', 'git://')):
            return self._parse_git_repository(str(input_path))
        
        # Handle local paths
        if not input_path.exists():
            raise FileNotFoundError(f"Input file not found: {input_path}")
        
        if input_path.suffix.lower() == '.zip':
            return self._parse_zip_archive(input_path)
        elif input_path.suffix.lower() in ['.html', '.htm']:
            return self._parse_single_file(input_path)
        elif input_path.is_dir():
            return self._parse_directory(input_path)
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
    
    def _parse_git_repository(self, repo_url: str) -> Dict[str, Any]:
        """Parse git repository containing website files."""
        temp_dir = tempfile.mkdtemp()
        try:
            # Clone the repository
            print(f"🔗 Cloning repository: {repo_url}")
            subprocess.run(['git', 'clone', repo_url, temp_dir], check=True, capture_output=True)
            
            # Parse the cloned directory
            return self._parse_directory(Path(temp_dir))
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Failed to clone repository: {e}")
        finally:
            # Clean up temporary directory
            shutil.rmtree(temp_dir, ignore_errors=True)
    
    def _parse_directory(self, dir_path: Path) -> Dict[str, Any]:
        """Parse directory containing website files."""
        result = {
            'type': 'directory',
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
        
        # Walk through the directory
        for root, dirs, files in os.walk(dir_path):
            for file in files:
                file_path = Path(root) / file
                relative_path = file_path.relative_to(dir_path)
                
                if file_path.suffix.lower() in ['.html', '.htm']:
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                    parsed_file = self._parse_html_content(content, str(relative_path))
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
                    result['assets']['css'].append(str(relative_path))
                elif file_path.suffix.lower() in ['.js']:
                    result['assets']['js'].append(str(relative_path))
                elif file_path.suffix.lower() in ['.jpg', '.jpeg', '.png', '.gif', '.svg', '.webp']:
                    result['assets']['images'].append(str(relative_path))
                elif file_path.suffix.lower() in ['.woff', '.woff2', '.ttf', '.eot']:
                    result['assets']['fonts'].append(str(relative_path))
        
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
        
        # Extract sections with better identification
        print(f"DEBUG: HTML Parser - Found {len(soup.find_all(['section', 'div']))} potential sections")
        
        # Find main sections only (not nested ones)
        main_sections = []
        for section in soup.find_all(['section', 'div']):
            # Skip if it's just a container div
            if section.name == 'div' and not section.get('id') and not section.get('class'):
                continue
                
            # Skip if this section is nested inside another section
            parent_section = section.find_parent(['section', 'div'])
            if parent_section and parent_section.name in ['section', 'div']:
                continue
                
            main_sections.append(section)
        
        print(f"DEBUG: HTML Parser - Found {len(main_sections)} main sections")
        
        for section in main_sections:
                
            section_id = section.get('id', '')
            section_classes = section.get('class', [])
            
            # Identify section type based on id, classes, or content
            section_type = 'general'
            if section_id in ['services', 'contact', 'about', 'home', 'hero']:
                section_type = section_id
            elif any(cls in ['services', 'contact', 'about', 'hero'] for cls in section_classes):
                section_type = next((cls for cls in section_classes if cls in ['services', 'contact', 'about', 'hero']), 'general')
            elif section.find('h2') and any(keyword in section.find('h2').get_text().lower() for keyword in ['service', 'contact', 'about']):
                section_type = 'services' if 'service' in section.find('h2').get_text().lower() else 'contact' if 'contact' in section.find('h2').get_text().lower() else 'about'
            
            # Extract title from h1, h2, h3
            title_elem = section.find(['h1', 'h2', 'h3'])
            section_title = title_elem.get_text(strip=True) if title_elem else ''
            
            # Skip sections that are just containers or have no meaningful content
            if not section_id and not section_title and len(section.get_text(strip=True)) < 50:
                print(f"DEBUG: HTML Parser - Skipping section with no meaningful content")
                continue
            
            # Extract section content more intelligently
            section_content = {
                'tag': section.name,
                'classes': section_classes,
                'id': section_id,
                'type': section_type,
                'title': section_title,
                'content': '',
                'cards': [],
                'form': None
            }
            
            # Extract cards for services section
            if section_type == 'services':
                cards = section.find_all(['div', 'article'], class_=re.compile(r'card|feature|service'))
                for card in cards:
                    card_title = card.find(['h3', 'h4', 'h5'])
                    card_text = card.find(['p', 'div'])
                    card_button = card.find('button')
                    
                    card_data = {
                        'title': card_title.get_text(strip=True) if card_title else '',
                        'text': card_text.get_text(strip=True) if card_text else '',
                        'button_text': card_button.get_text(strip=True) if card_button else '',
                        'button_class': ' '.join(card_button.get('class', [])) if card_button else ''
                    }
                    section_content['cards'].append(card_data)
            
            # Extract form for contact section
            if section_type == 'contact':
                form = section.find('form')
                if form:
                    form_data = {
                        'action': form.get('action', ''),
                        'method': form.get('method', 'get'),
                        'inputs': []
                    }
                    for input_elem in form.find_all(['input', 'textarea', 'select']):
                        input_data = {
                            'type': input_elem.get('type', input_elem.name),
                            'name': input_elem.get('name', ''),
                            'placeholder': input_elem.get('placeholder', ''),
                            'required': input_elem.get('required') is not None,
                            'label': ''
                        }
                        # Try to find associated label
                        if input_elem.get('id'):
                            label = form.find('label', attrs={'for': input_elem.get('id')})
                            if label:
                                input_data['label'] = label.get_text(strip=True)
                        section_content['form'] = form_data
                        form_data['inputs'].append(input_data)
            
            # Extract general content
            section_content['content'] = section.get_text(strip=True)
            
            print(f"DEBUG: HTML Parser - Added section: {section_content['type']} - {section_content['title']}")
            structure['sections'].append(section_content)
        
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