"""
HTML Analyzer for modernization insights.
"""

from typing import Dict, List, Any, Optional
import re


class HTMLAnalyzer:
    """
    Analyzes parsed HTML data to provide modernization insights.
    """
    
    def __init__(self):
        self.modernization_patterns = {
            'bootstrap_classes': {
                'grid': [
                    r'col-xs-', r'col-sm-', r'col-md-', r'col-lg-',
                    r'col-xl-', r'offset-', r'pull-', r'push-'
                ],
                'components': [
                    r'btn-', r'alert-', r'badge-', r'label-',
                    r'panel-', r'well-', r'thumbnail-', r'media-'
                ],
                'utilities': [
                    r'text-', r'bg-', r'm-', r'p-', r'border-',
                    r'rounded-', r'shadow-', r'position-'
                ]
            },
            'jquery_patterns': [
                r'\$\([\'"][^\'"]*[\'"]\)\.',
                r'\.on\([\'"][^\'"]*[\'"]',
                r'\.click\(',
                r'\.ajax\(',
                r'\.fadeIn\(',
                r'\.fadeOut\(',
                r'\.slideDown\(',
                r'\.slideUp\('
            ],
            'php_patterns': [
                r'<\?php\s+echo\s+',
                r'<\?=\s*\$',
                r'include\s*\([\'"][^\'"]*[\'"]\)',
                r'require\s*\([\'"][^\'"]*[\'"]\)',
                r'function\s+\w+\s*\(',
                r'\$\w+\s*=',
                r'if\s*\(.*\)\s*\{',
                r'foreach\s*\(.*\)\s*\{'
            ]
        }
    
    def analyze_website(self, parsed_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Analyze parsed website data and provide modernization insights.
        
        Args:
            parsed_data: Output from HTMLParser.parse_input()
            
        Returns:
            Analysis results with modernization recommendations
        """
        analysis = {
            'complexity_score': 0,
            'modernization_effort': 'low',
            'framework_migration': {},
            'component_mapping': {},
            'recommendations': [],
            'risks': [],
            'opportunities': []
        }
        
        # Analyze each file
        for file_data in parsed_data.get('files', []):
            file_analysis = self._analyze_file(file_data)
            
            # Aggregate analysis
            analysis['complexity_score'] = max(
                analysis['complexity_score'],
                file_analysis['complexity_score']
            )
            
            # Merge recommendations
            analysis['recommendations'].extend(file_analysis['recommendations'])
            analysis['risks'].extend(file_analysis['risks'])
            analysis['opportunities'].extend(file_analysis['opportunities'])
        
        # Determine overall modernization effort
        analysis['modernization_effort'] = self._calculate_effort_level(analysis['complexity_score'])
        
        # Generate framework migration plan
        analysis['framework_migration'] = self._generate_migration_plan(parsed_data)
        
        # Generate component mapping
        analysis['component_mapping'] = self._generate_component_mapping(parsed_data)
        
        return analysis
    
    def _analyze_file(self, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze a single file's modernization needs."""
        analysis = {
            'complexity_score': 0,
            'recommendations': [],
            'risks': [],
            'opportunities': []
        }
        
        # Analyze frameworks
        frameworks = file_data.get('frameworks', {})
        
        if frameworks.get('bootstrap', {}).get('detected'):
            bootstrap_analysis = self._analyze_bootstrap_usage(file_data)
            analysis['complexity_score'] += bootstrap_analysis['score']
            analysis['recommendations'].extend(bootstrap_analysis['recommendations'])
        
        if frameworks.get('jquery', {}).get('detected'):
            jquery_analysis = self._analyze_jquery_usage(file_data)
            analysis['complexity_score'] += jquery_analysis['score']
            analysis['recommendations'].extend(jquery_analysis['recommendations'])
        
        if frameworks.get('php', {}).get('detected'):
            php_analysis = self._analyze_php_usage(file_data)
            analysis['complexity_score'] += php_analysis['score']
            analysis['risks'].extend(php_analysis['risks'])
            analysis['recommendations'].extend(php_analysis['recommendations'])
        
        # Analyze structure complexity
        structure = file_data.get('structure', {})
        structure_analysis = self._analyze_structure_complexity(structure)
        analysis['complexity_score'] += structure_analysis['score']
        analysis['recommendations'].extend(structure_analysis['recommendations'])
        
        return analysis
    
    def _analyze_bootstrap_usage(self, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze Bootstrap usage and provide migration insights."""
        analysis = {
            'score': 0,
            'recommendations': []
        }
        
        content = self._get_file_content(file_data)
        structure = file_data.get('structure', {})
        
        # Count Bootstrap classes
        bootstrap_classes = []
        for pattern_group in self.modernization_patterns['bootstrap_classes'].values():
            for pattern in pattern_group:
                matches = re.findall(pattern, content, re.IGNORECASE)
                bootstrap_classes.extend(matches)
        
        analysis['score'] = len(bootstrap_classes) * 2
        
        if bootstrap_classes:
            analysis['recommendations'].append({
                'type': 'bootstrap_migration',
                'priority': 'high',
                'description': f'Found {len(bootstrap_classes)} Bootstrap classes to migrate to Tailwind CSS',
                'details': {
                    'classes_found': bootstrap_classes[:10],  # Show first 10
                    'total_classes': len(bootstrap_classes)
                }
            })
        
        # Analyze specific components
        if any('navbar' in str(section.get('classes', [])) for section in structure.get('sections', [])):
            analysis['recommendations'].append({
                'type': 'component_migration',
                'priority': 'medium',
                'description': 'Bootstrap navbar detected - migrate to Tailwind navigation component',
                'component': 'navbar'
            })
        
        if structure.get('forms'):
            analysis['recommendations'].append({
                'type': 'component_migration',
                'priority': 'medium',
                'description': 'Forms detected - migrate to Tailwind form components',
                'component': 'forms',
                'count': len(structure['forms'])
            })
        
        return analysis
    
    def _analyze_jquery_usage(self, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze jQuery usage and provide migration insights."""
        analysis = {
            'score': 0,
            'recommendations': []
        }
        
        content = self._get_file_content(file_data)
        javascript = file_data.get('javascript', [])
        
        # Count jQuery patterns
        jquery_patterns = []
        for pattern in self.modernization_patterns['jquery_patterns']:
            matches = re.findall(pattern, content, re.IGNORECASE)
            jquery_patterns.extend(matches)
        
        analysis['score'] = len(jquery_patterns) * 3
        
        if jquery_patterns:
            analysis['recommendations'].append({
                'type': 'jquery_migration',
                'priority': 'high',
                'description': f'Found {len(jquery_patterns)} jQuery patterns to migrate to React hooks',
                'details': {
                    'patterns_found': jquery_patterns[:10],
                    'total_patterns': len(jquery_patterns)
                }
            })
        
        # Analyze specific jQuery functionality
        if any('ajax' in pattern for pattern in jquery_patterns):
            analysis['recommendations'].append({
                'type': 'api_migration',
                'priority': 'high',
                'description': 'jQuery AJAX calls detected - migrate to React fetch/axios',
                'component': 'ajax_calls'
            })
        
        if any('click' in pattern for pattern in jquery_patterns):
            analysis['recommendations'].append({
                'type': 'event_migration',
                'priority': 'medium',
                'description': 'jQuery click handlers detected - migrate to React event handlers',
                'component': 'event_handlers'
            })
        
        return analysis
    
    def _analyze_php_usage(self, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze PHP usage and provide migration insights."""
        analysis = {
            'score': 0,
            'recommendations': [],
            'risks': []
        }
        
        php_code = file_data.get('php_code', [])
        
        analysis['score'] = len(php_code) * 5  # PHP is high complexity
        
        if php_code:
            analysis['risks'].append({
                'type': 'php_complexity',
                'severity': 'high',
                'description': f'Found {len(php_code)} PHP code blocks - server-side logic needs migration',
                'details': {
                    'php_blocks': len(php_code),
                    'recommendation': 'Consider migrating to API endpoints or static site generation'
                }
            })
            
            analysis['recommendations'].append({
                'type': 'php_migration',
                'priority': 'critical',
                'description': 'PHP code detected - requires backend migration strategy',
                'details': {
                    'migration_options': [
                        'Convert to static site with build-time data',
                        'Migrate to API endpoints',
                        'Use serverless functions',
                        'Implement headless CMS'
                    ]
                }
            })
        
        return analysis
    
    def _analyze_structure_complexity(self, structure: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze HTML structure complexity."""
        analysis = {
            'score': 0,
            'recommendations': []
        }
        
        # Count components
        component_counts = {
            'sections': len(structure.get('sections', [])),
            'forms': len(structure.get('forms', [])),
            'tables': len(structure.get('tables', [])),
            'images': len(structure.get('images', [])),
            'links': len(structure.get('links', []))
        }
        
        analysis['score'] = sum(component_counts.values())
        
        # Generate structure-based recommendations
        if component_counts['forms'] > 0:
            analysis['recommendations'].append({
                'type': 'form_migration',
                'priority': 'medium',
                'description': f'Found {component_counts["forms"]} forms to migrate to React components',
                'component': 'forms'
            })
        
        if component_counts['tables'] > 0:
            analysis['recommendations'].append({
                'type': 'table_migration',
                'priority': 'low',
                'description': f'Found {component_counts["tables"]} tables to migrate to React components',
                'component': 'tables'
            })
        
        if component_counts['images'] > 10:
            analysis['recommendations'].append({
                'type': 'image_optimization',
                'priority': 'medium',
                'description': f'Found {component_counts["images"]} images - consider optimization for modern web',
                'component': 'images'
            })
        
        return analysis
    
    def _calculate_effort_level(self, complexity_score: int) -> str:
        """Calculate modernization effort level based on complexity score."""
        if complexity_score < 10:
            return 'low'
        elif complexity_score < 30:
            return 'medium'
        elif complexity_score < 60:
            return 'high'
        else:
            return 'very_high'
    
    def _generate_migration_plan(self, parsed_data: Dict[str, Any]) -> Dict[str, Any]:
        """Generate framework migration plan."""
        frameworks = parsed_data.get('frameworks', {})
        
        migration_plan = {
            'bootstrap_to_tailwind': {
                'required': frameworks.get('bootstrap', {}).get('detected', False),
                'version': frameworks.get('bootstrap', {}).get('version'),
                'steps': [
                    'Install Tailwind CSS',
                    'Configure Tailwind theme',
                    'Replace Bootstrap classes with Tailwind equivalents',
                    'Update component structure',
                    'Test responsive behavior'
                ]
            },
            'jquery_to_react': {
                'required': frameworks.get('jquery', {}).get('detected', False),
                'steps': [
                    'Convert jQuery selectors to React refs',
                    'Replace jQuery event handlers with React event handlers',
                    'Migrate AJAX calls to React hooks',
                    'Convert jQuery animations to CSS transitions',
                    'Update DOM manipulation to React state management'
                ]
            },
            'php_to_static': {
                'required': frameworks.get('php', {}).get('detected', False),
                'steps': [
                    'Extract dynamic data to JSON files',
                    'Convert PHP templates to React components',
                    'Implement build-time data processing',
                    'Set up static site generation',
                    'Configure deployment pipeline'
                ]
            }
        }
        
        return migration_plan
    
    def _generate_component_mapping(self, parsed_data: Dict[str, Any]) -> Dict[str, Any]:
        """Generate mapping from legacy components to modern equivalents."""
        component_mapping = {
            'bootstrap_components': {
                'navbar': 'Tailwind navigation component',
                'container': 'Tailwind container classes',
                'row': 'Tailwind grid system',
                'col-*': 'Tailwind grid columns',
                'btn': 'Tailwind button classes',
                'alert': 'Tailwind alert component',
                'form': 'Tailwind form components',
                'table': 'Tailwind table classes'
            },
            'jquery_functions': {
                '$.ajax()': 'React useEffect + fetch',
                '$.get()': 'React useEffect + fetch',
                '$.post()': 'React form submission',
                '$(element).click()': 'React onClick handler',
                '$(element).on()': 'React event handlers',
                '$(element).fadeIn()': 'CSS transitions',
                '$(element).slideDown()': 'CSS animations'
            },
            'php_patterns': {
                '<?php echo $var ?>': 'React template literals',
                '<?php include ?>': 'React component imports',
                '<?php foreach ?>': 'React map() function',
                '<?php if ?>': 'React conditional rendering',
                '<?php function ?>': 'React custom hooks'
            }
        }
        
        return component_mapping
    
    def _get_file_content(self, file_data: Dict[str, Any]) -> str:
        """Extract file content for analysis."""
        # This would need to be implemented based on how file content is stored
        # For now, return empty string - this should be enhanced based on actual implementation
        return "" 