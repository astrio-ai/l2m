"""
jQuery to React transformation rules.
"""

from typing import Dict, List, Any
import re


class JQueryRules:
    """
    Rules for transforming jQuery code to React hooks and components.
    """
    
    def __init__(self):
        self.jquery_to_react = {
            # Event handlers
            'click': 'onClick',
            'change': 'onChange',
            'submit': 'onSubmit',
            'keyup': 'onKeyUp',
            'keydown': 'onKeyDown',
            'focus': 'onFocus',
            'blur': 'onBlur',
            'mouseenter': 'onMouseEnter',
            'mouseleave': 'onMouseLeave',
            'hover': 'onMouseEnter',
            
            # AJAX methods
            'ajax': 'fetch',
            'get': 'fetch',
            'post': 'fetch',
            'getJSON': 'fetch',
            'load': 'useEffect',
            
            # DOM manipulation
            'html': 'innerHTML',
            'text': 'textContent',
            'val': 'value',
            'attr': 'setAttribute',
            'removeAttr': 'removeAttribute',
            'addClass': 'className',
            'removeClass': 'className',
            'toggleClass': 'className',
            'show': 'display',
            'hide': 'display',
            'fadeIn': 'opacity',
            'fadeOut': 'opacity',
            'slideDown': 'height',
            'slideUp': 'height',
            
            # Effects and animations
            'animate': 'CSS transitions',
            'fadeTo': 'opacity',
            'slideToggle': 'height',
            'toggle': 'display',
            
            # Traversal
            'find': 'querySelector',
            'children': 'children',
            'parent': 'parentElement',
            'siblings': 'siblings',
            'next': 'nextElementSibling',
            'prev': 'previousElementSibling',
            'closest': 'closest',
            'filter': 'filter',
            'not': 'not',
            'has': 'has',
            'is': 'matches',
            
            # Attributes and properties
            'prop': 'property',
            'data': 'dataset',
            'removeData': 'removeAttribute',
            'hasClass': 'classList.contains',
            'index': 'index',
            'length': 'length',
            
            # Utility methods
            'each': 'map/forEach',
            'map': 'map',
            'filter': 'filter',
            'grep': 'filter',
            'inArray': 'indexOf',
            'merge': 'spread operator',
            'extend': 'Object.assign',
            'noConflict': 'not needed',
            'type': 'typeof',
            'isArray': 'Array.isArray',
            'isFunction': 'typeof === function',
            'isEmptyObject': 'Object.keys().length === 0',
            'isPlainObject': 'constructor === Object',
            'isWindow': 'window === this',
            'isNumeric': '!isNaN',
            'parseJSON': 'JSON.parse',
            'parseXML': 'DOMParser',
            'trim': 'trim',
            'param': 'URLSearchParams',
            'param': 'URLSearchParams',
            'serialize': 'FormData',
            'serializeArray': 'FormData',
            'sub': 'substring',
            'contains': 'includes',
            'unique': 'Set',
            'grep': 'filter',
            'map': 'map',
            'inArray': 'indexOf',
            'makeArray': 'Array.from',
            'merge': 'spread operator',
            'extend': 'Object.assign',
            'proxy': 'Proxy',
            'noop': '() => {}',
            'now': 'Date.now',
            'isXMLDoc': 'document.implementation.createDocument',
            'camelCase': 'camelCase function',
            'nodeName': 'nodeName',
            'isReady': 'DOMContentLoaded',
            'holdReady': 'not needed',
            'ready': 'useEffect',
            'fn': 'prototype',
            'extend': 'Object.assign',
            'each': 'forEach',
            'map': 'map',
            'grep': 'filter',
            'inArray': 'indexOf',
            'merge': 'spread operator',
            'extend': 'Object.assign',
            'proxy': 'Proxy',
            'noop': '() => {}',
            'now': 'Date.now',
            'isXMLDoc': 'document.implementation.createDocument',
            'camelCase': 'camelCase function',
            'nodeName': 'nodeName',
            'isReady': 'DOMContentLoaded',
            'holdReady': 'not needed',
            'ready': 'useEffect',
            'fn': 'prototype'
        }
    
    def transform_jquery_to_react(self, parsed_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Transform jQuery code to React hooks and components.
        
        Args:
            parsed_data: Parsed website data
            
        Returns:
            Transformed data with React components
        """
        transformed_data = {
            'components': [],
            'hooks_created': 0,
            'jquery_patterns_found': 0
        }
        
        # Process each file
        for file_data in parsed_data.get('files', []):
            file_transformed = self._transform_file_jquery(file_data)
            transformed_data['components'].extend(file_transformed.get('components', []))
            transformed_data['hooks_created'] += file_transformed.get('hooks_created', 0)
            transformed_data['jquery_patterns_found'] += file_transformed.get('jquery_patterns_found', 0)
        
        return transformed_data
    
    def _transform_file_jquery(self, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Transform jQuery code in a single file."""
        transformed = {
            'components': [],
            'hooks_created': 0,
            'jquery_patterns_found': 0
        }
        
        # Extract JavaScript code
        javascript = file_data.get('javascript', [])
        
        for js_code in javascript:
            if self._contains_jquery(js_code):
                transformed['jquery_patterns_found'] += 1
                react_component = self._transform_jquery_to_react(js_code)
                if react_component:
                    transformed['components'].append(react_component)
                    transformed['hooks_created'] += 1
        
        return transformed
    
    def _contains_jquery(self, js_code: str) -> bool:
        """Check if JavaScript code contains jQuery patterns."""
        jquery_patterns = [
            r'\$\([\'"][^\'"]*[\'"]\)',
            r'jQuery\(',
            r'\.on\(',
            r'\.click\(',
            r'\.ajax\(',
            r'\.get\(',
            r'\.post\(',
            r'\.fadeIn\(',
            r'\.fadeOut\(',
            r'\.slideDown\(',
            r'\.slideUp\(',
            r'\.show\(',
            r'\.hide\(',
            r'\.html\(',
            r'\.text\(',
            r'\.val\(',
            r'\.attr\(',
            r'\.addClass\(',
            r'\.removeClass\(',
            r'\.toggleClass\('
        ]
        
        for pattern in jquery_patterns:
            if re.search(pattern, js_code, re.IGNORECASE):
                return True
        
        return False
    
    def _transform_jquery_to_react(self, js_code: str) -> Dict[str, Any]:
        """Transform jQuery code to React component."""
        # Extract event handlers
        event_handlers = self._extract_event_handlers(js_code)
        
        # Extract AJAX calls
        ajax_calls = self._extract_ajax_calls(js_code)
        
        # Extract DOM manipulations
        dom_manipulations = self._extract_dom_manipulations(js_code)
        
        # Generate React component
        component_name = self._generate_component_name(js_code)
        component_code = self._generate_react_component(component_name, event_handlers, ajax_calls, dom_manipulations)
        
        return {
            'name': component_name,
            'type': 'component',
            'code': component_code,
            'dependencies': ['react'],
            'original_jquery': js_code
        }
    
    def _extract_event_handlers(self, js_code: str) -> List[Dict[str, Any]]:
        """Extract jQuery event handlers from code."""
        event_handlers = []
        
        # Pattern for jQuery event handlers
        patterns = [
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.on\([\'"]([^\'"]*)[\'"],?\s*function\s*\([^)]*\)\s*\{([^}]*)\}', 'on'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.click\s*\(\s*function\s*\([^)]*\)\s*\{([^}]*)\}', 'onClick'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.change\s*\(\s*function\s*\([^)]*\)\s*\{([^}]*)\}', 'onChange'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.submit\s*\(\s*function\s*\([^)]*\)\s*\{([^}]*)\}', 'onSubmit'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.keyup\s*\(\s*function\s*\([^)]*\)\s*\{([^}]*)\}', 'onKeyUp'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.focus\s*\(\s*function\s*\([^)]*\)\s*\{([^}]*)\}', 'onFocus'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.blur\s*\(\s*function\s*\([^)]*\)\s*\{([^}]*)\}', 'onBlur')
        ]
        
        for pattern, event_type in patterns:
            matches = re.findall(pattern, js_code, re.IGNORECASE | re.DOTALL)
            for match in matches:
                if len(match) >= 2:
                    selector = match[0]
                    handler_code = match[-1]  # Last group is the handler code
                    event_handlers.append({
                        'selector': selector,
                        'event_type': event_type,
                        'handler_code': handler_code.strip()
                    })
        
        return event_handlers
    
    def _extract_ajax_calls(self, js_code: str) -> List[Dict[str, Any]]:
        """Extract jQuery AJAX calls from code."""
        ajax_calls = []
        
        # Pattern for jQuery AJAX calls
        patterns = [
            (r'\$\.ajax\s*\(\s*\{([^}]*)\}', 'ajax'),
            (r'\$\.get\s*\(\s*[\'"]([^\'"]*)[\'"],?\s*function\s*\([^)]*\)\s*\{([^}]*)\}', 'get'),
            (r'\$\.post\s*\(\s*[\'"]([^\'"]*)[\'"],?\s*\{([^}]*)\},\s*function\s*\([^)]*\)\s*\{([^}]*)\}', 'post'),
            (r'\$\.getJSON\s*\(\s*[\'"]([^\'"]*)[\'"],?\s*function\s*\([^)]*\)\s*\{([^}]*)\}', 'getJSON')
        ]
        
        for pattern, ajax_type in patterns:
            matches = re.findall(pattern, js_code, re.IGNORECASE | re.DOTALL)
            for match in matches:
                if ajax_type == 'ajax':
                    # Parse AJAX options
                    options = self._parse_ajax_options(match)
                    ajax_calls.append({
                        'type': ajax_type,
                        'options': options
                    })
                else:
                    url = match[0] if match else ''
                    callback_code = match[-1] if len(match) > 1 else ''
                    ajax_calls.append({
                        'type': ajax_type,
                        'url': url,
                        'callback_code': callback_code.strip()
                    })
        
        return ajax_calls
    
    def _extract_dom_manipulations(self, js_code: str) -> List[Dict[str, Any]]:
        """Extract jQuery DOM manipulations from code."""
        dom_manipulations = []
        
        # Pattern for DOM manipulations
        patterns = [
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.html\s*\(\s*[\'"]([^\'"]*)[\'"]\)', 'html'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.text\s*\(\s*[\'"]([^\'"]*)[\'"]\)', 'text'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.val\s*\(\s*[\'"]([^\'"]*)[\'"]\)', 'val'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.addClass\s*\(\s*[\'"]([^\'"]*)[\'"]\)', 'addClass'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.removeClass\s*\(\s*[\'"]([^\'"]*)[\'"]\)', 'removeClass'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.show\s*\(\)', 'show'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.hide\s*\(\)', 'hide'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.fadeIn\s*\(\)', 'fadeIn'),
            (r'\$\([\'"]([^\'"]*)[\'"]\)\.fadeOut\s*\(\)', 'fadeOut')
        ]
        
        for pattern, manipulation_type in patterns:
            matches = re.findall(pattern, js_code, re.IGNORECASE)
            for match in matches:
                if len(match) >= 1:
                    selector = match[0]
                    value = match[1] if len(match) > 1 else ''
                    dom_manipulations.append({
                        'selector': selector,
                        'type': manipulation_type,
                        'value': value
                    })
        
        return dom_manipulations
    
    def _parse_ajax_options(self, options_str: str) -> Dict[str, Any]:
        """Parse jQuery AJAX options string."""
        options = {}
        
        # Extract common AJAX options
        patterns = {
            'url': r'url\s*:\s*[\'"]([^\'"]*)[\'"]',
            'method': r'type\s*:\s*[\'"]([^\'"]*)[\'"]',
            'data': r'data\s*:\s*\{([^}]*)\}',
            'success': r'success\s*:\s*function\s*\([^)]*\)\s*\{([^}]*)\}',
            'error': r'error\s*:\s*function\s*\([^)]*\)\s*\{([^}]*)\}'
        }
        
        for key, pattern in patterns.items():
            match = re.search(pattern, options_str, re.IGNORECASE | re.DOTALL)
            if match:
                options[key] = match.group(1)
        
        return options
    
    def _generate_component_name(self, js_code: str) -> str:
        """Generate a component name based on the jQuery code."""
        # Try to extract meaningful name from selectors or comments
        selectors = re.findall(r'\$\([\'"]([^\'"]*)[\'"]\)', js_code)
        if selectors:
            # Use the first selector to generate a name
            selector = selectors[0]
            if selector.startswith('#'):
                return f"{selector[1:].capitalize()}Component"
            elif selector.startswith('.'):
                return f"{selector[1:].capitalize()}Component"
            else:
                return f"{selector.capitalize()}Component"
        
        # Fallback to generic name
        return "JQueryComponent"
    
    def _generate_react_component(self, component_name: str, event_handlers: List[Dict], ajax_calls: List[Dict], dom_manipulations: List[Dict]) -> str:
        """Generate React component from jQuery code."""
        imports = []
        hooks = []
        handlers = []
        effects = []
        
        # Generate imports
        if ajax_calls:
            imports.append("import { useState, useEffect } from 'react';")
        elif event_handlers:
            imports.append("import { useState } from 'react';")
        else:
            imports.append("import React from 'react';")
        
        # Generate hooks for AJAX calls
        for i, ajax_call in enumerate(ajax_calls):
            if ajax_call['type'] == 'ajax':
                url = ajax_call.get('options', {}).get('url', '')
                method = ajax_call.get('options', {}).get('method', 'GET')
                hooks.append(f"const [data{i}, setData{i}] = useState(null);")
                effects.append(f"""
  useEffect(() => {{
    const fetchData = async () => {{
      try {{
        const response = await fetch('{url}', {{
          method: '{method}',
          headers: {{
            'Content-Type': 'application/json',
          }},
        }});
        const result = await response.json();
        setData{i}(result);
      }} catch (error) {{
        console.error('Error fetching data:', error);
      }}
    }};
    
    fetchData();
  }}, []);""")
        
        # Generate event handlers
        for i, handler in enumerate(event_handlers):
            handler_name = f"handle{handler['event_type'].capitalize()}{i}"
            handlers.append(f"""
  const {handler_name} = () => {{
    // Converted from jQuery: {handler['handler_code']}
    console.log('Handler for {handler['selector']}');
  }};""")
        
        # Generate component JSX
        jsx_content = self._generate_jsx_content(event_handlers, dom_manipulations)
        
        # Combine all parts
        component_code = f"""
{chr(10).join(imports)}

const {component_name} = () => {{
{chr(10).join(hooks)}
{chr(10).join(handlers)}
{chr(10).join(effects)}

  return (
    <div>
      {jsx_content}
    </div>
  );
}};

export default {component_name};
"""
        
        return component_code
    
    def _generate_jsx_content(self, event_handlers: List[Dict], dom_manipulations: List[Dict]) -> str:
        """Generate JSX content based on jQuery selectors and manipulations."""
        jsx_elements = []
        
        # Generate elements based on selectors
        for i, handler in enumerate(event_handlers):
            selector = handler['selector']
            event_type = handler['event_type']
            handler_name = f"handle{event_type.capitalize()}{i}"
            
            if selector.startswith('#'):
                element_id = selector[1:]
                jsx_elements.append(f'<div id="{element_id}" onClick={{{handler_name}}}>Content</div>')
            elif selector.startswith('.'):
                element_class = selector[1:]
                jsx_elements.append(f'<div className="{element_class}" onClick={{{handler_name}}}>Content</div>')
            else:
                jsx_elements.append(f'<div onClick={{{handler_name}}}>Content</div>')
        
        # Generate elements based on DOM manipulations
        for manipulation in dom_manipulations:
            selector = manipulation['selector']
            manipulation_type = manipulation['type']
            
            if selector.startswith('#'):
                element_id = selector[1:]
                jsx_elements.append(f'<div id="{element_id}">Content</div>')
            elif selector.startswith('.'):
                element_class = selector[1:]
                jsx_elements.append(f'<div className="{element_class}">Content</div>')
            else:
                jsx_elements.append(f'<div>Content</div>')
        
        if not jsx_elements:
            jsx_elements.append('<div>Converted from jQuery</div>')
        
        return chr(10).join(jsx_elements) 