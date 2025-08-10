"""
Website Modernization Functionality Mapper

This module provides specialized functionality mapping for website modernization,
including component mapping, API endpoints, and UI interactions between
legacy websites and modern frameworks.
"""

import logging
from typing import Dict, Any, List, Optional, Tuple, Union
from dataclasses import dataclass, field
from enum import Enum
import re
from datetime import datetime

from engine.functionality_mapper import (
    FunctionalityMapper, FunctionalityMapping, FunctionalityType,
    InputOutputMapping, BusinessLogicMapping, EquivalenceLevel
)


class WebsiteFramework(Enum):
    """Supported website frameworks."""
    REACT = "react"
    NEXTJS = "nextjs"
    ASTRO = "astro"
    VUE = "vue"
    ANGULAR = "angular"


class UIComponentType(Enum):
    """Types of UI components."""
    NAVIGATION = "navigation"
    FORM = "form"
    TABLE = "table"
    MODAL = "modal"
    CAROUSEL = "carousel"
    BUTTON = "button"
    INPUT = "input"
    SELECT = "select"
    CHECKBOX = "checkbox"
    RADIO = "radio"
    CARD = "card"
    LIST = "list"
    HEADER = "header"
    FOOTER = "footer"
    SIDEBAR = "sidebar"
    CONTENT = "content"


@dataclass
class UIComponentMapping:
    """Mapping of a UI component between legacy and modern frameworks."""
    component_id: str
    component_type: UIComponentType
    source_name: str
    target_name: str
    legacy_selector: str = ""  # CSS selector or jQuery selector
    modern_component: str = ""  # React/Astro component name
    props_mapping: Dict[str, str] = field(default_factory=dict)
    event_handlers: Dict[str, str] = field(default_factory=dict)
    styling_mapping: Dict[str, str] = field(default_factory=dict)
    accessibility_features: List[str] = field(default_factory=list)
    responsive_behavior: Dict[str, str] = field(default_factory=dict)


@dataclass
class APIMapping:
    """Mapping of API endpoints between legacy and modern systems."""
    endpoint_path: str
    http_method: str
    source_name: str
    target_name: str
    legacy_endpoint: str = ""
    modern_endpoint: str = ""
    request_mapping: Dict[str, str] = field(default_factory=dict)
    response_mapping: Dict[str, str] = field(default_factory=dict)
    authentication: Dict[str, str] = field(default_factory=dict)
    error_handling: Dict[str, str] = field(default_factory=dict)


@dataclass
class WebsiteModernizationMapping:
    """Complete mapping for website modernization."""
    legacy_url: str
    modern_url: str
    target_framework: WebsiteFramework
    component_mappings: List[UIComponentMapping] = field(default_factory=list)
    api_mappings: List[APIMapping] = field(default_factory=list)
    routing_mapping: Dict[str, str] = field(default_factory=dict)
    state_management: Dict[str, str] = field(default_factory=dict)
    styling_migration: Dict[str, str] = field(default_factory=dict)
    seo_improvements: List[str] = field(default_factory=list)
    performance_optimizations: List[str] = field(default_factory=list)


class WebsiteFunctionalityMapper(FunctionalityMapper):
    """
    Specialized functionality mapper for website modernization.
    
    This class extends the base FunctionalityMapper with website-specific
    features like component mapping, API endpoints, and UI interactions.
    """
    
    def __init__(self):
        super().__init__()
        self.logger = logging.getLogger(__name__)
        self.website_mappings: Dict[str, WebsiteModernizationMapping] = {}
        
    def create_website_mapping(
        self,
        legacy_url: str,
        target_framework: WebsiteFramework,
        modern_url: str,
        legacy_html: Optional[str] = None,
        modern_code: Optional[str] = None
    ) -> FunctionalityMapping:
        """
        Create a mapping for website modernization.
        
        Args:
            legacy_url: URL of the legacy website
            target_framework: Target framework for modernization
            modern_url: URL of the modernized website
            legacy_html: Legacy HTML code (optional)
            modern_code: Modern code (optional)
            
        Returns:
            FunctionalityMapping object
        """
        # Create base functionality mapping
        functionality_mapping = self.create_functionality_mapping(
            functionality_type=FunctionalityType.COMPONENT,
            source_name=legacy_url,
            target_name=modern_url,
            source_language="html",
            target_language=target_framework.value,
            source_code=legacy_html,
            target_code=modern_code,
            custom_id=f"COMP-{self.generate_component_id(legacy_url)}"
        )
        
        # Create website-specific mapping
        website_mapping = WebsiteModernizationMapping(
            legacy_url=legacy_url,
            modern_url=modern_url,
            target_framework=target_framework
        )
        
        # Store website mapping
        self.website_mappings[functionality_mapping.functionality_id] = website_mapping
        
        return functionality_mapping
    
    def map_ui_components(
        self,
        functionality_id: str,
        component_mappings: List[Union[UIComponentMapping, Dict[str, Any]]]
    ) -> List[UIComponentMapping]:
        """
        Map UI components between legacy and modern frameworks.
        
        Args:
            functionality_id: ID of the functionality mapping
            component_mappings: List of UI component mappings or dictionaries
            
        Returns:
            List of UIComponentMapping objects
        """
        if functionality_id not in self.website_mappings:
            raise ValueError(f"Website mapping {functionality_id} not found")
        
        # Convert dictionaries to UIComponentMapping objects if needed
        converted_mappings = []
        for component in component_mappings:
            if isinstance(component, dict):
                # Create UIComponentMapping from dictionary
                component_obj = UIComponentMapping(
                    component_id=component.get("legacy_selector", ""),
                    component_type=UIComponentType(component.get("component_type", "content")),
                    source_name=component.get("legacy_selector", ""),
                    target_name=component.get("modern_component", ""),
                    legacy_selector=component.get("legacy_selector", ""),
                    modern_component=component.get("modern_component", ""),
                    props_mapping=component.get("props_mapping", {}),
                    event_handlers=component.get("event_handlers", {})
                )
                converted_mappings.append(component_obj)
            else:
                converted_mappings.append(component)
        
        website_mapping = self.website_mappings[functionality_id]
        website_mapping.component_mappings = converted_mappings
        
        # Update base mapping
        mapping = self.mappings[functionality_id]
        mapping.updated_at = datetime.now()
        
        self.logger.info(f"Mapped {len(converted_mappings)} UI components for {functionality_id}")
        return converted_mappings
    
    def map_api_endpoints(
        self,
        functionality_id: str,
        api_mappings: List[Union[APIMapping, Dict[str, Any]]]
    ) -> List[APIMapping]:
        """
        Map API endpoints between legacy and modern systems.
        
        Args:
            functionality_id: ID of the functionality mapping
            api_mappings: List of API mappings or dictionaries
            
        Returns:
            List of APIMapping objects
        """
        if functionality_id not in self.website_mappings:
            raise ValueError(f"Website mapping {functionality_id} not found")
        
        # Convert dictionaries to APIMapping objects if needed
        converted_mappings = []
        for api in api_mappings:
            if isinstance(api, dict):
                # Create APIMapping from dictionary
                api_obj = APIMapping(
                    endpoint_path=api.get("legacy_endpoint", ""),
                    http_method=api.get("http_method", "GET"),
                    source_name=api.get("legacy_endpoint", ""),
                    target_name=api.get("modern_endpoint", ""),
                    legacy_endpoint=api.get("legacy_endpoint", ""),
                    modern_endpoint=api.get("modern_endpoint", ""),
                    request_mapping=api.get("request_mapping", {}),
                    response_mapping=api.get("response_mapping", {}),
                    authentication=api.get("authentication", {})
                )
                converted_mappings.append(api_obj)
            else:
                converted_mappings.append(api)
        
        website_mapping = self.website_mappings[functionality_id]
        website_mapping.api_mappings = converted_mappings
        
        # Update base mapping
        mapping = self.mappings[functionality_id]
        mapping.updated_at = datetime.now()
        
        self.logger.info(f"Mapped {len(converted_mappings)} API endpoints for {functionality_id}")
        return converted_mappings
    
    def map_routing(
        self,
        functionality_id: str,
        routing_mapping: Dict[str, str]
    ) -> Dict[str, str]:
        """
        Map routing between legacy and modern systems.
        
        Args:
            functionality_id: ID of the functionality mapping
            routing_mapping: Dictionary mapping legacy routes to modern routes
            
        Returns:
            Updated routing mapping
        """
        if functionality_id not in self.website_mappings:
            raise ValueError(f"Website mapping {functionality_id} not found")
        
        website_mapping = self.website_mappings[functionality_id]
        website_mapping.routing_mapping = routing_mapping
        
        # Update base mapping
        mapping = self.mappings[functionality_id]
        mapping.updated_at = datetime.now()
        
        self.logger.info(f"Mapped {len(routing_mapping)} routes for {functionality_id}")
        return routing_mapping
    
    def map_state_management(
        self,
        functionality_id: str,
        state_mapping: Dict[str, str]
    ) -> Dict[str, str]:
        """
        Map state management between legacy and modern systems.
        
        Args:
            functionality_id: ID of the functionality mapping
            state_mapping: Dictionary mapping legacy state to modern state
            
        Returns:
            Updated state mapping
        """
        if functionality_id not in self.website_mappings:
            raise ValueError(f"Website mapping {functionality_id} not found")
        
        website_mapping = self.website_mappings[functionality_id]
        website_mapping.state_management = state_mapping
        
        # Update base mapping
        mapping = self.mappings[functionality_id]
        mapping.updated_at = datetime.now()
        
        self.logger.info(f"Mapped {len(state_mapping)} state variables for {functionality_id}")
        return state_mapping
    
    def analyze_legacy_website(self, html_content: str) -> Dict[str, Any]:
        """
        Analyze legacy website structure.
        
        Args:
            functionality_id: ID of the functionality mapping
            html_content: HTML content of the legacy website
            
        Returns:
            Analysis results
        """
        analysis = {
            "framework_dependencies": [],
            "components": [],
            "javascript_functions": [],
            "forms": [],
            "tables": [],
            "navigation": [],
            "images": [],
            "links": [],
            "styles": [],
            "scripts": []
        }
        
        # Detect framework dependencies
        if "bootstrap" in html_content.lower():
            analysis["framework_dependencies"].append("bootstrap")
        if "jquery" in html_content.lower():
            analysis["framework_dependencies"].append("jquery")
        if "angular" in html_content.lower():
            analysis["framework_dependencies"].append("angular")
        if "react" in html_content.lower():
            analysis["framework_dependencies"].append("react")
        
        # Extract UI components
        components = self._extract_components(html_content)
        analysis["components"] = components
        
        # Extract forms
        forms = self._extract_forms(html_content)
        analysis["forms"] = forms
        
        # Extract tables
        tables = self._extract_tables(html_content)
        analysis["tables"] = tables
        
        # Extract navigation
        navigation = self._extract_navigation(html_content)
        analysis["navigation"] = navigation
        
        # Extract JavaScript functions
        scripts = self._extract_scripts(html_content)
        analysis["javascript_functions"] = scripts
        analysis["scripts"] = scripts
        
        # Extract styles
        styles = self._extract_styles(html_content)
        analysis["styles"] = styles
        
        # Extract images
        images = self._extract_images(html_content)
        analysis["images"] = images
        
        # Extract links
        links = self._extract_links(html_content)
        analysis["links"] = links
        
        return analysis
    
    def generate_modernization_plan(self, functionality_id: str) -> Dict[str, Any]:
        """
        Generate modernization plan for website.
        
        Args:
            functionality_id: ID of the functionality mapping
            
        Returns:
            Modernization plan
        """
        if functionality_id not in self.website_mappings:
            raise ValueError(f"Website mapping {functionality_id} not found")
        
        website_mapping = self.website_mappings[functionality_id]
        
        plan = {
            "target_framework": website_mapping.target_framework.value,
            "components_to_create": len(website_mapping.component_mappings),
            "api_endpoints_to_migrate": len(website_mapping.api_mappings),
            "routes_to_map": len(website_mapping.routing_mapping),
            "estimated_effort": "medium",
            "recommendations": [],
            "migration_steps": []
        }
        
        # Generate migration steps
        for component in website_mapping.component_mappings:
            step = self._generate_component_step(component, website_mapping.target_framework)
            plan["migration_steps"].append(step)
        
        for api in website_mapping.api_mappings:
            step = self._generate_api_step(api, website_mapping.target_framework)
            plan["migration_steps"].append(step)
        
        # Generate recommendations
        recommendations = self._generate_recommendations(website_mapping)
        plan["recommendations"] = recommendations
        
        return plan
    
    def generate_component_id(self, url: str) -> str:
        """
        Generate component ID from URL.
        
        Args:
            url: URL or component name
            
        Returns:
            Component ID
        """
        # Remove file extension and convert to snake_case
        component_id = url.replace('.html', '').replace('.htm', '')
        component_id = re.sub(r'[^a-zA-Z0-9]', '_', component_id)
        component_id = re.sub(r'_+', '_', component_id)
        component_id = component_id.strip('_').lower()
        
        return component_id
    
    def _create_ui_component_mapping(self, component_def: Dict[str, Any]) -> UIComponentMapping:
        """Create a UI component mapping from component definition."""
        # Generate component_id if not provided
        component_id = component_def.get("component_id", "")
        if not component_id:
            component_id = f"component_{len(self.mappings)}"
        
        # Generate source_name and target_name if not provided
        source_name = component_def.get("source_name", component_def.get("legacy_selector", ""))
        target_name = component_def.get("target_name", component_def.get("modern_component", ""))
        
        return UIComponentMapping(
            component_id=component_id,
            component_type=UIComponentType(component_def.get("component_type", "content")),
            source_name=source_name,
            target_name=target_name,
            legacy_selector=component_def.get("legacy_selector", ""),
            modern_component=component_def.get("modern_component", ""),
            props_mapping=component_def.get("props_mapping", {}),
            event_handlers=component_def.get("event_handlers", {})
        )
    
    def _create_api_mapping(self, api_def: Dict[str, Any]) -> APIMapping:
        """Create an API mapping from API definition."""
        # Generate endpoint_path if not provided
        endpoint_path = api_def.get("endpoint_path", "")
        if not endpoint_path:
            endpoint_path = api_def.get("legacy_endpoint", "")
        
        # Generate source_name and target_name if not provided
        source_name = api_def.get("source_name", api_def.get("legacy_endpoint", ""))
        target_name = api_def.get("target_name", api_def.get("modern_endpoint", ""))
        
        return APIMapping(
            endpoint_path=endpoint_path,
            http_method=api_def.get("http_method", "GET"),
            source_name=source_name,
            target_name=target_name,
            legacy_endpoint=api_def.get("legacy_endpoint", ""),
            modern_endpoint=api_def.get("modern_endpoint", ""),
            request_mapping=api_def.get("request_mapping", {}),
            response_mapping=api_def.get("response_mapping", {})
        )
    
    def _generate_component_id(self, url: str) -> str:
        """Generate component ID from URL (legacy method)."""
        # Extract domain from URL and convert to uppercase with underscores
        if url.startswith('http'):
            # Remove protocol and www
            domain = url.replace('https://', '').replace('http://', '').replace('www.', '')
            # Split by dots and take first part
            parts = domain.split('.')
            if len(parts) >= 2:
                # Convert to uppercase and replace dots with underscores
                component_id = f"{parts[0].upper()}_{parts[1].upper()}_HOME"
                return component_id
        
        # Fallback to the public method
        return self.generate_component_id(url)
    
    def _extract_components(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract UI components from HTML content."""
        components = []
        
        # Extract divs with classes
        div_pattern = r'<div[^>]*class=["\']([^"\']*)["\'][^>]*>'
        div_matches = re.findall(div_pattern, html_content, re.IGNORECASE)
        
        for classes in div_matches:
            class_list = classes.split()
            for class_name in class_list:
                if class_name in ['container', 'header', 'footer', 'sidebar', 'main', 'content']:
                    components.append({
                        "type": class_name,
                        "selector": f".{class_name}",
                        "classes": classes
                    })
        
        # Extract navigation elements
        if '<nav' in html_content.lower():
            components.append({
                "type": "navigation",
                "selector": "nav",
                "classes": "navigation"
            })
        
        # Extract forms
        if '<form' in html_content.lower():
            components.append({
                "type": "form",
                "selector": "form",
                "classes": "form"
            })
        
        # Extract card-like elements (divs with card-related classes)
        card_pattern = r'<div[^>]*class=["\']([^"\']*card[^"\']*)["\'][^>]*>'
        card_matches = re.findall(card_pattern, html_content, re.IGNORECASE)
        if card_matches:
            components.append({
                "type": "card",
                "selector": ".card",
                "classes": "card"
            })
        
        return components
    
    def _extract_forms(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract forms from HTML content."""
        forms = []
        
        # Extract form elements
        form_pattern = r'<form[^>]*>(.*?)</form>'
        form_matches = re.findall(form_pattern, html_content, re.IGNORECASE | re.DOTALL)
        
        for i, form_content in enumerate(form_matches):
            # Extract input fields from form content
            input_pattern = r'<input[^>]*>'
            inputs = re.findall(input_pattern, form_content, re.IGNORECASE)
            
            forms.append({
                "id": f"form_{i}",
                "content": form_content[:200] + "..." if len(form_content) > 200 else form_content,
                "fields": inputs
            })
        
        return forms
    
    def _extract_tables(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract tables from HTML content."""
        tables = []
        
        # Extract table elements
        table_pattern = r'<table[^>]*>(.*?)</table>'
        table_matches = re.findall(table_pattern, html_content, re.IGNORECASE | re.DOTALL)
        
        for i, table_content in enumerate(table_matches):
            tables.append({
                "id": f"table_{i}",
                "content": table_content[:200] + "..." if len(table_content) > 200 else table_content
            })
        
        return tables
    
    def _extract_navigation(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract navigation elements from HTML content."""
        navigation = []
        
        # Extract nav elements
        nav_pattern = r'<nav[^>]*>(.*?)</nav>'
        nav_matches = re.findall(nav_pattern, html_content, re.IGNORECASE | re.DOTALL)
        
        for i, nav_content in enumerate(nav_matches):
            # Extract links from navigation content
            link_pattern = r'<a[^>]*href=["\']([^"\']*)["\'][^>]*>(.*?)</a>'
            links = re.findall(link_pattern, nav_content, re.IGNORECASE | re.DOTALL)
            
            navigation.append({
                "id": f"nav_{i}",
                "content": nav_content[:200] + "..." if len(nav_content) > 200 else nav_content,
                "links": links
            })
        
        return navigation
    
    def _extract_scripts(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract JavaScript functions from HTML content."""
        scripts = []
        
        # Extract script tags
        script_pattern = r'<script[^>]*>(.*?)</script>'
        script_matches = re.findall(script_pattern, html_content, re.IGNORECASE | re.DOTALL)
        
        for i, script_content in enumerate(script_matches):
            # Extract script type attribute
            type_pattern = r'<script[^>]*type=["\']([^"\']*)["\'][^>]*>'
            type_match = re.search(type_pattern, script_content, re.IGNORECASE)
            script_type = type_match.group(1) if type_match else "text/javascript"
            
            scripts.append({
                "id": f"script_{i}",
                "content": script_content[:200] + "..." if len(script_content) > 200 else script_content,
                "type": script_type
            })
        
        return scripts
    
    def _extract_styles(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract styles from HTML content."""
        styles = []
        
        # Extract style tags
        style_pattern = r'<style[^>]*>(.*?)</style>'
        style_matches = re.findall(style_pattern, html_content, re.IGNORECASE | re.DOTALL)
        
        for i, style_content in enumerate(style_matches):
            styles.append({
                "id": f"style_{i}",
                "content": style_content[:200] + "..." if len(style_content) > 200 else style_content
            })
        
        return styles
    
    def _extract_images(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract images from HTML content."""
        images = []
        
        # Extract img tags
        img_pattern = r'<img[^>]*src=["\']([^"\']*)["\'][^>]*>'
        img_matches = re.findall(img_pattern, html_content, re.IGNORECASE)
        
        for i, src in enumerate(img_matches):
            images.append({
                "id": f"image_{i}",
                "src": src
            })
        
        return images
    
    def _extract_links(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract links from HTML content."""
        links = []
        
        # Extract a tags
        link_pattern = r'<a[^>]*href=["\']([^"\']*)["\'][^>]*>(.*?)</a>'
        link_matches = re.findall(link_pattern, html_content, re.IGNORECASE | re.DOTALL)
        
        for i, (href, text) in enumerate(link_matches):
            links.append({
                "id": f"link_{i}",
                "href": href,
                "text": text.strip()
            })
        
        return links
    
    def _generate_selector(self, content: str, component_type: str) -> str:
        """Generate CSS selector for component."""
        if component_type == "navigation":
            return "navigation"
        elif component_type == "form":
            return "form"
        elif component_type == "table":
            return "table"
        else:
            return f".{component_type}"
    
    def _generate_component_step(self, component: Union[UIComponentMapping, Dict[str, Any]], framework: WebsiteFramework) -> Dict[str, Any]:
        """Generate migration step for component."""
        if isinstance(component, dict):
            return {
                "step_type": "component_migration",
                "component_id": component.get("legacy_selector", ""),
                "source_component": component.get("legacy_selector", ""),
                "target_component": component.get("modern_component", ""),
                "framework": framework.value,
                "description": f"Migrate {component.get('legacy_selector', '')} to {component.get('modern_component', '')}"
            }
        else:
            return {
                "step_type": "component_migration",
                "component_id": component.component_id,
                "source_component": component.source_name,
                "target_component": component.target_name,
                "framework": framework.value,
                "description": f"Migrate {component.source_name} to {component.target_name}"
            }
    
    def _generate_api_step(self, api: APIMapping, framework: WebsiteFramework) -> Dict[str, Any]:
        """Generate migration step for API."""
        return {
            "step_type": "api_migration",
            "api_id": api.endpoint_path,
            "source_endpoint": api.source_name,
            "target_endpoint": api.target_name,
            "framework": framework.value,
            "description": f"Migrate API {api.source_name} to {api.target_name}"
        }
    
    def _generate_recommendations(self, website_mapping: WebsiteModernizationMapping) -> List[str]:
        """Generate recommendations for modernization."""
        recommendations = []
        
        if website_mapping.target_framework == WebsiteFramework.REACT:
            recommendations.append("Use React hooks for state management")
            recommendations.append("Implement component-based architecture")
            recommendations.append("Use TypeScript for better type safety")
        
        elif website_mapping.target_framework == WebsiteFramework.NEXTJS:
            recommendations.append("Use Next.js for server-side rendering")
            recommendations.append("Implement API routes for backend functionality")
            recommendations.append("Use Next.js Image component for optimization")
        
        elif website_mapping.target_framework == WebsiteFramework.ASTRO:
            recommendations.append("Use Astro for static site generation")
            recommendations.append("Implement component islands for interactivity")
            recommendations.append("Use Astro's built-in optimization features")
        
        return recommendations 