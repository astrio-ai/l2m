"""
Website Modernization Functionality Mapper

This module provides specialized functionality mapping for website modernization,
including component mapping, API endpoints, and UI interactions between
legacy websites and modern frameworks.
"""

import logging
from typing import Dict, Any, List, Optional, Tuple
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
    legacy_selector: str  # CSS selector or jQuery selector
    modern_component: str  # React/Astro component name
    component_type: UIComponentType
    props_mapping: Dict[str, str] = field(default_factory=dict)
    event_handlers: Dict[str, str] = field(default_factory=dict)
    styling_mapping: Dict[str, str] = field(default_factory=dict)
    accessibility_features: List[str] = field(default_factory=list)
    responsive_behavior: Dict[str, str] = field(default_factory=dict)


@dataclass
class APIMapping:
    """Mapping of API endpoints between legacy and modern systems."""
    legacy_endpoint: str
    modern_endpoint: str
    http_method: str
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
        modern_url: str,
        target_framework: WebsiteFramework,
        legacy_html: Optional[str] = None,
        modern_code: Optional[str] = None
    ) -> Tuple[FunctionalityMapping, WebsiteModernizationMapping]:
        """
        Create a mapping for website modernization.
        
        Args:
            legacy_url: URL of the legacy website
            modern_url: URL of the modernized website
            target_framework: Target framework for modernization
            legacy_html: Legacy HTML code (optional)
            modern_code: Modern code (optional)
            
        Returns:
            Tuple of (FunctionalityMapping, WebsiteModernizationMapping)
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
            custom_id=f"COMP-{self._generate_component_id(legacy_url)}"
        )
        
        # Create website-specific mapping
        website_mapping = WebsiteModernizationMapping(
            legacy_url=legacy_url,
            modern_url=modern_url,
            target_framework=target_framework
        )
        
        # Store website mapping
        self.website_mappings[functionality_mapping.functionality_id] = website_mapping
        
        return functionality_mapping, website_mapping
    
    def map_ui_components(
        self,
        functionality_id: str,
        component_mappings: List[Dict[str, Any]]
    ) -> List[UIComponentMapping]:
        """
        Map UI components between legacy and modern frameworks.
        
        Args:
            functionality_id: ID of the functionality mapping
            component_mappings: List of component mapping definitions
            
        Returns:
            List of UIComponentMapping objects
        """
        if functionality_id not in self.website_mappings:
            raise ValueError(f"Website mapping {functionality_id} not found")
        
        website_mapping = self.website_mappings[functionality_id]
        ui_mappings = []
        
        for component_def in component_mappings:
            ui_mapping = self._create_ui_component_mapping(component_def)
            ui_mappings.append(ui_mapping)
        
        # Update website mapping
        website_mapping.component_mappings = ui_mappings
        
        # Update base mapping
        mapping = self.mappings[functionality_id]
        mapping.updated_at = datetime.now()
        
        self.logger.info(f"Mapped {len(ui_mappings)} UI components for {functionality_id}")
        return ui_mappings
    
    def map_api_endpoints(
        self,
        functionality_id: str,
        api_mappings: List[Dict[str, Any]]
    ) -> List[APIMapping]:
        """
        Map API endpoints between legacy and modern systems.
        
        Args:
            functionality_id: ID of the functionality mapping
            api_mappings: List of API mapping definitions
            
        Returns:
            List of APIMapping objects
        """
        if functionality_id not in self.website_mappings:
            raise ValueError(f"Website mapping {functionality_id} not found")
        
        website_mapping = self.website_mappings[functionality_id]
        api_mappings_list = []
        
        for api_def in api_mappings:
            api_mapping = self._create_api_mapping(api_def)
            api_mappings_list.append(api_mapping)
        
        # Update website mapping
        website_mapping.api_mappings = api_mappings_list
        
        # Update base mapping
        mapping = self.mappings[functionality_id]
        mapping.updated_at = datetime.now()
        
        self.logger.info(f"Mapped {len(api_mappings_list)} API endpoints for {functionality_id}")
        return api_mappings_list
    
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
        
        self.logger.info(f"Mapped state management for {functionality_id}")
        return state_mapping
    
    def analyze_legacy_website(
        self,
        functionality_id: str,
        html_content: str
    ) -> Dict[str, Any]:
        """
        Analyze legacy website structure and extract mapping information.
        
        Args:
            functionality_id: ID of the functionality mapping
            html_content: HTML content of the legacy website
            
        Returns:
            Analysis result with extracted structure information
        """
        if functionality_id not in self.website_mappings:
            raise ValueError(f"Website mapping {functionality_id} not found")
        
        analysis_result = {
            "components": [],
            "forms": [],
            "tables": [],
            "navigation": [],
            "scripts": [],
            "styles": [],
            "images": [],
            "links": []
        }
        
        # Extract components
        components = self._extract_components(html_content)
        analysis_result["components"] = components
        
        # Extract forms
        forms = self._extract_forms(html_content)
        analysis_result["forms"] = forms
        
        # Extract tables
        tables = self._extract_tables(html_content)
        analysis_result["tables"] = tables
        
        # Extract navigation
        navigation = self._extract_navigation(html_content)
        analysis_result["navigation"] = navigation
        
        # Extract scripts
        scripts = self._extract_scripts(html_content)
        analysis_result["scripts"] = scripts
        
        # Extract styles
        styles = self._extract_styles(html_content)
        analysis_result["styles"] = styles
        
        # Extract images
        images = self._extract_images(html_content)
        analysis_result["images"] = images
        
        # Extract links
        links = self._extract_links(html_content)
        analysis_result["links"] = links
        
        return analysis_result
    
    def generate_modernization_plan(
        self,
        functionality_id: str
    ) -> Dict[str, Any]:
        """
        Generate a modernization plan based on the mapping.
        
        Args:
            functionality_id: ID of the functionality mapping
            
        Returns:
            Modernization plan with steps and recommendations
        """
        if functionality_id not in self.website_mappings:
            raise ValueError(f"Website mapping {functionality_id} not found")
        
        website_mapping = self.website_mappings[functionality_id]
        mapping = self.mappings[functionality_id]
        
        plan = {
            "target_framework": website_mapping.target_framework.value,
            "components_to_create": len(website_mapping.component_mappings),
            "api_endpoints_to_migrate": len(website_mapping.api_mappings),
            "routes_to_map": len(website_mapping.routing_mapping),
            "steps": [],
            "recommendations": [],
            "estimated_effort": "medium"
        }
        
        # Generate steps based on component mappings
        for component in website_mapping.component_mappings:
            step = self._generate_component_step(component, website_mapping.target_framework)
            plan["steps"].append(step)
        
        # Generate steps based on API mappings
        for api in website_mapping.api_mappings:
            step = self._generate_api_step(api, website_mapping.target_framework)
            plan["steps"].append(step)
        
        # Generate recommendations
        recommendations = self._generate_recommendations(website_mapping)
        plan["recommendations"] = recommendations
        
        return plan
    
    def _create_ui_component_mapping(self, component_def: Dict[str, Any]) -> UIComponentMapping:
        """Create a UI component mapping from component definition."""
        return UIComponentMapping(
            legacy_selector=component_def.get("legacy_selector", ""),
            modern_component=component_def.get("modern_component", ""),
            component_type=UIComponentType(component_def.get("component_type", "content")),
            props_mapping=component_def.get("props_mapping", {}),
            event_handlers=component_def.get("event_handlers", {}),
            styling_mapping=component_def.get("styling_mapping", {}),
            accessibility_features=component_def.get("accessibility_features", []),
            responsive_behavior=component_def.get("responsive_behavior", {})
        )
    
    def _create_api_mapping(self, api_def: Dict[str, Any]) -> APIMapping:
        """Create an API mapping from API definition."""
        return APIMapping(
            legacy_endpoint=api_def.get("legacy_endpoint", ""),
            modern_endpoint=api_def.get("modern_endpoint", ""),
            http_method=api_def.get("http_method", "GET"),
            request_mapping=api_def.get("request_mapping", {}),
            response_mapping=api_def.get("response_mapping", {}),
            authentication=api_def.get("authentication", {}),
            error_handling=api_def.get("error_handling", {})
        )
    
    def _generate_component_id(self, url: str) -> str:
        """Generate a component ID from URL."""
        # Extract domain and path
        url_parts = url.replace("http://", "").replace("https://", "").split("/")
        domain = url_parts[0].replace(".", "_")
        path = "_".join(url_parts[1:]) if len(url_parts) > 1 else "home"
        
        return f"{domain}_{path}".upper()
    
    def _extract_components(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract UI components from HTML content."""
        components = []
        
        # Find common UI patterns
        patterns = [
            (r'<nav[^>]*>.*?</nav>', "navigation"),
            (r'<form[^>]*>.*?</form>', "form"),
            (r'<table[^>]*>.*?</table>', "table"),
            (r'<div[^>]*class="[^"]*modal[^"]*"[^>]*>.*?</div>', "modal"),
            (r'<div[^>]*class="[^"]*carousel[^"]*"[^>]*>.*?</div>', "carousel"),
            (r'<button[^>]*>.*?</button>', "button"),
            (r'<input[^>]*>', "input"),
            (r'<select[^>]*>.*?</select>', "select"),
            (r'<input[^>]*type="checkbox"[^>]*>', "checkbox"),
            (r'<input[^>]*type="radio"[^>]*>', "radio"),
            (r'<div[^>]*class="[^"]*card[^"]*"[^>]*>.*?</div>', "card"),
            (r'<ul[^>]*>.*?</ul>|<ol[^>]*>.*?</ol>', "list"),
            (r'<header[^>]*>.*?</header>', "header"),
            (r'<footer[^>]*>.*?</footer>', "footer"),
            (r'<aside[^>]*>.*?</aside>', "sidebar")
        ]
        
        for pattern, component_type in patterns:
            matches = re.findall(pattern, html_content, re.IGNORECASE | re.DOTALL)
            for match in matches:
                components.append({
                    "type": component_type,
                    "content": match,
                    "selector": self._generate_selector(match, component_type)
                })
        
        return components
    
    def _extract_forms(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract forms from HTML content."""
        forms = []
        
        form_pattern = r'<form[^>]*>(.*?)</form>'
        form_matches = re.findall(form_pattern, html_content, re.IGNORECASE | re.DOTALL)
        
        for form_content in form_matches:
            # Extract form fields
            fields = []
            field_patterns = [
                (r'<input[^>]*>', "input"),
                (r'<select[^>]*>.*?</select>', "select"),
                (r'<textarea[^>]*>.*?</textarea>', "textarea")
            ]
            
            for pattern, field_type in field_patterns:
                field_matches = re.findall(pattern, form_content, re.IGNORECASE | re.DOTALL)
                for field in field_matches:
                    fields.append({
                        "type": field_type,
                        "content": field
                    })
            
            forms.append({
                "fields": fields,
                "action": re.search(r'action="([^"]*)"', form_content),
                "method": re.search(r'method="([^"]*)"', form_content)
            })
        
        return forms
    
    def _extract_tables(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract tables from HTML content."""
        tables = []
        
        table_pattern = r'<table[^>]*>(.*?)</table>'
        table_matches = re.findall(table_pattern, html_content, re.IGNORECASE | re.DOTALL)
        
        for table_content in table_matches:
            # Extract rows
            rows = []
            row_pattern = r'<tr[^>]*>(.*?)</tr>'
            row_matches = re.findall(row_pattern, table_content, re.IGNORECASE | re.DOTALL)
            
            for row_content in row_matches:
                # Extract cells
                cells = []
                cell_pattern = r'<(td|th)[^>]*>(.*?)</(td|th)>'
                cell_matches = re.findall(cell_pattern, row_content, re.IGNORECASE | re.DOTALL)
                
                for cell_match in cell_matches:
                    cells.append({
                        "type": cell_match[0],
                        "content": cell_match[1]
                    })
                
                rows.append({"cells": cells})
            
            tables.append({"rows": rows})
        
        return tables
    
    def _extract_navigation(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract navigation from HTML content."""
        navigation = []
        
        nav_pattern = r'<nav[^>]*>(.*?)</nav>'
        nav_matches = re.findall(nav_pattern, html_content, re.IGNORECASE | re.DOTALL)
        
        for nav_content in nav_matches:
            # Extract links
            links = []
            link_pattern = r'<a[^>]*href="([^"]*)"[^>]*>(.*?)</a>'
            link_matches = re.findall(link_pattern, nav_content, re.IGNORECASE | re.DOTALL)
            
            for href, text in link_matches:
                links.append({
                    "href": href,
                    "text": text.strip()
                })
            
            navigation.append({"links": links})
        
        return navigation
    
    def _extract_scripts(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract scripts from HTML content."""
        scripts = []
        
        script_pattern = r'<script[^>]*>(.*?)</script>'
        script_matches = re.findall(script_pattern, html_content, re.IGNORECASE | re.DOTALL)
        
        for script_content in script_matches:
            scripts.append({
                "content": script_content,
                "type": "inline"
            })
        
        # Also find external scripts
        external_script_pattern = r'<script[^>]*src="([^"]*)"[^>]*>'
        external_matches = re.findall(external_script_pattern, html_content, re.IGNORECASE)
        
        for src in external_matches:
            scripts.append({
                "src": src,
                "type": "external"
            })
        
        return scripts
    
    def _extract_styles(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract styles from HTML content."""
        styles = []
        
        # Find inline styles
        style_pattern = r'<style[^>]*>(.*?)</style>'
        style_matches = re.findall(style_pattern, html_content, re.IGNORECASE | re.DOTALL)
        
        for style_content in style_matches:
            styles.append({
                "content": style_content,
                "type": "inline"
            })
        
        # Find external stylesheets
        external_style_pattern = r'<link[^>]*rel="stylesheet"[^>]*href="([^"]*)"[^>]*>'
        external_matches = re.findall(external_style_pattern, html_content, re.IGNORECASE)
        
        for href in external_matches:
            styles.append({
                "href": href,
                "type": "external"
            })
        
        return styles
    
    def _extract_images(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract images from HTML content."""
        images = []
        
        img_pattern = r'<img[^>]*src="([^"]*)"[^>]*>'
        img_matches = re.findall(img_pattern, html_content, re.IGNORECASE)
        
        for src in img_matches:
            images.append({
                "src": src,
                "alt": re.search(r'alt="([^"]*)"', img_pattern)
            })
        
        return images
    
    def _extract_links(self, html_content: str) -> List[Dict[str, Any]]:
        """Extract links from HTML content."""
        links = []
        
        link_pattern = r'<a[^>]*href="([^"]*)"[^>]*>(.*?)</a>'
        link_matches = re.findall(link_pattern, html_content, re.IGNORECASE | re.DOTALL)
        
        for href, text in link_matches:
            links.append({
                "href": href,
                "text": text.strip()
            })
        
        return links
    
    def _generate_selector(self, content: str, component_type: str) -> str:
        """Generate a CSS selector for a component."""
        # Try to find an ID
        id_match = re.search(r'id="([^"]*)"', content, re.IGNORECASE)
        if id_match:
            return f"#{id_match.group(1)}"
        
        # Try to find a class
        class_match = re.search(r'class="([^"]*)"', content, re.IGNORECASE)
        if class_match:
            classes = class_match.group(1).split()
            return f".{classes[0]}"
        
        # Fallback to tag name
        tag_match = re.match(r'<(\w+)', content, re.IGNORECASE)
        if tag_match:
            return tag_match.group(1)
        
        return component_type
    
    def _generate_component_step(self, component: UIComponentMapping, framework: WebsiteFramework) -> Dict[str, Any]:
        """Generate a modernization step for a component."""
        return {
            "type": "component_migration",
            "component": component.modern_component,
            "legacy_selector": component.legacy_selector,
            "framework": framework.value,
            "effort": "medium",
            "dependencies": [],
            "notes": f"Migrate {component.component_type.value} component"
        }
    
    def _generate_api_step(self, api: APIMapping, framework: WebsiteFramework) -> Dict[str, Any]:
        """Generate a modernization step for an API endpoint."""
        return {
            "type": "api_migration",
            "legacy_endpoint": api.legacy_endpoint,
            "modern_endpoint": api.modern_endpoint,
            "method": api.http_method,
            "framework": framework.value,
            "effort": "high",
            "dependencies": [],
            "notes": f"Migrate {api.http_method} endpoint"
        }
    
    def _generate_recommendations(self, website_mapping: WebsiteModernizationMapping) -> List[str]:
        """Generate modernization recommendations."""
        recommendations = []
        
        # Framework-specific recommendations
        if website_mapping.target_framework == WebsiteFramework.REACT:
            recommendations.extend([
                "Use functional components with hooks",
                "Implement proper state management with Context or Redux",
                "Add TypeScript for better type safety",
                "Use modern CSS-in-JS or styled-components"
            ])
        elif website_mapping.target_framework == WebsiteFramework.NEXTJS:
            recommendations.extend([
                "Leverage server-side rendering for better SEO",
                "Use API routes for backend functionality",
                "Implement proper image optimization",
                "Add static generation where possible"
            ])
        elif website_mapping.target_framework == WebsiteFramework.ASTRO:
            recommendations.extend([
                "Use static generation for content-heavy pages",
                "Leverage partial hydration for interactive components",
                "Implement proper SEO meta tags",
                "Use Astro's built-in image optimization"
            ])
        
        # General recommendations
        recommendations.extend([
            "Implement responsive design",
            "Add proper accessibility features",
            "Optimize for performance",
            "Add proper error handling",
            "Implement proper loading states",
            "Add comprehensive testing"
        ])
        
        return recommendations 