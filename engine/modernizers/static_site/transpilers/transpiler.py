"""
Static Site Transpiler

Converts legacy static websites (HTML, CSS, Bootstrap, jQuery) into modern web applications.
Supports React, Next.js, and Astro with Tailwind CSS.
"""

import os
import json
import subprocess
import platform
from pathlib import Path
from typing import Dict, List, Any, Optional

from ..parser.html.html_parser import HTMLParser
from ..parser.html.html_analyzer import HTMLAnalyzer
from ..rules.bootstrap_rules import BootstrapRules
from ..rules.jquery_rules import JQueryRules
from ..templates.react.react_generator import ReactTemplateGenerator
from ..templates.astro.astro_generator import AstroTemplateGenerator
from ..templates.nextjs.nextjs_generator import NextJSTemplateGenerator


class StaticSiteTranspiler:
    """
    Transpiler for converting legacy static websites to modern frameworks.
    
    Supports:
    - HTML + Bootstrap + jQuery + PHP → React + Tailwind
    - HTML + Bootstrap + jQuery + PHP → Astro + Tailwind  
    - HTML + Bootstrap + jQuery + PHP → Next.js + Tailwind
    """
    
    def __init__(self):
        self.parser = HTMLParser()
        self.analyzer = HTMLAnalyzer()
        self.bootstrap_rules = BootstrapRules()
        self.jquery_rules = JQueryRules()
        self.template_generators = {
            'react': ReactTemplateGenerator(),
            'astro': AstroTemplateGenerator(),
            'nextjs': NextJSTemplateGenerator()
        }
    
    def transpile_website(
        self,
        input_path: str,
        output_dir: str,
        target_framework: str = 'react',
        analyze_only: bool = False
    ) -> Dict[str, Any]:
        """
        Transpile legacy website to modern framework.
        
        Args:
            input_path: Path to HTML file or ZIP archive
            output_dir: Directory to generate modern website
            target_framework: Target framework ('react', 'astro', 'nextjs')
            analyze_only: Only analyze without generating code
            
        Returns:
            Transpilation results
        """
        try:
            # Step 1: Parse the legacy website
            print("🔍 Parsing legacy website...")
            parsed_data = self.parser.parse_input(input_path)
            
            # Step 2: Analyze the website structure
            print("📊 Analyzing website structure...")
            analysis = self.analyzer.analyze_website(parsed_data)
            
            # Step 3: Apply transformation rules
            print("🔄 Applying transformation rules...")
            transformed_data = self._apply_transformation_rules(parsed_data, analysis)
            
            # Step 4: Generate modern project (if not analyze_only)
            generation_results = None
            if not analyze_only:
                print("🚀 Generating modern website...")
                template_generator = self.template_generators.get(target_framework)
                if template_generator:
                    generation_results = template_generator.generate_project(transformed_data, output_dir)
                else:
                    raise ValueError(f"Unsupported target framework: {target_framework}")
            
            return {
                'success': True,
                'parsed_data': parsed_data,
                'analysis': analysis,
                'transformed_data': transformed_data,
                'generation': generation_results,
                'summary': self._generate_summary(parsed_data, analysis, generation_results)
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'error_type': type(e).__name__
            }
    
    def analyze_website(self, input_path: str) -> Dict[str, Any]:
        """
        Analyze a legacy website without generating code.
        
        Args:
            input_path: Path to HTML file or ZIP archive
            
        Returns:
            Analysis results
        """
        try:
            parsed_data = self.parser.parse_input(input_path)
            analysis = self.analyzer.analyze_website(parsed_data)
            
            return {
                'success': True,
                'parsed_data': parsed_data,
                'analysis': analysis,
                'summary': self._generate_summary(parsed_data, analysis, None)
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'error_type': type(e).__name__
            }
    
    def _apply_transformation_rules(self, parsed_data: Dict[str, Any], analysis: Dict[str, Any]) -> Dict[str, Any]:
        """
        Apply transformation rules to convert legacy code to modern equivalents.
        """
        transformed_data = parsed_data.copy()
        
        # Apply Bootstrap to Tailwind transformations
        if analysis.get('bootstrap_detected', False):
            print("  🔄 Converting Bootstrap to Tailwind...")
            bootstrap_transformed = self.bootstrap_rules.transform_bootstrap_to_tailwind(parsed_data)
            transformed_data.update(bootstrap_transformed)
        
        # Apply jQuery to React transformations
        if analysis.get('jquery_detected', False):
            print("  🔄 Converting jQuery to React...")
            jquery_transformed = self.jquery_rules.transform_jquery_to_react(parsed_data)
            transformed_data.update(jquery_transformed)
        
        # Generate components and pages from parsed data
        print("  🔄 Generating React components and pages...")
        components = []
        pages = []
        
        # Start with components from bootstrap transformation
        if analysis.get('bootstrap_detected', False):
            bootstrap_components = transformed_data.get('components', [])
            components.extend(bootstrap_components)
            print(f"DEBUG: Added {len(bootstrap_components)} components from bootstrap transformation")
        
        print(f"DEBUG: Found {len(parsed_data.get('files', []))} files in parsed_data")
        for i, file_data in enumerate(parsed_data.get('files', [])):
            print(f"DEBUG: Processing file {i}: {file_data.get('name', 'unknown')}")
            
            # Generate components from HTML structure
            file_components = self._generate_page_components(file_data)
            components.extend(file_components)
            print(f"DEBUG: Generated {len(file_components)} components from file {i}")
            
            # Generate pages from file data
            page = self._generate_page_from_file(file_data)
            if page:
                pages.append(page)
                print(f"DEBUG: Generated page: {page.get('name', 'unknown')}")
            else:
                print(f"DEBUG: No page generated from file {i}")
        
        print(f"DEBUG: Total components generated: {len(components)}")
        print(f"DEBUG: Total pages generated: {len(pages)}")
        
        # Add default components if none generated
        if not components:
            print("DEBUG: No components generated, using defaults")
            components = self._generate_default_components()
        
        # Ensure Navigation component is always present since App.tsx imports it
        navigation_exists = any(comp.get('name') == 'Navigation' for comp in components)
        if not navigation_exists:
            print("DEBUG: Navigation component not found, adding default Navigation")
            navigation_component = {
                'name': 'Navigation',
                'content': self._generate_default_navigation(),
                'type': 'component'
            }
            components.append(navigation_component)
        
        if not pages:
            print("DEBUG: No pages generated, using defaults")
            pages = self._generate_default_pages()
        
        transformed_data['components'] = components
        transformed_data['pages'] = pages
        
        return transformed_data
    
    def _generate_page_components(self, file_data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Generate React/Astro components from HTML structure.
        """
        components = []
        structure = file_data.get('structure', {})
        sections = structure.get('sections', [])
        
        for section in sections:
            section_type = section.get('type', 'general')
            section_id = section.get('id', '')
            section_title = section.get('title', '')
            
            # Generate component based on section type or content
            if section_type in ['services', 'contact', 'about', 'hero', 'features']:
                # Use specific component generators for known types
                if section_type == 'services':
                    component = self._generate_services_component(section)
                elif section_type == 'contact':
                    component = self._generate_contact_component(section)
                elif section_type == 'about':
                    component = self._generate_about_component(section)
                elif section_type == 'hero':
                    component = self._generate_hero_component(section)
                elif section_type == 'features':
                    component = self._generate_features_component(section)
                
                if component:
                    components.append(component)
            else:
                # Generate generic component for unknown sections
                component = self._generate_generic_component(section)
                if component:
                    components.append(component)
        
        return components
    
    def _split_into_sections(self, content: str) -> List[str]:
        """
        Split HTML content into logical sections.
        """
        # Simple section splitting based on div tags
        sections = []
        current_section = ""
        
        lines = content.split('\n')
        for line in lines:
            if '<div' in line and 'class=' in line:
                if current_section:
                    sections.append(current_section.strip())
                current_section = line
            else:
                current_section += line + '\n'
        
        if current_section:
            sections.append(current_section.strip())
        
        return sections if sections else [content]
    
    def _generate_page_from_file(self, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Generate a page from file data."""
        structure = file_data.get('structure', {})
        sections = structure.get('sections', [])
        
        print(f"DEBUG: Generating page from file with {len(sections)} sections")
        print(f"DEBUG: Sections: {sections}")
        
        # Generate dynamic homepage that includes all sections
        page_content = self._generate_dynamic_homepage(sections)
        
        print(f"DEBUG: Generated page content length: {len(page_content)}")
        print(f"DEBUG: Page content preview: {page_content[:200]}...")
        
        page = {
            'name': 'HomePage',
            'title': structure.get('title', 'Modernized Website'),
            'content': page_content,
            'type': 'page'
        }
        
        return page
    
    def _generate_default_components(self) -> List[Dict[str, Any]]:
        """Generate default components if none are created from the website."""
        return [
            {
                'name': 'Navigation',
                'content': self._generate_default_navigation(),
                'type': 'component'
            },
            {
                'name': 'Footer',
                'content': self._generate_default_footer(),
                'type': 'component'
            }
        ]
    
    def _generate_default_pages(self) -> List[Dict[str, Any]]:
        """Generate default pages if none are created from the website."""
        return [
            {
                'name': 'HomePage',
                'title': 'Modernized Website',
                'content': self._generate_default_homepage(),
                'type': 'page'
            }
        ]
    
    def _generate_default_navigation(self) -> str:
        """Generate default navigation component."""
        return """import React from 'react';

const Navigation: React.FC = () => {
  return (
    <nav className="bg-white shadow-lg">
      <div className="max-w-7xl mx-auto px-4">
        <div className="flex justify-between h-16">
          <div className="flex items-center">
            <h1 className="text-xl font-bold text-gray-800">Modernized Website</h1>
          </div>
          <div className="flex items-center space-x-4">
            <a href="#home" className="text-gray-600 hover:text-gray-900">Home</a>
            <a href="#services" className="text-gray-600 hover:text-gray-900">Services</a>
            <a href="#contact" className="text-gray-600 hover:text-gray-900">Contact</a>
          </div>
        </div>
      </div>
    </nav>
  );
};

export default Navigation;"""
    
    def _generate_default_footer(self) -> str:
        """Generate default footer component."""
        return """import React from 'react';

const Footer: React.FC = () => {
  return (
    <footer className="bg-gray-800 text-white py-8">
      <div className="max-w-7xl mx-auto px-4">
        <div className="text-center">
          <p>&copy; 2024 Modernized Website. All rights reserved.</p>
        </div>
      </div>
    </footer>
  );
};

export default Footer;"""
    
    def _generate_services_component(self, section: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Services component from section data."""
        cards = section.get('cards', [])
        title = section.get('title', 'Our Services')
        
        # Generate JSX for service cards
        cards_jsx = ""
        for card in cards:
            card_title = card.get('title', 'Service')
            card_text = card.get('text', 'Service description')
            button_text = card.get('button_text', 'Learn More')
            
            cards_jsx += f"""
            <div className="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <h3 className="text-xl font-semibold mb-4">{card_title}</h3>
              <p className="text-gray-600 mb-4">{card_text}</p>
              <button className="bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700 transition-colors">
                {button_text}
              </button>
            </div>"""
        
        # If no cards found, generate default services
        if not cards_jsx:
            cards_jsx = """
            <div className="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <h3 className="text-xl font-semibold mb-4">Web Development</h3>
              <p className="text-gray-600 mb-4">Custom websites built with modern technologies.</p>
              <button className="bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700 transition-colors">
                Learn More
              </button>
            </div>
            <div className="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <h3 className="text-xl font-semibold mb-4">Mobile Apps</h3>
              <p className="text-gray-600 mb-4">Native and cross-platform mobile applications.</p>
              <button className="bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700 transition-colors">
                Learn More
              </button>
            </div>
            <div className="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <h3 className="text-xl font-semibold mb-4">Consulting</h3>
              <p className="text-gray-600 mb-4">Expert advice for your technology needs.</p>
              <button className="bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700 transition-colors">
                Learn More
              </button>
            </div>"""
        
        services_jsx = f"""import React from 'react';

const Services: React.FC = () => {{
  return (
    <section id="services" className="py-16 bg-gray-50">
      <div className="max-w-7xl mx-auto px-4">
        <h2 className="text-3xl font-bold text-center mb-12">{title}</h2>
        <div className="grid md:grid-cols-3 gap-8">
          {cards_jsx}
        </div>
      </div>
    </section>
  );
}};

export default Services;"""
        
        return {
            'name': 'Services',
            'content': services_jsx,
            'type': 'component'
        }
    
    def _generate_contact_component(self, section: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Contact component from section data."""
        form_data = section.get('form')
        title = section.get('title', 'Contact Us')
        
        if form_data:
            # Generate form inputs from form data
            inputs_jsx = ""
            for input_data in form_data.get('inputs', []):
                input_type = input_data.get('type', 'text')
                input_name = input_data.get('name', '')
                input_placeholder = input_data.get('placeholder', '')
                input_required = input_data.get('required', False)
                input_label = input_data.get('label', '')
                
                if input_type == 'textarea':
                    inputs_jsx += f"""
                    <div className="mb-4">
                      <label className="block text-gray-700 text-sm font-bold mb-2">
                        {input_label or 'Message'}
                      </label>
                      <textarea
                        className="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                        name="{input_name}"
                        placeholder="{input_placeholder}"
                        rows="5"
                        {f'required' if input_required else ''}
                      ></textarea>
                    </div>"""
                else:
                    inputs_jsx += f"""
                    <div className="mb-4">
                      <label className="block text-gray-700 text-sm font-bold mb-2">
                        {input_label or input_name.title()}
                      </label>
                      <input
                        className="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                        type="{input_type}"
                        name="{input_name}"
                        placeholder="{input_placeholder}"
                        {f'required' if input_required else ''}
                      />
                    </div>"""
        else:
            # Default contact form
            inputs_jsx = """
            <div className="mb-4">
              <label className="block text-gray-700 text-sm font-bold mb-2">
                Name
              </label>
              <input
                className="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                type="text"
                name="name"
                placeholder="Your name"
                required
              />
            </div>
            <div className="mb-4">
              <label className="block text-gray-700 text-sm font-bold mb-2">
                Email
              </label>
              <input
                className="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                type="email"
                name="email"
                placeholder="your@email.com"
                required
              />
            </div>
            <div className="mb-4">
              <label className="block text-gray-700 text-sm font-bold mb-2">
                Message
              </label>
              <textarea
                className="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                name="message"
                placeholder="Your message"
                rows="5"
                required
              ></textarea>
            </div>"""
        
        contact_jsx = f"""import React from 'react';

const Contact: React.FC = () => {{
  const handleSubmit = (e: React.FormEvent) => {{
    e.preventDefault();
    // Handle form submission here
    console.log('Form submitted');
  }};

  return (
    <section id="contact" className="py-16 bg-gray-50">
      <div className="max-w-7xl mx-auto px-4">
        <h2 className="text-3xl font-bold text-center mb-12">{title}</h2>
        <div className="max-w-2xl mx-auto">
          <form onSubmit={{handleSubmit}} className="bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4">
            {inputs_jsx}
            <div className="flex items-center justify-center">
              <button
                className="bg-blue-600 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline"
                type="submit"
              >
                Send Message
              </button>
            </div>
          </form>
        </div>
      </div>
    </section>
  );
}};

export default Contact;"""
        
        return {
            'name': 'Contact',
            'content': contact_jsx,
            'type': 'component'
        }
    
    def _generate_about_component(self, section: Dict[str, Any]) -> Dict[str, Any]:
        """Generate About component from section data."""
        title = section.get('title', 'About Us')
        content = section.get('content', '')
        
        about_jsx = f"""import React from 'react';

const About: React.FC = () => {{
  return (
    <section id="about" className="py-16 bg-white">
      <div className="max-w-7xl mx-auto px-4">
        <h2 className="text-3xl font-bold text-center mb-12">{title}</h2>
        <div className="max-w-4xl mx-auto text-center">
          <p className="text-lg text-gray-600 leading-relaxed">
            {content[:200]}{'...' if len(content) > 200 else ''}
          </p>
        </div>
      </div>
    </section>
  );
}};

export default About;"""
        
        return {
            'name': 'About',
            'content': about_jsx,
            'type': 'component'
        }
    
    def _generate_hero_component(self, section: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Hero component from section data."""
        title = section.get('title', 'Welcome')
        content = section.get('content', '')
        
        hero_jsx = f"""import React from 'react';

const Hero: React.FC = () => {{
  return (
    <section className="bg-gradient-to-r from-blue-600 to-purple-600 text-white py-20">
      <div className="max-w-7xl mx-auto px-4">
        <div className="text-center">
          <h1 className="text-4xl font-bold mb-4">{title}</h1>
          <p className="text-xl mb-8">{content[:100]}{'...' if len(content) > 100 else ''}</p>
          <button className="bg-white text-blue-600 px-6 py-3 rounded-lg font-semibold hover:bg-gray-100 transition-colors">
            Get Started
          </button>
        </div>
      </div>
    </section>
  );
}};

export default Hero;"""
        
        return {
            'name': 'Hero',
            'content': hero_jsx,
            'type': 'component'
        }
    
    def _generate_features_component(self, section: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Features component from section data."""
        title = section.get('title', 'Features')
        content = section.get('content', '')
        
        features_jsx = f"""import React from 'react';

const Features: React.FC = () => {{
  return (
    <section className="py-16 bg-gray-50">
      <div className="max-w-7xl mx-auto px-4">
        <h2 className="text-3xl font-bold text-center mb-12">{title}</h2>
        <div className="grid md:grid-cols-3 gap-8">
          <div className="text-center">
            <h3 className="text-xl font-semibold mb-4">Feature 1</h3>
            <p className="text-gray-600">Description of feature 1</p>
          </div>
          <div className="text-center">
            <h3 className="text-xl font-semibold mb-4">Feature 2</h3>
            <p className="text-gray-600">Description of feature 2</p>
          </div>
          <div className="text-center">
            <h3 className="text-xl font-semibold mb-4">Feature 3</h3>
            <p className="text-gray-600">Description of feature 3</p>
          </div>
        </div>
      </div>
    </section>
  );
}};

export default Features;"""
        
        return {
            'name': 'Features',
            'content': features_jsx,
            'type': 'component'
        }
    
    def _generate_generic_component(self, section: Dict[str, Any]) -> Dict[str, Any]:
        """Generate generic component for unknown sections."""
        section_id = section.get('id', '')
        section_title = section.get('title', 'Section')
        section_content = section.get('content', '')
        
        # Create a safe component name
        component_name = section_id.title().replace('-', '').replace('_', '') if section_id else 'Section'
        if not component_name or component_name == 'Section':
            component_name = section_title.title().replace(' ', '').replace('-', '').replace('_', '')
        if not component_name:
            component_name = 'GenericSection'
        
        generic_jsx = f"""import React from 'react';

const {component_name}: React.FC = () => {{
  return (
    <section id="{section_id}" className="py-16 bg-white">
      <div className="max-w-7xl mx-auto px-4">
        <h2 className="text-3xl font-bold text-center mb-12">{section_title}</h2>
        <div className="max-w-4xl mx-auto">
          <div className="prose prose-lg mx-auto">
            <p className="text-gray-600 leading-relaxed">
              {section_content[:300]}{'...' if len(section_content) > 300 else ''}
            </p>
          </div>
        </div>
      </div>
    </section>
  );
}};

export default {component_name};"""
        
        return {
            'name': component_name,
            'content': generic_jsx,
            'type': 'component'
        }
    
    def _generate_default_homepage(self) -> str:
        """Generate default homepage component."""
        return """import React from 'react';

const HomePage: React.FC = () => {
  return (
    <div className="min-h-screen">
      <section className="bg-gradient-to-r from-blue-600 to-purple-600 text-white py-20">
        <div className="max-w-7xl mx-auto px-4">
          <div className="text-center">
            <h1 className="text-4xl font-bold mb-4">Welcome to Our Modernized Website</h1>
            <p className="text-xl mb-8">Your legacy website has been successfully modernized!</p>
            <button className="bg-white text-blue-600 px-6 py-3 rounded-lg font-semibold hover:bg-gray-100 transition-colors">
              Get Started
            </button>
          </div>
        </div>
      </section>
    </div>
  );
};

export default HomePage;"""
    
    def _generate_dynamic_homepage(self, sections: List[Dict[str, Any]]) -> str:
        """Generate dynamic homepage that includes all generated components."""
        print(f"DEBUG: _generate_dynamic_homepage called with {len(sections)} sections")
        
        # Start with hero section
        homepage_content = """import React from 'react';

const HomePage: React.FC = () => {
  return (
    <div className="min-h-screen">
      <section className="bg-gradient-to-r from-blue-600 to-purple-600 text-white py-20">
        <div className="max-w-7xl mx-auto px-4">
          <div className="text-center">
            <h1 className="text-4xl font-bold mb-4">Welcome to Our Modernized Website</h1>
            <p className="text-xl mb-8">Your legacy website has been successfully modernized!</p>
            <button className="bg-white text-blue-600 px-6 py-3 rounded-lg font-semibold hover:bg-gray-100 transition-colors">
              Get Started
            </button>
          </div>
        </div>
      </section>"""
        
        # Add imports and components for each section
        imports = []
        components = []
        
        print(f"DEBUG: Processing {len(sections)} sections")
        for i, section in enumerate(sections):
            section_type = section.get('type', 'general')
            section_id = section.get('id', '')
            section_title = section.get('title', '')
            
            print(f"DEBUG: Section {i}: type={section_type}, id={section_id}, title={section_title}")
            
            if section_type in ['services', 'contact', 'about', 'hero', 'features']:
                component_name = section_type.title()
                # Avoid duplicate imports
                if f"import {component_name} from '../components/{component_name}';" not in imports:
                    imports.append(f"import {component_name} from '../components/{component_name}';")
                    components.append(f"      <{component_name} />")
                    print(f"DEBUG: Added {component_name} component")
                else:
                    print(f"DEBUG: Skipped duplicate {component_name} component")
            else:
                # For generic sections, create component name from id or title
                component_name = section_id.title().replace('-', '').replace('_', '') if section_id else 'Section'
                if not component_name or component_name == 'Section':
                    component_name = section_title.title().replace(' ', '').replace('-', '').replace('_', '')
                if not component_name:
                    component_name = 'GenericSection'
                
                # Avoid duplicate imports
                if f"import {component_name} from '../components/{component_name}';" not in imports:
                    imports.append(f"import {component_name} from '../components/{component_name}';")
                    components.append(f"      <{component_name} />")
                    print(f"DEBUG: Added generic {component_name} component")
                else:
                    print(f"DEBUG: Skipped duplicate {component_name} component")
        
        # Add the imports and components to the homepage
        if imports:
            homepage_content = homepage_content.replace("import React from 'react';", 
                                                      "import React from 'react';\n" + "\n".join(imports))
            homepage_content += "\n" + "\n".join(components)
            print(f"DEBUG: Added {len(imports)} imports and {len(components)} components")
        else:
            print(f"DEBUG: No sections found, using default homepage")
        
        homepage_content += """
    </div>
  );
};

export default HomePage;"""
        
        print(f"DEBUG: Final homepage content length: {len(homepage_content)}")
        return homepage_content
    
    def _generate_page_content(self, structure: Dict[str, Any]) -> str:
        """
        Generate page content from structure.
        """
        content = []
        
        # Add header
        if 'title' in structure:
            content.append(f"<title>{structure['title']}</title>")
        
        # Add meta tags
        if 'meta' in structure:
            for meta in structure['meta']:
                content.append(f"<meta {meta}>")
        
        # Add CSS
        if 'css' in structure:
            content.append(f"<style>{structure['css']}</style>")
        
        # Add body content
        if 'body' in structure:
            content.append(f"<body>{structure['body']}</body>")
        
        return '\n'.join(content)
    
    def _generate_summary(self, parsed_data: Dict, analysis: Dict, generation: Optional[Dict]) -> Dict[str, Any]:
        """
        Generate a summary of the transpilation process.
        """
        summary = {
            'input_files': len(parsed_data.get('files', [])),
            'bootstrap_detected': analysis.get('bootstrap_detected', False),
            'jquery_detected': analysis.get('jquery_detected', False),
            'php_detected': analysis.get('php_detected', False),
            'total_components': len(analysis.get('components', [])),
            'total_styles': len(analysis.get('styles', [])),
            'total_scripts': len(analysis.get('scripts', []))
        }
        
        if generation:
            summary['output_files'] = len(generation.get('files', []))
            summary['generation_success'] = generation.get('success', False)
        
        return summary
    
    def get_supported_frameworks(self) -> List[str]:
        """
        Get list of supported target frameworks.
        """
        return list(self.template_generators.keys())
    
    def validate_input(self, input_path: str) -> Dict[str, Any]:
        """
        Validate input file/directory.
        """
        try:
            if not os.path.exists(input_path):
                return {
                    'valid': False,
                    'error': f"Input path does not exist: {input_path}"
                }
            
            if os.path.isfile(input_path):
                if not input_path.endswith(('.html', '.htm')):
                    return {
                        'valid': False,
                        'error': f"Input file must be HTML: {input_path}"
                    }
            elif os.path.isdir(input_path):
                html_files = list(Path(input_path).glob('*.html')) + list(Path(input_path).glob('*.htm'))
                if not html_files:
                    return {
                        'valid': False,
                        'error': f"No HTML files found in directory: {input_path}"
                    }
            
            return {'valid': True}
            
        except Exception as e:
            return {
                'valid': False,
                'error': str(e)
            }
    
    def open_in_ide(self, project_path: str, ide: str = 'auto') -> bool:
        """
        Open the generated project in the specified IDE.
        
        Args:
            project_path: Path to the generated project
            ide: IDE to use ('auto', 'vscode', 'webstorm', 'sublime', 'atom')
            
        Returns:
            True if successful, False otherwise
        """
        try:
            project_path = Path(project_path)
            if not project_path.exists():
                print(f"❌ Project path does not exist: {project_path}")
                return False
            
            if ide == 'auto':
                ide = self._detect_default_ide()
            
            if ide == 'vscode':
                return self._open_in_vscode(project_path)
            elif ide == 'webstorm':
                return self._open_in_webstorm(project_path)
            elif ide == 'sublime':
                return self._open_in_sublime(project_path)
            elif ide == 'atom':
                return self._open_in_atom(project_path)
            else:
                print(f"❌ Unsupported IDE: {ide}")
                return False
                
        except Exception as e:
            print(f"❌ Error opening IDE: {e}")
            return False
    
    def _detect_default_ide(self) -> str:
        """Detect the default IDE based on system and available applications."""
        system = platform.system().lower()
        
        # Check for common IDEs
        ide_commands = {
            'vscode': ['code', 'code-insiders'],
            'webstorm': ['webstorm', 'wstorm'],
            'sublime': ['subl'],
            'atom': ['atom']
        }
        
        # macOS specific paths
        if system == 'darwin':
            macos_apps = {
                'vscode': [
                    '/Applications/Visual Studio Code.app/Contents/Resources/app/bin/code',
                    '/Applications/Visual Studio Code.app/Contents/MacOS/Electron'
                ],
                'webstorm': [
                    '/Applications/WebStorm.app/Contents/MacOS/webstorm',
                    '/Applications/WebStorm.app/Contents/MacOS/WebStorm'
                ],
                'sublime': [
                    '/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl',
                    '/Applications/Sublime Text.app/Contents/MacOS/Sublime Text'
                ],
                'atom': [
                    '/Applications/Atom.app/Contents/MacOS/Atom'
                ]
            }
            
            # Check macOS apps first
            for ide, paths in macos_apps.items():
                for path in paths:
                    if os.path.exists(path):
                        return ide
        
        # Check for commands in PATH
        for ide, commands in ide_commands.items():
            for command in commands:
                try:
                    subprocess.run([command, '--version'], 
                                 capture_output=True, check=True)
                    return ide
                except (subprocess.CalledProcessError, FileNotFoundError):
                    continue
        
        # Default to VS Code
        return 'vscode'
    
    def _open_in_vscode(self, project_path: Path) -> bool:
        """Open project in VS Code."""
        # Try different VS Code commands
        vscode_commands = [
            'code',
            '/Applications/Visual Studio Code.app/Contents/Resources/app/bin/code',
            '/Applications/Visual Studio Code.app/Contents/MacOS/Electron'
        ]
        
        for command in vscode_commands:
            try:
                subprocess.run([command, str(project_path)], check=True)
                print(f"✅ Opened project in VS Code: {project_path}")
                return True
            except (subprocess.CalledProcessError, FileNotFoundError):
                continue
        
        print("❌ VS Code not found. Please install VS Code or use a different IDE.")
        return False
    
    def _open_in_webstorm(self, project_path: Path) -> bool:
        """Open project in WebStorm."""
        # Try different WebStorm commands
        webstorm_commands = [
            'webstorm',
            'wstorm',
            '/Applications/WebStorm.app/Contents/MacOS/webstorm',
            '/Applications/WebStorm.app/Contents/MacOS/WebStorm'
        ]
        
        for command in webstorm_commands:
            try:
                subprocess.run([command, str(project_path)], check=True)
                print(f"✅ Opened project in WebStorm: {project_path}")
                return True
            except (subprocess.CalledProcessError, FileNotFoundError):
                continue
        
        print("❌ WebStorm not found. Please install WebStorm or use a different IDE.")
        return False
    
    def _open_in_sublime(self, project_path: Path) -> bool:
        """Open project in Sublime Text."""
        # Try different Sublime Text commands
        sublime_commands = [
            'subl',
            '/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl',
            '/Applications/Sublime Text.app/Contents/MacOS/Sublime Text'
        ]
        
        for command in sublime_commands:
            try:
                subprocess.run([command, str(project_path)], check=True)
                print(f"✅ Opened project in Sublime Text: {project_path}")
                return True
            except (subprocess.CalledProcessError, FileNotFoundError):
                continue
        
        print("❌ Sublime Text not found. Please install Sublime Text or use a different IDE.")
        return False
    
    def _open_in_atom(self, project_path: Path) -> bool:
        """Open project in Atom."""
        try:
            subprocess.run(['atom', str(project_path)], check=True)
            print(f"✅ Opened project in Atom: {project_path}")
            return True
        except (subprocess.CalledProcessError, FileNotFoundError):
            print("❌ Atom not found. Please install Atom or use a different IDE.")
            return False
    
    def start_dev_server(self, project_path: str, framework: str = 'react') -> bool:
        """
        Start the development server for the generated project.
        
        Args:
            project_path: Path to the generated project
            framework: Framework used ('react', 'nextjs', 'astro')
            
        Returns:
            True if successful, False otherwise
        """
        try:
            project_path = Path(project_path)
            if not project_path.exists():
                print(f"❌ Project path does not exist: {project_path}")
                return False
            
            # Change to project directory
            os.chdir(project_path)
            
            if framework == 'react':
                return self._start_react_dev_server()
            elif framework == 'nextjs':
                return self._start_nextjs_dev_server()
            elif framework == 'astro':
                return self._start_astro_dev_server()
            else:
                print(f"❌ Unsupported framework: {framework}")
                return False
                
        except Exception as e:
            print(f"❌ Error starting dev server: {e}")
            return False
    
    def _start_react_dev_server(self) -> bool:
        """Start React development server."""
        try:
            print("🚀 Starting React development server...")
            subprocess.run(['npm', 'start'], check=True)
            return True
        except (subprocess.CalledProcessError, FileNotFoundError):
            print("❌ Failed to start React dev server. Make sure npm is installed.")
            return False
    
    def _start_nextjs_dev_server(self) -> bool:
        """Start Next.js development server."""
        try:
            print("🚀 Starting Next.js development server...")
            subprocess.run(['npm', 'run', 'dev'], check=True)
            return True
        except (subprocess.CalledProcessError, FileNotFoundError):
            print("❌ Failed to start Next.js dev server. Make sure npm is installed.")
            return False
    
    def _start_astro_dev_server(self) -> bool:
        """Start Astro development server."""
        try:
            print("🚀 Starting Astro development server...")
            subprocess.run(['npm', 'run', 'dev'], check=True)
            return True
        except (subprocess.CalledProcessError, FileNotFoundError):
            print("❌ Failed to start Astro dev server. Make sure npm is installed.")
            return False 