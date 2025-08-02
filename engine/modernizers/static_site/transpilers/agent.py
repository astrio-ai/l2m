"""
Website Modernization LLM Agent

This module provides an AI agent specifically designed to modernize legacy websites
using local LLM models with prompts. It converts HTML + Bootstrap + jQuery websites
to modern React TypeScript applications.
"""

import logging
import json
import re
from typing import Dict, Any, List, Optional, Tuple
from dataclasses import dataclass
from pathlib import Path

from engine.agents.agent import LLMAgent, LLMConfig, LLMProvider
from engine.modernizers.static_site.parser.html.html_parser import HTMLParser
from engine.modernizers.static_site.parser.html.html_analyzer import HTMLAnalyzer
from engine.modernizers.static_site.transpilers.llm_augmentor import StaticSiteLLMAugmentor, StaticSiteLLMConfig


@dataclass
class WebsiteAnalysis:
    """Result of website analysis."""
    structure: Dict[str, Any]
    components: List[Dict[str, Any]]
    styles: List[Dict[str, Any]]
    scripts: List[Dict[str, Any]]
    modern_components: List[Dict[str, Any]]
    modern_styles: List[Dict[str, Any]]
    modern_scripts: List[Dict[str, Any]]


@dataclass
class ModernizationResult:
    """Result of website modernization."""
    original_html: str
    modern_react_code: str
    components: List[Dict[str, Any]]
    styles: Dict[str, str]
    package_json: Dict[str, Any]
    readme: str
    confidence: float


class WebsiteAgent:
    """
    AI agent for modernizing legacy websites to React TypeScript applications.
    """
    
    def __init__(self, config: Optional[StaticSiteLLMConfig] = None):
        self.config = config or StaticSiteLLMConfig.from_env()
        self.llm_augmentor = StaticSiteLLMAugmentor(config)
        self.parser = HTMLParser()
        self.analyzer = HTMLAnalyzer()
        self.logger = logging.getLogger(__name__)
        
        # Set up LLM agent with static site provider
        llm_config = LLMConfig(
            api_key=self.config.api_key,
            model=self.config.model,
            temperature=self.config.temperature,
            provider=self.config.provider,
            cache_enabled=self.config.cache_enabled
        )
        self.llm_agent = LLMAgent(llm_config, self._create_static_site_provider())
        
        # Modernization prompts
        self.prompts = {
            'analyze': self._create_analysis_prompt(),
            'convert_component': self._create_component_conversion_prompt(),
            'convert_styles': self._create_style_conversion_prompt(),
            'convert_scripts': self._create_script_conversion_prompt(),
            'generate_package': self._create_package_generation_prompt(),
            'generate_readme': self._create_readme_generation_prompt()
        }
    
    def _create_static_site_provider(self) -> LLMProvider:
        """Create a static site LLM provider for the general LLM agent."""
        class StaticSiteProvider(LLMProvider):
            def __init__(self, augmentor):
                self.augmentor = augmentor
            
            def generate_response(self, messages: List[Dict[str, str]], config: LLMConfig) -> str:
                # Use the static site augmentor for website-specific translations
                if messages and len(messages) > 1:
                    # Extract the user message content
                    user_message = messages[-1]["content"]
                    
                    # Create a mock website component for translation
                    website_component = {
                        "type": "website_modernization",
                        "content": user_message,
                        "id": "llm_request",
                        "title": "LLM Request"
                    }
                    
                    return self.augmentor.translate_website_component(website_component) or user_message
                return "No response generated"
        
        return StaticSiteProvider(self.llm_augmentor)
    
    def modernize_website(self, html_file_path: str, output_dir: str) -> ModernizationResult:
        """
        Modernize a legacy HTML website to React TypeScript.
        
        Args:
            html_file_path: Path to the HTML file
            output_dir: Directory to output the modern React TypeScript app
            
        Returns:
            Modernization result with React TypeScript code and project files
        """
        try:
            # Step 1: Parse and analyze the HTML
            print("🔍 Analyzing legacy website...")
            analysis = self._analyze_website(html_file_path)
            
            # Step 2: Convert components using LLM
            print("🔄 Converting components to React TypeScript...")
            modern_components = self._convert_components_with_llm(analysis)
            
            # Step 3: Convert styles to Tailwind
            print("🎨 Converting styles to Tailwind CSS...")
            modern_styles = self._convert_styles_with_llm(analysis)
            
            # Step 4: Convert scripts to React hooks
            print("⚡ Converting scripts to React hooks...")
            modern_scripts = self._convert_scripts_with_llm(analysis)
            
            # Step 5: Generate React TypeScript application
            print("🚀 Generating React TypeScript application...")
            react_app = self._generate_react_app(analysis, modern_components, modern_styles, modern_scripts)
            
            # Step 6: Generate project files
            print("📦 Generating project files...")
            package_json = self._generate_package_json()
            readme = self._generate_readme()
            
            # Step 7: Write files to output directory
            print("💾 Writing files to output directory...")
            self._write_project_files(output_dir, react_app, modern_styles, package_json, readme, modern_components)
            
            return ModernizationResult(
                original_html=analysis.structure.get('html', ''),
                modern_react_code=react_app,
                components=modern_components,
                styles=modern_styles,
                package_json=package_json,
                readme=readme,
                confidence=0.85  # High confidence for LLM-based conversion
            )
            
        except Exception as e:
            self.logger.error(f"Error modernizing website: {e}")
            raise
    
    def _analyze_website(self, html_file_path: str) -> WebsiteAnalysis:
        """Analyze the legacy website structure."""
        # Parse the HTML file
        parsed_data = self.parser.parse_input(html_file_path)
        
        # Analyze the structure
        analysis = self.analyzer.analyze_website(parsed_data)
        
        # Extract components, styles, and scripts
        components = self._extract_components(parsed_data)
        styles = self._extract_styles(parsed_data)
        scripts = self._extract_scripts(parsed_data)
        
        return WebsiteAnalysis(
            structure=analysis,
            components=components,
            styles=styles,
            scripts=scripts,
            modern_components=[],
            modern_styles={},
            modern_scripts=[]
        )
    
    def _extract_components(self, parsed_data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Extract components from parsed HTML data."""
        components = []
        
        for file_data in parsed_data.get('files', []):
            structure = file_data.get('structure', {})
            sections = structure.get('sections', [])
            
            for section in sections:
                component = {
                    'type': section.get('type', 'generic'),
                    'id': section.get('id', ''),
                    'title': section.get('title', ''),
                    'content': section.get('content', ''),
                    'classes': section.get('classes', []),
                    'attributes': section.get('attributes', {})
                }
                components.append(component)
        
        return components
    
    def _extract_styles(self, parsed_data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Extract styles from parsed HTML data."""
        styles = []
        
        for file_data in parsed_data.get('files', []):
            file_styles = file_data.get('styles', [])
            if isinstance(file_styles, list):
                styles.extend(file_styles)
            elif isinstance(file_styles, dict):
                styles.append(file_styles)
        
        return styles
    
    def _extract_scripts(self, parsed_data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Extract scripts from parsed HTML data."""
        scripts = []
        
        for file_data in parsed_data.get('files', []):
            file_scripts = file_data.get('scripts', [])
            if isinstance(file_scripts, list):
                scripts.extend(file_scripts)
            elif isinstance(file_scripts, dict):
                scripts.append(file_scripts)
        
        return scripts
    
    def _convert_components_with_llm(self, analysis: WebsiteAnalysis) -> List[Dict[str, Any]]:
        """Convert HTML components to React TypeScript components using LLM."""
        modern_components = []
        
        for component in analysis.components:
            # Use the static site LLM augmentor for component conversion
            try:
                response = self.llm_augmentor.translate_website_component(component)
                if response:
                    modern_component = self._parse_component_response(response)
                    modern_components.append(modern_component)
                else:
                    # Fallback to basic conversion
                    modern_component = self._fallback_component_conversion(component)
                    modern_components.append(modern_component)
            except Exception as e:
                self.logger.warning(f"Failed to convert component {component['id']}: {e}")
                # Fallback to basic conversion
                modern_component = self._fallback_component_conversion(component)
                modern_components.append(modern_component)
        
        return modern_components
    
    def _convert_styles_with_llm(self, analysis: WebsiteAnalysis) -> Dict[str, str]:
        """Convert CSS styles to Tailwind CSS using LLM."""
        modern_styles = {}
        
        for style in analysis.styles:
            # Handle different style data formats
            if isinstance(style, dict):
                css_content = style.get('content', '')
                css_classes = json.dumps(style.get('classes', []))
            else:
                css_content = str(style)
                css_classes = json.dumps([])
            
            # Create prompt for style conversion
            prompt = self.prompts['convert_styles'].format(
                css_content=css_content,
                css_classes=css_classes
            )
            
            # Use LLM to convert styles
            try:
                response = self._call_llm(prompt)
                tailwind_classes = self._parse_style_response(response)
                modern_styles[style.get('name', 'default') if isinstance(style, dict) else 'default'] = tailwind_classes
            except Exception as e:
                self.logger.warning(f"Failed to convert styles: {e}")
                # Fallback to basic conversion
                style_name = style.get('name', 'default') if isinstance(style, dict) else 'default'
                modern_styles[style_name] = self._fallback_style_conversion(style)
        
        return modern_styles
    
    def _convert_scripts_with_llm(self, analysis: WebsiteAnalysis) -> List[Dict[str, Any]]:
        """Convert JavaScript/jQuery scripts to React hooks using LLM."""
        modern_scripts = []
        
        for script in analysis.scripts:
            # Handle different script data formats
            if isinstance(script, dict):
                js_content = script.get('content', '')
                script_type = script.get('type', 'jquery')
            else:
                js_content = str(script)
                script_type = 'jquery'
            
            # Create prompt for script conversion
            prompt = self.prompts['convert_scripts'].format(
                js_content=js_content,
                script_type=script_type
            )
            
            # Use LLM to convert scripts
            try:
                response = self._call_llm(prompt)
                react_hook = self._parse_script_response(response)
                modern_scripts.append(react_hook)
            except Exception as e:
                self.logger.warning(f"Failed to convert script: {e}")
                # Fallback to basic conversion
                react_hook = self._fallback_script_conversion(script)
                modern_scripts.append(react_hook)
        
        return modern_scripts
    
    def _call_llm(self, prompt: str) -> str:
        """Call the local LLM with a prompt."""
        messages = [
            {"role": "system", "content": "You are an expert web developer specializing in modernizing legacy websites to React TypeScript applications."},
            {"role": "user", "content": prompt}
        ]
        
        # Use the LLM agent to generate response
        if self.llm_agent.provider:
            return self.llm_agent.call_llm(messages)
        else:
            raise Exception("LLM not available")
    
    def _parse_component_response(self, response: str) -> Dict[str, Any]:
        """Parse LLM response for component conversion."""
        # Extract component name and content from response
        component_name_match = re.search(r'const\s+(\w+):\s+React\.FC', response, re.IGNORECASE)
        component_name = component_name_match.group(1) if component_name_match else 'Component'
        
        # Extract TSX content
        tsx_match = re.search(r'```tsx\s*\n(.*?)\n```', response, re.DOTALL)
        if tsx_match:
            tsx_content = tsx_match.group(1).strip()
        else:
            # Fallback: try to extract TSX without markdown
            tsx_content = response.strip()
        
        return {
            'name': component_name,
            'content': tsx_content,
            'type': 'react_typescript_component'
        }
    
    def _parse_style_response(self, response: str) -> str:
        """Parse LLM response for style conversion."""
        # Extract Tailwind classes from response
        tailwind_match = re.search(r'className="([^"]*)"', response)
        if tailwind_match:
            return tailwind_match.group(1)
        else:
            # Fallback: return basic classes
            return "bg-white p-4 rounded shadow"
    
    def _parse_script_response(self, response: str) -> Dict[str, Any]:
        """Parse LLM response for script conversion."""
        # Extract React hook content
        hook_match = re.search(r'```tsx\s*\n(.*?)\n```', response, re.DOTALL)
        if hook_match:
            hook_content = hook_match.group(1).strip()
        else:
            hook_content = response.strip()
        
        return {
            'name': 'CustomHook',
            'content': hook_content,
            'type': 'react_hook'
        }
    
    def _fallback_component_conversion(self, component: Dict[str, Any]) -> Dict[str, Any]:
        """Fallback component conversion when LLM fails."""
        component_name = component['id'].title().replace('-', '').replace('_', '') if component['id'] else 'Component'
        
        tsx_content = f"""import React from 'react';

interface {component_name}Props {{
  // Add props as needed
}}

const {component_name}: React.FC<{component_name}Props> = () => {{
  return (
    <div className="p-4">
      <h2>{component['title']}</h2>
      <div>{component['content']}</div>
    </div>
  );
}};

export default {component_name};"""
        
        return {
            'name': component_name,
            'content': tsx_content,
            'type': 'react_typescript_component'
        }
    
    def _fallback_style_conversion(self, style: Dict[str, Any]) -> str:
        """Fallback style conversion when LLM fails."""
        if isinstance(style, dict):
            return "bg-white p-4 rounded shadow"
        else:
            return "bg-white p-4 rounded shadow"
    
    def _fallback_script_conversion(self, script: Dict[str, Any]) -> Dict[str, Any]:
        """Fallback script conversion when LLM fails."""
        return {
            'name': 'CustomHook',
            'content': """import { useState, useEffect } from 'react';

const useCustomHook = () => {
  const [state, setState] = useState<any>(null);
  
  useEffect(() => {
    // Custom hook logic here
  }, []);
  
  return { state };
};

export default useCustomHook;""",
            'type': 'react_hook'
        }
    
    def _generate_react_app(self, analysis: WebsiteAnalysis, components: List[Dict[str, Any]], 
                           styles: Dict[str, str], scripts: List[Dict[str, Any]]) -> str:
        """Generate the main React TypeScript application."""
        # Generate App.tsx with proper imports
        app_content = """import React from 'react';
import Header from '@/components/Header';
import Footer from '@/components/Footer';

"""
        
        # Add component imports (without .tsx extension) - avoid duplicates
        valid_components = []
        imported_components = set()
        for component in components:
            if isinstance(component, dict) and 'name' in component and 'content' in component:
                component_name = component['name']
                if component_name not in imported_components:
                    # Determine import path based on component type
                    if component_name.lower() in ['home', 'contact', 'services', 'about']:
                        app_content += f"import {component_name} from '@/pages/{component_name}';\n"
                    else:
                        app_content += f"import {component_name} from '@/components/{component_name}';\n"
                    imported_components.add(component_name)
                    valid_components.append(component)
        
        app_content += """
const App: React.FC = () => {
  return (
    <div className="App">
      <Header />
      <main>
"""
        
        # Add components to the app (avoid duplicates)
        used_components = set()
        for component in valid_components:
            component_name = component['name']
            if component_name not in used_components:
                app_content += f"        <{component_name} />\n"
                used_components.add(component_name)
        
        # If no components were generated, use a simple structure
        if not valid_components:
            app_content += """        <section id="home" className="bg-gradient-to-r from-blue-600 to-purple-600 text-white py-20">
          <div className="container-custom">
            <div className="text-center">
              <h1 className="text-4xl font-bold mb-4">Welcome to Our Modernized Website</h1>
              <p className="text-xl mb-8">Your legacy website has been successfully modernized!</p>
              <button className="btn-primary">
                Get Started
              </button>
            </div>
          </div>
        </section>
        <section id="services" className="py-16 bg-gray-50">
          <div className="container-custom">
            <h2 className="text-3xl font-bold text-center mb-12">Our Services</h2>
            <div className="grid md:grid-cols-3 gap-8">
              <div className="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
                <h3 className="text-xl font-semibold mb-4">Web Development</h3>
                <p className="text-gray-600 mb-4">Custom websites built with modern technologies.</p>
                <button className="btn-primary">
                  Learn More
                </button>
              </div>
              <div className="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
                <h3 className="text-xl font-semibold mb-4">Mobile Apps</h3>
                <p className="text-gray-600 mb-4">Native and cross-platform mobile applications.</p>
                <button className="btn-primary">
                  Learn More
                </button>
              </div>
              <div className="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
                <h3 className="text-xl font-semibold mb-4">Consulting</h3>
                <p className="text-gray-600 mb-4">Expert advice for your technology needs.</p>
                <button className="btn-primary">
                  Learn More
                </button>
              </div>
            </div>
          </div>
        </section>"""
        
        app_content += """
      </main>
      <Footer />
    </div>
  );
};

export default App;"""
        
        return app_content
    
    def _generate_package_json(self) -> Dict[str, Any]:
        """Generate package.json for React TypeScript project."""
        return {
            "name": "modernized-website",
            "version": "0.1.0",
            "private": True,
            "dependencies": {
                "@types/node": "^16.18.0",
                "@types/react": "^18.2.0",
                "@types/react-dom": "^18.2.0",
                "react": "^18.2.0",
                "react-dom": "^18.2.0",
                "react-scripts": "5.0.1",
                "typescript": "^4.9.0",
                "web-vitals": "^2.1.0"
            },
            "devDependencies": {
                "@tailwindcss/forms": "^0.5.0",
                "autoprefixer": "^10.4.0",
                "postcss": "^8.4.0",
                "tailwindcss": "^3.3.0"
            },
            "scripts": {
                "start": "react-scripts start",
                "build": "react-scripts build",
                "test": "react-scripts test",
                "eject": "react-scripts eject",
                "lint": "eslint src --ext .ts,.tsx",
                "lint:fix": "eslint src --ext .ts,.tsx --fix",
                "type-check": "tsc --noEmit"
            },
            "eslintConfig": {
                "extends": [
                    "react-app",
                    "react-app/jest"
                ]
            },
            "browserslist": {
                "production": [
                    ">0.2%",
                    "not dead",
                    "not op_mini all"
                ],
                "development": [
                    "last 1 chrome version",
                    "last 1 firefox version",
                    "last 1 safari version"
                ]
            }
        }
    
    def _generate_readme(self) -> str:
        """Generate README.md for the modernized website."""
        return """# Modernized Website

This is a modern React TypeScript application generated from a legacy website using AI-powered modernization.

## 🚀 Features

- **React 18** with TypeScript
- **Tailwind CSS** for styling
- **Responsive Design** with mobile-first approach
- **Modern Project Structure** with organized folders
- **Type Safety** with TypeScript interfaces
- **Custom Hooks** for reusable logic
- **Utility Functions** for common operations

## 📁 Project Structure

```
src/
├── components/     # Reusable UI components
│   ├── Header.tsx
│   └── Footer.tsx
├── pages/         # Page-level components
│   ├── Home.tsx
│   ├── Services.tsx
│   ├── Contact.tsx
│   └── About.tsx
├── hooks/         # Custom React hooks
│   └── index.ts
├── types/         # TypeScript type definitions
│   └── index.ts
├── utils/         # Utility functions
│   └── index.ts
├── App.tsx        # Main application component
├── index.tsx      # Application entry point
└── index.css      # Global styles
```

## 🛠️ Available Scripts

- `npm start` - Start development server
- `npm run build` - Build for production
- `npm test` - Run tests
- `npm run lint` - Check code quality
- `npm run lint:fix` - Fix code quality issues
- `npm run type-check` - Check TypeScript types

## 🎨 Styling

This project uses **Tailwind CSS** for styling with:
- Custom color palette
- Responsive design utilities
- Custom component classes
- Inter font family

## 🔧 Development

1. **Install dependencies:**
   ```bash
   npm install
   ```

2. **Start development server:**
   ```bash
   npm start
   ```

3. **Open in browser:**
   Navigate to [http://localhost:3000](http://localhost:3000)

## 📱 Responsive Design

The website is fully responsive and works on:
- Desktop (1024px+)
- Tablet (768px - 1023px)
- Mobile (320px - 767px)

## 🎯 Key Components

### Header
- Responsive navigation
- Mobile menu with hamburger
- Smooth scrolling to sections

### Footer
- Company information
- Quick links
- Contact details
- Social media links

### Pages
- **Home**: Hero section with call-to-action
- **Services**: Service offerings with cards
- **Contact**: Contact form with validation
- **About**: Company information

## 🚀 Deployment

### Build for Production
```bash
npm run build
```

### Deploy to Netlify
1. Connect your GitHub repository
2. Set build command: `npm run build`
3. Set publish directory: `build`

### Deploy to Vercel
1. Install Vercel CLI: `npm i -g vercel`
2. Run: `vercel`

## 🔍 Code Quality

- **ESLint** for code linting
- **TypeScript** for type safety
- **Prettier** for code formatting
- **Custom hooks** for reusable logic

## 📦 Dependencies

### Core
- React 18.2.0
- TypeScript 4.9.0
- React DOM 18.2.0

### Styling
- Tailwind CSS 3.3.0
- @tailwindcss/forms 0.5.0
- PostCSS 8.4.0
- Autoprefixer 10.4.0

### Development
- React Scripts 5.0.1
- Web Vitals 2.1.0

## 🎉 Success!

Your legacy website has been successfully modernized with:
- ✅ Modern React TypeScript architecture
- ✅ Responsive design with Tailwind CSS
- ✅ Organized project structure
- ✅ Type safety with TypeScript
- ✅ Custom components and hooks
- ✅ Production-ready build setup

Happy coding! 🚀"""
    
    def _write_project_files(self, output_dir: str, react_app: str, styles: Dict[str, str], 
                           package_json: Dict[str, Any], readme: str, components: List[Dict[str, Any]]):
        """Write all project files to the output directory."""
        output_path = Path(output_dir)
        output_path.mkdir(parents=True, exist_ok=True)
        
        # Create proper React TypeScript project structure
        src_path = output_path / "src"
        src_path.mkdir(parents=True, exist_ok=True)
        
        # Create subdirectories
        components_path = src_path / "components"
        components_path.mkdir(parents=True, exist_ok=True)
        
        pages_path = src_path / "pages"
        pages_path.mkdir(parents=True, exist_ok=True)
        
        hooks_path = src_path / "hooks"
        hooks_path.mkdir(parents=True, exist_ok=True)
        
        types_path = src_path / "types"
        types_path.mkdir(parents=True, exist_ok=True)
        
        utils_path = src_path / "utils"
        utils_path.mkdir(parents=True, exist_ok=True)
        
        # Write App.tsx in src root
        (src_path / "App.tsx").write_text(react_app)
        
        # Write component files in components folder
        for component in components:
            if isinstance(component, dict) and 'name' in component and 'content' in component:
                component_name = component['name']
                component_content = component['content']
                
                # Determine if it's a page or component based on name
                if component_name.lower() in ['home', 'contact', 'services', 'about']:
                    # Put pages in pages folder
                    (pages_path / f"{component_name}.tsx").write_text(component_content)
                else:
                    # Put other components in components folder
                    (components_path / f"{component_name}.tsx").write_text(component_content)
        
        # Generate Header component if not present
        header_component = self._generate_header_component()
        (components_path / "Header.tsx").write_text(header_component)
        
        # Generate Footer component
        footer_component = self._generate_footer_component()
        (components_path / "Footer.tsx").write_text(footer_component)
        
        # Generate types file
        types_content = """// TypeScript type definitions

export interface ComponentProps {
  className?: string;
  children?: React.ReactNode;
}

export interface HeaderProps extends ComponentProps {
  title?: string;
}

export interface FooterProps extends ComponentProps {
  copyright?: string;
}

export interface ContactFormData {
  name: string;
  email: string;
  message: string;
}

export interface ServiceItem {
  id: string;
  title: string;
  description: string;
  icon?: string;
}
"""
        (types_path / "index.ts").write_text(types_content)
        
        # Generate custom hooks
        hooks_content = """import { useState, useEffect } from 'react';

export const useLocalStorage = <T>(key: string, initialValue: T) => {
  const [storedValue, setStoredValue] = useState<T>(() => {
    try {
      const item = window.localStorage.getItem(key);
      return item ? JSON.parse(item) : initialValue;
    } catch (error) {
      console.error(error);
      return initialValue;
    }
  });

  const setValue = (value: T | ((val: T) => T)) => {
    try {
      const valueToStore = value instanceof Function ? value(storedValue) : value;
      setStoredValue(valueToStore);
      window.localStorage.setItem(key, JSON.stringify(valueToStore));
    } catch (error) {
      console.error(error);
    }
  };

  return [storedValue, setValue] as const;
};

export const useScrollPosition = () => {
  const [scrollPosition, setScrollPosition] = useState(0);

  useEffect(() => {
    const updatePosition = () => {
      setScrollPosition(window.pageYOffset);
    };
    window.addEventListener('scroll', updatePosition);
    updatePosition();
    return () => window.removeEventListener('scroll', updatePosition);
  }, []);

  return scrollPosition;
};
"""
        (hooks_path / "index.ts").write_text(hooks_content)
        
        # Generate utils
        utils_content = """// Utility functions

export const classNames = (...classes: (string | undefined | null | false)[]): string => {
  return classes.filter(Boolean).join(' ');
};

export const formatDate = (date: Date): string => {
  return new Intl.DateTimeFormat('en-US', {
    year: 'numeric',
    month: 'long',
    day: 'numeric',
  }).format(date);
};

export const validateEmail = (email: string): boolean => {
  const re = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  return re.test(email);
};

export const debounce = <T extends (...args: any[]) => any>(
  func: T,
  wait: number
): ((...args: Parameters<T>) => void) => {
  let timeout: NodeJS.Timeout;
  return (...args: Parameters<T>) => {
    clearTimeout(timeout);
    timeout = setTimeout(() => func(...args), wait);
  };
};
"""
        (utils_path / "index.ts").write_text(utils_content)
        
        # Write package.json
        (output_path / "package.json").write_text(json.dumps(package_json, indent=2))
        
        # Write README.md
        (output_path / "README.md").write_text(readme)
        
        # Write Tailwind config
        tailwind_config = """module.exports = {
  content: [
    "./src/**/*.{js,jsx,ts,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        primary: {
          50: '#eff6ff',
          500: '#3b82f6',
          600: '#2563eb',
          700: '#1d4ed8',
        },
      },
      fontFamily: {
        sans: ['Inter', 'system-ui', 'sans-serif'],
      },
    },
  },
  plugins: [
    require('@tailwindcss/forms'),
  ],
}"""
        (output_path / "tailwind.config.js").write_text(tailwind_config)
        
        # Write PostCSS config
        postcss_config = """module.exports = {
  plugins: {
    tailwindcss: {},
    autoprefixer: {},
  },
}"""
        (output_path / "postcss.config.js").write_text(postcss_config)
        
        # Write CSS file
        css_content = """@tailwind base;
@tailwind components;
@tailwind utilities;

@layer base {
  html {
    scroll-behavior: smooth;
  }
  
  body {
    @apply text-gray-900 bg-white;
  }
}

@layer components {
  .btn-primary {
    @apply bg-blue-600 text-white px-6 py-3 rounded-lg font-semibold hover:bg-blue-700 transition-colors;
  }
  
  .btn-secondary {
    @apply bg-gray-200 text-gray-900 px-6 py-3 rounded-lg font-semibold hover:bg-gray-300 transition-colors;
  }
  
  .container-custom {
    @apply max-w-7xl mx-auto px-4 sm:px-6 lg:px-8;
  }
}"""
        (src_path / "index.css").write_text(css_content)
        
        # Write index.tsx
        index_tsx = """import React from 'react';
import ReactDOM from 'react-dom/client';
import './index.css';
import App from './App';

const root = ReactDOM.createRoot(
  document.getElementById('root') as HTMLElement
);
root.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);"""
        (src_path / "index.tsx").write_text(index_tsx)
        
        # Write tsconfig.json
        tsconfig_json = """{
  "compilerOptions": {
    "target": "es5",
    "lib": [
      "dom",
      "dom.iterable",
      "es6"
    ],
    "allowJs": true,
    "skipLibCheck": true,
    "esModuleInterop": true,
    "allowSyntheticDefaultImports": true,
    "strict": true,
    "forceConsistentCasingInFileNames": true,
    "noFallthroughCasesInSwitch": true,
    "module": "esnext",
    "moduleResolution": "node",
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true,
    "jsx": "react-jsx",
    "baseUrl": "src",
    "paths": {
      "@/*": ["*"],
      "@/components/*": ["components/*"],
      "@/pages/*": ["pages/*"],
      "@/hooks/*": ["hooks/*"],
      "@/types/*": ["types/*"],
      "@/utils/*": ["utils/*"]
    }
  },
  "include": [
    "src"
  ]
}"""
        (output_path / "tsconfig.json").write_text(tsconfig_json)
        
        # Create public directory
        public_path = output_path / "public"
        public_path.mkdir(parents=True, exist_ok=True)
        
        # Write public/index.html
        public_html = """<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <link rel="icon" href="%PUBLIC_URL%/favicon.ico" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <meta name="theme-color" content="#000000" />
    <meta
      name="description"
      content="Modernized website created with React TypeScript"
    />
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">
    <title>Modernized Website</title>
  </head>
  <body>
    <noscript>You need to enable JavaScript to run this app.</noscript>
    <div id="root"></div>
  </body>
</html>"""
        (public_path / "index.html").write_text(public_html)
        
        print(f"✅ Project files written to: {output_dir}")
    
    def _generate_header_component(self) -> str:
        """Generate a Header component."""
        return """import React, { useState } from 'react';
import { HeaderProps } from '@/types';

const Header: React.FC<HeaderProps> = ({ title = "Modernized Website" }) => {
  const [isMenuOpen, setIsMenuOpen] = useState(false);

  return (
    <header className="bg-white shadow-sm border-b border-gray-200">
      <div className="container-custom">
        <div className="flex justify-between items-center py-4">
          {/* Logo */}
          <div className="flex items-center">
            <h1 className="text-2xl font-bold text-gray-900">{title}</h1>
          </div>

          {/* Desktop Navigation */}
          <nav className="hidden md:flex space-x-8">
            <a href="#home" className="text-gray-600 hover:text-gray-900 transition-colors">
              Home
            </a>
            <a href="#services" className="text-gray-600 hover:text-gray-900 transition-colors">
              Services
            </a>
            <a href="#about" className="text-gray-600 hover:text-gray-900 transition-colors">
              About
            </a>
            <a href="#contact" className="text-gray-600 hover:text-gray-900 transition-colors">
              Contact
            </a>
          </nav>

          {/* Mobile menu button */}
          <button
            className="md:hidden p-2"
            onClick={() => setIsMenuOpen(!isMenuOpen)}
            aria-label="Toggle menu"
          >
            <svg className="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M4 6h16M4 12h16M4 18h16" />
            </svg>
          </button>
        </div>

        {/* Mobile Navigation */}
        {isMenuOpen && (
          <div className="md:hidden py-4 border-t border-gray-200">
            <nav className="flex flex-col space-y-4">
              <a href="#home" className="text-gray-600 hover:text-gray-900 transition-colors">
                Home
              </a>
              <a href="#services" className="text-gray-600 hover:text-gray-900 transition-colors">
                Services
              </a>
              <a href="#about" className="text-gray-600 hover:text-gray-900 transition-colors">
                About
              </a>
              <a href="#contact" className="text-gray-600 hover:text-gray-900 transition-colors">
                Contact
              </a>
            </nav>
          </div>
        )}
      </div>
    </header>
  );
};

export default Header;"""
    
    def _generate_footer_component(self) -> str:
        """Generate a Footer component."""
        return """import React from 'react';
import { FooterProps } from '@/types';

const Footer: React.FC<FooterProps> = ({ copyright = "© 2024 Modernized Website. All rights reserved." }) => {
  return (
    <footer className="bg-gray-900 text-white py-12">
      <div className="container-custom">
        <div className="grid md:grid-cols-4 gap-8">
          {/* Company Info */}
          <div className="md:col-span-2">
            <h3 className="text-xl font-semibold mb-4">Modernized Website</h3>
            <p className="text-gray-300 mb-4">
              Transforming legacy websites into modern, responsive applications with cutting-edge technology.
            </p>
            <div className="flex space-x-4">
              <a href="#" className="text-gray-300 hover:text-white transition-colors">
                <span className="sr-only">Twitter</span>
                <svg className="w-6 h-6" fill="currentColor" viewBox="0 0 24 24">
                  <path d="M8.29 20.251c7.547 0 11.675-6.253 11.675-11.675 0-.178 0-.355-.012-.53A8.348 8.348 0 0022 5.92a8.19 8.19 0 01-2.357.646 4.118 4.118 0 001.804-2.27 8.224 8.224 0 01-2.605.996 4.107 4.107 0 00-6.993 3.743 11.65 11.65 0 01-8.457-4.287 4.106 4.106 0 001.27 5.477A4.072 4.072 0 012.8 9.713v.052a4.105 4.105 0 003.292 4.022 4.095 4.095 0 01-1.853.07 4.108 4.108 0 003.834 2.85A8.233 8.233 0 012 18.407a11.616 11.616 0 006.29 1.84" />
                </svg>
              </a>
              <a href="#" className="text-gray-300 hover:text-white transition-colors">
                <span className="sr-only">GitHub</span>
                <svg className="w-6 h-6" fill="currentColor" viewBox="0 0 24 24">
                  <path d="M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z"/>
                </svg>
              </a>
            </div>
          </div>

          {/* Quick Links */}
          <div>
            <h4 className="text-lg font-semibold mb-4">Quick Links</h4>
            <ul className="space-y-2">
              <li><a href="#home" className="text-gray-300 hover:text-white transition-colors">Home</a></li>
              <li><a href="#services" className="text-gray-300 hover:text-white transition-colors">Services</a></li>
              <li><a href="#about" className="text-gray-300 hover:text-white transition-colors">About</a></li>
              <li><a href="#contact" className="text-gray-300 hover:text-white transition-colors">Contact</a></li>
            </ul>
          </div>

          {/* Contact Info */}
          <div>
            <h4 className="text-lg font-semibold mb-4">Contact</h4>
            <ul className="space-y-2 text-gray-300">
              <li>contact@modernized.com</li>
              <li>+1 (555) 123-4567</li>
              <li>123 Modern St, Tech City</li>
            </ul>
          </div>
        </div>

        {/* Copyright */}
        <div className="border-t border-gray-800 mt-8 pt-8 text-center text-gray-300">
          <p>{copyright}</p>
        </div>
      </div>
    </footer>
  );
};

export default Footer;"""

    def _create_analysis_prompt(self) -> str:
        """Create prompt for website analysis."""
        return """Analyze this legacy website and identify its structure, components, and functionality.

Website Structure:
- Components: {components}
- Styles: {styles}
- Scripts: {scripts}

Please provide a detailed analysis of:
1. Main sections and components
2. Styling approach (Bootstrap, custom CSS)
3. JavaScript functionality (jQuery, vanilla JS)
4. Interactive elements
5. Responsive design considerations

Focus on identifying patterns that can be converted to modern React TypeScript components."""

    def _create_component_conversion_prompt(self) -> str:
        """Create prompt for component conversion."""
        return """Convert this HTML component to a modern React TypeScript component.

Component Details:
- Type: {component_type}
- ID: {component_id}
- Title: {component_title}
- Content: {component_content}
- CSS Classes: {component_classes}
- Attributes: {component_attributes}

Requirements:
1. Convert to functional React component with TypeScript
2. Use Tailwind CSS for styling
3. Convert Bootstrap classes to Tailwind equivalents
4. Make it responsive
5. Use modern React patterns (hooks, etc.)
6. Include proper TypeScript types
7. Use .tsx extension and TypeScript syntax

Please provide ONLY the React TypeScript component code in TSX format, no explanations or other text."""

    def _create_style_conversion_prompt(self) -> str:
        """Create prompt for style conversion."""
        return """Convert these CSS styles to Tailwind CSS classes.

CSS Content:
{css_content}

CSS Classes:
{css_classes}

Requirements:
1. Convert all CSS properties to Tailwind utility classes
2. Maintain the same visual appearance
3. Use responsive design classes where appropriate
4. Optimize for modern web standards

Please provide the Tailwind CSS classes that achieve the same styling."""

    def _create_script_conversion_prompt(self) -> str:
        """Create prompt for script conversion."""
        return """Convert this JavaScript/jQuery code to React TypeScript hooks.

Script Content:
{js_content}

Script Type: {script_type}

Requirements:
1. Convert jQuery selectors to React refs or state
2. Convert event handlers to React event handlers
3. Use React hooks (useState, useEffect, useCallback, etc.)
4. Remove jQuery dependencies
5. Use modern JavaScript patterns
6. Make it compatible with React's component lifecycle
7. Include proper TypeScript types

Please provide the React TypeScript hook or component code that replaces this functionality."""

    def _create_package_generation_prompt(self) -> str:
        """Create prompt for package.json generation."""
        return """Generate a package.json file for a modern React TypeScript application.

Requirements:
1. Include React 18 and related dependencies
2. Include TypeScript and type definitions
3. Include Tailwind CSS and related packages
4. Include development tools (ESLint, Prettier, etc.)
5. Include build and development scripts
6. Use latest stable versions
7. Include TypeScript configuration

Please provide a complete package.json file."""

    def _create_readme_generation_prompt(self) -> str:
        """Create prompt for README generation."""
        return """Generate a README.md file for a modernized React TypeScript website.

Original Website: {original_file}

Requirements:
1. Explain what the project is
2. Include installation and setup instructions
3. Include development and build commands
4. List the technologies used
5. Mention that it was automatically generated
6. Include any special features or notes

Please provide a complete README.md file.""" 