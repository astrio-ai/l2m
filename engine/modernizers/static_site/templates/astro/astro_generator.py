"""
Astro template generator for modernizing legacy websites.
"""

import os
import json
from pathlib import Path
from typing import Dict, List, Any


class AstroTemplateGenerator:
    """
    Generator for creating modern Astro projects with Tailwind CSS.
    """
    
    def __init__(self):
        self.project_structure = {
            'src': {
                'components': [],
                'pages': [],
                'layouts': [],
                'styles': ['index.css']
            },
            'public': ['favicon.ico'],
            'config': ['package.json', 'tailwind.config.js', 'postcss.config.js', 'astro.config.mjs']
        }
    
    def generate_project(self, transformed_data: Dict[str, Any], output_dir: str) -> Dict[str, Any]:
        """
        Generate a complete Astro project.
        
        Args:
            transformed_data: Transformed website data
            output_dir: Output directory for the project
            
        Returns:
            Generation results
        """
        try:
            output_path = Path(output_dir)
            output_path.mkdir(parents=True, exist_ok=True)
            
            # Generate project structure
            self._generate_project_structure(output_path)
            
            # Generate package.json
            self._generate_package_json(output_path)
            
            # Generate configuration files
            self._generate_tailwind_config(output_path)
            self._generate_postcss_config(output_path)
            self._generate_astro_config(output_path)
            
            # Generate source files
            self._generate_src_files(output_path, transformed_data)
            
            # Generate public files
            self._generate_public_files(output_path)
            
            # Generate README
            self._generate_readme(output_path)
            
            return {
                'success': True,
                'output_dir': str(output_path),
                'files_generated': self._count_generated_files(output_path),
                'components_count': len(transformed_data.get('components', [])) if isinstance(transformed_data.get('components'), list) else 0,
                'pages_count': len(transformed_data.get('pages', [])) if isinstance(transformed_data.get('pages'), list) else 0
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def _generate_project_structure(self, output_path: Path):
        """Generate the basic project directory structure."""
        # Create main directories
        (output_path / 'src').mkdir(exist_ok=True)
        (output_path / 'src' / 'components').mkdir(exist_ok=True)
        (output_path / 'src' / 'pages').mkdir(exist_ok=True)
        (output_path / 'src' / 'layouts').mkdir(exist_ok=True)
        (output_path / 'src' / 'styles').mkdir(exist_ok=True)
        (output_path / 'public').mkdir(exist_ok=True)
    
    def _generate_package_json(self, output_path: Path):
        """Generate package.json for Astro project."""
        package_json = {
            "name": "modernized-website",
            "type": "module",
            "version": "0.0.0",
            "scripts": {
                "dev": "astro dev",
                "start": "astro dev",
                "build": "astro build",
                "preview": "astro preview",
                "astro": "astro"
            },
            "dependencies": {
                "astro": "^4.0.0"
            },
            "devDependencies": {
                "@astrojs/tailwind": "^5.0.0",
                "autoprefixer": "^10.4.16",
                "postcss": "^8.4.32",
                "tailwindcss": "^3.3.6"
            }
        }
        
        with open(output_path / 'package.json', 'w') as f:
            json.dump(package_json, f, indent=2)
    
    def _generate_tailwind_config(self, output_path: Path):
        """Generate Tailwind CSS configuration."""
        tailwind_config = """/** @type {import('tailwindcss').Config} */
export default {
  content: ['./src/**/*.{astro,html,js,jsx,md,mdx,svelte,ts,tsx,vue}'],
  theme: {
    extend: {
      colors: {
        primary: {
          50: '#eff6ff',
          100: '#dbeafe',
          200: '#bfdbfe',
          300: '#93c5fd',
          400: '#60a5fa',
          500: '#3b82f6',
          600: '#2563eb',
          700: '#1d4ed8',
          800: '#1e40af',
          900: '#1e3a8a',
        },
      },
      fontFamily: {
        sans: ['Inter', 'system-ui', 'sans-serif'],
      },
    },
  },
  plugins: [],
}"""
        
        with open(output_path / 'tailwind.config.js', 'w') as f:
            f.write(tailwind_config)
    
    def _generate_postcss_config(self, output_path: Path):
        """Generate PostCSS configuration."""
        postcss_config = """export default {
  plugins: {
    tailwindcss: {},
    autoprefixer: {},
  },
}"""
        
        with open(output_path / 'postcss.config.js', 'w') as f:
            f.write(postcss_config)
    
    def _generate_astro_config(self, output_path: Path):
        """Generate Astro configuration."""
        astro_config = """import { defineConfig } from 'astro/config';
import tailwind from '@astrojs/tailwind';

export default defineConfig({
  integrations: [tailwind()],
  site: 'https://your-site.com',
  base: '/',
});"""
        
        with open(output_path / 'astro.config.mjs', 'w') as f:
            f.write(astro_config)
    
    def _generate_src_files(self, output_path: Path, transformed_data: Dict[str, Any]):
        """Generate source files including components and pages."""
        # Generate main layout
        self._generate_main_layout(output_path)
        
        # Generate main index page
        self._generate_main_index(output_path)
        
        # Generate CSS file
        self._generate_css_file(output_path)
        
        # Generate components
        components = transformed_data.get('components', [])
        if isinstance(components, list):
            for component in components:
                self._generate_component_file(output_path, component)
        
        # Generate pages
        pages = transformed_data.get('pages', [])
        if isinstance(pages, list):
            for page in pages:
                self._generate_page_file(output_path, page)
    
    def _generate_main_layout(self, output_path: Path):
        """Generate the main layout component."""
        layout_code = """---
export interface Props {
  title: string;
  description?: string;
}

const { title, description = "Modernized website built with Astro and Tailwind CSS" } = Astro.props;
---

<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="description" content={description} />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <link rel="icon" type="image/svg+xml" href="/favicon.svg" />
    <meta name="generator" content={Astro.generator} />
    <title>{title}</title>
  </head>
  <body class="min-h-screen bg-gray-50">
    <slot />
  </body>
</html>

<style is:global>
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
</style>"""
        
        with open(output_path / 'src' / 'layouts' / 'Layout.astro', 'w') as f:
            f.write(layout_code)
    
    def _generate_main_index(self, output_path: Path):
        """Generate the main index page."""
        index_code = """---
import Layout from '../layouts/Layout.astro';
import Navigation from '../components/Navigation.astro';
---

<Layout title="Modernized Website">
  <Navigation />
  <main class="container mx-auto px-4 py-8">
    <h1 class="text-4xl font-bold text-gray-900 mb-8">
      Welcome to Your Modernized Website
    </h1>
    <p class="text-lg text-gray-600">
      This website has been modernized using Astro and Tailwind CSS.
    </p>
  </main>
</Layout>"""
        
        with open(output_path / 'src' / 'pages' / 'index.astro', 'w') as f:
            f.write(index_code)
    
    def _generate_css_file(self, output_path: Path):
        """Generate the main CSS file with Tailwind imports."""
        css_code = """@tailwind base;
@tailwind components;
@tailwind utilities;

@layer base {
  html {
    font-family: 'Inter', system-ui, sans-serif;
  }
}

@layer components {
  .btn-primary {
    @apply px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 transition-colors;
  }
  
  .btn-secondary {
    @apply px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-700 transition-colors;
  }
  
  .form-input {
    @apply w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500;
  }
  
  .card {
    @apply bg-white rounded-lg shadow-md p-6;
  }
}"""
        
        with open(output_path / 'src' / 'styles' / 'index.css', 'w') as f:
            f.write(css_code)
    
    def _generate_component_file(self, output_path: Path, component: Dict[str, Any]):
        """Generate a component file."""
        component_name = component.get('name', 'Component')
        component_code = component.get('content', component.get('code', ''))
        
        if not component_code:
            component_code = f"""---
// {component_name} component
---

<div class="p-4">
  <h2 class="text-2xl font-bold mb-4">{component_name}</h2>
  <p>This component was generated from legacy code.</p>
</div>"""
        
        # Convert React JSX to Astro syntax
        component_code = self._convert_jsx_to_astro(component_code)
        
        with open(output_path / 'src' / 'components' / f'{component_name}.astro', 'w') as f:
            f.write(component_code)
    
    def _generate_page_file(self, output_path: Path, page: Dict[str, Any]):
        """Generate a page file."""
        page_name = page.get('name', 'Page')
        page_code = page.get('content', '')
        
        if not page_code:
            page_code = f"""---
import Layout from '../layouts/Layout.astro';
---

<Layout title="{page_name}">
  <div class="container mx-auto px-4 py-8">
    <h1 class="text-4xl font-bold text-gray-900 mb-8">{page_name}</h1>
    <p class="text-lg text-gray-600">
      This page was generated from legacy HTML.
    </p>
  </div>
</Layout>"""
        else:
            # Convert React JSX to Astro syntax
            page_code = self._convert_jsx_to_astro(page_code)
            page_code = f"""---
import Layout from '../layouts/Layout.astro';
---

<Layout title="{page_name}">
{page_code}
</Layout>"""
        
        with open(output_path / 'src' / 'pages' / f'{page_name.lower().replace(" ", "-")}.astro', 'w') as f:
            f.write(page_code)
    
    def _convert_jsx_to_astro(self, jsx_code: str) -> str:
        """Convert React JSX code to Astro syntax."""
        # Basic conversions
        conversions = [
            ('className', 'class'),
            ('onClick', 'on:click'),
            ('onChange', 'on:change'),
            ('onSubmit', 'on:submit'),
            ('import React from \'react\';', ''),
            ('export default', ''),
            ('const', ''),
            ('= () => {', ''),
            ('return (', ''),
            (');', ''),
            ('};', ''),
            ('{/*', '<!--'),
            ('*/}', '-->'),
        ]
        
        for old, new in conversions:
            jsx_code = jsx_code.replace(old, new)
        
        # Remove React component wrapper
        jsx_code = jsx_code.replace('function App() {', '')
        jsx_code = jsx_code.replace('function App() {', '')
        
        return jsx_code
    
    def _generate_public_files(self, output_path: Path):
        """Generate public files."""
        # Generate favicon
        favicon_svg = """<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="w-6 h-6">
  <path stroke-linecap="round" stroke-linejoin="round" d="M12 6v12m6-6H6" />
</svg>"""
        
        with open(output_path / 'public' / 'favicon.svg', 'w') as f:
            f.write(favicon_svg)
    
    def _generate_readme(self, output_path: Path):
        """Generate README file."""
        readme_content = """# Modernized Website (Astro)

This project was generated from a legacy HTML website using the Legacy2Modern CLI.

## Features

- ✅ Astro for static site generation
- ✅ Tailwind CSS for styling
- ✅ Fast development server
- ✅ Responsive design
- ✅ Zero JavaScript by default
- ✅ SEO optimized

## Getting Started

1. Install dependencies:
   ```bash
   npm install
   ```

2. Start the development server:
   ```bash
   npm run dev
   ```

3. Build for production:
   ```bash
   npm run build
   ```

## Project Structure

```
src/
├── components/     # Reusable Astro components
├── pages/         # Page components (auto-routed)
├── layouts/       # Layout components
└── styles/        # CSS and styling files
```

## Deployment

This project can be deployed to:
- Netlify
- Vercel
- GitHub Pages
- Any static hosting service

## Migration Notes

This project was automatically generated from legacy HTML/Bootstrap/jQuery code. Some manual adjustments may be needed for:

- Complex JavaScript interactions
- Server-side functionality
- Database connections
- Third-party integrations

## Support

For issues or questions, please refer to the original Legacy2Modern documentation.
"""
        
        with open(output_path / 'README.md', 'w') as f:
            f.write(readme_content)
    
    def _count_generated_files(self, output_path: Path) -> int:
        """Count the number of files generated."""
        count = 0
        for root, dirs, files in os.walk(output_path):
            if isinstance(files, list):
                count += len(files)
            else:
                count += 1  # Fallback if files is not a list
        return count 