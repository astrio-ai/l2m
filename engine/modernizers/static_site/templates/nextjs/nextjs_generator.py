"""
Next.js template generator for modernizing legacy websites.
"""

import os
import json
from pathlib import Path
from typing import Dict, List, Any


class NextJSTemplateGenerator:
    """
    Generator for creating modern Next.js projects with Tailwind CSS.
    """
    
    def __init__(self):
        self.project_structure = {
            'src': {
                'app': [],
                'components': [],
                'lib': [],
                'styles': ['globals.css']
            },
            'public': ['favicon.ico'],
            'config': ['package.json', 'tailwind.config.js', 'postcss.config.js', 'next.config.js']
        }
    
    def generate_project(self, transformed_data: Dict[str, Any], output_dir: str) -> Dict[str, Any]:
        """
        Generate a complete Next.js project.
        
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
            self._generate_next_config(output_path)
            
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
        (output_path / 'src' / 'app').mkdir(exist_ok=True)
        (output_path / 'src' / 'components').mkdir(exist_ok=True)
        (output_path / 'src' / 'lib').mkdir(exist_ok=True)
        (output_path / 'src' / 'styles').mkdir(exist_ok=True)
        (output_path / 'public').mkdir(exist_ok=True)
    
    def _generate_package_json(self, output_path: Path):
        """Generate package.json for Next.js project."""
        package_json = {
            "name": "modernized-website",
            "version": "0.1.0",
            "private": True,
            "scripts": {
                "dev": "next dev",
                "build": "next build",
                "start": "next start",
                "lint": "next lint"
            },
            "dependencies": {
                "next": "14.0.0",
                "react": "^18",
                "react-dom": "^18"
            },
            "devDependencies": {
                "@types/node": "^20",
                "@types/react": "^18",
                "@types/react-dom": "^18",
                "autoprefixer": "^10.0.1",
                "eslint": "^8",
                "eslint-config-next": "14.0.0",
                "postcss": "^8",
                "tailwindcss": "^3.3.0",
                "typescript": "^5"
            }
        }
        
        with open(output_path / 'package.json', 'w') as f:
            json.dump(package_json, f, indent=2)
    
    def _generate_tailwind_config(self, output_path: Path):
        """Generate Tailwind CSS configuration."""
        tailwind_config = """/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './src/pages/**/*.{js,ts,jsx,tsx,mdx}',
    './src/components/**/*.{js,ts,jsx,tsx,mdx}',
    './src/app/**/*.{js,ts,jsx,tsx,mdx}',
  ],
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
        postcss_config = """module.exports = {
  plugins: {
    tailwindcss: {},
    autoprefixer: {},
  },
}"""
        
        with open(output_path / 'postcss.config.js', 'w') as f:
            f.write(postcss_config)
    
    def _generate_next_config(self, output_path: Path):
        """Generate Next.js configuration."""
        next_config = """/** @type {import('next').NextConfig} */
const nextConfig = {
  experimental: {
    appDir: true,
  },
}

module.exports = nextConfig"""
        
        with open(output_path / 'next.config.js', 'w') as f:
            f.write(next_config)
    
    def _generate_src_files(self, output_path: Path, transformed_data: Dict[str, Any]):
        """Generate source files including components and pages."""
        # Generate app layout
        self._generate_app_layout(output_path)
        
        # Generate main page
        self._generate_main_page(output_path)
        
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
    
    def _generate_app_layout(self, output_path: Path):
        """Generate the app layout component."""
        layout_code = """import type { Metadata } from 'next'
import { Inter } from 'next/font/google'
import './globals.css'
import Navigation from '@/components/Navigation'

const inter = Inter({ subsets: ['latin'] })

export const metadata: Metadata = {
  title: 'Modernized Website',
  description: 'Modernized website built with Next.js and Tailwind CSS',
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en">
      <body className={inter.className}>
        <div className="min-h-screen bg-gray-50">
          <Navigation />
          <main>
            {children}
          </main>
        </div>
      </body>
    </html>
  )
}"""
        
        with open(output_path / 'src' / 'app' / 'layout.tsx', 'w') as f:
            f.write(layout_code)
    
    def _generate_main_page(self, output_path: Path):
        """Generate the main page component."""
        page_code = """import HomePage from '@/components/HomePage'

export default function Home() {
  return <HomePage />
}"""
        
        with open(output_path / 'src' / 'app' / 'page.tsx', 'w') as f:
            f.write(page_code)
    
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
        
        with open(output_path / 'src' / 'app' / 'globals.css', 'w') as f:
            f.write(css_code)
    
    def _generate_component_file(self, output_path: Path, component: Dict[str, Any]):
        """Generate a component file."""
        component_name = component.get('name', 'Component')
        component_code = component.get('content', component.get('code', ''))
        
        if not component_code:
            component_code = f"""import React from 'react';

const {component_name}: React.FC = () => {{
  return (
    <div className="p-4">
      <h2 className="text-2xl font-bold mb-4">{component_name}</h2>
      <p>This component was generated from legacy code.</p>
    </div>
  );
}};

export default {component_name};"""
        
        with open(output_path / 'src' / 'components' / f'{component_name}.tsx', 'w') as f:
            f.write(component_code)
    
    def _generate_page_file(self, output_path: Path, page: Dict[str, Any]):
        """Generate a page file."""
        page_name = page.get('name', 'Page')
        page_code = page.get('content', '')
        
        if not page_code:
            page_code = f"""import React from 'react';

const {page_name}: React.FC = () => {{
  return (
    <div className="container mx-auto px-4 py-8">
      <h1 className="text-4xl font-bold text-gray-900 mb-8">{page_name}</h1>
      <p className="text-lg text-gray-600">
        This page was generated from legacy HTML.
      </p>
    </div>
  );
}};

export default {page_name};"""
        
        with open(output_path / 'src' / 'components' / f'{page_name}.tsx', 'w') as f:
            f.write(page_code)
    
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
        readme_content = """# Modernized Website (Next.js)

This project was generated from a legacy HTML website using the Legacy2Modern CLI.

## Features

- ✅ Next.js 14 with App Router
- ✅ TypeScript for type safety
- ✅ Tailwind CSS for styling
- ✅ Fast development server
- ✅ Responsive design
- ✅ SEO optimized
- ✅ Server-side rendering

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
├── app/           # App Router pages and layouts
├── components/    # Reusable React components
├── lib/          # Utility functions and libraries
└── styles/       # CSS and styling files
```

## Deployment

This project can be deployed to:
- Vercel (recommended)
- Netlify
- AWS Amplify
- Any Node.js hosting service

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