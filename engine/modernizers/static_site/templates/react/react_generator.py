"""
React template generator for modernizing legacy websites.
"""

import os
import json
from pathlib import Path
from typing import Dict, List, Any


class ReactTemplateGenerator:
    """
    Generator for creating modern React projects with Tailwind CSS.
    """
    
    def __init__(self):
        self.project_structure = {
            'src': {
                'components': [],
                'pages': [],
                'hooks': [],
                'utils': [],
                'styles': ['index.css']
            },
            'public': ['index.html', 'favicon.ico'],
            'config': ['package.json', 'tailwind.config.js', 'postcss.config.js', 'vite.config.js']
        }
    
    def generate_project(self, transformed_data: Dict[str, Any], output_dir: str) -> Dict[str, Any]:
        """
        Generate a complete React project.
        
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
            self._generate_vite_config(output_path)
            self._generate_tsconfig(output_path)
            self._generate_tsconfig_node(output_path)
            
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
        (output_path / 'src' / 'hooks').mkdir(exist_ok=True)
        (output_path / 'src' / 'utils').mkdir(exist_ok=True)
        (output_path / 'src' / 'styles').mkdir(exist_ok=True)
        (output_path / 'public').mkdir(exist_ok=True)
    
    def _generate_package_json(self, output_path: Path):
        """Generate package.json for React project."""
        package_json = {
            "name": "modernized-website",
            "private": True,
            "version": "0.0.0",
            "type": "module",
            "scripts": {
                "dev": "vite",
                "build": "vite build",
                "lint": "eslint . --ext js,jsx,ts,tsx --report-unused-disable-directives --max-warnings 0",
                "preview": "vite preview",
                "type-check": "tsc --noEmit"
            },
            "dependencies": {
                "react": "^18.2.0",
                "react-dom": "^18.2.0"
            },
            "devDependencies": {
                "@types/react": "^18.2.43",
                "@types/react-dom": "^18.2.17",
                "@typescript-eslint/eslint-plugin": "^6.13.2",
                "@typescript-eslint/parser": "^6.13.2",
                "@vitejs/plugin-react": "^4.2.1",
                "autoprefixer": "^10.4.16",
                "eslint": "^8.55.0",
                "eslint-plugin-react": "^7.33.2",
                "eslint-plugin-react-hooks": "^4.6.0",
                "eslint-plugin-react-refresh": "^0.4.5",
                "postcss": "^8.4.32",
                "tailwindcss": "^3.3.6",
                "typescript": "^5.2.2",
                "vite": "^5.0.8"
            }
        }
        
        with open(output_path / 'package.json', 'w') as f:
            json.dump(package_json, f, indent=2)
    
    def _generate_tailwind_config(self, output_path: Path):
        """Generate Tailwind CSS configuration."""
        tailwind_config = """/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
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
        postcss_config = """export default {
  plugins: {
    tailwindcss: {},
    autoprefixer: {},
  },
}"""
        
        with open(output_path / 'postcss.config.js', 'w') as f:
            f.write(postcss_config)
    
    def _generate_vite_config(self, output_path: Path):
        """Generate Vite configuration."""
        vite_config = """import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  server: {
    port: 3000,
    open: true,
  },
  build: {
    outDir: 'dist',
    sourcemap: true,
  },
  resolve: {
    extensions: ['.js', '.jsx', '.ts', '.tsx']
  },
})"""
        
        with open(output_path / 'vite.config.ts', 'w') as f:
            f.write(vite_config)
    
    def _generate_tsconfig(self, output_path: Path):
        """Generate TypeScript configuration."""
        tsconfig = """{
  "compilerOptions": {
    "target": "ES2020",
    "useDefineForClassFields": true,
    "lib": ["ES2020", "DOM", "DOM.Iterable"],
    "module": "ESNext",
    "skipLibCheck": true,

    /* Bundler mode */
    "moduleResolution": "bundler",
    "allowImportingTsExtensions": true,
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true,
    "jsx": "react-jsx",

    /* Linting */
    "strict": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "noFallthroughCasesInSwitch": true
  },
  "include": ["src"],
  "references": [{ "path": "./tsconfig.node.json" }]
}"""
        
        with open(output_path / 'tsconfig.json', 'w') as f:
            f.write(tsconfig)
    
    def _generate_tsconfig_node(self, output_path: Path):
        """Generate TypeScript configuration for Node.js files."""
        tsconfig_node = """{
  "compilerOptions": {
    "composite": true,
    "skipLibCheck": true,
    "module": "ESNext",
    "moduleResolution": "bundler",
    "allowSyntheticDefaultImports": true
  },
  "include": ["vite.config.ts"]
}"""
        
        with open(output_path / 'tsconfig.node.json', 'w') as f:
            f.write(tsconfig_node)
    
    def _generate_src_files(self, output_path: Path, transformed_data: Dict[str, Any]):
        """Generate source files including components and pages."""
        # Generate main App component
        self._generate_app_component(output_path)
        
        # Generate main index file
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
    
    def _generate_app_component(self, output_path: Path):
        """Generate the main App component."""
        app_code = """import React from 'react';
import Navigation from './components/Navigation';
import HomePage from './pages/HomePage';

const App: React.FC = () => {
  return (
    <div className="min-h-screen bg-gray-50">
      <Navigation />
      <main>
        <HomePage />
      </main>
    </div>
  );
};

export default App;"""
        
        with open(output_path / 'src' / 'App.tsx', 'w') as f:
            f.write(app_code)
    
    def _generate_main_index(self, output_path: Path):
        """Generate the main index file."""
        index_code = """import React from 'react'
import ReactDOM from 'react-dom/client'
import App from './App.tsx'
import './styles/index.css'

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
)"""
        
        with open(output_path / 'src' / 'main.tsx', 'w') as f:
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
        print(f"DEBUG: Generating component: {component}")  # Debug line
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
        print(f"DEBUG: Generating page: {page}")  # Debug line
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
        
        with open(output_path / 'src' / 'pages' / f'{page_name}.tsx', 'w') as f:
            f.write(page_code)
    
    def _generate_public_files(self, output_path: Path):
        """Generate public files."""
        # Generate index.html
        index_html = """<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <link rel="icon" type="image/svg+xml" href="/vite.svg" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Modernized Website</title>
  </head>
  <body>
    <div id="root"></div>
    <script type="module" src="/src/main.tsx"></script>
  </body>
</html>"""
        
        with open(output_path / 'index.html', 'w') as f:
            f.write(index_html)
    
    def _generate_readme(self, output_path: Path):
        """Generate README file."""
        readme_content = """# Modernized Website

This project was generated from a legacy HTML website using the Legacy2Modern CLI.

## Features

- ✅ React 18 with modern hooks
- ✅ TypeScript for type safety
- ✅ Tailwind CSS for styling
- ✅ Vite for fast development
- ✅ Responsive design
- ✅ Modern JavaScript (ES6+)

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
├── components/     # Reusable React components
├── pages/         # Page components
├── hooks/         # Custom React hooks
├── utils/         # Utility functions
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
        print(f"DEBUG: Counting files in {output_path}")  # Debug line
        for root, dirs, files in os.walk(output_path):
            print(f"DEBUG: root={root}, dirs={dirs}, files={files}, type(files)={type(files)}")  # Debug line
            if isinstance(files, list):
                count += len(files)
            else:
                count += 1  # Fallback if files is not a list
        print(f"DEBUG: Total count: {count}")  # Debug line
        return count 