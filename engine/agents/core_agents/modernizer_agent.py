"""
Modernizer Agent - Generates new code using chosen stack/template.
"""

import logging
from pathlib import Path
from typing import Dict, List, Optional, Any
from .base_agent import BaseAgent, AgentRole
from ..utilities.prompt import PromptContext

logger = logging.getLogger(__name__)

class ModernizerAgent(BaseAgent):
    """
    Modernizer Agent - Generates modern code from legacy analysis.
    
    This agent is responsible for:
    - Converting legacy code to modern frameworks
    - Generating new code using chosen technology stack
    - Applying modern best practices and patterns
    - Creating component-based architectures
    - Implementing responsive and accessible designs
    """
    
    def __init__(self, ai, memory, config, **kwargs):
        super().__init__(
            name="ModernizerAgent",
            role=AgentRole.MODERNIZER,
            ai=ai,
            memory=memory,
            config=config,
            **kwargs
        )
        
        # Initialize prompt builder
        from ..utilities.prompt import PromptBuilder
        self.prompt_builder = PromptBuilder()
        
        self.generated_files = {}
        self.templates_used = {}
        self.conversion_mappings = {}
        
    def _get_default_system_prompt(self) -> str:
        """Return the default system prompt for the modernizer agent."""
        return """You are a Modernizer Agent specialized in converting legacy code to modern frameworks and technologies.

Your primary responsibilities:
1. Convert legacy code to modern frameworks (React, Next.js, Astro, etc.)
2. Apply modern best practices and design patterns
3. Generate clean, maintainable, and performant code
4. Implement responsive and accessible designs
5. Use modern tooling and development practices

You should focus on creating high-quality, production-ready code that follows modern standards."""
    
    async def process_message(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Process incoming messages for the modernizer agent."""
        message_type = message.get('type', 'unknown')
        
        if message_type == 'generate_code':
            return await self._generate_code(message.get('source_file', ''), message.get('target_stack', ''))
        elif message_type == 'convert_file':
            return await self._convert_file(message.get('file_path', ''), message.get('content', ''))
        elif message_type == 'create_component':
            return await self._create_component(message.get('component_name', ''), message.get('specs', {}))
        elif message_type == 'generate_project_structure':
            return await self._generate_project_structure(message.get('project_map', {}))
        else:
            return {
                'type': 'error',
                'message': f'Unknown message type: {message_type}'
            }
    
    async def execute_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a specific task assigned to the modernizer agent."""
        task_type = task.get('type', 'unknown')
        
        if task_type == 'full_modernization':
            return await self._perform_full_modernization(task)
        elif task_type == 'file_conversion':
            return await self._convert_single_file(task)
        elif task_type == 'component_generation':
            return await self._generate_components(task)
        elif task_type == 'project_setup':
            return await self._setup_modern_project(task)
        else:
            return {
                'success': False,
                'error': f'Unknown task type: {task_type}'
            }
    
    async def _perform_full_modernization(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """Perform full modernization of a project."""
        try:
            self.state.current_task = "full_modernization"
            self.update_progress(0.1)
            
            # Get project map from parser agent
            project_map = await self.memory.get('project_map', {})
            if not project_map:
                return {
                    'success': False,
                    'error': 'No project map available. Run parser agent first.'
                }
            
            target_stack = self.config.get_target_stack()
            self.update_progress(0.2)
            
            # Step 1: Generate project structure
            logger.info("Generating modern project structure")
            project_structure = await self._generate_modern_project_structure(project_map, target_stack)
            self.update_progress(0.4)
            
            # Step 2: Generate components (skip legacy file conversion)
            logger.info("Generating modern components")
            components = await self._generate_modern_components(project_map, target_stack)
            self.update_progress(0.7)
            
            # Step 3: Create configuration files
            logger.info("Creating configuration files")
            config_files = await self._create_configuration_files(target_stack)
            self.update_progress(1.0)
            
            # Combine all results (no converted_files since we're using modern components)
            modernization_result = {
                'project_structure': project_structure,
                'components': components,
                'config_files': config_files,
                'target_stack': target_stack
            }
            
            # Store results
            self.generated_files = modernization_result
            await self.memory.set('modernization_result', modernization_result)
            
            return {
                'success': True,
                'modernization_result': modernization_result,
                'message': 'Full modernization completed successfully'
            }
            
        except Exception as e:
            logger.error(f"Error in full modernization: {e}")
            return {
                'success': False,
                'error': str(e)
            }
    
    async def _generate_modern_project_structure(self, project_map: Dict, target_stack: str) -> Dict[str, Any]:
        """Generate modern project structure based on target stack."""
        try:
            # Build context for structure generation
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=target_stack,
                current_task="project_structure_generation",
                user_requirements="Generate a modern project structure for the target stack"
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_modernization_prompt(
                context, task_type="generation"
            )
            
            # Add project structure data
            enhanced_prompt = f"{prompt_result.user_prompt}\n\nLegacy Project Structure:\n{str(project_map.get('structure', {}))}"
            
            # Get AI response
            structure_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': enhanced_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse and structure the response
            modern_structure = self._parse_project_structure(structure_response, target_stack)
            
            return modern_structure
            
        except Exception as e:
            logger.error(f"Error generating project structure: {e}")
            raise
    
    async def _convert_all_files(self, file_analyses: Dict, target_stack: str) -> Dict[str, Any]:
        """Convert all legacy files to modern code."""
        converted_files = {}
        
        for file_path, analysis in file_analyses.items():
            if analysis.get('status') == 'success':
                try:
                    logger.debug(f"Converting file: {file_path}")
                    
                    # Convert file
                    converted = await self._convert_single_file_content(file_path, analysis, target_stack)
                    converted_files[file_path] = converted
                    
                except Exception as e:
                    logger.warning(f"Failed to convert {file_path}: {e}")
                    converted_files[file_path] = {
                        'error': str(e),
                        'status': 'failed'
                    }
        
        return converted_files
    
    async def _convert_single_file_content(self, file_path: str, analysis: Dict, target_stack: str) -> Dict[str, Any]:
        """Convert a single file's content to modern code."""
        try:
            # Read the actual file content
            with open(file_path, 'r', encoding='utf-8') as f:
                file_content = f.read()
            
            # Build context for conversion
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=target_stack,
                current_task="file_conversion",
                file_content=file_content,
                file_path=file_path,
                user_requirements=f"Convert this legacy file to modern {target_stack} code"
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_code_generation_prompt(
                context, target_stack, target_stack
            )
            
            # Create a specific prompt for file conversion
            conversion_prompt = f"""
{prompt_result.user_prompt}

LEGACY FILE TO CONVERT:
File: {file_path}
Content:
{file_content}

ANALYSIS:
{analysis.get('analysis', 'No analysis available')}

REQUIREMENTS:
1. Convert this specific legacy file to modern {target_stack} code
2. Replace all template variables with actual values
3. Create meaningful component names based on the file content
4. Implement the actual functionality from the legacy code
5. Use modern best practices and patterns
6. Generate complete, runnable code (not templates)

IMPORTANT: Generate ONLY the actual code content. Do NOT include:
- Markdown code blocks (```tsx, ```jsx, etc.)
- Explanatory text or commentary
- Numbered lists or step-by-step instructions
- "Here's the code:" or similar phrases

Provide ONLY the clean, runnable code that can be directly saved to a file.
"""
            
            # Get AI conversion
            conversion_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': conversion_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse conversion response
            converted = self._parse_file_conversion(conversion_response, file_path, target_stack)
            
            return converted
            
        except Exception as e:
            logger.error(f"Error converting file {file_path}: {e}")
            return {
                'error': str(e),
                'status': 'failed',
                'file_path': file_path
            }
    
    async def _generate_modern_components(self, project_map: Dict, target_stack: str) -> Dict[str, Any]:
        """Generate modern components based on project analysis."""
        try:
            # Extract component requirements from project map
            patterns = project_map.get('patterns', [])
            dependencies = project_map.get('dependencies', {})
            structure = project_map.get('structure', {})
            
            # Build context for component generation
            context = PromptContext(
                project_name=self.config.get_project_name(),
                target_stack=target_stack,
                current_task="component_generation",
                user_requirements="Generate modern components based on the analyzed patterns and dependencies"
            )
            
            # Build prompt
            prompt_result = self.prompt_builder.build_modernization_prompt(
                context, task_type="generation"
            )
            
            # Create specific component generation prompt
            component_prompt = f"""
{prompt_result.user_prompt}

PROJECT ANALYSIS:
Structure: {structure}
Patterns: {patterns}
Dependencies: {dependencies}

REQUIREMENTS:
1. Generate specific React components based on the actual project structure
2. Create separate components for: Navigation, Hero Section, Services Section, Contact Form, Footer
3. Replace all template variables with actual values
4. Implement the actual functionality from the legacy HTML
5. Use modern React patterns and best practices
6. Generate complete, runnable components (not templates)

PROJECT STRUCTURE:
Create a proper React project structure with the following files:

// src/App.jsx - Main App component
// src/index.js - Entry point (REQUIRED for Create React App)
// src/components/Navigation.jsx - Navigation component
// src/components/Hero.jsx - Hero section component
// src/components/Services.jsx - Services section component
// src/components/ContactForm.jsx - Contact form component
// src/components/Footer.jsx - Footer component
// src/styles/globals.css - Global styles
// public/index.html - HTML template
// vite.config.js - Vite configuration

IMPORTANT: You MUST include the src/index.js file as it's required for Create React App to work.

IMPORTANT: Generate ONLY the actual code content. Do NOT include:
- Markdown code blocks (```tsx, ```jsx, etc.)
- Explanatory text or commentary
- Numbered lists or step-by-step instructions
- "Here's the code:" or similar phrases

IMPORTANT: For CSS files:
- Do NOT reference image files that don't exist (like /hero-bg.jpg)
- Use CSS gradients or solid colors instead of background images
- Use Tailwind CSS classes when possible

IMPORTANT: For React components:
- Use valid href values for links (not just "#")
- Add proper accessibility attributes (target="_blank", rel="noopener noreferrer")
- Follow React best practices

Provide ONLY the clean, runnable React components that can be directly saved to the proper directory structure.
"""
            
            # Get AI response
            components_response = await self.ai.chat(
                messages=[{'role': 'user', 'content': component_prompt}],
                system_prompt=prompt_result.system_prompt
            )
            
            # Parse components
            components = self._parse_components(components_response, target_stack)
            
            return components
            
        except Exception as e:
            logger.error(f"Error generating components: {e}")
            return {
                'error': str(e),
                'status': 'failed'
            }
    
    async def _create_configuration_files(self, target_stack: str) -> Dict[str, Any]:
        """Create configuration files for the modern project."""
        try:
            config_files = {}
            
            # Generate package.json for Node.js projects
            if target_stack in ['react', 'nextjs', 'astro', 'vue', 'svelte']:
                package_json = await self._generate_package_json(target_stack)
                config_files['package.json'] = package_json
            
            # Generate configuration files based on stack
            if target_stack == 'nextjs':
                next_config = await self._generate_nextjs_config()
                config_files['next.config.js'] = next_config
            elif target_stack == 'astro':
                astro_config = await self._generate_astro_config()
                config_files['astro.config.mjs'] = astro_config
            
            # Generate TypeScript config if needed
            if self.config.get_code_quality_setting('use_typescript', True):
                ts_config = await self._generate_typescript_config(target_stack)
                config_files['tsconfig.json'] = ts_config
            
            # Generate ESLint config
            if self.config.get_code_quality_setting('use_eslint', True):
                eslint_config = await self._generate_eslint_config(target_stack)
                config_files['.eslintrc.json'] = eslint_config
            
            # Generate Prettier config
            if self.config.get_code_quality_setting('use_prettier', True):
                prettier_config = await self._generate_prettier_config()
                config_files['.prettierrc'] = prettier_config
            
            # Generate .gitignore
            gitignore = await self._generate_gitignore()
            config_files['.gitignore'] = gitignore
            
            # Generate README.md
            project_name = self.config.get_project_name()
            readme = await self._generate_readme(project_name)
            config_files['README.md'] = readme
            
            # Generate Vite config for React projects
            if target_stack == 'react':
                vite_config = await self._generate_vite_config()
                config_files['vite.config.js'] = vite_config
                
                # Generate Tailwind config
                tailwind_config = await self._generate_tailwind_config()
                config_files['tailwind.config.js'] = tailwind_config
                
                # Generate public files for React
                public_index = await self._generate_public_index_html()
                config_files['public/index.html'] = public_index
                
                public_manifest = await self._generate_public_manifest_json()
                config_files['public/manifest.json'] = public_manifest
            
            return config_files
            
        except Exception as e:
            logger.error(f"Error creating configuration files: {e}")
            return {
                'error': str(e),
                'status': 'failed'
            }
    
    async def _generate_package_json(self, target_stack: str) -> Dict[str, Any]:
        """Generate package.json for the target stack."""
        try:
            import json
            project_name = self.config.get_project_name()
            
            # Create specific package.json based on target stack
            if target_stack == 'react':
                package_json_content = {
                    "name": project_name.lower().replace(' ', '-'),
                    "version": "1.0.0",
                    "description": f"Modernized {project_name} using React",
                    "main": "src/index.js",
                    "scripts": {
                        "start": "react-scripts start",
                        "build": "react-scripts build",
                        "test": "react-scripts test",
                        "eject": "react-scripts eject"
                    },
                    "dependencies": {
                        "react": "^18.2.0",
                        "react-dom": "^18.2.0",
                        "react-scripts": "5.0.1"
                    },
                    "devDependencies": {
                        "@types/react": "^18.2.0",
                        "@types/react-dom": "^18.2.0",
                        "typescript": "^4.9.0"
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
            else:
                # Generic package.json for other frameworks
                package_json_content = {
                    "name": project_name.lower().replace(' ', '-'),
                    "version": "1.0.0",
                    "description": f"Modernized {project_name} using {target_stack}",
                    "main": "src/main.jsx",
                    "scripts": {
                        "start": "npm run dev",
                        "dev": "echo 'Add your dev script here'",
                        "build": "echo 'Add your build script here'"
                    },
                    "dependencies": {},
                    "devDependencies": {}
                }
            
            return {
                'status': 'success',
                'content': json.dumps(package_json_content, indent=2),
                'target_stack': target_stack
            }
            
        except Exception as e:
            logger.error(f"Error generating package.json: {e}")
            return {
                'error': str(e),
                'status': 'failed'
            }
    
    def _parse_project_structure(self, response: str, target_stack: str) -> Dict[str, Any]:
        """Parse the AI response for project structure."""
        try:
            # This is a simplified implementation
            # In a real system, you'd use more sophisticated parsing
            structure = {
                'status': 'success',
                'target_stack': target_stack,
                'structure': response,
                'directories': self._extract_directories_from_response(response),
                'files': self._extract_files_from_response(response)
            }
            
            return structure
            
        except Exception as e:
            logger.error(f"Error parsing project structure: {e}")
            return {
                'status': 'failed',
                'error': str(e)
            }
    
    def _parse_file_conversion(self, response: str, file_path: str, target_stack: str) -> Dict[str, Any]:
        """Parse the AI response for file conversion."""
        try:
            from pathlib import Path
            import re
            
            # Extract actual code content from LLM response
            # Remove markdown code blocks and LLM commentary
            code_content = self._extract_code_from_response(response)
            
            if not code_content:
                return {
                    'status': 'failed',
                    'error': 'No code content found in response',
                    'original_file': file_path
                }
            
            converted = {
                'status': 'success',
                'original_file': file_path,
                'target_stack': target_stack,
                'converted_content': code_content,
                'new_file_path': self._determine_new_file_path(file_path, target_stack),
                'conversion_notes': self._extract_conversion_notes(response)
            }
            
            return converted
            
        except Exception as e:
            logger.error(f"Error parsing file conversion: {e}")
            return {
                'status': 'failed',
                'error': str(e),
                'original_file': file_path
            }
    
    def _extract_code_from_response(self, response: str) -> str:
        """Extract actual code content from LLM response, removing markdown and commentary."""
        import re
        
        # Remove LLM commentary before code blocks
        lines = response.split('\n')
        code_lines = []
        in_code_block = False
        code_block_content = []
        
        for line in lines:
            # Check for code block start
            if line.strip().startswith('```'):
                if not in_code_block:
                    in_code_block = True
                    code_block_content = []
                else:
                    # End of code block, add the content
                    if code_block_content:
                        code_lines.extend(code_block_content)
                        code_lines.append('')  # Add empty line between blocks
                    in_code_block = False
                continue
            
            if in_code_block:
                code_block_content.append(line)
            elif not line.strip().startswith(('I\'ll help', 'Based on', 'Here\'s', 'These components', 'To use', 'This modernized')):
                # Skip common LLM commentary lines
                if line.strip() and not line.strip().isdigit() and not line.strip().endswith(':'):
                    code_lines.append(line)
        
        # If we have code content, return it
        if code_lines:
            return '\n'.join(code_lines).strip()
        
        # Fallback: try to extract any code-like content
        code_pattern = r'```(?:tsx?|jsx?|js|ts)?\s*\n(.*?)\n```'
        matches = re.findall(code_pattern, response, re.DOTALL)
        if matches:
            return '\n\n'.join(matches).strip()
        
        return response.strip()
    
    def _split_components_from_response(self, response: str) -> Dict[str, str]:
        """Split multiple components from a single response into separate files."""
        import re
        
        components = {}
        lines = response.split('\n')
        current_component = None
        current_content = []
        
        for line in lines:
            # Look for component file headers (e.g., "// src/App.jsx", "// src/components/Navigation.jsx")
            if line.strip().startswith('//') and ('.tsx' in line or '.jsx' in line or '.css' in line or '.html' in line or '.js' in line):
                # Save previous component if exists
                if current_component and current_content:
                    components[current_component] = '\n'.join(current_content).strip()
                
                # Start new component
                component_path = line.strip()[2:].strip()  # Remove "// "
                current_component = component_path
                current_content = []
            elif current_component:
                current_content.append(line)
        
        # Save the last component
        if current_component and current_content:
            components[current_component] = '\n'.join(current_content).strip()
        
        # If no components found, treat the whole response as a single component
        if not components:
            clean_content = self._extract_code_from_response(response)
            if clean_content:
                components['src/index.js'] = clean_content
        
        return components
    
    def _parse_components(self, response: str, target_stack: str) -> Dict[str, Any]:
        """Parse the AI response for components."""
        try:
            # Split components into separate files
            component_files = self._split_components_from_response(response)
            
            if not component_files:
                return {
                    'status': 'failed',
                    'error': 'No component code found in response'
                }
            
            components = {
                'status': 'success',
                'target_stack': target_stack,
                'component_files': component_files,
                'component_list': list(component_files.keys())
            }
            
            return components
            
        except Exception as e:
            logger.error(f"Error parsing components: {e}")
            return {
                'status': 'failed',
                'error': str(e)
            }
    
    def _parse_package_json(self, response: str, target_stack: str) -> Dict[str, Any]:
        """Parse the AI response for package.json."""
        try:
            # This is a simplified implementation
            package_json = {
                'status': 'success',
                'target_stack': target_stack,
                'package_json': response,
                'dependencies': self._extract_dependencies_from_package_json(response)
            }
            
            return package_json
            
        except Exception as e:
            logger.error(f"Error parsing package.json: {e}")
            return {
                'status': 'failed',
                'error': str(e)
            }
    
    def _extract_directories_from_response(self, response: str) -> List[str]:
        """Extract directory structure from AI response."""
        # This is a simplified implementation
        directories = []
        
        # Basic extraction logic
        lines = response.split('\n')
        for line in lines:
            if '/' in line and not line.startswith('#'):
                directories.append(line.strip())
        
        return directories
    
    def _extract_files_from_response(self, response: str) -> List[str]:
        """Extract file list from AI response."""
        # This is a simplified implementation
        files = []
        
        # Basic extraction logic
        lines = response.split('\n')
        for line in lines:
            if '.' in line and not line.startswith('#'):
                files.append(line.strip())
        
        return files
    
    def _determine_new_file_path(self, original_path: str, target_stack: str) -> str:
        """Determine the new file path for converted code."""
        from pathlib import Path
        
        # Extract file name and extension
        file_name = Path(original_path).stem
        file_ext = Path(original_path).suffix
        
        # Map extensions to modern equivalents
        extension_map = {
            '.html': '.jsx' if target_stack == 'react' else '.js',
            '.js': '.jsx' if target_stack == 'react' else '.js',
            '.py': '.jsx' if target_stack == 'react' else '.js',
            '.php': '.jsx' if target_stack == 'react' else '.js'
        }
        
        new_ext = extension_map.get(file_ext, '.jsx' if target_stack == 'react' else '.js')
        
        # Create meaningful component name based on file content analysis
        if file_ext == '.html':
            # For HTML files, try to extract meaningful name from content
            try:
                with open(original_path, 'r', encoding='utf-8') as f:
                    content = f.read()
                    # Look for title tag
                    import re
                    title_match = re.search(r'<title[^>]*>([^<]+)</title>', content, re.IGNORECASE)
                    if title_match:
                        title = title_match.group(1).strip()
                        # Clean up title for component name
                        component_name = re.sub(r'[^a-zA-Z0-9\s]', '', title)
                        component_name = component_name.replace(' ', '').title()
                        if component_name:
                            return f"{component_name}{new_ext}"
            except:
                pass
            
            # Fallback for HTML files
            if file_name.lower() in ['index', 'main', 'app', 'legacy-site']:
                return f"App{new_ext}"
            else:
                return f"{file_name.title()}{new_ext}"
        else:
            # For other files, use the original name
            component_name = file_name.replace('_', '').title()
            if component_name.lower() in ['index', 'main', 'app']:
                component_name = 'App'
            
            return f"{component_name}{new_ext}"
    
    def _extract_conversion_notes(self, response: str) -> List[str]:
        """Extract conversion notes from AI response."""
        # This is a simplified implementation
        notes = []
        
        # Basic extraction logic
        if 'note:' in response.lower() or 'todo:' in response.lower():
            notes.append('Conversion notes extracted')
        
        return notes
    
    def _extract_component_list(self, response: str) -> List[str]:
        """Extract component list from AI response."""
        # This is a simplified implementation
        components = []
        
        # Basic extraction logic
        if 'component' in response.lower():
            components.append('Component extracted')
        
        return components
    
    def _extract_dependencies_from_package_json(self, response: str) -> Dict[str, str]:
        """Extract dependencies from package.json response."""
        # This is a simplified implementation
        dependencies = {}
        
        # Basic extraction logic
        if 'dependencies' in response.lower():
            dependencies['example'] = '^1.0.0'
        
        return dependencies
    
    async def _generate_nextjs_config(self) -> Dict[str, Any]:
        """Generate Next.js configuration."""
        return {
            'status': 'success',
            'content': '// Next.js configuration\nmodule.exports = {\n  // Configuration options\n}'
        }
    
    async def _generate_astro_config(self) -> Dict[str, Any]:
        """Generate Astro configuration."""
        return {
            'status': 'success',
            'content': '// Astro configuration\nexport default {\n  // Configuration options\n}'
        }
    
    async def _generate_typescript_config(self, target_stack: str) -> Dict[str, Any]:
        """Generate TypeScript configuration."""
        return {
            'status': 'success',
            'content': '{\n  "compilerOptions": {\n    "target": "es5",\n    "lib": ["dom", "dom.iterable", "es6"],\n    "allowJs": true,\n    "skipLibCheck": true,\n    "esModuleInterop": true,\n    "allowSyntheticDefaultImports": true,\n    "strict": true,\n    "forceConsistentCasingInFileNames": true,\n    "noFallthroughCasesInSwitch": true,\n    "module": "esnext",\n    "moduleResolution": "node",\n    "resolveJsonModule": true,\n    "isolatedModules": true,\n    "noEmit": true,\n    "jsx": "react-jsx",\n    "baseUrl": ".",\n    "paths": {\n      "@/*": ["src/*"]\n    }\n  },\n  "include": ["src/**/*", "*.tsx", "*.ts", "*.jsx", "*.js"],\n  "exclude": ["node_modules", "dist", "build"]\n}'
        }
    
    async def _generate_eslint_config(self, target_stack: str) -> Dict[str, Any]:
        """Generate ESLint configuration."""
        return {
            'status': 'success',
            'content': '{\n  "extends": ["react-app", "react-app/jest"]\n}'
        }
    
    async def _generate_prettier_config(self) -> Dict[str, Any]:
        """Generate Prettier configuration."""
        return {
            'status': 'success',
            'content': '{\n  "semi": true,\n  "trailingComma": "es5",\n  "singleQuote": true,\n  "printWidth": 80,\n  "tabWidth": 2\n}'
        }
    
    async def _generate_gitignore(self) -> Dict[str, Any]:
        """Generate .gitignore file."""
        return {
            'status': 'success',
            'content': '# Dependencies\nnode_modules/\npnpm-debug.log*\nyarn-debug.log*\nyarn-error.log*\n\n# Production\nbuild/\ndist/\n\n# Environment variables\n.env\n.env.local\n.env.development.local\n.env.test.local\n.env.production.local\n\n# IDE\n.vscode/\n.idea/\n*.swp\n*.swo\n\n# OS\n.DS_Store\nThumbs.db\n\n# Logs\nnpm-debug.log*\nyarn-debug.log*\nyarn-error.log*\n\n# Runtime data\npids\n*.pid\n*.seed\n*.pid.lock\n\n# Coverage directory used by tools like istanbul\ncoverage/\n\n# nyc test coverage\n.nyc_output\n\n# Dependency directories\njspm_packages/\n\n# Optional npm cache directory\n.npm\n\n# Optional eslint cache\n.eslintcache\n\n# Microbundle cache\n.rpt2_cache/\n.rts2_cache_cjs/\n.rts2_cache_es/\n.rts2_cache_umd/\n\n# Optional REPL history\n.node_repl_history\n\n# Output of \'npm pack\'\n*.tgz\n\n# Yarn Integrity file\n.yarn-integrity\n\n# parcel-bundler cache (https://parceljs.org/)\n.cache\n.parcel-cache\n\n# Next.js build output\n.next\n\n# Nuxt.js build / generate output\n.nuxt\ndist\n\n# Storybook build outputs\n.out\n.storybook-out\n\n# Temporary folders\ntmp/\ntemp/\n'
        }
    
    async def _generate_readme(self, project_name: str) -> Dict[str, Any]:
        """Generate README.md file."""
        return {
            'status': 'success',
            'content': f'# {project_name}\n\nThis is a modernized React application generated by Legacy2Modern.\n\n## Getting Started\n\n### Prerequisites\n\n- Node.js (version 16 or higher)\n- npm or yarn\n\n### Installation\n\n1. Install dependencies:\n```bash\nnpm install\n```\n\n2. Start the development server:\n```bash\nnpm start\n```\n\n3. Open [http://localhost:3000](http://localhost:3000) to view it in the browser.\n\n## Available Scripts\n\n- `npm start` - Runs the app in development mode\n- `npm run build` - Builds the app for production\n- `npm test` - Launches the test runner\n- `npm run eject` - Ejects from Create React App\n\n## Project Structure\n\n```\nproject-root/\n├── public/          # Static files\n├── src/             # Source code\n│   ├── components/  # Reusable components\n│   ├── pages/       # Page components\n│   ├── styles/      # CSS files\n│   └── utils/       # Utility functions\n└── package.json     # Dependencies and scripts\n```\n\n## Technologies Used\n\n- React 18\n- TypeScript\n- Tailwind CSS\n- Vite (for development)\n\n## Contributing\n\n1. Fork the repository\n2. Create your feature branch (`git checkout -b feature/amazing-feature`)\n3. Commit your changes (`git commit -m \'Add some amazing feature\'`)\n4. Push to the branch (`git push origin feature/amazing-feature`)\n5. Open a Pull Request\n\n## License\n\nThis project is licensed under the MIT License.\n'
        }
    
    async def _generate_vite_config(self) -> Dict[str, Any]:
        """Generate Vite configuration."""
        return {
            'status': 'success',
            'content': 'import { defineConfig } from \'vite\'\nimport react from \'@vitejs/plugin-react\'\nimport path from \'path\'\n\n// https://vitejs.dev/config/\nexport default defineConfig({\n  plugins: [react()],\n  resolve: {\n    alias: {\n      \'@\': path.resolve(__dirname, \'./src\'),\n    },\n  },\n  server: {\n    port: 3000,\n    open: true,\n  },\n  build: {\n    outDir: \'dist\',\n    sourcemap: true,\n  },\n})\n'
        }
    
    async def _generate_tailwind_config(self) -> Dict[str, Any]:
        """Generate Tailwind CSS configuration."""
        return {
            'status': 'success',
            'content': '/** @type {import(\'tailwindcss\').Config} */\nexport default {\n  content: [\n    "./index.html",\n    "./src/**/*.{js,ts,jsx,tsx}",\n  ],\n  theme: {\n    extend: {\n      colors: {\n        primary: {\n          50: \'#eff6ff\',\n          500: \'#3b82f6\',\n          600: \'#2563eb\',\n          700: \'#1d4ed8\',\n        },\n      },\n    },\n  },\n  plugins: [],\n}\n'
        }
    
    async def _generate_public_index_html(self) -> Dict[str, Any]:
        """Generate public/index.html file."""
        return {
            'status': 'success',
            'content': '<!DOCTYPE html>\n<html lang="en">\n  <head>\n    <meta charset="utf-8" />\n    <link rel="icon" href="%PUBLIC_URL%/favicon.ico" />\n    <meta name="viewport" content="width=device-width, initial-scale=1" />\n    <meta name="theme-color" content="#000000" />\n    <meta\n      name="description"\n      content="Modernized React application"\n    />\n    <link rel="apple-touch-icon" href="%PUBLIC_URL%/logo192.png" />\n    <link rel="manifest" href="%PUBLIC_URL%/manifest.json" />\n    <title>Modernized React App</title>\n  </head>\n  <body>\n    <noscript>You need to enable JavaScript to run this app.</noscript>\n    <div id="root"></div>\n  </body>\n</html>'
        }
    
    async def _generate_public_manifest_json(self) -> Dict[str, Any]:
        """Generate public/manifest.json file."""
        return {
            'status': 'success',
            'content': '{\n  "short_name": "React App",\n  "name": "Modernized React Application",\n  "icons": [\n    {\n      "src": "favicon.ico",\n      "sizes": "64x64 32x32 24x24 16x16",\n      "type": "image/x-icon"\n    }\n  ],\n  "start_url": ".",\n  "display": "standalone",\n  "theme_color": "#000000",\n  "background_color": "#ffffff"\n}'
        }
    
    async def _generate_public_favicon(self) -> Dict[str, Any]:
        """Generate a simple favicon.ico placeholder."""
        return {
            'status': 'success',
            'content': '<!-- This is a placeholder for favicon.ico -->\n<!-- In a real project, you would have an actual .ico file here -->\n<!-- For now, this file serves as a marker that favicon.ico should exist -->',
            'is_binary': True
        }
    
    async def _generate_code(self, source_file: str, target_stack: str) -> Dict[str, Any]:
        """Generate modern code from a source file."""
        return await self._convert_single_file_content(source_file, {}, target_stack)
    
    async def _convert_file(self, file_path: str, content: str) -> Dict[str, Any]:
        """Convert a file with given content."""
        analysis = {'analysis': content, 'status': 'success'}
        return await self._convert_single_file_content(file_path, analysis, self.config.get_target_stack())
    
    async def _create_component(self, component_name: str, specs: Dict) -> Dict[str, Any]:
        """Create a modern component."""
        # This would be implemented to create components based on specifications
        return {
            'status': 'success',
            'component_name': component_name,
            'specs': specs,
            'generated_component': f'// Generated component: {component_name}'
        }
    
    async def _generate_project_structure(self, project_map: Dict) -> Dict[str, Any]:
        """Generate project structure from project map."""
        return await self._generate_modern_project_structure(project_map, self.config.get_target_stack())
    
    async def _convert_single_file(self, task: Dict) -> Dict[str, Any]:
        """Convert a single file task."""
        return await self._convert_single_file_content(
            task.get('file_path', ''),
            task.get('analysis', {}),
            task.get('target_stack', self.config.get_target_stack())
        )
    
    async def _generate_components(self, task: Dict) -> Dict[str, Any]:
        """Generate components task."""
        project_map = task.get('project_map', {})
        return await self._generate_modern_components(project_map, self.config.get_target_stack())
    
    async def _setup_modern_project(self, task: Dict) -> Dict[str, Any]:
        """Setup modern project task."""
        return await self._create_configuration_files(self.config.get_target_stack()) 