"""
Stores configuration for target stack, hosting, and other project settings.
"""

import json
import logging
import os
from typing import Dict, List, Optional, Any, Union
from pathlib import Path
from dataclasses import dataclass, field, asdict
from enum import Enum

logger = logging.getLogger(__name__)

class TargetStack(Enum):
    """Supported target technology stacks."""
    REACT = "react"
    NEXTJS = "nextjs"
    ASTRO = "astro"
    VUE = "vue"
    SVELTE = "svelte"
    ANGULAR = "angular"
    VANILLA = "vanilla"

class HostingPlatform(Enum):
    """Supported hosting platforms."""
    VERCEL = "vercel"
    NETLIFY = "netlify"
    GITHUB_PAGES = "github_pages"
    AWS_S3 = "aws_s3"
    FIREBASE = "firebase"
    CLOUDFLARE = "cloudflare"
    CUSTOM = "custom"

class StylingFramework(Enum):
    """Supported styling frameworks."""
    TAILWIND = "tailwind"
    BOOTSTRAP = "bootstrap"
    MATERIAL_UI = "material_ui"
    CHAKRA_UI = "chakra_ui"
    ANT_DESIGN = "ant_design"
    VANILLA_CSS = "vanilla_css"
    SCSS = "scss"
    STYLED_COMPONENTS = "styled_components"

@dataclass
class ModernizationSettings:
    """Settings for the modernization process."""
    preserve_structure: bool = True
    maintain_seo: bool = True
    optimize_performance: bool = True
    add_analytics: bool = False
    add_pwa_features: bool = False
    enable_ssr: bool = False
    enable_ssg: bool = True
    minify_output: bool = True
    generate_sitemap: bool = True
    generate_robots_txt: bool = True

@dataclass
class CodeQualitySettings:
    """Settings for code quality and formatting."""
    use_typescript: bool = True
    use_eslint: bool = True
    use_prettier: bool = True
    use_husky: bool = False
    use_lint_staged: bool = False
    add_tests: bool = False
    test_framework: str = "jest"
    coverage_threshold: int = 80

@dataclass
class DeploymentSettings:
    """Settings for deployment and hosting."""
    auto_deploy: bool = False
    build_command: str = "npm run build"
    output_directory: str = "dist"
    environment_variables: Dict[str, str] = field(default_factory=dict)
    custom_domain: Optional[str] = None
    ssl_enabled: bool = True

class ProjectConfig:
    """
    Manages project configuration for modernization.
    
    Stores and manages settings for target technology stack, hosting,
    code quality, and other project-specific configurations.
    """
    
    def __init__(self, config_file: Optional[str] = None):
        self.config_file = config_file or "l2m_config.json"
        self.config: Dict[str, Any] = {}
        
        # Default configuration
        self._set_defaults()
        
        # Load existing configuration if available
        self.load_config()
    
    def _set_defaults(self):
        """Set default configuration values."""
        self.config = {
            'project_name': '',
            'target_stack': TargetStack.REACT.value,
            'hosting_platform': HostingPlatform.VERCEL.value,
            'styling_framework': StylingFramework.TAILWIND.value,
            'modernization_settings': asdict(ModernizationSettings()),
            'code_quality_settings': asdict(CodeQualitySettings()),
            'deployment_settings': asdict(DeploymentSettings()),
            'custom_settings': {},
            'metadata': {
                'created_at': '',
                'last_modified': '',
                'version': '1.0.0'
            }
        }
    
    def load_config(self):
        """Load configuration from file."""
        try:
            if os.path.exists(self.config_file):
                with open(self.config_file, 'r', encoding='utf-8') as f:
                    loaded_config = json.load(f)
                    
                    # Merge with defaults, preserving existing values
                    self._merge_config(loaded_config)
                    logger.info(f"Loaded configuration from {self.config_file}")
            else:
                logger.info("No existing configuration found, using defaults")
                
        except Exception as e:
            logger.error(f"Failed to load configuration: {e}")
    
    def save_config(self):
        """Save configuration to file."""
        try:
            # Update metadata
            import datetime
            if not self.config['metadata']['created_at']:
                self.config['metadata']['created_at'] = datetime.datetime.now().isoformat()
            self.config['metadata']['last_modified'] = datetime.datetime.now().isoformat()
            
            with open(self.config_file, 'w', encoding='utf-8') as f:
                json.dump(self.config, f, indent=2, default=str)
            
            logger.info(f"Configuration saved to {self.config_file}")
            
        except Exception as e:
            logger.error(f"Failed to save configuration: {e}")
    
    def _merge_config(self, loaded_config: Dict[str, Any]):
        """Merge loaded configuration with defaults."""
        for key, value in loaded_config.items():
            if key in self.config:
                if isinstance(value, dict) and isinstance(self.config[key], dict):
                    # Recursively merge dictionaries
                    self.config[key].update(value)
                else:
                    # Replace non-dict values
                    self.config[key] = value
            else:
                # Add new keys
                self.config[key] = value
    
    # Project information
    def set_project_name(self, name: str):
        """Set the project name."""
        self.config['project_name'] = name
    
    def get_project_name(self) -> str:
        """Get the project name."""
        return self.config.get('project_name', '')
    
    # Target stack configuration
    def set_target_stack(self, stack: Union[TargetStack, str]):
        """Set the target technology stack."""
        if isinstance(stack, TargetStack):
            self.config['target_stack'] = stack.value
        else:
            self.config['target_stack'] = stack
    
    def get_target_stack(self) -> str:
        """Get the target technology stack."""
        return self.config.get('target_stack', TargetStack.REACT.value)
    
    def is_target_stack(self, stack: Union[TargetStack, str]) -> bool:
        """Check if the target stack matches the given stack."""
        current_stack = self.get_target_stack()
        if isinstance(stack, TargetStack):
            return current_stack == stack.value
        return current_stack == stack
    
    # Hosting configuration
    def set_hosting_platform(self, platform: Union[HostingPlatform, str]):
        """Set the hosting platform."""
        if isinstance(platform, HostingPlatform):
            self.config['hosting_platform'] = platform.value
        else:
            self.config['hosting_platform'] = platform
    
    def get_hosting_platform(self) -> str:
        """Get the hosting platform."""
        return self.config.get('hosting_platform', HostingPlatform.VERCEL.value)
    
    # Styling framework configuration
    def set_styling_framework(self, framework: Union[StylingFramework, str]):
        """Set the styling framework."""
        if isinstance(framework, StylingFramework):
            self.config['styling_framework'] = framework.value
        else:
            self.config['styling_framework'] = framework
    
    def get_styling_framework(self) -> str:
        """Get the styling framework."""
        return self.config.get('styling_framework', StylingFramework.TAILWIND.value)
    
    # Modernization settings
    def get_modernization_settings(self) -> Dict[str, Any]:
        """Get modernization settings."""
        return self.config.get('modernization_settings', {})
    
    def set_modernization_setting(self, key: str, value: Any):
        """Set a specific modernization setting."""
        if 'modernization_settings' not in self.config:
            self.config['modernization_settings'] = {}
        self.config['modernization_settings'][key] = value
    
    def get_modernization_setting(self, key: str, default: Any = None) -> Any:
        """Get a specific modernization setting."""
        settings = self.get_modernization_settings()
        return settings.get(key, default)
    
    # Code quality settings
    def get_code_quality_settings(self) -> Dict[str, Any]:
        """Get code quality settings."""
        return self.config.get('code_quality_settings', {})
    
    def set_code_quality_setting(self, key: str, value: Any):
        """Set a specific code quality setting."""
        if 'code_quality_settings' not in self.config:
            self.config['code_quality_settings'] = {}
        self.config['code_quality_settings'][key] = value
    
    def get_code_quality_setting(self, key: str, default: Any = None) -> Any:
        """Get a specific code quality setting."""
        settings = self.get_code_quality_settings()
        return settings.get(key, default)
    
    # Deployment settings
    def get_deployment_settings(self) -> Dict[str, Any]:
        """Get deployment settings."""
        return self.config.get('deployment_settings', {})
    
    def set_deployment_setting(self, key: str, value: Any):
        """Set a specific deployment setting."""
        if 'deployment_settings' not in self.config:
            self.config['deployment_settings'] = {}
        self.config['deployment_settings'][key] = value
    
    def get_deployment_setting(self, key: str, default: Any = None) -> Any:
        """Get a specific deployment setting."""
        settings = self.get_deployment_settings()
        return settings.get(key, default)
    
    # Custom settings
    def set_custom_setting(self, key: str, value: Any):
        """Set a custom setting."""
        if 'custom_settings' not in self.config:
            self.config['custom_settings'] = {}
        self.config['custom_settings'][key] = value
    
    def get_custom_setting(self, key: str, default: Any = None) -> Any:
        """Get a custom setting."""
        custom_settings = self.config.get('custom_settings', {})
        return custom_settings.get(key, default)
    
    def get_all_custom_settings(self) -> Dict[str, Any]:
        """Get all custom settings."""
        return self.config.get('custom_settings', {})
    
    # Configuration validation
    def validate_config(self) -> Dict[str, Any]:
        """Validate the current configuration."""
        errors = []
        warnings = []
        
        # Check required fields
        if not self.get_project_name():
            errors.append("Project name is required")
        
        # Validate target stack
        target_stack = self.get_target_stack()
        valid_stacks = [stack.value for stack in TargetStack]
        if target_stack not in valid_stacks:
            errors.append(f"Invalid target stack: {target_stack}")
        
        # Validate hosting platform
        hosting_platform = self.get_hosting_platform()
        valid_platforms = [platform.value for platform in HostingPlatform]
        if hosting_platform not in valid_platforms:
            errors.append(f"Invalid hosting platform: {hosting_platform}")
        
        # Validate styling framework
        styling_framework = self.get_styling_framework()
        valid_frameworks = [framework.value for framework in StylingFramework]
        if styling_framework not in valid_frameworks:
            errors.append(f"Invalid styling framework: {styling_framework}")
        
        # Check for potential conflicts
        if self.get_modernization_setting('enable_ssr') and self.get_modernization_setting('enable_ssg'):
            warnings.append("Both SSR and SSG are enabled - this may cause conflicts")
        
        if self.get_code_quality_setting('use_typescript') and not self.get_target_stack() in ['react', 'nextjs', 'vue', 'angular']:
            warnings.append("TypeScript may not be fully supported for the selected target stack")
        
        return {
            'valid': len(errors) == 0,
            'errors': errors,
            'warnings': warnings
        }
    
    # Configuration export/import
    def export_config(self, file_path: str):
        """Export configuration to a file."""
        try:
            with open(file_path, 'w', encoding='utf-8') as f:
                json.dump(self.config, f, indent=2, default=str)
            logger.info(f"Configuration exported to {file_path}")
        except Exception as e:
            logger.error(f"Failed to export configuration: {e}")
    
    def import_config(self, file_path: str):
        """Import configuration from a file."""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                imported_config = json.load(f)
            
            self._merge_config(imported_config)
            logger.info(f"Configuration imported from {file_path}")
        except Exception as e:
            logger.error(f"Failed to import configuration: {e}")
    
    # Configuration reset
    def reset_config(self):
        """Reset configuration to defaults."""
        self._set_defaults()
        logger.info("Configuration reset to defaults")
    
    # Configuration summary
    def get_config_summary(self) -> Dict[str, Any]:
        """Get a summary of the current configuration."""
        validation = self.validate_config()
        
        return {
            'project_name': self.get_project_name(),
            'target_stack': self.get_target_stack(),
            'hosting_platform': self.get_hosting_platform(),
            'styling_framework': self.get_styling_framework(),
            'modernization_settings_count': len(self.get_modernization_settings()),
            'code_quality_settings_count': len(self.get_code_quality_settings()),
            'deployment_settings_count': len(self.get_deployment_settings()),
            'custom_settings_count': len(self.get_all_custom_settings()),
            'validation': validation,
            'metadata': self.config.get('metadata', {})
        }
    
    # Utility methods
    def get_package_manager(self) -> str:
        """Get the recommended package manager for the target stack."""
        target_stack = self.get_target_stack()
        
        if target_stack in ['nextjs', 'react', 'vue', 'angular']:
            return 'npm'
        elif target_stack == 'astro':
            return 'npm'
        elif target_stack == 'svelte':
            return 'npm'
        else:
            return 'npm'
    
    def get_build_tool(self) -> str:
        """Get the recommended build tool for the target stack."""
        target_stack = self.get_target_stack()
        
        if target_stack == 'nextjs':
            return 'next'
        elif target_stack == 'astro':
            return 'astro'
        elif target_stack == 'vue':
            return 'vite'
        elif target_stack == 'svelte':
            return 'vite'
        elif target_stack == 'angular':
            return 'angular'
        else:
            return 'vite'
    
    def get_framework_version(self) -> str:
        """Get the recommended framework version for the target stack."""
        target_stack = self.get_target_stack()
        
        versions = {
            'react': '^18.0.0',
            'nextjs': '^14.0.0',
            'astro': '^4.0.0',
            'vue': '^3.0.0',
            'svelte': '^4.0.0',
            'angular': '^17.0.0'
        }
        
        return versions.get(target_stack, 'latest') 