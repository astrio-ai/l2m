"""
Bootstrap to Tailwind CSS transformation rules.
"""

from typing import Dict, List, Any
import re


class BootstrapRules:
    """
    Rules for transforming Bootstrap classes to Tailwind CSS.
    """
    
    def __init__(self):
        self.bootstrap_to_tailwind = {
            # Grid system
            'container': 'container mx-auto px-4',
            'row': 'flex flex-wrap -mx-4',
            'col-xs-12': 'w-full px-4',
            'col-sm-6': 'w-full md:w-1/2 px-4',
            'col-md-4': 'w-full md:w-1/3 px-4',
            'col-lg-3': 'w-full md:w-1/4 px-4',
            'col-xl-2': 'w-full lg:w-1/6 px-4',
            
            # Components
            'btn': 'px-4 py-2 rounded font-medium',
            'btn-primary': 'bg-blue-600 text-white hover:bg-blue-700',
            'btn-secondary': 'bg-gray-600 text-white hover:bg-gray-700',
            'btn-success': 'bg-green-600 text-white hover:bg-green-700',
            'btn-danger': 'bg-red-600 text-white hover:bg-red-700',
            'btn-warning': 'bg-yellow-600 text-white hover:bg-yellow-700',
            'btn-info': 'bg-blue-500 text-white hover:bg-blue-600',
            'btn-light': 'bg-gray-200 text-gray-800 hover:bg-gray-300',
            'btn-dark': 'bg-gray-800 text-white hover:bg-gray-900',
            'btn-outline-primary': 'border border-blue-600 text-blue-600 hover:bg-blue-600 hover:text-white',
            'btn-outline-secondary': 'border border-gray-600 text-gray-600 hover:bg-gray-600 hover:text-white',
            
            # Navigation
            'navbar': 'bg-white shadow-lg',
            'navbar-nav': 'flex space-x-4',
            'nav-item': 'px-3 py-2',
            'nav-link': 'text-gray-700 hover:text-gray-900',
            'navbar-brand': 'text-xl font-bold text-gray-900',
            'navbar-toggler': 'md:hidden',
            'navbar-collapse': 'hidden md:flex',
            
            # Forms
            'form-control': 'w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500',
            'form-group': 'mb-4',
            'form-label': 'block text-sm font-medium text-gray-700 mb-2',
            'form-check': 'flex items-center',
            'form-check-input': 'h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded',
            'form-check-label': 'ml-2 block text-sm text-gray-900',
            
            # Alerts
            'alert': 'px-4 py-3 rounded',
            'alert-success': 'bg-green-100 text-green-800 border border-green-200',
            'alert-danger': 'bg-red-100 text-red-800 border border-red-200',
            'alert-warning': 'bg-yellow-100 text-yellow-800 border border-yellow-200',
            'alert-info': 'bg-blue-100 text-blue-800 border border-blue-200',
            
            # Cards
            'card': 'bg-white rounded-lg shadow-md',
            'card-body': 'p-6',
            'card-title': 'text-xl font-semibold text-gray-900 mb-2',
            'card-text': 'text-gray-600',
            'card-header': 'px-6 py-4 border-b border-gray-200 bg-gray-50',
            'card-footer': 'px-6 py-4 border-t border-gray-200 bg-gray-50',
            
            # Utilities
            'text-center': 'text-center',
            'text-left': 'text-left',
            'text-right': 'text-right',
            'text-justify': 'text-justify',
            'mt-3': 'mt-3',
            'mb-3': 'mb-3',
            'ml-3': 'ml-3',
            'mr-3': 'mr-3',
            'p-3': 'p-3',
            'pt-3': 'pt-3',
            'pb-3': 'pb-3',
            'pl-3': 'pl-3',
            'pr-3': 'pr-3',
            'm-3': 'm-3',
            'd-none': 'hidden',
            'd-block': 'block',
            'd-flex': 'flex',
            'd-inline': 'inline',
            'd-inline-block': 'inline-block',
            'd-grid': 'grid',
            'justify-content-center': 'justify-center',
            'justify-content-start': 'justify-start',
            'justify-content-end': 'justify-end',
            'justify-content-between': 'justify-between',
            'align-items-center': 'items-center',
            'align-items-start': 'items-start',
            'align-items-end': 'items-end',
            'flex-column': 'flex-col',
            'flex-row': 'flex-row',
            'flex-wrap': 'flex-wrap',
            'flex-nowrap': 'flex-nowrap',
            'w-100': 'w-full',
            'h-100': 'h-full',
            'position-relative': 'relative',
            'position-absolute': 'absolute',
            'position-fixed': 'fixed',
            'position-sticky': 'sticky',
            'top-0': 'top-0',
            'bottom-0': 'bottom-0',
            'start-0': 'left-0',
            'end-0': 'right-0',
            'border': 'border',
            'border-0': 'border-0',
            'border-top': 'border-t',
            'border-bottom': 'border-b',
            'border-start': 'border-l',
            'border-end': 'border-r',
            'rounded': 'rounded',
            'rounded-0': 'rounded-none',
            'rounded-1': 'rounded-sm',
            'rounded-2': 'rounded',
            'rounded-3': 'rounded-md',
            'rounded-circle': 'rounded-full',
            'shadow': 'shadow',
            'shadow-sm': 'shadow-sm',
            'shadow-lg': 'shadow-lg',
            'shadow-none': 'shadow-none',
            'bg-primary': 'bg-blue-600',
            'bg-secondary': 'bg-gray-600',
            'bg-success': 'bg-green-600',
            'bg-danger': 'bg-red-600',
            'bg-warning': 'bg-yellow-600',
            'bg-info': 'bg-blue-500',
            'bg-light': 'bg-gray-200',
            'bg-dark': 'bg-gray-800',
            'text-primary': 'text-blue-600',
            'text-secondary': 'text-gray-600',
            'text-success': 'text-green-600',
            'text-danger': 'text-red-600',
            'text-warning': 'text-yellow-600',
            'text-info': 'text-blue-500',
            'text-light': 'text-gray-200',
            'text-dark': 'text-gray-800',
            'text-muted': 'text-gray-500',
            'text-white': 'text-white',
            'text-black': 'text-black',
            'text-body': 'text-gray-900',
            'text-opacity-75': 'text-opacity-75',
            'text-opacity-50': 'text-opacity-50',
            'text-opacity-25': 'text-opacity-25',
            'display-1': 'text-6xl font-bold',
            'display-2': 'text-5xl font-bold',
            'display-3': 'text-4xl font-bold',
            'display-4': 'text-3xl font-bold',
            'display-5': 'text-2xl font-bold',
            'display-6': 'text-xl font-bold',
            'lead': 'text-xl text-gray-600',
            'small': 'text-sm',
            'mark': 'bg-yellow-200 px-1',
            'text-decoration-underline': 'underline',
            'text-decoration-line-through': 'line-through',
            'text-lowercase': 'lowercase',
            'text-uppercase': 'uppercase',
            'text-capitalize': 'capitalize',
            'fw-bold': 'font-bold',
            'fw-bolder': 'font-black',
            'fw-normal': 'font-normal',
            'fw-light': 'font-light',
            'fw-lighter': 'font-thin',
            'fst-italic': 'italic',
            'fst-normal': 'not-italic',
            'text-start': 'text-left',
            'text-end': 'text-right',
            'text-break': 'break-words',
            'text-reset': 'text-current',
            'text-decoration-none': 'no-underline',
            'user-select-all': 'select-all',
            'user-select-auto': 'select-auto',
            'user-select-none': 'select-none',
            'pe-none': 'pointer-events-none',
            'pe-auto': 'pointer-events-auto',
            'visible': 'visible',
            'invisible': 'invisible',
            'overflow-auto': 'overflow-auto',
            'overflow-hidden': 'overflow-hidden',
            'overflow-visible': 'overflow-visible',
            'overflow-scroll': 'overflow-scroll',
            'd-print-none': 'print:hidden',
            'd-print-inline': 'print:inline',
            'd-print-inline-block': 'print:inline-block',
            'd-print-block': 'print:block',
            'd-print-grid': 'print:grid',
            'd-print-table': 'print:table',
            'd-print-table-row': 'print:table-row',
            'd-print-table-cell': 'print:table-cell',
            'd-print-flex': 'print:flex',
            'd-print-inline-flex': 'print:inline-flex'
        }
    
    def transform_bootstrap_to_tailwind(self, parsed_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Transform Bootstrap classes to Tailwind CSS equivalents.
        
        Args:
            parsed_data: Parsed website data
            
        Returns:
            Transformed data with Tailwind classes
        """
        transformed_data = {
            'components': [],
            'classes_mapped': 0,
            'unmapped_classes': []
        }
        
        # Process each file
        for file_data in parsed_data.get('files', []):
            file_transformed = self._transform_file_classes(file_data)
            transformed_data['components'].extend(file_transformed.get('components', []))
            transformed_data['classes_mapped'] += file_transformed.get('classes_mapped', 0)
            transformed_data['unmapped_classes'].extend(file_transformed.get('unmapped_classes', []))
        
        return transformed_data
    
    def _transform_file_classes(self, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Transform Bootstrap classes in a single file."""
        transformed = {
            'components': [],
            'classes_mapped': 0,
            'unmapped_classes': []
        }
        
        # Transform navigation components
        structure = file_data.get('structure', {})
        if structure.get('navigation'):
            nav_component = self._transform_navigation(structure['navigation'])
            transformed['components'].append(nav_component)
        
        # Transform form components
        if structure.get('forms'):
            for form in structure['forms']:
                form_component = self._transform_form(form)
                transformed['components'].append(form_component)
        
        # Transform section components
        if structure.get('sections'):
            for section in structure['sections']:
                section_component = self._transform_section(section)
                transformed['components'].append(section_component)
        
        return transformed
    
    def _transform_navigation(self, navigation: List[List[Dict]]) -> Dict[str, Any]:
        """Transform Bootstrap navigation to Tailwind navigation."""
        nav_items = []
        for nav_group in navigation:
            for item in nav_group:
                nav_items.append({
                    'text': item['text'],
                    'href': item['href'],
                    'target': item['target']
                })
        
        nav_links = []
        for item in nav_items:
            nav_links.append(f'<a href="{item["href"]}" className="text-gray-700 hover:text-gray-900 px-3 py-2 rounded-md text-sm font-medium">{item["text"]}</a>')
        
        component_code = f"""
import React from 'react';

const Navigation = () => {{
  return (
    <nav className="bg-white shadow-lg">
      <div className="container mx-auto px-4">
        <div className="flex justify-between items-center py-4">
          <div className="flex space-x-4">
            {chr(10).join(nav_links)}
          </div>
        </div>
      </div>
    </nav>
  );
}};

export default Navigation;
"""
        
        return {
            'name': 'Navigation',
            'type': 'component',
            'code': component_code,
            'dependencies': ['react']
        }
    
    def _transform_form(self, form: Dict[str, Any]) -> Dict[str, Any]:
        """Transform Bootstrap form to Tailwind form."""
        form_name = f"Form_{len(form.get('inputs', []))}"
        
        # Generate form fields
        form_fields = []
        for input_elem in form.get('inputs', []):
            field_code = self._generate_form_field(input_elem)
            form_fields.append(field_code)
        
        component_code = f"""
import React, {{ useState }} from 'react';

const {form_name} = () => {{
  const handleSubmit = async (e) => {{
    e.preventDefault();
    const formData = new FormData(e.target);
    
    try {{
      const response = await fetch('{form.get('action', '')}', {{
        method: '{form.get('method', 'POST')}',
        body: formData
      }});
      
      if (response.ok) {{
        console.log('Form submitted successfully');
      }}
    }} catch (error) {{
      console.error('Error submitting form:', error);
    }}
  }};

  return (
    <form onSubmit={{handleSubmit}} className="space-y-4">
      {chr(10).join(form_fields)}
      <button 
        type="submit" 
        className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700"
      >
        Submit
      </button>
    </form>
  );
}};

export default {form_name};
"""
        
        return {
            'name': form_name,
            'type': 'component',
            'code': component_code,
            'dependencies': ['react']
        }
    
    def _transform_section(self, section: Dict[str, Any]) -> Dict[str, Any]:
        """Transform Bootstrap section to Tailwind section."""
        section_name = f"Section_{section.get('id', 'default')}"
        
        # Convert Bootstrap classes to Tailwind
        classes = section.get('classes', [])
        tailwind_classes = self._convert_classes(classes)
        
        component_code = f"""
import React from 'react';

const {section_name} = () => {{
  return (
    <section className="{tailwind_classes}">
      <div className="container mx-auto px-4">
        {section.get('content', '')}
      </div>
    </section>
  );
}};

export default {section_name};
"""
        
        return {
            'name': section_name,
            'type': 'component',
            'code': component_code,
            'dependencies': ['react']
        }
    
    def _convert_classes(self, classes: List[str]) -> str:
        """Convert Bootstrap classes to Tailwind equivalents."""
        tailwind_classes = []
        unmapped = []
        
        for class_name in classes:
            if class_name in self.bootstrap_to_tailwind:
                tailwind_classes.append(self.bootstrap_to_tailwind[class_name])
            else:
                # Try to match partial classes
                matched = self._match_partial_class(class_name)
                if matched:
                    tailwind_classes.append(matched)
                else:
                    unmapped.append(class_name)
                    # Keep original class if no mapping found
                    tailwind_classes.append(class_name)
        
        return ' '.join(tailwind_classes)
    
    def _match_partial_class(self, class_name: str) -> str:
        """Match partial Bootstrap class names to Tailwind equivalents."""
        # Handle responsive prefixes
        responsive_prefixes = ['col-xs-', 'col-sm-', 'col-md-', 'col-lg-', 'col-xl-']
        
        for prefix in responsive_prefixes:
            if class_name.startswith(prefix):
                size = class_name[len(prefix):]
                if size == '12':
                    return 'w-full px-4'
                elif size == '6':
                    return 'w-full md:w-1/2 px-4'
                elif size == '4':
                    return 'w-full md:w-1/3 px-4'
                elif size == '3':
                    return 'w-full md:w-1/4 px-4'
                elif size == '2':
                    return 'w-full lg:w-1/6 px-4'
        
        # Handle utility classes with numbers
        if class_name.startswith('m-') or class_name.startswith('p-'):
            size = class_name[2:]
            if size.isdigit():
                return f"{class_name[:2]}{size}"
        
        return None
    
    def _generate_form_field(self, input_elem: Dict[str, Any]) -> str:
        """Generate form field JSX with Tailwind classes."""
        field_type = input_elem.get('type', 'text')
        field_name = input_elem.get('name', '')
        placeholder = input_elem.get('placeholder', '')
        required = input_elem.get('required', False)
        
        if field_type == 'textarea':
            return f'''
      <div className="mb-4">
        <label className="block text-sm font-medium text-gray-700 mb-2">
          {field_name}
        </label>
        <textarea
          name="{field_name}"
          placeholder="{placeholder}"
          className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
          {f'required' if required else ''}
        />
      </div>'''
        else:
            return f'''
      <div className="mb-4">
        <label className="block text-sm font-medium text-gray-700 mb-2">
          {field_name}
        </label>
        <input
          type="{field_type}"
          name="{field_name}"
          placeholder="{placeholder}"
          className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
          {f'required' if required else ''}
        />
      </div>''' 