#!/usr/bin/env python3
"""
Test script to run the HTML parser and see its output.
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from engine.modernizers.static_site.parser.html.html_parser import HTMLParser
import json

def test_html_parser():
    """Test the HTML parser with the example legacy site."""
    
    # Initialize the parser
    parser = HTMLParser()
    
    # Test with the example legacy site
    input_file = "examples/website/legacy-site.html"
    
    print("🔍 Testing HTML Parser")
    print("=" * 50)
    print(f"Input file: {input_file}")
    print()
    
    try:
        # Parse the HTML file
        print("📊 Parsing HTML file...")
        result = parser.parse_input(input_file)
        
        print("\n✅ Parsing completed!")
        print("\n📋 Parsed Data Structure:")
        print("=" * 50)
        
        # Display the structure
        print(f"Files found: {len(result.get('files', []))}")
        
        for i, file_data in enumerate(result.get('files', [])):
            print(f"\n📄 File {i + 1}: {file_data.get('name', 'unknown')}")
            print("-" * 30)
            
            structure = file_data.get('structure', {})
            print(f"Title: {structure.get('title', 'No title')}")
            print(f"Meta tags: {len(structure.get('meta', {}))}")
            print(f"Navigation items: {len(structure.get('navigation', []))}")
            print(f"Sections found: {len(structure.get('sections', []))}")
            print(f"Forms found: {len(structure.get('forms', []))}")
            print(f"Images found: {len(structure.get('images', []))}")
            print(f"Links found: {len(structure.get('links', []))}")
            
            # Display sections in detail
            sections = structure.get('sections', [])
            if sections:
                print(f"\n🔍 Sections Details:")
                for j, section in enumerate(sections):
                    print(f"  Section {j + 1}:")
                    print(f"    Type: {section.get('type', 'unknown')}")
                    print(f"    ID: {section.get('id', 'none')}")
                    print(f"    Title: {section.get('title', 'none')}")
                    print(f"    Classes: {section.get('classes', [])}")
                    print(f"    Content length: {len(section.get('content', ''))}")
                    
                    # Show cards for services
                    if section.get('type') == 'services':
                        cards = section.get('cards', [])
                        print(f"    Service cards: {len(cards)}")
                        for k, card in enumerate(cards):
                            print(f"      Card {k + 1}: {card.get('title', 'No title')}")
                    
                    # Show form for contact
                    if section.get('type') == 'contact':
                        form = section.get('form')
                        if form:
                            inputs = form.get('inputs', [])
                            print(f"    Form inputs: {len(inputs)}")
                            for k, input_data in enumerate(inputs):
                                print(f"      Input {k + 1}: {input_data.get('type', 'unknown')} - {input_data.get('name', 'unnamed')}")
                    
                    print()
        
        # Display frameworks detected
        frameworks = result.get('frameworks', {})
        if frameworks:
            print("🔧 Frameworks Detected:")
            print("-" * 30)
            for framework, detected in frameworks.items():
                print(f"  {framework}: {'✅' if detected else '❌'}")
        
        # Save detailed output to JSON file for inspection
        output_file = "parser_output.json"
        with open(output_file, 'w') as f:
            json.dump(result, f, indent=2, default=str)
        
        print(f"\n💾 Detailed output saved to: {output_file}")
        
    except Exception as e:
        print(f"❌ Error parsing HTML: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    test_html_parser() 