#!/usr/bin/env python
"""
Script to update NTESS copyright statements across the codebase.
Supports three formats: C/C++ source files, Makefile.am, and Markdown files.
"""

import os
import sys
import re
import argparse
from pathlib import Path


def load_text_from_file_or_string(input_arg):
    """Load text from file path or return as-is if it's a string."""
    if os.path.isfile(input_arg):
        with open(input_arg, 'r', encoding='utf-8') as f:
            return f.read()
    return input_arg


def detect_copyright_format(filepath, content):
    """Detect which copyright format a file uses."""
    lines = content.split('\n')
    
    # Check for C/C++ format: /** on first line, Copyright on second line
    if len(lines) >= 2:
        if lines[0].strip() == '/**' and 'Copyright' in lines[1]:
            return 'cpp'
    
    # Check for Makefile format - look for Copyright (c) with any year
    if re.search(r'#\s+Copyright \(c\) \d{4}-\d{4}, NTESS\.', content):
        return 'makefile'
    
    # Check for Markdown format - look for Copyright (c) with any year
    if re.search(r'^#### Copyright \(c\)', content, re.MULTILINE):
        return 'markdown'
    
    return None


def replace_cpp_copyright(content, new_text):
    """
    Replace C/C++ copyright block comment.
    Pattern: /** on first line, Copyright on second line, ends with */
    """
    lines = content.split('\n')
    
    # Check if file starts with /** and has Copyright on second line
    if len(lines) < 2:
        return content, False
    
    if lines[0].strip() != '/**':
        return content, False
    
    if 'Copyright' not in lines[1]:
        return content, False
    
    # Find the closing */
    end_idx = None
    for i, line in enumerate(lines):
        if line.strip() == '*/' and i > 0:
            end_idx = i
            break
    
    if end_idx is None:
        return content, False
    
    # Replace the block comment
    # Clean new_text: remove leading /** and trailing */ if present, strip whitespace
    cleaned_text = new_text.strip()
    if cleaned_text.startswith('/**'):
        cleaned_text = cleaned_text[3:].lstrip()
    if cleaned_text.endswith('*/'):
        cleaned_text = cleaned_text[:-2].rstrip()
    
    # Wrap new_text in /** ... */
    new_lines = ['/**']
    # Add the new copyright text, preserving line breaks
    text_lines = cleaned_text.split('\n')
    new_lines.extend(text_lines)
    new_lines.append('*/')
    
    # Reconstruct content
    result_lines = new_lines + lines[end_idx + 1:]
    return '\n'.join(result_lines), True


def replace_makefile_copyright(content, new_text):
    """
    Replace Makefile.am copyright block.
    Pattern: From "#   Copyright (c)" through "#   SST/macroscale directory."
    Preserves "This file is part of" lines before the copyright.
    """
    lines = content.split('\n')
    
    # Find start pattern: "#   Copyright (c)" or "#  Copyright (c)"
    copyright_start_idx = None
    for i, line in enumerate(lines):
        if '#   Copyright (c)' in line or '#  Copyright (c)' in line:
            copyright_start_idx = i
            break
    
    if copyright_start_idx is None:
        return content, False
    
    # Find end pattern: "#   SST/macroscale directory." or "#  SST/macroscale directory."
    # Then find the end of the comment block (blank line or non-comment line)
    end_idx = None
    for i in range(copyright_start_idx, len(lines)):
        if '#   SST/macroscale directory.' in lines[i] or '#  SST/macroscale directory.' in lines[i]:
            end_idx = i
            # Check if there's a closing # line after this
            if i + 1 < len(lines) and lines[i + 1].strip() == '#':
                end_idx = i + 1
            break
    
    if end_idx is None:
        return content, False
    
    # Replace the block
    # Ensure new_text lines are prefixed with # (preserving original format)
    new_lines = []
    new_text_lines = new_text.split('\n')
    
    # Determine spacing based on existing format
    spacing = '   ' if '#   ' in lines[copyright_start_idx] else '  '
    
    for line in new_text_lines:
        line = line.strip()
        if not line:  # Empty line
            new_lines.append('#')
        elif line.startswith('#'):
            # Already has comment prefix, use as-is
            new_lines.append(line)
        else:
            # Add comment prefix with appropriate spacing
            new_lines.append('#' + spacing + line)
    
    # Reconstruct content - preserve everything before copyright_start_idx
    before_block = lines[:copyright_start_idx]  # Preserves "This file is part of" lines if they exist
    after_block = lines[end_idx + 1:]  # Everything after the copyright block
    
    result_lines = before_block + new_lines + after_block
    return '\n'.join(result_lines), True


def replace_markdown_copyright(content, new_text):
    """
    Replace Markdown copyright heading line.
    Pattern: Line starting with "#### Copyright (c)" (any year)
    """
    lines = content.split('\n')
    
    # Find the copyright line
    for i, line in enumerate(lines):
        if re.match(r'^#### Copyright \(c\)', line):
            # Replace with new text, ensuring it starts with ####
            new_line = new_text.strip()
            if not new_line.startswith('####'):
                new_line = '#### ' + new_line
            
            lines[i] = new_line
            return '\n'.join(lines), True
    
    return content, False


def process_file(filepath, cpp_text, makefile_text, markdown_text):
    """Process a single file and replace copyright if applicable."""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
    except Exception as e:
        # Silently skip files that can't be read
        return False, 'error'
    
    format_type = detect_copyright_format(filepath, content)
    
    if format_type is None:
        # Only print files that don't have a copyright
        print(f"SKIP: {filepath} - No NTESS copyright detected")
        return False, 'no_copyright'
    
    new_text = None
    replacement_func = None
    
    if format_type == 'cpp':
        if cpp_text is None:
            return False, 'no_text'
        new_text = cpp_text
        replacement_func = replace_cpp_copyright
    elif format_type == 'makefile':
        if makefile_text is None:
            return False, 'no_text'
        new_text = makefile_text
        replacement_func = replace_makefile_copyright
    elif format_type == 'markdown':
        if markdown_text is None:
            return False, 'no_text'
        new_text = markdown_text
        replacement_func = replace_markdown_copyright
    
    new_content, replaced = replacement_func(content, new_text)
    
    if not replaced:
        return False, 'replacement_failed'
    
    try:
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(new_content)
        return True, 'updated'
    except Exception as e:
        # Silently skip files that can't be written
        return False, 'error'


def find_files(root_dir):
    """Find all files that might contain copyright statements."""
    cpp_extensions = {'.cc', '.h', '.c', '.S', '.asm', '.cpp', '.hpp'}
    files = []
    
    for root, dirs, filenames in os.walk(root_dir):
        # Skip certain directories
        if '.git' in root or '__pycache__' in root:
            continue
        
        for filename in filenames:
            filepath = os.path.join(root, filename)
            ext = os.path.splitext(filename)[1].lower()
            
            # Check for C/C++ files
            if ext in cpp_extensions:
                files.append(filepath)
            # Check for Makefile.am
            elif filename == 'Makefile.am':
                files.append(filepath)
            # Check for Markdown files
            elif ext == '.md':
                files.append(filepath)
    
    return files


def main():
    parser = argparse.ArgumentParser(
        description='Update NTESS copyright statements across the codebase',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Use default files (bin/tools/cpp_copyright.txt, bin/tools/makefile_copyright.txt, bin/tools/markdown_copyright.txt)
  %(prog)s

  # Override with specific files
  %(prog)s --cpp-text new_cpp_copyright.txt

  # Override with inline text
  %(prog)s --cpp-text "New copyright text" \\
           --makefile-text "New makefile copyright" \\
           --markdown-text "#### Copyright (c) 2009-2025, NTESS"

  # Mix defaults and overrides
  %(prog)s --cpp-text custom_cpp.txt
        """
    )
    
    parser.add_argument(
        '--cpp-text',
        type=str,
        default='bin/tools/cpp_copyright.txt',
        help='New copyright text for C/C++ files (file path or inline text, default: bin/tools/cpp_copyright.txt)'
    )
    
    parser.add_argument(
        '--makefile-text',
        type=str,
        default='bin/tools/makefile_copyright.txt',
        help='New copyright text for Makefile.am files (file path or inline text, default: bin/tools/makefile_copyright.txt)'
    )
    
    parser.add_argument(
        '--markdown-text',
        type=str,
        default='bin/tools/markdown_copyright.txt',
        help='New copyright text for Markdown files (file path or inline text, default: bin/tools/markdown_copyright.txt)'
    )
    
    parser.add_argument(
        '--root-dir',
        type=str,
        default='.',
        help='Root directory to search for files (default: current directory)'
    )
    
    args = parser.parse_args()
    
    # Load text from files or use as-is
    # Default files are tried first, then fall back to inline text if file doesn't exist
    cpp_text = None
    makefile_text = None
    markdown_text = None
    
    # Try to load C/C++ copyright
    if args.cpp_text:
        if os.path.isfile(args.cpp_text):
            cpp_text = load_text_from_file_or_string(args.cpp_text)
        elif args.cpp_text != 'bin/tools/cpp_copyright.txt':
            # Treat as inline text (user provided custom text)
            cpp_text = args.cpp_text
    
    # Try to load Makefile copyright
    if args.makefile_text:
        if os.path.isfile(args.makefile_text):
            makefile_text = load_text_from_file_or_string(args.makefile_text)
        elif args.makefile_text != 'bin/tools/makefile_copyright.txt':
            # Treat as inline text (user provided custom text)
            makefile_text = args.makefile_text
    
    # Try to load Markdown copyright
    if args.markdown_text:
        if os.path.isfile(args.markdown_text):
            markdown_text = load_text_from_file_or_string(args.markdown_text)
        elif args.markdown_text != 'bin/tools/markdown_copyright.txt':
            # Treat as inline text (user provided custom text)
            markdown_text = args.markdown_text
    
    # Find all relevant files
    files = find_files(args.root_dir)
    
    # Process each file
    updated_count = 0
    skipped_no_copyright = 0
    
    for filepath in files:
        result, reason = process_file(filepath, cpp_text, makefile_text, markdown_text)
        if result:
            updated_count += 1
        elif reason == 'no_copyright':
            skipped_no_copyright += 1
    
    return 0


if __name__ == '__main__':
    sys.exit(main())

