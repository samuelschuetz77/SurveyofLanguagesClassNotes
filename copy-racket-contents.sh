#!/bin/bash

# Script to copy Racket contents to another repository
# Usage: ./copy-racket-contents.sh [destination_path]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RACKET_DIR="$SCRIPT_DIR/Racket"
DEST_DIR="${1:-./racket-export}"

echo "=== Racket Content Copy Utility ==="
echo "Source: $RACKET_DIR"
echo "Destination: $DEST_DIR"
echo

# Check if Racket directory exists
if [ ! -d "$RACKET_DIR" ]; then
    echo "Error: Racket directory not found at $RACKET_DIR"
    exit 1
fi

# Create destination directory
echo "Creating destination directory..."
mkdir -p "$DEST_DIR"

# Copy all Racket files
echo "Copying Racket files..."
cp -r "$RACKET_DIR"/* "$DEST_DIR/"

# Count files copied
FILE_COUNT=$(find "$DEST_DIR" -type f | wc -l)
RKT_COUNT=$(find "$DEST_DIR" -name "*.rkt" | wc -l)

echo "✓ Copied $FILE_COUNT files ($RKT_COUNT .rkt files)"

# Generate README for the copied content
echo "Generating README..."
cat > "$DEST_DIR/README.md" << 'EOF'
# Racket Programming Files

This directory contains Racket programming exercises, homework assignments, and example code from a Survey of Programming Languages class.

## File Overview

| File | Description |
|------|-------------|
| `822racket.rkt` | Basic map function implementation with square function |
| `827rack.rkt` | Simple evaluator with lookup-by-name functionality |
| `ConsCarCadr.rkt` | Implementation of cons, car, and cdr using lambdas |
| `eval.rkt` | Enhanced evaluator with arithmetic operations and error handling |
| `hw2.rkt` | Comprehensive homework with sam-eval interpreter, environment lookup, and lambda support |
| `mylang.rkt` | Basic language implementation with quote support |
| `rackHW.rkt` | Multiple implementations including cons/car/cdr, filter, reduce, and map functions |
| `sam1_1.rkt` | Basic exercises with squares, averages, list operations, and string manipulation |
| `racket notes.txt` | Quick reference notes about Racket syntax and concepts |

## Key Concepts Demonstrated

- **Functional Programming**: Higher-order functions, recursion, immutability
- **List Processing**: cons, car, cdr operations and list manipulation
- **Interpreters**: Building simple evaluators and interpreters
- **Lambda Calculus**: Function definitions and closures
- **Environment Models**: Variable lookup and scoping
- **Data Abstraction**: Creating abstract data types

## Running the Code

These files are written in Racket. To run them:

1. Install Racket from https://racket-lang.org/
2. Run individual files: `racket filename.rkt`
3. Use DrRacket IDE for interactive development

## Notes

- All files use `#lang racket` 
- Some files contain commented-out test cases
- The code demonstrates progression from basic concepts to more advanced interpreter implementation

EOF

echo "✓ Generated README.md"

# Create a simple git setup script
cat > "$DEST_DIR/setup-git-repo.sh" << 'EOF'
#!/bin/bash

# Quick setup script for a new git repository
# Run this in your new repository directory

echo "Setting up git repository for Racket code..."

# Check if git is configured
if ! git config user.name >/dev/null 2>&1 || ! git config user.email >/dev/null 2>&1; then
    echo "⚠️  Git user not configured. Please set up git first:"
    echo "   git config --global user.name 'Your Name'"
    echo "   git config --global user.email 'your.email@example.com'"
    echo
    echo "Or set locally for this repo only:"
    echo "   git config user.name 'Your Name'"
    echo "   git config user.email 'your.email@example.com'"
    echo
    read -p "Continue anyway? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Setup cancelled. Please configure git and try again."
        exit 1
    fi
fi

# Initialize git if not already done
if [ ! -d .git ]; then
    git init
    echo "✓ Initialized git repository"
fi

# Add all files
git add .
echo "✓ Added all files to git"

# Create initial commit
if git diff --cached --quiet; then
    echo "ℹ️  No changes to commit"
else
    git commit -m "Initial commit: Racket programming exercises and assignments

- Added 8 Racket source files (.rkt)
- Added notes file (racket notes.txt)
- Added README with file descriptions
- Includes functional programming exercises, interpreters, and list processing examples"
    echo "✓ Created initial commit"
fi

echo
echo "Next steps:"
echo "1. Add remote: git remote add origin <your-repo-url>"
echo "2. Push: git push -u origin main"
echo
echo "Example for GitHub:"
echo "   git remote add origin https://github.com/yourusername/your-repo-name.git"
echo "   git branch -M main"
echo "   git push -u origin main"

EOF

chmod +x "$DEST_DIR/setup-git-repo.sh"
echo "✓ Created setup-git-repo.sh script"

echo
echo "=== Copy Complete ==="
echo "Destination: $DEST_DIR"
echo "Files copied: $FILE_COUNT"
echo
echo "To use in a new repository:"
echo "1. Copy the contents of '$DEST_DIR' to your new repository"
echo "2. Run './setup-git-repo.sh' in the new repository"
echo "3. Add your remote and push"
echo
echo "Or to copy directly to an existing repo directory:"
echo "./copy-racket-contents.sh /path/to/your/repo"
