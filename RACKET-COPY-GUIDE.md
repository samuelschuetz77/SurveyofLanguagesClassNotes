# Racket Content Copy Guide

This repository contains a utility to help copy the Racket programming files to another GitHub repository.

## Quick Start

### Option 1: Copy to a New Directory
```bash
./copy-racket-contents.sh /path/to/destination
```

### Option 2: Copy to Current Directory
```bash
./copy-racket-contents.sh ./racket-export
```

## What Gets Copied

The script will copy all files from the `Racket/` directory:

- **8 Racket source files (.rkt)**: Programming exercises and homework assignments
- **1 notes file**: Quick reference for Racket syntax
- **Generated README.md**: Documentation of all files with descriptions
- **Generated setup-git-repo.sh**: Helper script for git repository setup

## Complete Workflow for New Repository

1. **Run the copy script:**
   ```bash
   ./copy-racket-contents.sh ./my-racket-code
   ```

2. **Create a new GitHub repository** (on GitHub.com)

3. **Copy files to your local clone:**
   ```bash
   git clone https://github.com/yourusername/your-new-repo.git
   cp -r ./my-racket-code/* ./your-new-repo/
   cd your-new-repo
   ```

4. **Setup and push:**
   ```bash
   ./setup-git-repo.sh
   git remote add origin https://github.com/yourusername/your-new-repo.git
   git push -u origin main
   ```

## Files Included

| Original File | Description |
|---------------|-------------|
| `822racket.rkt` | Map function with square implementation |
| `827rack.rkt` | Simple evaluator with dictionary lookup |
| `ConsCarCadr.rkt` | Lambda-based cons, car, cdr implementation |
| `eval.rkt` | Arithmetic evaluator with error handling |
| `hw2.rkt` | Complete interpreter with lambda support |
| `mylang.rkt` | Basic language with quote support |
| `rackHW.rkt` | Multiple implementations (cons/car/cdr, filter, reduce, map) |
| `sam1_1.rkt` | Basic exercises and string manipulation |
| `racket notes.txt` | Syntax reference notes |

## Generated Files

The script also creates:

- **README.md**: Comprehensive documentation of all files
- **setup-git-repo.sh**: Automated git repository initialization script

## Requirements

- Bash shell
- Git (for the setup script)
- Racket (to run the copied .rkt files)

## Troubleshooting

- **Permission denied**: Make sure the script is executable: `chmod +x copy-racket-contents.sh`
- **Directory not found**: Ensure you're running the script from the repository root
- **Git issues**: The setup script requires git to be installed and configured

## Alternative: Manual Copy

If you prefer manual copying:

1. Copy the entire `Racket/` directory to your new repository
2. Manually create a README.md file
3. Initialize git and commit the files

The automated script just makes this process easier and more consistent.