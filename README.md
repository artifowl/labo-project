# Labo Project

## Description
Labo Project is a server-side system designed to analyze submitted C source code, track student progress, and provide personalized feedback.

---

## Workflow
1. **Parsing**: Syntax analysis of submitted code.
2. **Analysis**: Code evaluation based on predefined criteria.
3. **Conclusion**: Feedback and recommendations generation.

---

## Interfaces
- **Web Application**: Browser-based interface.
- **CLI**: Command Line Interface for quick analysis.
- **Desktop App**: Standalone application.

---

## Project Timeline

### Parsing Phase (02/04/2025 - 15/04/2025)
- Project architecture definition.
- C libraries research.
- Parser module development.

### Analysis Phase (15/04/2025 - 15/06/2025)
- Vulnerability detection:
  - Buffer overflows (CWE-121, CWE-787).
  - Use-after-free (CWE-416).
  - Integer overflows (CWE-190).
  - Null pointer dereference (CWE-476).
  - Out-of-bounds reads (CWE-125).

### Final Phase (2 weeks)
- UI integration.
- Comprehensive testing.
- Deployment and documentation.

---

## Technology Stack
- **Main Language**: Haskell.
- **Key Libraries**:
  - `language-c` for parsing.
  - `tasty` for testing.
- **Additional Tools**: GCC toolchain.

---

## Current Status
**Active development** - Analysis phase completed.

---

## Installation & Execution

### Prerequisites
Before building and running the project, ensure the following tools are installed:
- [**GHC**](https://www.haskell.org/ghc/) (The Glasgow Haskell Compiler).
- [**Stack**](https://docs.haskellstack.org/en/stable/README/) (Haskell build tool).
- A working **GCC toolchain** (for `language-c` parsing backend).

You can install Stack by running:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

### Project Setup
Clone the project and install the dependencies:

```bash
git clone https://github.com/yourusername/labo-project.git
cd labo-project
stack setup
stack build
```

### Run the Analyzer on a C File
To run the static analyzer on a C source file (e.g., `test.c`), use the following command:

```bash
stack run -- test.c
```

This command will parse the given file and generate two output files:
- `preprocessed_output.i`: The preprocessed C source file.
- `AST.txt`: A textual representation of the Abstract Syntax Tree (AST).

These outputs can be used to examine the code structure and assist in further analysis or debugging.

### Run the Test Suite
To execute the full test suite and validate the analyzer:

```bash
stack test
```

You will see output indicating which tests passed or failed, ensuring the analyzer behaves as expected on a variety of code patterns.

---

## Screenshots

### Example of Usage
![Example of usage](https://i.ibb.co/d44vHQNq/Screenshot-From-2025-06-12-15-47-01.png)

### Test Suite Output
![Test suite output](https://i.ibb.co/XrDY59Lg/Screenshot-From-2025-06-12-15-49-15.png)

---

## Future Enhancements
Future updates will expand the scope of analysis to cover:
- Additional vulnerabilities.
- Code smells.
- Advanced static analysis techniques.