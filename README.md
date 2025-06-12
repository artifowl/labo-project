# Labo Project

## Description
Labo Project is a server-side system designed to analyze submitted source code, track student progress, and provide personalized advice.

The project follows a three-step logic:
1. **Parsing**: Syntax analysis of the submitted code.
2. **Analysis**: Evaluation of the code based on various criteria.
3. **Conclusion**: Generation of recommendations and feedback.

## User Interface (UI/UX)
Three types of interfaces will be explored:
- **Web Application**: An interface accessible via a browser.
- **Command Line Interface (CLI)**: A lightweight and fast solution.
- **Desktop Application**: A standalone software with a graphical interface.

## Project Timeline
### 1. Parsing (`02/04/2025 - 15/04/2025`) - *2 weeks*
- 📌 Define the project architecture.✅
- 📚 Study and understand the necessary C libraries.✅
- 🛠 Develop the parsing module to analyze submitted source code.✅

### 2. Analysis (`15/04/2025 - 15/06/2025`) - *2 month*
- 📝 Define the criteria for code analysis.
  - 💥 Buffer overflows (CWE-121) and out-of-bounds writes (CWE-787)
  - ☠️ Use after free (CWE-416)
  - ➗ Integer overflows/underflow (CWE-190)
  - ❌ Null pointer dereference (CWE-476)
  - 👀 Out-of-bounds read (CWE-125)
- 🔍 Implement algorithms for error detection and code quality assessment.
- 🔗 Integrate with the parsing module.

### 3. Conclusion and Feedback (*2 weeks*)
- 🖥 Integrate with user interfaces.
- 🧪 Testing and improvements
- 📦 Deployment and documentation.

## Technologies Used
- **Primary Language**: `Haskell` (for analysis and parsing)
- **Libraries**: 
  - `language C` (for parsing and analysing)
  - `tasty` (for test implementation)

- **Frameworks/Web**: *(To be determined based on the chosen option)*

## Current Status
🔄 *In the definition and structuring phase of the project.*


## 🧰 Installation & Execution

### Prerequisites

Before building and running the project, ensure the following tools are installed:

- [**GHC**](https://www.haskell.org/ghc/) (The Glasgow Haskell Compiler)
- [**Stack**](https://docs.haskellstack.org/en/stable/README/) (Haskell build tool)
- A working **GCC toolchain** (for `language-c` parsing backend)

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

- `preprocessed_output.i` :  the preprocessed C source file
- `AST.txt` :  a textual representation of the Abstract Syntax Tree (AST)

These outputs can be used to examine the code structure and assist in further analysis or debugging.
Yet the analysis currently focuses on detecting:

- Buffer overflows
- Use of uninitialized variables

Future enhancements will expand the scope of analysis to cover more vulnerabilities and code smells.

### Run the Test Suite
To execute the full test suite and validate the analyzer:

```bash
stack test
```
You will see output indicating which tests passed or failed, ensuring the analyzer behaves as expected on a variety of code patterns.