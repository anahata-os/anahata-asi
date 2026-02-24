/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI CLI (`anahata-asi-cli`)

This module provides the Command Line Interface for interacting with the Anahata ASI framework.

## 1. Purpose

The CLI serves as a lightweight, terminal-based alternative to the Swing UI. It is designed for environments where a GUI is not available or for users who prefer terminal interactions. It also serves as a primary test harness for core orchestration and provider logic.

## 2. Key Components

- **`Cli`**: The main entry point and orchestrator for the terminal session.
- **`CommandLineArgs`**: Handles parsing of startup arguments (e.g., session ID, model selection).
- **Menu System**:
    - `CliConfigMenu`: Manage chat and provider configurations.
    - `ToolkitsMenu`: Browse and manage enabled toolkits.
    - `ResourcesMenu`: Inspect and manage active workspace resources.
    - `SystemInstructionsMenu`: View and edit system instructions.
    - `ToolExecutionMenu`: Handle manual tool approvals and execution feedback.

## 3. Usage

To run the CLI from the project root:
```bash
mvn -pl anahata-asi-cli exec:java -Dexec.mainClass="uno.anahata.asi.cli.Cli"
```

Força Barça!
