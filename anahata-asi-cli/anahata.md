/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI CLI (`anahata-asi-cli`)

> [!IMPORTANT]
> This file is an extension of the `anahata.md` in the parent project. Always keep the root `anahata.md` in context as it contains the master Coding Principles and Javadoc Standards.

> [!WARNING]
> This module is totally not working, don't waste too much time on it. It was started months ago but it is not currently being maintained.

This module provides the Command Line Interface for interacting with the Anahata ASI framework.

## 1. Purpose

The CLI serves as a lightweight, terminal-based alternative to the Swing UI. It serves as a primary test harness for core orchestration and provider logic.

## 2. Key Components

- **`Cli`**: The main entry point and orchestrator for the terminal session.
- **`CommandLineArgs`**: Handles parsing of startup arguments.
- **Menu System**:
    - `CliConfigMenu`: Manage agi and provider configurations.
    - `ToolkitsMenu`: Browse and manage enabled toolkits.
    - `ResourcesMenu`: Inspect and manage active workspace resources.
    - `SystemInstructionsMenu`: View and edit system instructions.
    - `ToolExecutionMenu`: Handle manual tool approvals.

## 3. Usage

To run the CLI from the project root:
```bash
mvn -pl anahata-asi-cli exec:java -Dexec.mainClass="uno.anahata.asi.cli.Cli"
```

Força Barça!
