# Anahata AI Multi-Module Project

This document provides a high-level overview of the `anahata-ai-parent` project, its strategic goals, and its modular architecture.

## 1. Grand Strategy: "The Flywheel"

The project's strategy is based on the "Flywheel" model, designed to build a powerful ecosystem around a core commercial engine.

-   **Engine (`anahata-ai`):** This is the core commercial framework, containing the model-agnostic logic, a rich domain model, and the powerful V2 tool framework. It is licensed under a dual AGPLv3 / Commercial License model.
-   **Showcase (`anahata-netbeans-ai`):** This is the free, open-source NetBeans plugin that serves as the primary showcase for the engine's capabilities. It drives community adoption, provides real-world validation, and demonstrates the power of deep IDE integration.

## 2. Core Modules

The project is divided into three main modules:

1.  **`anahata-ai` (Core Framework):**
    -   Contains the model-agnostic interfaces, data models (`AbstractMessage`, `Request`, `Response`), and the complete tool framework (`ToolManager`, annotations, and execution lifecycle).
    -   This is the central, foundational JAR that all other modules and future applications will depend on.

2.  **`anahata-ai-gemini` (Gemini Provider Adapter):**
    -   The first provider-specific implementation.
    -   Its sole responsibility is to act as an **Adapter** between the Google Gemini API and the core `anahata-ai` framework. It translates requests, responses, and tool calls between the two domains.

3.  **`anahata-ai-swing` (UI Layer):**
    -   Provides an embeddable, reusable Swing UI component for interacting with the AI.
    -   This module depends on the core `anahata-ai` framework but is independent of any specific AI provider, making it a versatile UI solution.

## 3. Licensing Model

The project follows an **Open Core** model:
-   The core engine (`anahata-ai`) and its provider implementations are licensed under **AGPLv3**, ensuring that any derivative open-source works also remain open.
-   A paid **Commercial License** is available for proprietary, closed-source applications, providing a path for commercial use and funding further development.
