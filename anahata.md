# Anahata AI Multi-Module Project

This document provides the high-level strategic overview for the entire `anahata-ai-parent` project.

## 1. Grand Strategy: "The Flywheel"

The project's strategy is based on the "Flywheel" model, designed to build a powerful ecosystem around a core commercial engine.

-   **Engine (`anahata-ai-core`):** This is the core commercial framework, containing the model-agnostic logic, a rich domain model, and the powerful V2 tool framework. It is licensed under a dual AGPLv3 / Commercial License model.
-   **Showcase (`anahata-netbeans-ai`):** This is the free, open-source NetBeans plugin that serves as the primary showcase for the engine's capabilities. It drives community adoption, provides real-world validation, and demonstrates the power of deep IDE integration.

## 2. Core Modules

The project is divided into three main modules:

1.  **`anahata-ai-core`:** The foundational, model-agnostic framework. Contains all core interfaces, the domain model, and the tool-chain.
2.  **`anahata-ai-gemini`:** The first provider-specific implementation, acting as an Adapter between the Google Gemini API and the core framework.
3.  **`anahata-ai-swing`:** A reusable, provider-agnostic Swing UI component for building agentic workflows.

## 3. Strategic Documents

This project uses a set of key documents to guide development. For detailed information, please refer to the following:

-   **`v2-commanders-briefing.md`**: Located in the project root, this document contains the overall mission objectives, V2 migration plan, and high-priority tactical goals.
-   **`anahata-ai-core/anahata.md`**: Contains the detailed technical vision and architectural summary for the core framework module.

## 4. Coding Principles (Applies to ALL Modules)

1.  **Javadoc Integrity:** As an open-source Java library, comprehensive documentation is paramount. Existing Javadoc, comments, and blank lines **must never be removed**. **Javadoc is mandatory for everything that can be javadocced (all classes and all methods: public, protected, and private).** Either use the `{@inheritDoc}` feature or provide a concise statement of what the specific implementation is doing.
2.  **Logging Standard:** All logging **must** be done through the SLF4J API (`@Slf4j`). **Never** use `System.out.println()`. Use placeholders (`{}`) for dynamic content.
