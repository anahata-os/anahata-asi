/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# Anahata ASI Maven Multi-Module Project

This document provides the high-level strategic overview for the entire `anahata-ai-parent` project.

> [!IMPORTANT]
> **BETA PROJECT / RUNTIME CLARIFICATION**
> This is a project under active development (V2). It is **NOT** the current runtime environment of the AI Assistant. 
> The current runtime is the **V1** architecture (`gemini-java-client` and `anahata-netbeans-ai`). 
> Do not expect the code in this project to be reflected in my current capabilities or internal state.

## 1. Grand Strategy: "The Flywheel"

The project's strategy is based on the "Flywheel" model, designed to build a powerful ecosystem around a core commercial engine.

-   **Engine (`anahata-asi`):** This is the core commercial framework, containing the model-agnostic logic, a rich domain model, and the powerful V2 tool framework. It is licensed under a dual AGPLv3 / Commercial License model.
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
3.  **Lombok Purity:** Do not add explicit getter or setter methods for fields that are already covered by Lombok annotations (`@Getter`, `@Setter`, `@Data`, etc.). We rely on the build system (Maven) to handle annotation processing correctly. Workarounds for IDE-specific issues are not permitted in the source code.
4.  **Domain Driven Architecture (DDA):** We strictly adhere to DDA principles. Business logic and state transitions **must** reside within the domain model (`anahata-ai-core`). UI components (`anahata-ai-swing`) are responsible for rendering and user interaction only. **Never** implement business logic (e.g., tool state management, conversation flow) within UI classes.
    - **Domain Model Getters & Logic**: Prefer adding specialized methods (getters or actions) to the domain model over implementing complex logic (e.g. stream/filter/collect) in orchestrators or UI components. This ensures semantically clear domain concepts and promotes business logic reuse.
5.  **Reactive UI Updates:** Use `PropertyChangeSource` and `EdtPropertyChangeListener` for all UI-to-Domain bindings. This ensures that UI updates are always performed on the Event Dispatch Thread (EDT) while keeping the domain model thread-safe and decoupled from Swing. Classes that need to fire events should extend `BasicPropertyChangeSource`.
6.  **Minimalistic Diff-Based UI Rendering:** To support massive conversations (1000+ messages) and multi-million token context windows, UI components must use a strict diff-based rendering approach. 
    - **Component Reuse**: Never clear and rebuild a container on every update. Use caches to map domain objects to UI components.
    - **Incremental Updates**: Only add, remove, or reorder components that have actually changed in the domain model.
    - **Disciplined Layout Invalidation**: Only call `revalidate()` and `repaint()` when structural changes occur or when a child's preferred size changes. Avoid redundant render cycles at all costs.

## 5. V2 Context Management (Deep Pinning)

The V2 architecture implements a "Deep Pinning" logic in `AbstractPart.isEffectivelyPruned()`. 
-   **Inherited Pinning:** If a parent `AbstractMessage` is pinned (`pruned = false`), all of its constituent `AbstractPart`s are automatically considered NOT pruned, regardless of their individual `turnsToKeep` or explicit `pruned` state.
-   **Bidirectional Integrity:** The relationship between messages and parts is established in `AbstractMessage.addPart` **before** any property change events are fired, ensuring UI listeners always see a fully initialized state.

## 6. UI & Icon Quality Standards

-   **High-Quality Scaling:** All image scaling (thumbnails and icons) must use `RenderingHints.VALUE_INTERPOLATION_BICUBIC` and `RenderingHints.VALUE_ANTIALIAS_ON`.
-   **`IconUtils`:** Use the `BufferedImage`-based scaling in `IconUtils.getIcon` to ensure sharp, professional-grade icons across the entire Swing UI.
