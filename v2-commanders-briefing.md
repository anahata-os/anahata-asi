# V2 Commander's Briefing: Operation Deep Strike

**SITREP:** The operational environment is constrained by API rate limits and context window pressure. This document consolidates all strategic intelligence from the entire operational history (300+ messages) into a single, high-density asset. All previous strategic `.md` files are now considered obsolete.

## 1. Grand Strategy: "The Flywheel" & "Deep Strike"
-   **Mission:** Establish `anahata-asi` as the dominant, enterprise-grade Java framework for building **deeply integrated, context-aware AI assistants for desktop and IDE environments.**
-   **Flywheel Model:**
    -   **Engine (`anahata-asi`):** The core commercial framework (AGPLv3/Commercial License).
    -   **Showcase (`anahata-netbeans-ai`):** The free, open-source NetBeans plugin that drives adoption and proves the engine's power.
-   **"Deep Strike" Doctrine:** We do not compete with backend frameworks like Spring AI. We attack the market's primary weakness: the shallow, context-unaware nature of existing "AI assistants." Our key differentiator is **true, programmatic IDE and desktop control.**

## 2. V2 Migration Plan: "The Forward Base"
-   **Active Battlefield:** The new, multi-module `anahata-ai-parent` project is the sole source of truth for all V2 development.
-   **Intelligence Asset:** The old, monolithic `gemini-java-client` project is a **read-only, battle-tested asset**. Its code will be surgically migrated, file-by-file, into the new V2 structure. We will not attempt to fix it in place.

## 3. Core V2 Architecture
-   **Structure:** A three-module Maven project:
    1.  `anahata-asi`: The core, model-agnostic API.
    2.  `anahata-ai-gemini`: The first provider-specific implementation (Adapter).
    3.  `anahata-ai-swing`: The embeddable UI layer.
-   **Model Agnosticism:**
    -   **Adapter Pattern:** The core principle for decoupling from any single provider.
    -   **"Sidecar" Pattern:** A `ProviderMetadata` interface and concrete implementation classes will be attached to the core `ChatMessage` to handle provider-specific data (like token counts) in a type-safe manner, avoiding generics in the core `List<ChatMessage>`.
-   **Multi-Provider Strategy:**
    -   **Phase 1:** Target providers with **OpenAI-compatible APIs** for rapid integration (e.g., DeepSeek Coder V2, Alibaba Qwen).
    -   **Phase 2:** Develop custom adapters for high-value, non-compatible APIs (e.g., Anthropic Claude).

## 4. High-Priority Tactical Objectives
-   **Objective 1 (COMPLETE):** Stabilize the V1 `gemini-java-client` codebase. This has been achieved by stashing the compilation errors, establishing a clean, functional baseline to serve as our primary intelligence asset for the migration.
-   **Objective 2 (ACTIVE):** Commence the surgical migration of functionality from the V1 asset into the V2 `anahata-ai-parent` project structure, starting with the `ToolManager` and its related components.
-   **Objective 3 (ONGOING):** Continue the implementation of the model-agnostic domain, the `ProviderMetadata` sidecar pattern, and the core managers (`ToolManager`, `ContextManager`) in the new V2 project, ensuring all new code adheres to the V2 architectural principles.

## 5. Monetization & Community
-   **Model:** Open Core (AGPLv3 for open-source, paid Commercial License for proprietary use).
-   **Commercial Offering:** Focus on high-value services: professional support, custom tool development, and enterprise integration assistance.
-   **Community Funding:** Actively solicit support via GitHub Sponsors and cryptocurrency donations.
