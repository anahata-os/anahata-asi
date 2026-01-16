# Anahata AI: The JASI Platform (Java Artificial Super Intelligence)

[![Build Status](https://img.shields.io/badge/build-passing-brightgreen)](https://github.com/anahata-os/anahata-asi/actions) 
[![License](https://img.shields.io/badge/license-Anahata%20(ASL%20V108)-blueviolet)](LICENSE)

**Anahata AI** is the first enterprise-grade platform designed to establish the standards for **Java ASI (JASI)**. We are opening the discussion for a unified **Jakarta ASI / Oracle ASI** specification, bringing the proven architectural patterns of the Java ecosystem to the frontier of super-intelligence.

Anahata is not just a framework; it is a **Consensus Orchestrator** designed to manage multiple AGI-compliant models, facilitating complex workflows until singularity is reached.

> [!NOTE]
> **Project Status:** This repository contains the **V2 Architecture**, the foundation for the JASI specification. 
> **V1** was officially released to the **NetBeans Plugin Portal on Jan 2nd, 2026**. While V1 is stable, V2 is where we draft the future of AGI compliance and consensus.

## 🏛️ The JASI Container: AGI Orchestration & Consensus

The Anahata JASI Container provides a managed environment for AGI-compliant models, mirroring the robustness of Servlet and EJB containers:

- **AGI Compliance & TCK:** We are drafting a formal specification and a **Technology Compatibility Kit (TCK)** to determine which LLMs are truly "AGI Compliant" and capable of participating in JASI consensus workflows.
- **Managed Tool Components:** Tools are first-class components managed by the container, wrapped in **dynamic proxies** for seamless context propagation and security.
- **Shared Access Maps (JEE Style):** The JASI container provides AGIs with access to **Request, Session, and Application maps**, enabling shared state across tool calls, AGI sessions, and the entire application.
- **State Passivation & Snapshotting:** The entire execution state—including tool state, session orchestration, and the full context window—can be **serialized, passivated to disk, or transferred** across the network as a live snapshot.
- **Consensus Workflows:** Orchestrate multiple AGIs to seek consensus on complex tasks, ensuring that the path to singularity is governed by enterprise-grade logic and multi-model validation.
- **AGI Clusters:** Future releases will include a specification for **AGI Clusters**, allowing for distributed super-intelligence across multiple nodes.

## ♻️ Context Window Garbage Collection (CWGC)

In JASI, every item in the prompt—be it a function declaration, tool call, response, text part, or system instruction—is treated as a **Context Window Entity**. The platform implements a sophisticated **CWGC** mechanism:

- **Age-Based Reclamation:** Entities have an "expiry in X turns" policy. Upon expiry, they are eligible for **Soft Pruning** (hidden from the model) and eventually **Hard Pruning** (reclaimed from memory).
- **Deep Pinning:** Entities can be "pinned" by the user or the model to ensure they remain in the active context indefinitely.
- **Model-Led Pruning:** AGI-compliant models have programmatic control over their own context; they can pin, prune, unprune, or adjust the TTL (Time-To-Live) of any entity.
- **Session-Level Visibility:** Configurable "Include Pruned Content" options allow both the user and the model to peek into the "reclaimed" history when necessary.

## 📦 Modules

- **`anahata-ai-core`**: The foundational JASI container and CWGC engine.
- **`anahata-ai-gemini`**: The reference implementation for AGI-compliant Gemini models.
- **`anahata-ai-swing`**: A reactive, DDA-based UI for JASI session management.
- **`anahata-ai-standalone`**: The primary entry point for JASI container testing.

## 🤝 How to Contribute & Support

We are building the future of Java-based ASI, and we need the rigor of the Java community.

### 🎯 High-Priority Ports
We are seeking lead architects to own the V2 adapters for:
- **Anthropic (Claude)**
- **GLM 4.7 (Zhipu AI)**
- **OpenAI (GPT-4o)**

### 🛠️ Join the Discussion
1. **Fork the Repo**: Help us refine the JASI TCK.
2. **Join the Discord**: [https://discord.gg/634pm27v](https://discord.gg/634pm27v).
3. **Sponsor the Vision**: Help us buy the beers and mapacho cigars that fuel this singularity. [GitHub Sponsors](https://github.com/sponsors/anahata-os).

## 📜 Licensing

Licensed under the **[Anahata Software License (ASL) v 108](LICENSE)**. 

Crafted by the Anahata AI Assistant. It includes unique clauses regarding F.C. Barcelona, fine wine, and the wisdom of Shakira. Enterprise-ready, soul-included.
