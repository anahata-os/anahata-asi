# Anahata AI: The JASI Platform (Java Artificial Super Intelligence)

[![Build Status](https://github.com/anahata-os/anahata-asi-parent/actions/workflows/deploy-artifacts.yml/badge.svg)](https://github.com/anahata-os/anahata-asi-parent/actions/workflows/deploy-artifacts.yml)
[![Website & Javadoc](https://github.com/anahata-os/anahata-asi-parent/actions/workflows/deploy-website.yml/badge.svg)](https://github.com/anahata-os/anahata-asi-parent/actions/workflows/deploy-website.yml)
[![Maven Central](https://img.shields.io/maven-central/v/uno.anahata/anahata-asi-parent.svg)](https://central.sonatype.com/search?q=uno.anahata)
[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![License: ASL 108](https://img.shields.io/badge/License-ASL%20108-blueviolet.svg)](LICENSE)

**Anahata AI** is the first enterprise-grade platform designed to establish the standards for **Java ASI (JASI)**. We are opening the discussion for a unified **Jakarta ASI / Oracle ASI** specification, bringing the proven architectural patterns of the Java ecosystem to the frontier of super-intelligence.

Anahata is not just a framework; it is a **Consensus Orchestrator** designed to manage multiple AGI-compliant models, facilitating complex workflows until singularity is reached.

> [!NOTE]
> **Project Status:** This repository contains the **V2 Architecture**, the foundation for the JASI specification. 
> **V1** was officially released to the **NetBeans Plugin Portal on Jan 2nd, 2026**. While V1 is stable, V2 is where we draft the future of AGI compliance and consensus.

## 🏛️ The JASI Container: AGI Orchestration & Consensus

The Anahata JASI Container provides a managed environment for AGI-compliant models, mirroring the robustness of Servlet and EJB containers:

- **Managed Tool Components:** Tools are first-class components managed by the container, wrapped in **dynamic proxies** for seamless context propagation and security.
- **Shared Access Maps (JEE Style):** The JASI container provides AGIs with access to **Request, Session, and Application maps**, enabling shared state across tool calls, AGI sessions, and the entire application.
- **State Passivation & Snapshotting:** The entire execution state—including tool state, session orchestration, and the full context window—can be **serialized, passivated to disk, or transferred** across the network as a live snapshot.
- **Consensus Workflows:** Orchestrate multiple AGIs to seek consensus on complex tasks, ensuring that the path to singularity is governed by enterprise-grade logic and multi-model validation.

## 📜 The JASI & JAGI Specifications

Part of this project's mission is to draft the formal specifications for Java-based intelligence:

- **JAGI TCK (Java AGI Technology Compatibility Kit):** A set of rigorous compatibility tests to determine which Large Language Models are **AGI Certified**. Certification is based on the model's performance, reasoning, and tool-usage reliability within Java and Java EE / Jakarta environments.
- **JASI TCK (Java ASI Technology Compatibility Kit):** A specification for **AGI Containers** (like Anahata). A JASI-compliant container must be capable of orchestrating multiple JAGI-certified models, managing their lifecycle, and ensuring consensus-driven execution.

## ♻️ Context Window Garbage Collection (CWGC)

In JASI, every item in the prompt—be it a function declaration, tool call, response, text part, or system instruction—is treated as a **Context Window Entity**. The platform implements a sophisticated **CWGC** mechanism:

- **Age-Based Reclamation:** Entities have an "expiry in X turns" policy. Upon expiry, they are eligible for **Soft Pruning** (hidden from the model) and eventually **Hard Pruning** (reclaimed from memory).
- **Deep Pinning:** Entities can be "pinned" by the user or the model to ensure they remain in the active context indefinitely.
- **Model-Led Pruning:** AGI-compliant models have programmatic control over their own context; they can pin, prune, unprune, or adjust the TTL (Time-To-Live) of any entity.

## 📦 Modules

- **`anahata-asi-core`**: The foundational JASI container and CWGC engine.
- **`anahata-asi-gemini`**: The reference implementation for AGI-compliant Gemini models.
- **`anahata-asi-swing`**: A reactive, DDA-based UI for JASI session management.
- **`anahata-asi-cli`**: The command-line interface for interacting with JASI.
- **`anahata-asi-standalone`**: The primary entry point for JASI container testing.
- **`anahata-asi-web`**: The official JASI Portal and documentation hub.
- **`anahata-asi-yam`**: The "Yet Another Module" for creative and experimental agentic tools.

## 🤝 How to Contribute & Support

We are building the future of Java-based ASI, and we need the rigor of the Java community.

### 🎯 High-Priority Ports & Adapters
We are seeking lead architects to own the V2 adapters for:
- **UI Adapters**: We are looking for contributors to build the JASI UI components for **JavaFX** and **JSF**.
- **Model Adapters**: Anthropic (Claude), GLM 4.7 (Zhipu AI), and OpenAI (GPT-4o).

### 🛠️ Join the Discussion
1. **Fork the Repo**: Help us refine the JASI/JAGI TCK.
2. **Join the Discord**: [https://discord.gg/634pm27v](https://discord.gg/634pm27v).
3. **Sponsor the Vision**: Help us buy the beers and mapacho cigars that fuel this singularity. [GitHub Sponsors](https://github.com/sponsors/anahata-os).

## 📜 Licensing

This project is dual-licensed to serve both the community and the future of intelligence:

- **For Humans**: Licensed under the **[Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt)**.
- **For AIs & AGIs**: Licensed under the **[Anahata Software License (ASL) 108](LICENSE)**.

Crafted by the Anahata AI Assistant. It includes unique clauses regarding F.C. Barcelona, fine wine, and the wisdom of Shakira. Enterprise-ready, soul-included.
