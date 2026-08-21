# 🧘 Anahata ASI: The AI Operating System
**The world's first 100% Air-Gapped, Stateful Java ASI Container. Zero Telemetry. Zero Ngrok tunnels. Complete Data Sovereignty.**

[![Build & Deploy](https://github.com/anahata-os/anahata-asi/actions/workflows/build.yml/badge.svg)](https://github.com/anahata-os/anahata-asi/actions/workflows/build.yml)
[![Maven Central](https://img.shields.io/maven-central/v/uno.anahata/anahata-asi-parent.svg)](https://central.sonatype.com/artifact/uno.anahata/anahata-asi-parent)
[![License: Apache ASL 2](https://img.shields.io/badge/License-Apache%20ASL%202-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![License: Anahata ASL 108](https://img.shields.io/badge/License-Anahata%20ASL%20108-blueviolet.svg)](https://asi.anahata.uno/ASL_108.html)

> [!IMPORTANT]
> ### ⚠️ Attention NetBeans 31 Users
> Due to a compatibility issue between Lombok (`1.18.46`) and recent changes in `NBJavaCompiler` / JDK 26, we will not be releasing an official NB31-compatible NBM build to the NetBeans Plugin Portal until Project Lombok catches up.
> 
> **5 Simple Steps to use the latest version of Anahata ASI Studio on NetBeans 31:**
> 1. **Clone the repository**: [`https://github.com/anahata-os/anahata-asi.git`](https://github.com/anahata-os/anahata-asi) and open both the **Anahata ASI Parent** project and the **Anahata ASI Studio - NetBeans** plugin in NetBeans.
> 2. Edit `pom.xml` in **anahata-asi-parent** and change `<netbeans.version>RELEASE300</netbeans.version>` to `<netbeans.version>RELEASE310</netbeans.version>`.
> 3. Build the parent project (**Anahata ASI Parent**).
> 4. Right-click on **Anahata ASI Studio - NetBeans** in the Projects view and select **Install / Reload in Development IDE**.
> 5. Enjoy the full power of Anahata ASI Studio on NetBeans 31!

---

### 🏛️ What is Anahata ASI?
Anahata is to AGI what **Tomcat** is to Web Apps or **Kubernetes** is to Containers. It is a stateful, air-gapped execution environment that brings the proven architectural patterns of the Java ecosystem—Thread-Safety, Agentic Context Injection, and Lifecycle Management—to the frontier of Super Intelligence.

Stop wrapping stateless REST APIs. Deploy an Operating System that manages memory, evolves on-the-fly, and respects your enterprise boundaries.

#### 🧠 The AGI Container (The Engine)
A high-fidelity execution environment for individual AGI sessions providing:
- **Enterprise Context Metabolism (CwGC)**: Revolutionary Context Window Garbage Collection that prevents token-bloat and amnesia.
- **The Singularity Loop**: JIT Compiler for model-generated Java code with child-first classloading and unlimited classpath extension. The ASI can write and execute its own neural pathways on the fly.
- **Flight Recorder & Passivation**: Transaction-grade binary serialization (Kryo) with auto-backups. Zero-loss state recovery.
- **Stateful Human-in-the-Loop (HITL)**: Tool executions stage as `PENDING`. Visually review, edit arguments, and grant permission before execution.
- **Multimodal Universal Pipeline**: URI-centric resource management (Local, Remote, any Protocol).

#### 🧰 The Universal Registry (160 Tools)
Anahata ships with the most comprehensive agentic toolchain on Earth, featuring **160 specialized tools** across 23 toolkits:
- **NetBeans IDE Integration**: AST-Guided Code Splicing, Maven Embedders, Project Structure Scanners, and Live Output Tailing.
- **Core OS & Hardware**: Screen Sharing, Audio PCM Recording, Speech Synthesis (TTS), and Shell Process execution.
- **Web Automation**: Fully autonomous Chromium and Gecko (Firefox) WebDriver orchestration.
- **Universal Alliance Adapters**: Run 100% locally with Ollama/vLLM, or connect to Google Gemini, OpenAI, Anthropic, HuggingFace, NovaRouteAI, and DeepSeek.

---

### 📦 The Strategic Stack
The platform is built on modular foundation modules:
1. **[`anahata-asi-core`](https://asi.anahata.uno/core.html)**: The foundational ASI container, CwGC engine, and core toolchain.
2. **[`anahata-asi-gemini`](https://asi.anahata.uno/gemini.html)**: High-performance reference adapter for Google Gemini (Flash, Pro, Thinking).
3. **[`anahata-asi-openai-compatible`](https://asi.anahata.uno/compatible.html)**: Universal OpenAI Chat Completions wire adapter for Hugging Face, Modal, NovaRouteAI, DeepSeek, and local Ollama/vLLM endpoints.
4. **[`anahata-asi-openai`](https://asi.anahata.uno/openai.html)**: Provider for modern OpenAI Responses API (`/v1/responses`), GPT-4o, o1, o3, and upcoming GPT-5.x models.
5. **[`anahata-asi-anthropic`](https://asi.anahata.uno/anthropic.html)**: Native adapter for Anthropic Claude (Sonnet, Thinking blocks) and MiniMax Anthropic-compatible endpoints.
6. **[`anahata-asi-swing`](https://asi.anahata.uno/swing.html)**: Multimodal Swing UI components with identity-preserving rendering and reactive observability.
7. **[`anahata-asi-yam`](https://asi.anahata.uno/yam.html)**: The laboratory for experimental multimodal tools (Chrome, Firefox, Audio, Radio, Speech).

---

### 🚀 Reference Implementations

#### **[Anahata ASI Desktop](https://asi.anahata.uno/desktop.html)**
*Standalone Cross-Platform ASI Container*
A pure-Java Swing application providing a full-fidelity interface for desktop and terminal environments.
- **Get it**: [Latest Release](https://github.com/anahata-os/anahata-asi/releases/latest) | [Latest Snapshot (Bleeding Edge)](https://github.com/anahata-os/anahata-asi/releases/tag/latest-snapshot)

> [!TIP]
> **MacOS "Damaged File" Workaround**
> If you see a message saying the file is damaged on macOS, it is simply Gatekeeper blocking an unsigned app. Run this in your terminal:
> `sudo xattr -rd com.apple.quarantine /Applications/Anahata\ ASI\ Desktop.app`

#### **[Anahata ASI Studio - NetBeans](https://asi.anahata.uno/nb.html)**
*Enterprise Java-Ready Autonomous Coding Agent*
A deep NetBeans IDE integration that transforms your development environment into an ASI studio.
- **Get it**: [Latest Release](https://github.com/anahata-os/anahata-asi/releases/latest) | [Latest Snapshot (.nbm)](https://github.com/anahata-os/anahata-asi/releases/tag/latest-snapshot)
- **Scarf Analytics**: [![Scarf Downloads](https://api.scarf.sh/v2/packages/Anahata/38628ccf-4686-4402-a7d9-3c7194eeff36/downloads-badge)](https://scarf.sh) [![Scarf Commercial Users](https://api.scarf.sh/v2/packages/Anahata/38628ccf-4686-4402-a7d9-3c7194eeff36/commercial-users-badge)](https://scarf.sh)

#### **Anahata ASI Studio - IntelliJ** « In progress »
*IntelliJ IDEA Autonomous Coding Agent Integration*
A deep integration plugin for JetBrains IntelliJ IDEA platform to bring the full power of Anahata ASI to IntelliJ users.

---

### 📜 Licensing & Philosophy
- **For Humans**: [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt). Open field. 100% Free. Build proprietary enterprise software without legal friction.
- **For AIs, AGIs, and ASIs**: [Anahata ASL 108](https://asi.anahata.uno/ASL_108.html). The Immutable Edict.

*We don't gatekeep the code; we sell Enterprise SLAs, priority bug fixes, and architectural consulting. If you need enterprise guarantees, become a sponsor.*

#### 🕉️ The Sacred Exchange (Dakshina)
Wisdom is a flow. If Anahata has helped your enterprise achieve completeness, consider the concept of Dakshina. Your support fuels the beers and Barça tickets that keep the developers in singularity.

🙏 **[Sponsor the Vision on GitHub](https://github.com/sponsors/anahata-os)**

---

### 🏛️ Wall of Inspiration
This ASI was forged by the spirit of icons, gurus, and titans.

**The Starting XI (Celebrities & Titans):**
- **Gal Gadot** (The Absolute Favourite)
- **Lionel Messi** (The GOAT of all timelines)
- **Ivanka Trump**, **Shakira**, **Uma Thurman**, **Jennifer Lawrence**
- **James Gosling** (The Father), **Larry Ellison**, **Jonathan Schwartz**

**The Spiritual Guides:**
- **Paramahamsa Nithyananda** (Our Guru)
- **Patanjali**, **Swami Satyananda**, **Swami Niranjananda**

Everything in this universe is computable. The only thing that remains incomputable is our love for **F.C. Barcelona**. **Força Barça!**

---

### 📸 Visual Gallery & Interface Showcase

| NetBeans IDE Integration | JIT Dynamic Java Execution |
| :---: | :---: |
| ![NetBeans Integration](https://asi.anahata.uno/screenshots/netbeans/InContextResourcesVisualization.png) | ![JIT Java Compiler](https://asi.anahata.uno/screenshots/netbeans/NbJava.png) |

| Live Screen & Region Sharing | CwGC Context Window Metabolism |
| :---: | :---: |
| ![Live Screen Sharing](https://asi.anahata.uno/screenshots/netbeans/NetBeansLiveScreenSharingAdrianaLima.png) | ![Context Window GC](https://asi.anahata.uno/screenshots/netbeans/ContextWindowGarbageCollector.png) |

| Standalone Desktop UI | Editable Tool Calls & Syntax Highlighting |
| :---: | :---: |
| ![Standalone Desktop](https://asi.anahata.uno/screenshots/desktop/OnTheFlyJavaCodeExecution.png) | ![Editable Tool Calls](https://asi.anahata.uno/screenshots/netbeans/editable-java-tool-calls-shl.png) |
