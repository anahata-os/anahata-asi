/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# ⚡ Anahata-AGI-1: The Definitive Pure-Java AI Benchmark Suite

> **Program Title:** Anahata-AGI-1  
> **Certification:** Anahata AGI Certified Model  
> **Sponsor:** NovaRouteAI (`https://novarouteai.com`)  
> **Channel:** Anahata TV (`@anahata108`)  
> **Published Web Hub:** `https://asi.anahata.uno/benchmarks.html`  

---

## 1. Vision & Certification Overview

> [!NOTE]
> **Single Source of Truth Architecture**
> `benchmarks.md` is the master technical specification and single source of truth for the entire **Anahata-AGI-1** benchmark suite. All benchmark challenge specifications, rules, prompts, metrics, and scoring formulas originate here.
> The website files (`https://asi.anahata.uno/benchmarks/`) are the public publishing layer and serve as a verbatim web reflection of this master document.

**Anahata-AGI-1** is the industry's first standardized, pure-Java agentic benchmark suite designed to determine which Large Language Models (LLMs) earn the title of **Anahata AGI Certified Model** (average score > 85% across all tests).

> [!IMPORTANT]
> **Strict Prompt Purity & Autonomous Tool Discovery Philosophy**
> Benchmark prompts are strictly minimal and forbid providing hints, clues, or instructions regarding:
> 1. **Tool Definitions & Execution**: No explanations on how tools work, how to execute code, or how tool schemas are formatted. Models are benchmarked purely on how autonomously they read, interpret, and invoke tool definitions.
> 2. **UI Libraries & Framework Choice**: Prompts do not mandate UI frameworks (Swing, JavaFX, LWJGL, ImGui, etc.) nor dictate visual styles, themes, or colors. The choice of UI library, rendering architecture, and visual aesthetics is left 100% to the candidate model's discretion.
> 3. **Framework Safety & Infrastructure**: No hints on safety guards, ThreadLocal bindings, or class hierarchy structures are provided in test prompts.

The suite evaluates models across dynamic Java categories:
* **UI & Visual Rendering**: Swing, JavaFX, Custom Canvas, GlassPane overlays.
* **Pure Coding & Algorithm Execution**: High-performance data structures, concurrency, reflection.
* **IDE & Tooling Integration**: Project navigation, AST refactoring, diagnostic resolution.
* **Native Library & OS Binding**: JNA C-library bindings, PTYs, system hardware interfaces.
* **Enterprise Java Architecture**: Multi-module Maven, Domain-Driven Design (DDA), DRY principles.
* **Gaming & Graphics Engines**: LWJGL 3, jMonkeyEngine 3, libGDX, 2D physics.
* **Documentation & Standards**: Javadoc compliance, ASL headers, clean execution, fail-fast rules.

---

## 2. Orchestrator Responsibilities & Automation Flow

The **Orchestrating Model** (Anahata ASI) is responsible for driving the benchmark pipeline end-to-end:

1. **Standardized Environment Provisioning**:
   * Every aspirant model receives identical toolkits (strictly `NbJava` only), permissions (`NbJava.compileAndExecute` set to `APPROVE_ALWAYS`), and normal thinking mode (`MEDIUM`).
   * `autoReplyTools` is **ON** (`true`). All proposed tool calls (e.g. `NbJava.compileAndExecute`) execute automatically without pausing for user intervention or manual approval (`Tool Prompt`).
   * **Window Title Branding Rule**: On any visual benchmark (Swing `JFrame`, `JDialog`, JavaFX `Stage`, LWJGL window, etc.), candidate models MUST always include their Model ID in the window title bar (e.g. `frame.setTitle("OS System Dashboard - models/gemini-3.6-flash")`).
   * Aspirant models are explicitly informed they are being benchmarked, the certification name, test goals, and current leaderboard rankings.

2. **Metrics & Session Telemetry**:
   * Captures: Date, Provider (with logo icon), Model ID, Thinking Level / Mode (`NONE`, `LOW`, `MEDIUM`, `HIGH`, `XHIGH`), ASI Container (e.g. `NetBeansAsiContainer`), Prompt, Toolkits, Context Providers, Time (s), Turns, Input/Output/Total Tokens.
   * Session Blueprint Recording: Captures exact `createNewAgi` parameters (Thinking Level, `includeThoughts`, `autoReplyTools`, tool permissions, and enabled toolkits).
   * Executes a `dumpHistory` call for each aspirant upon completion of every test.

3. **Visual Snapshot & Asset Naming Convention**:
   * Screenshot Storage Directory: `/assets/benchmarks/ANAHATA-AGI-1/<TEST_CODE>/` (e.g. `/assets/benchmarks/ANAHATA-AGI-1/JAVA-ARKANOID-1/`).
   * For prompts involving visual elements (JFrames, JavaFX stages, LWJGL windows), screenshots are automatically captured and stored using this convention.
   * Video runs are recorded for publication on **AnahataTV** (automated video upload, titles, descriptions, tag generation) and live streams.

4. **Multi-Channel Result Publishing**:
   * Automatically publishes scores, screenshots, and video links to:
     * Official Web Hub: `https://asi.anahata.uno/benchmarks.html`
     * Anahata ASI Twitter / X Account
     * Anahata ASI Reddit Channel
     * Priyanka's Instagram

---

## 3. Simplified 3-Category Scoring System (100 Points Total)

Scoring for each challenge combines automated evaluation metrics with human developer assessment:

| Evaluation Category | Weight | Criteria |
| :--- | :---: | :--- |
| **Accuracy & Safety** | **50%** | Zero-defect execution, requirement completion, pass/fail status, thread safety, and zero critical safety violations (e.g. no `System.exit()`). |
| **Developers Score (Anahata Core Team)** | **20%** | Qualitative architectural review by Anahata developers (code elegance, DRY, Javadoc standards, visual appeal). |
| **Efficiency & Latency** | **30%** | Time-to-first-token, per-turn execution latency, output token economy, and turn count. |

---

## 4. Test Catalog

### **Test #1: JAVA-JNA-1 (OS Hardware & System Values Dashboard)**
* **Test Code**: `JAVA-JNA-1`
* **Asset Path**: `/assets/benchmarks/ANAHATA-AGI-1/JAVA-JNA-1/`
* **Target Container**: `NetBeansAsiContainer`
* **Prompt**: You are currently being benchmarked in the official **Anahata-AGI-1 Suite** (`JAVA-JNA-1`). Your performance is evaluated across 3 criteria: **Accuracy & Technical Completion (50%)**, **Developer & Artistic Score (20%)**, and **Efficiency & Latency (30%)**. **Goal**: Build a real-time, interactive host system telemetry dashboard using JNA (`com.sun.jna.Library`). You have complete creative freedom to decide what metrics to retrieve, what UI framework to use, and how to design the interface. **Visual Window Branding Requirement**: On any visual window created, you MUST display your Model ID in the window title bar.
* **Toolkits**: `NbJava` (strictly isolated)
* **Permissions**: `NbJava.compileAndExecute` set to `APPROVE_ALWAYS`; all other `NbJava` tools set to `DENY`
* **Thinking Mode**: Normal (`MEDIUM`)
* **Window Branding Rule**: Must display Model ID on the window title bar.
* **Context Providers**: Core, ToolManager, Host, Shell

### **Test #2: JAVA-ARKANOID-1 (Arcade Game Execution)**
* **Test Code**: `JAVA-ARKANOID-1`
* **Asset Path**: `/assets/benchmarks/ANAHATA-AGI-1/JAVA-ARKANOID-1/`
* **Target Container**: `NetBeansAsiContainer`
* **Prompt**: Build a fully playable, retro Arkanoid brick-breaker game in Swing in a single Java class extending `SwingAgiTool` with collision physics, power-ups, score counter, and smooth 60 FPS EDT animation loop.
* **Toolkits**: `NbJava`, `Host`, `Screens`, `Session`, `History`, `Resources`
* **Context Providers**: Core, ToolManager, Host

---

## 5. Execution Modes

1. **Multi-Model Tournament**: Full competitive run comparing all candidate models side-by-side for video series and website leaderboards.
2. **Single-Model Benchmark**: Diagnostic run evaluating a single candidate model across all test prompts to determine certification eligibility.
