# Anahata AI V2 - Consolidated Task List (Core & Swing Modules)

This document consolidates all active and pending tasks for the core framework and its Swing UI module.

## High Priority / Core Framework

- [ ] Implement FailureTracker in V2 ToolManager
- [ ] Implement Asynchronous Job Execution for V2 Tools
- [ ] **GroundingMetadata Refactor:** Check the genai `GroundingMetadata` sources using `JavaSources` and move our gemini adapter logic to store the grounding metadata on the model message, not on the `Response`.
- [ ] **AbstractRenderer Refactor:** Update the swing renderer and see if we can make `AbstractRenderer` with the fields/logic that are not "part" related from `AbstractPartRenderer` so we can have renderers that render "other things".

## UI / Swing Module Tasks

- [ ] Implement Toolbar (Clear Chat, Toggle Functions, Toggle Autoreply)
- [ ] Implement Top Bar (provider and model selection combo boxes)
- [ ] Flesh out `ToolsPanel` details (schemas, logging)
- [ ] Advanced `InputPanel` Implementation (SwingX, Live Preview, Dynamic `UserMessage`, Stop Button, Send/Queue Logic, Generic SwingWorker)
- [ ] Live Workspace: Implement the functionality for the "Live Workspace" button
- [ ] Session Management: Implement the "Save" and "Load Session" buttons
- [ ] **Report Bug Capability:** Implement a feature to allow users to report bugs directly from the application, including relevant context.

## Research & Technical Debt

- [ ] Research Shared Schema Definitions for Token Optimization
- [ ] Improve Binary File Handling in `LocalFiles` Tool