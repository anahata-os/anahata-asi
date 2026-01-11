# Anahata AI V2 Mission Board

This board tracks the high-priority tactical goals for the V2 architecture migration and refinement.

## 🎯 Active Mission: V2 UI & Tooling Refinement

### ✅ Completed
- [x] **Integrated Tool Execution Rendering**: Implement a left-to-right layout where tool responses are displayed next to their corresponding tool calls within the model message, eliminating the separate tool message panel.
- [x] **Elegant Java Context**: Move the tool execution context (ThreadLocal) to `JavaMethodToolResponse` and support a base class (`AnahataTool`) for model-compiled code, with rich system instructions.
- [x] **Staged Message Visibility**: Fix the bug where staged messages were not visible in the UI and ensure layout refreshes correctly.
- [x] **Live Sessions UI**: Change live sessions to show sessions in a tabbed pane (rather than in a stack pane).

### 🔴 Backlog
- [ ] **In-Band Metadata Injection**: Inject message and part IDs/metadata into the prompt (Text headers for User/Model, JSON metadata for Tools) to improve model self-awareness.
- [ ] **Billed Tokens Display**: Test if billed tokens are correctly shown in the model message headers.
- [ ] **GLM 4.7 Provider**: Implement the V2 adapter for Zhipu AI's GLM 4.7.
- [ ] **OpenAI Provider**: Implement the V2 adapter for OpenAI.
- [ ] **Anthropic Provider**: Implement the V2 adapter for Anthropic.


### 🧊 Hanging in Limbo (API Limitations)
- [ ] **Multi-Candidate Selection**: Refine the `CandidateSelectionPanel` to handle grounding metadata and token counts per candidate. (Gemini API currently supports only 1 candidate).
- [ ] **Test Multi-Candidate Selection**: Verify the UI and logic for handling multiple model responses.

---
*Last Updated: Sat Jan 10 13:30:00 CET 2026*
