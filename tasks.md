# Anahata AI V2 Mission Board

This board tracks the high-priority tactical goals for the JASI Platform (Java Artificial Super Intelligence) migration and refinement.

## 🎯 Active Mission: JASI Container & CWGC Refinement

### ✅ Completed
- [x] **Integrated Tool Execution Rendering**: Implement a left-to-right layout where tool responses are displayed next to their corresponding tool calls within the model message.
- [x] **Elegant Java Context**: Move the tool execution context (ThreadLocal) to `JavaMethodToolResponse` and support a base class (`AnahataTool`) for model-compiled code.
- [x] **Staged Message Visibility**: Fix the bug where staged messages were not visible in the UI.
- [x] **Live Sessions UI**: Change live sessions to show sessions in a tabbed pane.
- [x] **In-Band Metadata Injection**: Inject message and part IDs/metadata into the prompt (Text headers for User/Model, JSON metadata for Tools).
- [x] **Billed Tokens Display**: Implemented reactive display of billed tokens in model message headers.
- [x] **Token Estimation & Streaming Optimization**: Implemented robust token estimation for all parts and incremental counting for streaming text parts.
- [x] **Tool Call/Response Atomic Pruning**: Implemented 'Response is Master' architecture where tool calls delegate their context state to the response.
- [x] **Provider-Level Token Accuracy**: Update token counts using the `toJson()` of the underlying provider when building final API elements.

### 🔴 Backlog
- [ ] **V2 Serialization Investigation**: Investigate why V2 tool response serialization (including logs and attachments) works standalone but fails with `GenAiIOException`/`InterruptedIOException` in NetBeans. Explore if payload size, circular references, or environment-specific timeouts are the cause.
- [ ] **Editable Tool Parameters**: Support editing tool call parameters that are easy to edit (like strings) before execution.
- [ ] **Vertical Tool Layout**: Explore wrapping tool calls and responses in `JXTitledPanels` and laying them vertically rather than left-to-right.
- [ ] **V1 Tool Migration**: Move V1 tools (`Maven`, `NetBeansProjectJVM`, `CodeModel`, `Coding`, etc.) to V2.
- [ ] **NetBeans Files Integration**: Explore ways to integrate the `Files` tool into NetBeans (subclassing or common interface) for locking on write and local history integration.
- [ ] **Context Annotators**: Integrate "in-context" icons, project/file annotators, and "add to actions" from `mavenproject9` proof of concept.
- [ ] **Serialization**: Implement full session serialization (Kryo) to support JASI container passivation.
- [ ] **Global Preferences**: Implement a global preferences panel in the Swing UI for tool settings, etc.
- [ ] **Bug: Nickname Sync**: Fix bug where live sessions card view doesn't update when the chat nickname changes.
- [ ] **V2 Website & Javadocs**: Create the v2 website with the aggregated javadocs and support versioned documentation (e.g., 1.0.0-SNAPSHOT).
- [ ] **Commander Check**: Review and update the Commander's Briefing.
- [ ] **Bug: Toolkit Panel Vanishing**: Fix bug where the toolkit details panel vanishes when updating permissions or clicking the enabled button.
- [ ] **Plugin Update Center**: Explore having a NetBeans plugin update center and a plugin "just" to install the "Anahata Plugin Update Center".
- [ ] **Resources Heatmap**: Add a resources panel to the chat UI to quickly view all resources in context, similar to the V1 heatmap.
- [ ] **ScreenCapture Migration**: Move the `ScreenCapture` tool from V1.
- [ ] **Media Tool Migration**: Move `RadioTool` and `DjTool` from V1 into `anahata-asi-yam`.
- [ ] **RadioTool Awareness**: In the V2 `RadioTool`, let the model know what station is playing.
- [ ] **Projects Tool Migration**: Move the NetBeans `Projects` tool from V1.
- [ ] **Semantic Code Overview**: Explore `CodeModel` ways of providing a "java components/elements" based view of open projects (hierarchies, package info, class javadocs).
- [ ] **Social Presence**: Create an Anahata Twitter account.
- [ ] **Demo Automation**: Explore uploading YouTube demo videos straight from NetBeans.
- [ ] **Competitive Analysis**: Explore how to "beat" Open Code.
- [ ] **Diff Screenshots**: Enable the user to attach screenshots of a diff.
- [ ] **GLM 4.7 Provider**: Implement the V2 adapter for Zhipu AI's GLM 4.7.
- [ ] **OpenAI Provider**: Implement the V2 adapter for OpenAI.
- [ ] **Anthropic Provider**: Implement the V2 adapter for Anthropic.

### 🧊 Hanging in Limbo (API Limitations)
- [ ] **Multi-Candidate Selection**: Refine the `CandidateSelectionPanel` to handle grounding metadata and token counts per candidate.
- [ ] **Test Multi-Candidate Selection**: Verify the UI and logic for handling multiple model responses.

---
*Last Updated: Thu Jan 15 23:30:00 CET 2026*
