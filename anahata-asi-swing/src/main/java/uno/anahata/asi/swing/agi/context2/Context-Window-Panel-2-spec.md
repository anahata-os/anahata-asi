# 🧘 Context Panel 2 Specification (`Context-Window-Panel-2-spec.md`)

## 1. Overview & Vision
`ContextPanel` in `uno.anahata.asi.swing.agi.context2` is the V2 reactive observability dashboard for the Anahata ASI Context Window. It replaces the legacy V1 `ContextPanel` with a **100% diff-driven, fine-grained `JXTreeTable`** architecture that eliminates tree flickering, selection loss, and scroll jumping during live AI generation turns.

---

## 2. Package & Class Naming Standard

All V2 classes reside in package `uno.anahata.asi.swing.agi.context2` without artificial `2` suffixes:

- `ContextPanel.java`: Top-level panel hosting `JXTreeTable`, totals banner, and detail card views.
- `ContextTreeTableModel.java`: Fine-grained `AbstractTreeTableModel` implementation.
- `AbstractContextNode.java`: Base node class with parent back-references and domain property listeners.
- **Node Implementations**:
  - `ContextManagerNode.java` (Root)
  - `HistoryNode.java` ➔ `MessageNode.java` ➔ `PartNode.java`
  - `ResourcesNode.java` ➔ `ResourceNode.java`
  - `ToolManagerNode.java` ➔ `ToolkitNode.java` ➔ `ToolsNode.java` ➔ `ToolNode.java`
  - `ContextProviderNode.java`
- **Utility**:
  - `TreeModelDiff.java`: Thread-safe index-preserving child list diffing engine.

---

## 3. Tree Hierarchy (Preserved Layout)

The node hierarchy remains 100% faithful to the V1 layout:

- 📄 **Core Instructions** (`ContextProviderNode`)
- ⚛️ **Tool Manager** (`ToolManagerNode`)
  - ☕ **Toolkit** (`ToolkitNode`)
    - 💡 **Context** (`ContextProviderNode`)
    - 🛠️ **Tools** (`ToolsNode`)
      - 🔧 **Tool** (`ToolNode`)
- 💎 **Resources (V2)** (`ResourcesNode`)
  - 📄 **Resource** (`ResourceNode`)
- 💓 **History** (`HistoryNode`)
  - 👤 **USER #n** (`MessageNode` - User Icon)
  - 🤖 **MODEL #n** (`MessageNode` - **Dynamic AI Provider Logo**)
    - 📝 **Part #n** (`PartNode` - Pruning state & depth badge)

---

## 4. Key Architectural Rules & Enhancements

### 4.1. Dynamic AI Provider Branding in History
- `ModelMessage` nodes query `message.getAgi().getSelectedModel().getProvider()` and render the **official AI Provider Logo** (Gemini, OpenAI, Anthropic, MiniMax, Mistral, NovaRoute, etc.) instead of generic email icons.

### 4.2. Totals Banner & Summary Footer
- Includes a dedicated **Totals Banner** displaying aggregated metrics across all categories:
  - `Instructions Tokens`
  - `Declarations Tokens`
  - `History Tokens`
  - `RAG Tokens`
  - `Grand Total Load / Token Threshold`
- *(Note: No duplicate ContextUsageBar is added in ContextPanel since `StatusPanel` already hosts `ContextUsageBar`).*

### 4.3. Visual Indicators for Truncated Resources & Disabled Items
- **Truncated Resources**: If a `ResourceNode` has a truncated viewport (`isTruncated() == true`), its token count cells are rendered in an adaptive warning/orange color (`SwingAgiConfig.getTruncatedTokenColor()`) with a helpful tooltip:
  `"Viewport is truncated (only partial file content loaded into prompt)"`.
- **Disabled Items**: Disabled toolkits, non-providing resources, disabled context providers, and offline files are rendered with desaturated icons and grayed-out text (`Color.GRAY`).

### 4.4. Multi-Selection Bulk Actions
Right-clicking multiple selected rows in `JXTreeTable` provides bulk context actions:
- **"Start / Stop Providing (Selected Resources)"**
- **"Enable / Disable (Selected Toolkits)"**
- **"Remove Selected from Context"**
- **"Pin / Un-pin Selected Parts"**

### 4.5. Organic Real-Time Ticking (Zero "Refresh Tokens" Button)
- Eliminates manual "Refresh Tokens" buttons.
- As background token calculation passes finish or as streaming message chunks arrive, domain objects fire standard JavaBeans `"tokenCount"` events.
- Nodes catch `"tokenCount"` and execute `model.notifyNodeChanged(node)`, repainting **only the specific table row in place**.

---

## 5. Fine-Grained Reactive Event Architecture

### 5.1. Zero Root Resets (`fireTreeStructureChanged` eliminated)
`ContextPanel` replaces root structural resets with fine-grained Swing `TreeModelSupport` notifications:
- **Cell Value Updates** (`fireTreeNodesChanged`): Repaints **only the specific modified row** (tokens, status, permissions).
- **Subtree Insertions** (`fireTreeNodesInserted`): Inserts new message or resource rows without touching or collapsing other rows.
- **Subtree Removals** (`fireTreeNodesRemoved`): Removes garbage-collected messages or unregistered resources smoothly.

### 5.2. Index-Preserving Child Diff Engine (`TreeModelDiff`)
When a structural change event (`"history"`, `"resources"`) arrives:
1. `diffSyncChildren()` compares current child nodes with new domain objects.
2. Reuses existing node instances for retained objects (preserving selection and open/closed expansion state).
3. Fires `fireTreeNodesInserted` for additions and `fireTreeNodesRemoved` for deletions.

---

## 6. Complete Inventory of Loaded Context Resources

This is the complete list of files and resources loaded in context during the research phase:

1. `README.md`
2. `ToolManager.java`
3. `AbstractToolkit.java`
4. `AbstractTool.java`
5. `Resources.java`
6. `TextResourceReplacements.java`
7. `TextReplacement.java`
8. `Resource.java`
9. `ResourceManager.java`
10. `ResourceView.java`
11. `AbstractResourceView.java`
12. `TextView.java`
13. `TextViewport.java`
14. `TextViewportSettings.java`
15. `ResourcePanel.java`
16. `AbstractTextResourceViewer.java`
17. `TextViewPanel.java`
18. `ContextPanel.java`
19. `ContextTreeTableModel.java`
20. `ResourceNode.java`
21. `AbstractContextNode.java`
22. `ContextManagerNode.java`
23. `ContextManager.java`
24. `ContextWindowGarbageCollector.java`
25. `ContextTreeCellRenderer.java`
26. `SwingAgiConfig.java`
27. `ContextTableCellRenderer.java`
28. `SwingTask.java`
29. `AbstractHandlePanel.java`
30. `PathHandlePanel.java`
31. `StringHandlePanel.java`
32. `UrlHandlePanel.java`
33. `AbstractViewPanel.java`
34. `MediaViewPanel.java`
35. `EdtPropertyChangeListener.java`
36. `ResourcesNode.java`
37. `ResourceUI.java`
38. `DefaultResourceUI.java`
39. `AbstractResourceHandle.java`
40. `PathHandle.java`
41. `ResourceHandle.java`
42. `StringHandle.java`
43. `UrlHandle.java`
44. `Projects.java`
45. `NetBeansAsiContainer.java`
46. `RequestConfigPanel.java`
47. `AgiConfigPanel.java`
48. `SessionConfigPanel.java`
49. `AgisTableModel.java`
50. `BasicPropertyChangeSource.java`
51. `StatusManager.java`
52. `InputPanel.java`
53. `CandidateSelectionPanel.java`
54. `ConversationPanel.java`
55. `ModelMessagePanel.java`
56. `ToolCallPanel.java`
57. `AbstractPartPanel.java`
58. `AbstractMessagePanel.java`
59. `SwingTaskMonitor.java`
60. `TaskStatusComponent.java`
61. `StatusPanel.java`
62. `ToolbarPanel.java`
63. `HeaderPanel.java`
64. `ToolPanel.java`
65. `CwGcPanel.java`
66. `AgiCard.java`
67. `MicrophonePanel.java`
68. `AudioPlaybackPanel.java`
69. `AbstractToolkitRenderer.java`
70. `IDE.java`
71. `TerminalTab.java`
72. `RSyntaxTextAreaTextResourceViewer.java`
73. `NetBeansTextResourceViewer.java`
74. `AbstractTextResourceWrite.java`
75. `ToolManagerNode.java`
76. `ToolkitNode.java`
77. `ToolsNode.java`
78. `ToolNode.java`
79. `ContextProviderNode.java`
80. `HistoryNode.java`
81. `MessageNode.java`
82. `PartNode.java`
83. `Agi.java`
84. `AgiConfig.java`
85. `RequestConfig.java`
86. `AbstractMessage.java`
87. `AbstractPart.java`
88. `AbstractToolCall.java`
89. `AbstractToolResponse.java`
90. `AbstractAsiContainer.java`
91. `BasicContextProvider.java`
92. `Context-Window-Panel-2-spec.md`
93. Master `anahata.md` instructions across all modules.
