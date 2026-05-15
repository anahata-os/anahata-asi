# Anahata ASI Project Tasks

This file tracks the actionable tasks and tactical goals for the Anahata ASI (V2) project.

## 0. Zero Day Go Live: 
- [ ] updateTextResource doesnt let you edit the right hand side of the diff
- [ ] check ide logs work
- [ ] make "cached data" in MediaView transient? possibly in text resources too?
- [ ] Tool's turns to expire doesn't have a field in context details panel
- [ ] change all log.info to log.debug


## 1. Have to do before Go Live: 
- [ ] merge helders database branch
- [ ] chatgpt responses api and chat completions api with files, images, audio on responses and completions
- [ ] test one hf model with the chat completions api and put (Beta) 
- [ ] test minimax provider
- [ ] path parameter renderer in netbeans and resource param renderer?
    
## 2. Post Go Live (v1.1)
- [ ] **Investigate editTextResource Diff limitations**: Investigate why in `editTextResource` we cannot edit the right-hand side of the diff or do cherry-picking.
- [ ] **[CORE] Generic "TOO LARGE" Response Handling**: Implement a mechanism to detect when a `JavaMethodToolResponse` (including logs, errors, and result) exceeds a safe token/size threshold. If too large, the status should be set to `TOO_LARGE` and the content truncated or replaced with a summary to prevent context window exhaustion.
- [x] Error highlighting and code folds on diff viewer and java tool
- [ ] Rework system instructions to be more natural
- [ ] **ContextPanel**: Still some flickers when i have a node selected in the tree and a resource changes or a message arrives the right hand side disappears
- [x] **ContextPanel Message / part Details**: The messages are not like in the conversation view, the have a massive horizontal scrollbar, see if in the part viewer we can force it to show expanded without changing the expanded attribute on the model

- [x] **ATRV Height Calculation**: Add `public abstract AgiPanel getAgiPanel(Agi)` to `AbstractSwingAsiContainer` to enable hot-reloading tests for the `NetBeansTextResourceViewer` preferred height math.
- [x] **Resources Toolkit**: Add `unloadResourcesByUri(List<String> uris)` and `setProviding(List<String> uuids, boolean providing)`.
- [x] **Log Monitoring Popup**: Refactor `IDE.monitorLogs` to use `PathHandle` to bypass the NetBeans VFS and suppress the "File modified externally" dialog.
- [x] **URI/UUID Renderers**: Split into `UriParameterRenderer` and `ResourceUUIDParameterRenderer` in the core swing module. Update to generically accept `Object` and parse Lists safely without `ClassCastException`.
- [x] **Editor Shortcuts**: Fix `Ctrl+Z` (UndoManager binding to ancestor) and implement `Ctrl+S` (Save & Continue).

- [ ] **NetBeans Local History File System Integration **:
    - [ ] Local History integration via change messages.
    - [ ] Version Control with line numbers (text based glyph gutter)

- [ ] See what it would take to do the "add / remove to AGI Context for "files in a jar"
- [ ] Metabollic Donut Chart with click in to expand any section to an inner donut chart 
- [ ] **Next-Gen Project Overview/Structure**: 
    Explore UML-like structural representations for Project Structure or including the TreePathHandle or a short version of the extends and implements clauses like e Throwable i so you can include what extends and what implements along with the class level types, check token costs
    Include maven phases similar to mavne action runner nb plugin

- [ ] **Hierarchical Agent Management**:
    - [ ] **Subagent API**: Improve API for the model to spawn subagents with 
            - fine-grained control over `AgiConfig` `RequestConfig` and Tool permissions, 
            - something to approve pending tool calls as well and to simply send messages as the user or a sendContext
            - get the full details of any part or message
            - get the consolidated metadata index or always include it in the rag message of the parent
    - [ ] **Reporting Mechanism**: Implement a way for subagents to report task completion and results back to the "Boss" agent via shared dashboard or messaging system or something like that.
    

## 3. Post Go Live Go Live (v1.2)
- [ ] **CwQL**: Create a Context Window Query Language spec and implementation. So if the model spawans subagents or wants to peek into saved or disposed sessions. A simple query language can be used like 
        - sessionUUID/history(role=model)/partType=text/thought=false 
        - sessionUUID/tools/RadioTool/selectedPlaybackDevice (to look up the selectedPlaybackDevice field in the RadioTool) 
        - sessionUUID/status or sessionUUID/history/size 
        - disposed/sessionUUID/history/role=model/(matching:'Task completed')
        - remoteContaier/*(all sessions)/history/role=model/(matching:'Task completed')
        - or anything that would allow the ASI to surgically check what other agents are doing or what is in the saved or dispossed sessions dir (infinte memory)



