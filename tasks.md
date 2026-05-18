# Anahata ASI Project Tasks

This file tracks the actionable tasks and tactical goals for the Anahata ASI (V2) project.

## 0. Zero Day Go Live: 
- [ ] make "cached data" in MediaView transient? possibly in text resources too?

- [] ContextPanel -> 
    - ResourcePanel shows the editors way below for Reformatter.java for example, may be related to resources that don't fit on the default viewport and the issue may occur after doing expand to fit viewport.

- [ ] change all log.info to log.debug

- [ ] enums in json schemas dont have description

- [ ] recording audio on the microphon button default recording device leaves a dangling swing task and doesn't record

- [ ] test playback lines work correctly

- [ ] "add / remove to AGI Context for "files in a jar" causes double context menu item 

## 1. Have to do before Go Live: 
- [ ] merge helders database branch
- [ ] chatgpt responses api verified organisation/store/text reasoning
- [ ] test one hf model with the chat completions api and put (Beta) 
    
## 2. Post Go Live (v1.1)
- [ ] **Preferences Panel**: 
    [ ] Enabled / Disabled toolkits

- [ ] **Context Panel**: 
    [ ] prune / remove multiple messages
    [ ] disable multiple toolkits at once
    [ ] convert to diff based update events and add something to 
- [ ] **Investigate editTextResource Diff limitations**: Investigate why in `editTextResource` no model can figure out the lines.
- [ ] **[CORE] Generic "TOO LARGE" Response Handling**: Implement a mechanism to detect when a `JavaMethodToolResponse` (including logs, errors, and result) exceeds a safe token/size threshold. If too large, the status should be set to `TOO_LARGE` and the content truncated or replaced with a summary to prevent context window exhaustion.
- [ ] Rework system instructions to be more natural
- [ ] **ContextPanel**: Still some flickers when i have a node selected in the tree and a resource changes or a message arrives the right hand side disappears

- [ ] **NetBeans Local History File System Integration **:
    - [ ] Local History integration via change messages.
    - [ ] Version Control with line numbers (text based glyph gutter)

- [ ] Metabollic Donut Chart with click in to expand any section to an inner donut chart 

- [ ] **Next-Gen Project Overview/Structure**: 
    Explore UML-like structural representations for Project Structure or including the TreePathHandle or a short version of the extends and implements clauses like e Throwable i so you can include what extends and what implements along with the class level types, check token costs
    Include maven phases similar to mavne action runner nb plugin

- [ ] **Improve Hierarchical Agent Management**:
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



