# Anahata ASI Project Tasks

This file tracks the actionable tasks and tactical goals for the Anahata ASI (V2) project.

## 0. Zero Day Go Live: 
- [ ] findAndReplace with wrong totalOccurrences gives a bad error message (resulting content is identical)
- [ ] change resource and session uuids to be way smaller or consider using atomicLong for resourceuids
- [ ] check if stop providing on a resource makes it actually stop providing
- [ ] check if updating the text viewport manually does update it
- [ ] model changing session nickname doesnt work or maybe the ui is not updating it
- [ ] test BatchCodeRefiner and see about adding comments to each intent to show in the ui like the other bubbles
- [ ] line comments on top of diff viewer are not aligned to the right
- [ ] test javadoc toolkit and see what would it take to allow InsertIntent or UpdateIntent to take a Javadoc object but this could take a long time
- [ ] Tool turns to expire doesn't have a field in context details panel
- [ ] add a string parameter to autobackup to log what is triggering it, check all places that trigger an autobackup and update the docos to reflect that
- [ ] change all log.info to log.debug


## 1. Have to do before Go Live: 
- [ ] merge helders database branch
- [ ] chatgpt responses api and chat completions api with files, images, audio on responses and completions
- [ ] test one hf model with the chat completions api and put (Beta) 
- [ ] test minimax provider
- [ ] path parameter renderer in netbeans and resource param renderer?
    
## 2. Post Go Live (v1.1)
- [ ] **[CORE] Generic "TOO LARGE" Response Handling**: Implement a mechanism to detect when a `JavaMethodToolResponse` (including logs, errors, and result) exceeds a safe token/size threshold. If too large, the status should be set to `TOO_LARGE` and the content truncated or replaced with a summary to prevent context window exhaustion.
- [ ] Error highlighting and code folds on diff viewer and java tool
- [ ] Rework system instructions to be more natural
- [ ] **ContextPanel**: Still some flickers when i have a node selected in the tree and a resource changes or a message arrives the right hand side disappears
- [ ] **ContextPanel Message / part Details**: The messages are not like in the conversation view, the have a massive horizontal scrollbar, see if in the part viewer we can force it to show expanded without changing the expanded attribute on the model

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



