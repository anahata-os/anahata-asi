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


## 1. Have to do before Go Live: 
- [ ] merge helders branch
- [ ] chatgpt responses api with files, images, audio on responses and completions
- [ ] test minimax provider
- [ ] path param renderer / resource param renderer?
    
## 2. Post Go Live (v1.1)
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
    - [ ] **Subagent API**: Improve API for the model to spawn subagents with fine-grained control over `AgiConfig` `RequestConfig` Tool permissions.
    - [ ] **Reporting Mechanism**: Implement a way for subagents to report task completion and results back to the "Boss" agent.
    - [ ] **Parent/Child Chats**: Establish a formal parent-child relationship between `Chat` instances to support complex agentic hierarchies.

## 3. Post Go Live Go Live (v1.2)
- [ ] **CwQL**: Create a Context Window Query Language spec and implementation. So if the model spawans subagents or wants to peek into saved or disposed sessions. A simple query language can be used like 
        - sessionUUID/history(role=model)/partType=text/thought=false 
        - sessionUUID/tools/RadioTool/selectedPlaybackDevice (to look up the selectedPlaybackDevice field in the RadioTool) 
        - sessionUUID/status or sessionUUID/history/size 
        - disposed/sessionUUID/history/role=model/(matching:'Task completed')
        - remoteContaier/*(all sessions)/history/role=model/(matching:'Task completed')
        - or anything that would allow the ASI to surgically check what other agents are doing or what is in the saved or dispossed sessions dir (infinte memory)



