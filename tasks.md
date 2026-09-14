# Anahata ASI Project Tasks

This file tracks the actionable tasks and tactical goals for the Anahata ASI (V2) project.

    
## 1. 1.3.0 tasks
- [ ] "add / remove to AGI Context for "files in a jar" in netbeans first

- [ ] check playback lines on linux actually match what the user sess on his ubuntu because in output lines currently shows 6 HDMI entries when there are only 2 monitors and it doesn't tell you 'which' monitor it is.

- [ ] tell helder to hurry up so we can merge helders netbeans database branch

- [ ] **[CORE] Generic "TOO LARGE" Response Handling**: Implement a mechanism to detect when a `JavaMethodToolResponse` (including logs, errors, and result) exceeds a safe token/size threshold. If too large, the status should be set to `TOO_LARGE` and the content truncated or replaced with a summary to prevent context window exhaustion.

- [ ] **NetBeans Local History File System Integration **:
    - [ ] Local History integration via change messages.
    - [ ] Version Control with line numbers (text based glyph gutter)

- [ ] Metabollic Donut Chart with click in to expand any section to an inner donut chart 

- [ ] **Next-Gen Project Overview/Structure with granular selection and a UI**: 
    Explore UML-like structural representations for Project Structure or including the TreePathHandle or a short version of the extends and implements clauses like e Throwable i so the user or the model can decide whether to include the extends or implements clauses along with the class level types, maybe even class level annotations check token costs
    Include maven phases in project overview similar to the maven action runner nb plugin
    make a ui for the projects toolkit 


## 2. Parked "enterprise grade" ideas for a 1.4.0 or a 2.0.0
- [ ] **Extract an anahata-asi-ide module**: as a base layer for both nb and intellij

- [ ] **Implement Remote ASI Containers**: think of a way to do java-to-java kryo baesd rpc one a remote asi container over tcp/http so one asi container can connect to another 
        - explore whether to use json instead of kryo for invoking remote @AgiTool annotated methods. 
        - think of the "behind a firewall" problem and how to set up a VPS on the internet to just do routing of tcp traffic so people can connect/log in to a server on the internet, let's call it singularity.anahata.uno, let's say it would be a server of ours either in OCI or in Vultr so if i want to connect to arslans's asi container on his netbeans or intellij or to anyone logged in to singularity.anahata.uno, i can "find" him and connect to his ASI Container through the singularity server without NAT/hole punching shenanigans.
        - explore if this singularity "broker" would be better /easier of implemented as war module on a glassfish (using glassfishes http piepline) or just a standalone java process using TCP.


- [ ] **CwQL**: Create a Context Window Query Language spec and implementation. So if the model spawans subagents or wants to peek into saved or disposed sessions. A simple query language can be used like 
        - sessionUUID/history(role=model)/partType=text/thought=false 
        - sessionUUID/tools/RadioTool/selectedPlaybackDevice (to look up the selectedPlaybackDevice field in the RadioTool) 
        - sessionUUID/status or sessionUUID/history/size 
        - disposed/sessionUUID/history/role=model/(matching:'Task completed')
        - remoteContaier/*(all sessions)/history/role=model/(matching:'Task completed')
        - or anything that would allow the ASI to surgically check what other agents are doing or what is in the saved or dispossed sessions dir (infinte memory)

- [ ] **Improve Hierarchical Agent Management**:
    - [ ] **Subagent API**: Improve API for the model to spawn subagents with 
            - fine-grained control over `AgiConfig` `RequestConfig` and Tool permissions, 
            - something to approve pending tool calls as well and to simply send messages as the user or a sendContext
            - get the full details of any part or message
            - get the consolidated metadata index or always include it in the rag message of the parent
    - [ ] **Reporting Mechanism**: Implement a way for subagents to report task completion and results back to the "Boss" agent via shared dashboard or messaging system or something like that.





