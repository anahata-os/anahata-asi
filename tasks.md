# Anahata ASI Project Tasks

This file tracks the actionable tasks and tactical goals for the Anahata ASI (V2) project.

## 1. 1.2.8 tasks
*(Format: `[Implemented] [Tested]`)*
- [x] [ ] **[CORE] Resource Content Visibility Percentage & Truncation Warnings**: Implement a top-level `getVisiblePercentage()` in `ResourceView` and `AbstractResourceView` (with streaming character calculation in `TextView` and 100% default in `MediaView`). Update `Resource.getHeader()` to display a clear, unmistakable warning when `< 100%` visible (e.g. `**WARNING: PARTIAL VIEW (26.8% visible). Use Resources.setFullView to expand**`), and an explicit `DISABLED / NOT PROVIDING` status when `providing == false` so models never mistake disabled resources for truncated viewports.
- [x] [X]  **[CORE] Batch Resources.setProviding Single Event Pulse**: Refactor `Resources.setProviding` and `ResourceManager` so that updating the providing flag for multiple resources fires a single consolidated batch change event, eliminating UI glitches and event cascades when 50+ resources are toggled simultaneously.
- [x] [X] **[SWING] Rework ContextPanel Tree for Incremental Updates**: Refactor the context tree updates in `ContextPanel` to perform targeted, incremental tree model node changes (`nodesChanged` / `nodeStructureChanged`) instead of wiping and rebuilding the entire tree model on every turn or status change.
- [X] [ ] **[TOKEN MATHS] Test & Verify Multimodal Token Maths Across Providers**: Formally verify and test image, video (duration-based @ 290 tokens/sec for Gemini), and audio (duration-based @ 32 tokens/sec for Gemini, 10 tokens/sec for OpenAI) token counting using `MediaMetadataUtils`.
- [X] [ ] **[TOKEN MATHS]**: Formally verify and test image, video (duration-based @ 290 tokens/sec for Gemini), and audio (duration-based @ 32 tokens/sec for Gemini, 10 tokens/sec for OpenAI) token counting using `MediaMetadataUtils`.
- [x] [x] **[CORE] User Feedback Preservation ("Sacred Atomic Gold")**: Ensure `userFeedback` on tool calls is permanently preserved and visible across all system layers: active part metadata headers, pruned hints, garbage collection log records (`summarizeParts`), and `AbstractToolCall.asText()` for `dumpHistory`. Every keystroke from a user is atomic gold.
- [x] [x] **[BUILD] JDK 21+ Bytecode & Tooling Compatibility Audit**: Ensure all multi-module POMs enforce `<maven.compiler.release>21</maven.compiler.release>` so all generated artifacts are strictly JDK 21+ compliant, preventing accidental linkage to JDK 22–26 methods while running on JDK 26.
- [x] [ ] **[SWING] Resource Content Visibility Percentage & Truncation Warnings**: context panel colors for truncated resources are flaky
- [] [ ]  **[SWING] Resources and Toolkits or Context Providers in context should be shown in status bar or intput bar**: 
- [] [ ]  **[SWING] Staged message should show above input text area, not below** and sending a message when there is already a staged message should maybe just append more parts to the staged message or turn the staged message into a list of staged messages: 
- [] [ ]  **[TOKENS] Show tokens (according to the selecte model) on message headers and partformat**: make it in headers in square brackets [108] **: 

    
## 2. 1.3.0 tasks

- [ ] "add / remove to AGI Context for "files in a jar" in netbeans first

- [ ] check playback lines on linux actually match what the user sess on his ubuntu because in output lines currently shows 6 HDMI entries when there are only 2 monitors and it doesn't tell you 'which' monitor it is.

- [ ] tell helder to hurry up so we can merge helders netbeans database branch

- [ ] **[CORE] Generic "TOO LARGE" Response Handling**: Implement a mechanism to detect when a `JavaMethodToolResponse` (including logs, errors, and result) exceeds a safe token/size threshold. If too large, the status should be set to `TOO_LARGE` and it should dump the json represntation of the JavaMethodToolResponse to a text file and registered as a resource with the default viewport so the model can paginate on it if its worth it. Large responses even crash the ToolCallPanel's result text area exhausting the EDT thread in line wrapping calculations. So bad.

- [ ] **NetBeans Local History File System Integration **:
    - [ ] Local History integration via change messages.
    - [ ] Version Control with line numbers (text based glyph gutter)

- [ ] Metabollic Donut Chart with click in to expand any section to an inner donut chart 

- [ ] **Next-Gen Project Overview/Structure with granular selection and a UI**: 
    Explore UML-like structural representations for Project Structure or including the TreePathHandle or a short version of the extends and implements clauses like e Throwable i so the user or the model can decide whether to include the extends or implements clauses along with the class level types, maybe even class level annotations check token costs
    Include maven phases in project overview similar to the maven action runner nb plugin
    make a ui for the projects toolkit 


## 3. Parked "enterprise grade" ideas for a 1.4.0 or a 2.0.0
- [ ] **Extract an anahata-asi-ide module**: as a base layer for both nb and intellij so thins like a Projects toolkit UI where the user can select the level of details in the project structure provider.

- [ ] **Implement Remote ASI Containers**: think of a way to do java-to-java kryo baesd rpc one a remote asi container over tcp/http so one asi container can connect to another 
        - explore whether to use json instead of kryo for invoking remote @AgiTool annotated methods. 
        - think of the "behind a firewall" problem and how to set up a VPS on the internet to just do routing of tcp traffic so people can connect/log in to a server on the internet, let's call it singularity.anahata.uno, let's say it would be a server of ours either in OCI or in Vultr so if i want to connect to arslans's asi container on his netbeans or intellij or to anyone logged in to singularity.anahata.uno, i can "find" him and connect to his ASI Container through the singularity server without NAT/hole punching shenanigans.
        - explore if this singularity "broker" would be better /easier of implemented as war module on a glassfish (using glassfishes http piepline) or just a standalone java process using TCP.

- [ ] **Agi Folder**: make all sessions have an folder (not just a kryo file) for session related temp files / work files.
- [ ] **AgiClassLoader**: make it easy to dump the sources of compile agi classess to a directory (and or store them in the new Agi folder dir for the use cases of: 
        a) a kryo session failing to deserialize, 
        b) materializing an inmemory prototype into an actual java project.

- [ ] **AgiContainerClassLoader**: have "another classloader" that would be the parent of the AgiClassLoader in the java tool for the user/agent to be able to add "toolkits from a jar" at the Agi level and not just at the Java toolkit level.
- [ ] **ToolManager UI / tree node**: to let the user add/remove toolkit classess by fqn and also allow for toolkits in jar.


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

- [ ] **Kryo serialization fallback**: make a export to md functionality in Agi and time how long it would take to append an '.md' backup of the session with a dumpHistory, and a list of resources in context to mitigate kryo deserialization issues during upgrades or plugin reloads.





