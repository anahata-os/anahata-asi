# NetBeans Annotation Pipeline Specification (`annotators.md`) 

### Annotators SPEC v1.0

Intro
-----------
Anahata ASI supports multiple concurrent sessions within the IDE and there are multiple ways of adding context to a given session: e.g. The user can right click on files or folders and do either via user actions such as
- add files to context, 
- remove files from context
- add projects to context
- remove projects from context
- toggling the "providing flag" in the Context tab of the ChatPanel.

Moreover, the model can also add / remove context by loading / unloading resources or by toggling the 'providing' flag of any context providers by their unique id.

Any class that implements ContextProvider that is either explicitely registered in ContextManager ("top level") or is a child of a descendant of one.
ContextProviders are hierarchical so a ContextProvider like ProjectContextProvider can have many children and those children can have more children.
Disabling a parent context provider (setting providing= false) automatically disables all descendants of that context provider from providing context.

Any toolkit that extends AnahataToolkit already implements the ContextProvider interface (as seen in the context tab in the ui, every toolkit that extends AnahataToolkit has a child node called "Context") so most toolkits in ASI have both capacities: running tools and providing additional context (either by implementing populateRagMessage 'OR' by implementing getChildContextProviders()).

Resources (anything that extends AbstractResource) also implement ContextProvider and are also included in the RagMessage via ResourceManager.

In summary, there are currently three root context providers:
- CoreContextProvider (very important)
- ToolManager (provides context about itself (e.g. what are the installed toolkits, what tools they have and their state (enabled / disabled) and implements getChildrenContextProviders returning a child for each toolkit that implements ContextProvider (all in our case as all our toolkits extend AnahataToolkit which itself implements ContextProvider). Note that while a "toolkit" can be enabled or disabled from a tool execution point of view, its associated context provider may still be 'providing'. e.g. The java toolkit may be disabled but the jvm properties of the system can still be added to the context)
- ResourceManager (provides context for all loaded resources)

All 'effectively providing' context providers gets added to the RAG message on every turn and resources like files dont need explict reload as there are lastModified checks in place to ensure that any resources that were modified since they were initiallly loaded gets refreshed from disk -if needed be- during the RAG message generation if needed be. That is what the LIVE refresh policy means. 

The list of "active" sessions should follow the same order as AsiContainter.getActiveSessions().

##NetBeans Options to give visual cues about context for active sessions
------------------------------------------------------------------------
-Adding badges to projects or any file or folder. (like the error badges or the git badges)
-annotating file names (like the [master] annotation that the git annotation provider does)
-adding content to the tooltip of any file or folder (e.g. like netbeans adds info a about the project and the netbeans gir plugin adds 'in synch with master branch')

-ProjectIconAnnoatator - can add "badges" to project icons (and possibly tooltips to that badge)

-FileAnnoatationProvider - can annotate any FileObject's names and add "badges" regardless of whether it is a file or a folder.

As far as we know, the only way to add to tooltips is by "hacking" the icon image through ImageUtilities.addToolTipToImage

Very important to read the javadocs of AnnotationProvider and BaseAnnotationProvider to understand the semantics as in the "netbeans" model, once something annotates a name (returns not null), the susquent annotation providers are skipped. Very often we break the git annotations when we annotate a file or we cause duplicate git annotations.

When working out how many "resources in context" a folder has, use path comparisons rather than recursive looking throuh each folders.

ContextProviders that are not resources (like "Project Alerts Context Providers") only apply to "projects" not to files or folders. Files or folders should only worry about AbstractPathResources (like TextFileResource or BinaryFileResource)


##Projects Tab
----------------
- **Project** 
  - name annotation: `[sum1][sum]` The total number of "effectively providing" context providers regardless of their nature (resources/toolkits/standalone context providers)    
  - anahata badge at 8,0 if that project has any ContextProviders in any session
  - tooltip: 
    In Context In: 
    for each "active" session:
     <b>sessionDisplayName<b>: 
            Providing: List of effecitvely Providing context providers

- **Files **: Use **Round Brackets** `()`.
  - name annotation: `(displayName)` if in **exactly one** session (e.g., `(messi)`).
  - name annotation : `(n)` if in **multiple** sessions (e.g., `(2)`).
  - icon: anahata icon badge at 16,0
  - tooltip: In Context in: 
            session 1 `displayName`,'\n'
            session 2 displayName

- **Source Packages**: Use **Square Brackets** `[]`.
  - format: `[sum1][sum2]...` (one bracket per active session with the sum of all resources in that package).
  - note: in netbeans users can toggle the projects tab to either show "source packages" as "folders", so depending on the user' selection, netbeans can show one node for each source package or just the top most package (e.g. uno.anahata.asi) and then subpackages under that node (folder style)

- **Folders (e.g Other Sources "Virtual Folder" and real folders in there like `nbm` or `src/main/resources`) **: Use **Square Brackets** `[]`.
  - name annotations: `[sum1][sum2]...` (one bracket group[] per active session) with the recursive acumulative number of context providers.
  - Problems: 

- Known Dangers!!! root nodes (projects) and possibly virtual folders like "Other Sources" they don't behave as expected, very difficult in the methods of FileAnnotationProvider. Also, in a FileAnnoatationProvider its difficult to know if you are annotating something in the Projects, Files or Favourites tab.
also annotating already annotated files that already have html can be tricky.



##Files Tab
----------------
Here we don't have "projects" but we have the same problems as in the projects tab when it comes to root nodes, as they are may not be normal "folders". If nb detects the folder is a project, it may pass the pom.xml to the file annotator as FileObject rather than the FileObject representing the project folder and "things like that". Expect the unexpected.

Normally for anything that "is" or "looks like an OS folder" the behaviour should be the same as for "Folders" in the Projects tab (badge if there is any resource anywhere under that folder in *any* session) and [sum1][sum2] with the cumulative total of all resources in context for each active session.

##Favourites Tab
------------------
Same quirks as files, even if they are not root nodes, any folder that netbeans "detects" as a project behaves differently, netbeans may not pass the actual FileObject for the folder, it may pass things like the pom.xml FileObject to the file annotator (rather than the folder itself, this is the problematic zone)

















 
### C. Name Annotation Logic (`annotateNameHtml`)
- **Aggregation**: Perform safe manual delegation first to get the Git HTML.
- **Injection**: Append our label before the closing `</html>` tag of the delegated result.


## 1. The Core Conflict
NetBeans has two distinct pipelines for decorating the same set of physical files:
- **Logical View (Projects Tab)**: Uses `ProjectIconAnnotator`. Authority: `AnahataProjectIconAnnotator`. Input: `Project` object.
- **Physical View (Files/Favorites Tabs)**: Uses `AnnotationProvider`. Authority: `FileAnnotationProvider`. Input: `Set<FileObject>`.

## 2. Technical Findings (From NetBeans Sources)
Studying `AnnotationProvider` and `BaseAnnotationProvider` revealed the following:
- **Replacement Model**: The pipeline for names and icons is a "First Responder Wins" or "Replacement" model. If a provider returns a non-null value, it terminates the chain for that specific file cluster.
- **HTML Parameter Constraint**: The `name` parameter passed to `annotateNameHtml` is **guaranteed to be plain text** (no HTML tags, just escaped characters). This means providers *cannot* see what previous providers in the chain have done unless they manually delegate.
- **Recursive Trap**: Because multiple providers might try to delegate to each other, a simple `Lookup.lookupAll` loop leads to a `StackOverflowError` (as seen in Round 3).

## 3. The "Hybrid" Pain Points
A "Hybrid" node is a Project Root folder.
- **In Projects Tab**: It is a `Project` node. NetBeans handles the Git branch name (e.g. `[main]`) as part of the logical node display.
- **In Files/Favorites Tabs**: It is just a folder (`FileObject`). NetBeans applies Git branch names via the `AnnotationProvider` pipeline.
- **Detection**: To know which view is being annotated inside `FileAnnotationProvider`, we use `StackWalker` to check if the caller is `org.netbeans.modules.project.ui`.

## 4. Round-by-Round Log

### Round 1-3: Interception & Delegation
- **What we tried**: Manual delegation in `FileAnnotationProvider` by looking up all `AnnotationProvider`s and calling them recursively.
- **What worked**: We successfully picked up Git icons and branch names.
- **What didn't work**: It was architecturally fragile.
- **Regressions**: **JVM CRASH.** Created an infinite recursion loop in the NetBeans lookup system, leading to a `StackOverflowError`.

### Round 4-6: Active Cleanup & Unified Indexing
- **What we tried**: Stripping our own `<font>` tags to prevent "stale" labels. Removing manual delegation to fix the crash.
- **What worked**: The Context tab became clean. Stable JVM.
- **What didn't work**: Git branch names and status colors (blue/red) disappeared because we were "terminating" the pipeline early with our own HTML.
- **Regressions**: No Git info in Physical views.

### Round 7: The Safe Master Aggregator (Current)
- **What we tried**: Moving to position 10000. Implementing a **ThreadLocal Guard** for safe manual delegation.
- **What worked**: TBD (Awaiting reload).
- **Strategy**: By running last (position 10000) and using a `DELEGATING` guard, we can safely call all other providers to get the "Base" state (Git/SVN), and then append our session labels to that base.
- **Hybrid Detection**: Improved `isProjectRoot` logic to allow badging in Physical views (Files/Favorites) while skipping in Logical views (Projects).

## Round 10.1: The "Messi" Fix (Stability)
- **Authority**: 
    - `AnahataProjectIconAnnotator` is the **exclusive** authority for Project nodes in the Projects tab.
    - `FileAnnotationProvider` **must return null** for Project Roots to prevent duplicate/incorrect labeling (like `(messi)` on project nodes).
- **Git Preservation**: 
    - `FileAnnotationProvider` now uses `delegateNameHtml` with a `ThreadLocal` guard to retrieve Git info before appending session labels.
    - Positions remain at 2000 (Projects) and 2100 (Files) to ensure we run AFTER Git but before the platform finishes.

## 5. Formatting Spec (Round 10 Unified)

### A. Counting & Ordering Logic
- **Unified Totals**: For a folder or project node, the count for each session is the **SUM** of all its context providers (Overview, anahata.md) plus all its resources (files).
- **Session Ordering**: Brackets MUST follow the exact order of `AsiContainer.getActiveChats()`.


---
*Last Updated: 2026-02-11 23:14 (Round 7 Safe Aggregator)*
