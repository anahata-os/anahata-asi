/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.terminal;

import java.awt.Component;
import java.awt.Container;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import javax.swing.JComponent;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.lib.terminalemulator.Term;
import org.netbeans.modules.dlight.api.terminal.TerminalSupport;
import org.netbeans.modules.dlight.terminal.ui.TerminalContainerTopComponent;
import org.netbeans.modules.nativeexecution.api.ExecutionEnvironment;
import org.netbeans.modules.nativeexecution.api.ExecutionEnvironmentFactory;
import org.netbeans.modules.terminal.api.ui.TerminalContainer;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.resource.view.TextViewportSettings;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * A NetBeans-integrated toolkit for managing, tracking, and typing into native
 * terminal tabs.
 * <p>
 * This toolkit acts as a global ContextProvider that maintains a dynamic list
 * of active {@link TerminalTab}s. It manages their persistent viewport settings
 * and facilitates seamless interaction between the AI and the IDE's terminal system.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for opening, tracking, and typing into NetBeans Terminal tabs.")
public class NbTerminal extends AnahataToolkit {

    /**
     * Constructs a new NbTerminal toolkit instance.
     * The child terminal tabs are lazily and dynamically populated during the turn's sync pass.
     */
    public NbTerminal() {
        // Constructor is empty; children are dynamically populated by syncTerminalTabs()
    }

    /**
     * {@inheritDoc}
     * <p>
     * Performs the initial synchronization pass to register any pre-existing terminal tabs in the workspace.
     * </p>
     */
    @Override
    public void initialize() {
        super.initialize();
        syncTerminalTabs();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Re-synchronizes active terminal tabs with their newly instantiated Swing Term components
     * upon session activation, restoring their transient memory references seamlessly.
     * </p>
     */
    @Override
    public void postActivate() {
        super.postActivate();
        syncTerminalTabs();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Performs a real-time synchronization scan of the terminal window layout.
     * Appends a beautifully formatted Markdown table of all open terminal tabs,
     * their unique IDs, display titles, and providing states (active or muted) 
     * directly into the JIT-compiled RAG message.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) {
        syncTerminalTabs();

        if (childrenProviders.isEmpty()) {
            return;
        }

        StringBuilder sb = new StringBuilder();
        sb.append("### NetBeans Terminal Guidance\n");
        sb.append("- **NbTerminal.run**: Executes commands inside an interactive NetBeans GUI terminal tab (real POSIX PTY, visible in the IDE). If terminalTabId is provided, executes in that specific tab; if omitted, creates a new terminal tab with tabTitle (or 'Anahata - <displayName>'). Supports persistent shell state across turns (environment variables, working directory, SSH sessions, sudo). If timeoutMillis is provided, blocks until completion and returns the combined visual output (throwing TimeoutException on timeout); if omitted, dispatches fire-and-forget and returns null immediately.\n");
        sb.append("- **Shell.runAndWait**: Spawns an isolated, headless, non-interactive subprocess directly on the local host with no GUI and no persistent session state.\n\n");
        sb.append("### NetBeans Terminal Tabs\n");
        sb.append("| Tab ID | Tab Title | Context Status | Description |\n");
        sb.append("|---|---|---|---|\n");

        for (ContextProvider cp : childrenProviders) {
            if (cp instanceof TerminalTab tab) {
                String status = tab.isProviding() ? "Providing" : "Muted (Not Providing)";
                sb.append(String.format("| `%d` | %s | %s | %s |\n", 
                        tab.getTabId(), 
                        tab.getName(), 
                        status, 
                        tab.getDescription()));
            }
        }
        
        ragMessage.addTextPart(sb.toString());
    }

    /**
     * Executes a command in a NetBeans terminal tab.
     * <p>
     * If {@code terminalTabId} is provided, executes the command in that specific tab.
     * If {@code terminalTabId} is omitted (null), a new terminal tab is created with
     * {@code tabTitle} (or defaults to {@code "Anahata - " + getAgi().getDisplayName()}).
     * </p>
     * <p>
     * If {@code timeoutMillis} is specified and greater than 0, blocks until command completion,
     * returning the combined stdout and stderr visual output, or throwing an exception if the timeout
     * is exceeded. If {@code timeoutMillis} is omitted or non-positive, dispatches asynchronously
     * (fire-and-forget) and returns {@code null} immediately.
     * </p>
     * 
     * @param command The shell command sequence to execute.
     * @param terminalTabId Optional unique ID of an existing terminal tab. If omitted, a new tab is created.
     * @param tabTitle Title of the new tab if creating one. Defaults to "Anahata - <displayName>".
     * @param directory Initial working directory if a new tab needs to be created.
     * @param timeoutMillis Maximum milliseconds to wait for completion. If null, runs fire-and-forget.
     * @return The captured command output if timeoutMillis is provided, or null if fire-and-forget.
     * @throws Exception If command execution fails or times out.
     */
    @AgiTool("Executes a command in a NetBeans terminal tab. "
            + "If terminalTabId is specified, executes in that tab; if omitted, creates a new terminal tab with tabTitle (or 'Anahata - <displayName>'). "
            + "If timeoutMillis is specified, blocks until execution completes and returns the output (throwing an exception on timeout); "
            + "if omitted, dispatches asynchronously (fire-and-forget) and returns null immediately.")
    public String run(
            @AgiToolParam(value = "The shell command to execute.", required = true) String command,
            @AgiToolParam(value = "Optional unique ID of an existing terminal tab. If omitted, a new terminal tab is created.", required = false) Long terminalTabId,
            @AgiToolParam(value = "Title for the new tab if creating one. Defaults to 'Anahata - <displayName>'.", required = false) String tabTitle,
            @AgiToolParam(value = "Initial working directory if a new tab needs to be created.", required = false) String directory,
            @AgiToolParam(value = "Maximum timeout in milliseconds to wait for completion. If provided, blocks and returns output; if omitted, runs fire-and-forget and returns null.", required = false) Long timeoutMillis
    ) throws Exception {
        TerminalTab targetTab = null;
        if (terminalTabId != null) {
            targetTab = findTabById(terminalTabId).orElseThrow(() -> 
                    new AgiToolException("Terminal tab not found with ID: " + terminalTabId));
            log("Targeting existing terminal tab with ID: " + terminalTabId + " ('" + targetTab.getName() + "')");
        } else {
            String baseTitle = (tabTitle != null && !tabTitle.isBlank())
                    ? tabTitle.trim()
                    : "Anahata - " + getAgi().getDisplayName();
            String resolvedTitle = resolveUniqueTabTitle(baseTitle);

            List<Long> existingIds = new ArrayList<>();
            for (ContextProvider cp : childrenProviders) {
                if (cp instanceof TerminalTab t) {
                    existingIds.add(t.getTabId());
                }
            }

            log("Opening new terminal tab: '" + resolvedTitle + "'" + (directory != null ? " in directory: " + directory : ""));
            openLocalTerminal(resolvedTitle, directory);

            long startWait = System.currentTimeMillis();
            while (System.currentTimeMillis() - startWait < 3000) {
                syncTerminalTabs();
                for (ContextProvider cp : childrenProviders) {
                    if (cp instanceof TerminalTab t && !existingIds.contains(t.getTabId())) {
                        targetTab = t;
                        break;
                    }
                }
                if (targetTab != null) {
                    break;
                }
                Thread.sleep(50);
            }
            if (targetTab == null) {
                throw new AgiToolException("Failed to create or locate newly opened terminal tab: " + resolvedTitle);
            }
            log("Found newly opened terminal tab with ID: " + targetTab.getTabId() + " ('" + targetTab.getName() + "')");

            // Wait for the shell process to initialize and render its initial prompt
            long promptWait = System.currentTimeMillis();
            while (System.currentTimeMillis() - promptWait < 2500) {
                if (!targetTab.getTermContent().trim().isEmpty()) {
                    break;
                }
                Thread.sleep(50);
            }
            Thread.sleep(100);
        }

        final TerminalTab tabToRun = targetTab;
        log("Dispatching command to tab '" + tabToRun.getName() + "': " + command);
        if (timeoutMillis != null && timeoutMillis > 0) {
            log("Waiting for command completion (timeout: " + timeoutMillis + " ms)...");
            String output = tabToRun.runAndWait(command, timeoutMillis);
            log("Command completed successfully. Output length: " + (output != null ? output.length() : 0) + " chars.");
            return output;
        } else {
            SwingUtils.runInEDTAndWait(() -> {
                try {
                    tabToRun.typeCommand(command);
                } catch (Exception e) {
                    log.error("Failed to dispatch command to terminal tab {}: {}", tabToRun.getTabId(), command, e);
                    throw new RuntimeException("Failed to dispatch command to terminal tab " + tabToRun.getTabId() + ": " + e.getMessage(), e);
                }
            });
            log("Command dispatched asynchronously (fire-and-forget).");
            return null;
        }
    }

    /**
     * Resolves a unique user-visible tab title, appending ' (2)', ' (3)', etc. if a tab with
     * the base title already exists.
     * 
     * @param baseTitle The desired base title.
     * @return A unique title not currently used by any open terminal tab.
     */
    private String resolveUniqueTabTitle(String baseTitle) {
        syncTerminalTabs();
        List<String> existingTitles = new ArrayList<>();
        for (ContextProvider cp : childrenProviders) {
            if (cp instanceof TerminalTab tab) {
                existingTitles.add(tab.getName());
            }
        }
        if (!existingTitles.contains(baseTitle)) {
            return baseTitle;
        }
        int count = 2;
        while (existingTitles.contains(baseTitle + " (" + count + ")")) {
            count++;
        }
        return baseTitle + " (" + count + ")";
    }

    /**
     * Opens an interactive local terminal tab in the IDE.
     * 
     * @param title             The title for the terminal tab.
     * @param workingDirectory  The initial working directory.
     * @return A confirmation message.
     */
    public String openLocalTerminal(String title, String workingDirectory) throws Exception {
        
        SwingUtils.runInEDTAndWait(() -> {
            try {
                ExecutionEnvironment localEnv = ExecutionEnvironmentFactory.getLocal();
                TerminalSupport.openTerminal(title, localEnv, workingDirectory);
            } catch (Exception e) {
                log.error("Failed to open local terminal tab: {}", title, e);
                throw new RuntimeException("Failed to open local terminal tab: " + e.getMessage(), e);
            }
        });

        return "Requested to open local terminal tab with title: " + title;
    }

    /**
     * Programmatically updates the viewport settings (tail, grep, line numbering)
     * of a specific terminal tab.
     * 
     * @param terminalTabId The unique ID of the target terminal tab.
     * @param settings      The new viewport settings.
     * @return A status message.
     */
    @AgiTool("Updates the viewport configuration (lines to show, grep filter, etc.) for a specific active terminal tab.")
    public String updateTerminalViewport(
            @AgiToolParam("The unique ID of the terminal tab.") long terminalTabId,
            @AgiToolParam("The new viewport settings configuration.") TextViewportSettings settings) {
        
        Optional<TerminalTab> tab = findTabById(terminalTabId);
        if (tab.isEmpty()) {
            return "Error: Terminal tab not found with ID: " + terminalTabId;
        }
        
        TextViewportSettings current = tab.get().getViewportSettings();
        current.setTail(settings.isTail());
        current.setTailLines(settings.getTailLines());
        current.setGrepPattern(settings.getGrepPattern());
        current.setIncludeLineNumbers(settings.isIncludeLineNumbers());
        current.setColumnWidth(settings.getColumnWidth());
        current.setPageSizeInChars(settings.getPageSizeInChars());
        current.setStartChar(settings.getStartChar());
        
        return "Successfully updated viewport for terminal tab: " + tab.get().getName();
    }

    /**
     * Closes a specific terminal tab.
     * 
     * @param terminalTabId The unique ID of the target terminal tab.
     * @return A status message.
     */
    @AgiTool("Closes a specific terminal tab.")
    public String closeTerminal(
            @AgiToolParam("The unique ID of the terminal tab.") long terminalTabId) throws Exception {
        Optional<TerminalTab> tab = findTabById(terminalTabId);
        if (tab.isEmpty()) {
            throw new AgiToolException("Terminal tab not found with ID: " + terminalTabId);
        }

        final Term targetTerm = tab.get().getTerm();
        if (targetTerm != null) {
            SwingUtils.runInEDTAndWait(() -> {
                try {
                    Container parent = targetTerm.getParent();
                    while (parent != null) {
                        if (parent instanceof TerminalContainer tc) {
                            tc.ioContainer().remove((JComponent) targetTerm.getParent());
                            break;
                        }
                        parent = parent.getParent();
                    }
                } catch (Exception e) {
                    log.error("Failed to close terminal tab {}", terminalTabId, e);
                    throw new RuntimeException("Failed to close terminal tab: " + e.getMessage(), e);
                }
            });
        }

        syncTerminalTabs();
        return "Successfully requested to close terminal tab: " + tab.get().getName();
    }

    /**
     * Synchronizes terminal tabs with active Swing Term components in the window system.
     * Matches JComponents with saved provider configurations using persistent client property UUIDs.
     */
    private synchronized void syncTerminalTabs() {
        List<Term> activeTerms = findActiveTerms();
        List<String> activeIds = new ArrayList<>();

        for (Term term : activeTerms) {
            long tabId = System.identityHashCode(term);
            String idStr = String.valueOf(tabId);
            activeIds.add(idStr);

            String title = getTabTitle(term);
            Optional<TerminalTab> existing = findTabById(tabId);
            if (existing.isPresent()) {
                existing.get().setTerm(term); // Rebind transient Term reference
            } else {
                TerminalTab newTab = new TerminalTab(tabId, title, term);
                newTab.setParent(this);
                childrenProviders.add(newTab);
                log.info("Registered TerminalTab context provider for: {}", title);
            }
        }

        // Prune any closed terminal tabs from children list
        childrenProviders.removeIf(cp -> {
            if (cp instanceof TerminalTab tab) {
                boolean active = activeIds.contains(tab.getId());
                if (!active) {
                    log.info("Pruning closed terminal tab: {}", tab.getName());
                }
                return !active;
            }
            return false;
        });
    }

    /**
     * Finds a registered TerminalTab context provider by its transient identity hashcode.
     * 
     * @param id The identity hashcode of the Term component.
     * @return An Optional containing the TerminalTab if found.
     */
    private Optional<TerminalTab> findTabById(long id) {
        String idStr = String.valueOf(id);
        return childrenProviders.stream()
                .filter(cp -> cp instanceof TerminalTab)
                .map(cp -> (TerminalTab) cp)
                .filter(tab -> tab.getId().equals(idStr))
                .findFirst();
    }

    /**
     * Searches the TopComponent window tree on the Event Dispatch Thread and returns all active Term instances.
     * Uses runInEDTAndWait because we must block the background sync loop until the Swing crawl completes.
     * 
     * @return A list of active Term components.
     */
    private List<Term> findActiveTerms() {
        final List<Term> terms = new ArrayList<>();
        try {
            SwingUtils.runInEDTAndWait(() -> {
                TopComponent tcTC = WindowManager.getDefault().findTopComponent("TerminalContainerTopComponent");
                if (tcTC != null) {
                    findTermsRecursive(tcTC, terms);
                }
            });
        } catch (Exception e) {
            log.error("Failed to crawl terminal TopComponent", e);
        }
        return terms;
    }

    /**
     * Recursively traverses a Swing container to identify any instances of {@link Term}.
     * 
     * @param comp  The parent component.
     * @param terms The output list of found Term components.
     */
    private void findTermsRecursive(Component comp, List<Term> terms) {
        if (comp == null) {
            return;
        }
        if (comp instanceof Term term) {
            terms.add(term);
        }
        if (comp instanceof Container container) {
            for (Component child : container.getComponents()) {
                findTermsRecursive(child, terms);
            }
        }
    }

    /**
     * Traverses the Swing hierarchy upwards from a Term component to retrieve its user-visible tab title.
     * 
     * @param term The Term component.
     * @return The display name of the parent tab panel.
     */
    private String getTabTitle(Term term) {
        Component parent = term;
        while (parent != null && !(parent instanceof TerminalContainerTopComponent)) {
            String name = parent.getName();
            if (name != null && !name.isBlank()) {
                return name;
            }
            parent = parent.getParent();
        }
        return "Terminal";
    }
}