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
     * Opens an interactive local terminal tab in the IDE.
     * 
     * @param title             The title for the terminal tab.
     * @param workingDirectory  The initial working directory.
     * @return A confirmation message.
     */
    @AgiTool("Opens a new local terminal tab in the IDE.")
    public String openLocalTerminal(
            @AgiToolParam("The title of the terminal tab.") String title,
            @AgiToolParam("The initial working directory for the shell.") String workingDirectory) throws Exception {
        
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
     * Types a command sequence into a specific terminal tab and presses Enter.
     * 
     * @param terminalTabId The unique ID of the target terminal tab.
     * @param command       The command sequence to execute.
     * @return A status message.
     */
    @AgiTool("Types a command sequence into a specific terminal tab and executes it.")
    public String typeCommand(
            @AgiToolParam("The unique ID of the terminal tab.") long terminalTabId,
            @AgiToolParam("The command characters to type.") String command) throws Exception {
        Optional<TerminalTab> tab = findTabById(terminalTabId);
        if (tab.isEmpty()) {
            throw new IllegalArgumentException("Terminal tab not found with ID: " + terminalTabId);
        }

        SwingUtils.runInEDTAndWait(() -> {
            try {
                tab.get().typeCommand(command);
            } catch (Exception e) {
                log.error("Failed to type command into terminal tab {}: {}", terminalTabId, command, e);
                throw new RuntimeException("Failed to type command into terminal tab " + terminalTabId + ": " + e.getMessage(), e);
            }
        });
        return "Successfully dispatched command to terminal tab: " + tab.get().getName();
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
            throw new IllegalArgumentException("Terminal tab not found with ID: " + terminalTabId);
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