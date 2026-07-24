/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.terminal;

import java.awt.Component;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import javax.swing.JTabbedPane;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.lib.terminalemulator.Term;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.agi.context.BasicContextProvider;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.agi.resource.view.TextViewport;
import uno.anahata.asi.agi.resource.view.TextViewportSettings;

/**
 * A hierarchical context provider representing an active terminal tab in NetBeans.
 * Each tab actively participates in the RAG pipeline by capturing its text scrollback
 * buffer thread-safely off the EDT and rendering it via a local, fully serializable
 * {@link TextViewportSettings}.
 * 
 * @author anahata
 */
@Slf4j
public class TerminalTab extends BasicContextProvider {

    /**
     * Persistent viewport configuration for this terminal tab.
     */
    @Getter
    private final TextViewportSettings viewportSettings;

    /**
     * In-memory text viewport engine for line slicing, tailing, and grepping.
     */
    private transient TextViewport viewport;

    private TextViewport getViewport() {
        if (viewport == null) {
            viewport = new TextViewport();
        }
        return viewport;
    }

    /**
     * The active rendering JComponent. Marked transient as Swing UI elements
     * cannot be serialized directly.
     */
    @Getter
    @Setter
    private transient Term term;

    /**
     * Constructs a new TerminalTab context provider.
     * 
     * @param tabId The transient identity hashcode of the Term component.
     * @param name  The display title of the tab.
     * @param term  The active Swing Term component.
     */
    public TerminalTab(long tabId, String name, Term term) {
        super(String.valueOf(tabId), name, "Active terminal tab: " + name);
        this.term = term;
        this.viewportSettings = TextViewportSettings.builder()
                .tail(true)
                .tailLines(100)
                .includeLineNumbers(true)
                .build();
    }

    /**
     * Resolves the transient identity hash code of the Term component.
     * 
     * @return The identity hash code, or -1 if the Term is null.
     */
    public long getTabId() {
        return term != null ? System.identityHashCode(term) : -1L;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Reconstructs the terminal's text buffer completely thread-safe and off the EDT,
     * processes it via our local viewports, and injects a formatted Markdown code block
     * directly into the RAG prompt.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        if (term == null) {
            return;
        }

        // 1. Thread-safe content extraction completely off the EDT!
        String content = getTermContent();

        // 2. Wrap and process via TextViewport locally
        StringHandle handle = new StringHandle(getId(), content);
        TextViewport vp = getViewport();
        vp.setSettings(viewportSettings);
        vp.process(handle);

        String visibleContent = vp.getVisibleContent();
        if (visibleContent != null && !visibleContent.isBlank()) {
            ragMessage.addTextPart("### Terminal Tab: " + getName() + " (ID: " + getTabId() + ")\n"
                    + "```text\n" + visibleContent + "\n```");
        }
    }

    /**
     * Safely reads the terminal scrollback buffer completely off the EDT thread
     * using raw line getters.
     * 
     * @return The reconstructed terminal buffer as a String.
     */
    public String getTermContent() {
        if (term == null) {
            return "";
        }
        int totalLines = term.getHistorySize() + term.getRows();
        StringBuilder sb = new StringBuilder();
        for (int r = 0; r < totalLines; r++) {
            String rowText = term.getRowText(r);
            if (rowText != null) {
                sb.append(rowText).append("\n");
            }
        }
        return sb.toString();
    }

    /**
     * Activates the {@code TerminalContainerTopComponent} and selects this tab in its parent tabbed container.
     */
    public void selectTab() {
        if (term == null) {
            return;
        }
        TopComponent tc = WindowManager.getDefault().findTopComponent("TerminalContainerTopComponent");
        if (tc != null) {
            tc.open();
            tc.requestActive();
        }
        Component p = term;
        while (p != null) {
            if (p instanceof JTabbedPane tp) {
                Component child = term;
                while (child != null && child.getParent() != tp) {
                    child = child.getParent();
                }
                if (child != null) {
                    int idx = tp.indexOfComponent(child);
                    if (idx != -1) {
                        tp.setSelectedIndex(idx);
                    }
                }
            }
            p = p.getParent();
        }
        term.requestFocusInWindow();
    }

    /**
     * Types a command sequence into the terminal via reflection on the Term instance.
     * <p>
     * Automatically activates and selects this tab before sending the command,
     * ensuring it is brought into focus in the IDE layout.
     * </p>
     * 
     * @param command The command sequence to execute.
     * @throws java.lang.IllegalAccessException If access to the fireChars method is denied.
     * @throws java.lang.reflect.InvocationTargetException If invoking the fireChars method throws an exception.
     * @throws java.lang.NoSuchMethodException If the fireChars method cannot be found on Term.
     */
    public void typeCommand(String command) throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        if (term == null) {
            throw new IllegalStateException("Associated Term component is null.");
        }
        selectTab();
        String cmd = command != null ? command : "";
        if (!cmd.endsWith("\n") && !cmd.endsWith("\r")) {
            cmd = cmd + "\n";
        }
        char[] chars = cmd.toCharArray();
        for (int i = 0; i < chars.length; i++) {
            if (chars[i] == '\n') {
                chars[i] = '\r';
            }
        }
        Method fireCharsMethod = Term.class.getDeclaredMethod("fireChars", char[].class, int.class, int.class);
        fireCharsMethod.setAccessible(true);
        fireCharsMethod.invoke(term, chars, 0, chars.length);
    }
}