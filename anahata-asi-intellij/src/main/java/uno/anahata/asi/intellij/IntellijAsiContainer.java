package uno.anahata.asi.intellij;

import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AsiContainerPreferences;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.intellij.ui.IntellijTextResourceWriteRenderer;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.message.part.tool.param.ParameterRendererFactory;
import uno.anahata.asi.toolkit.resources.text.FullTextFileCreate;
import uno.anahata.asi.toolkit.resources.text.FullTextResourceUpdate;
import uno.anahata.asi.toolkit.resources.text.TextResourceReplacements;
import uno.anahata.asi.toolkit.resources.text.lines.TextResourceLineEdits;

/**
 * Concrete implementation of the ASI Container for IntelliJ IDEA.
 * <p>
 * This container integrates the Anahata framework with the IntelliJ IDEA 
 * tool window system, managing dashboard and session tabs reactively.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class IntellijAsiContainer extends AbstractSwingAsiContainer {

    /**
     * Live registry of tool-window-backed containers in this JVM.
     * <p>
     * Lets host-side UI hooks that have no direct container reference — the Project-view
     * node decorator and the "AGI Context" popup action — enumerate active sessions and
     * their resource managers. Uses a copy-on-write list for safe concurrent iteration
     * from EDT and background action threads.
     * </p>
     */
    private static final List<IntellijAsiContainer> INSTANCES = new CopyOnWriteArrayList<>();

    /**
     * Registers the IntelliJ diff visualization for the core text-write tool arguments.
     * <p>
     * Mirrors the NetBeans container's renderer registration: each concrete
     * {@code AbstractTextResourceWrite} DTO type is mapped to the shared
     * {@link IntellijTextResourceWriteRenderer}, so every file-writing tool call renders as
     * an editable side-by-side diff instead of raw JSON.
     * </p>
     */
    static {
        ParameterRendererFactory.register(FullTextResourceUpdate.class, IntellijTextResourceWriteRenderer.class);
        ParameterRendererFactory.register(FullTextFileCreate.class, IntellijTextResourceWriteRenderer.class);
        ParameterRendererFactory.register(TextResourceReplacements.class, IntellijTextResourceWriteRenderer.class);
        ParameterRendererFactory.register(TextResourceLineEdits.class, IntellijTextResourceWriteRenderer.class);
    }

    /**
     * The IntelliJ ToolWindow instance.
     */
    @Getter
    @Setter
    private transient ToolWindow toolWindow;

    /**
     * Default constructor initializing with the "intellij" host application ID.
     */
    public IntellijAsiContainer() {
        super("intellij");
    }

    /**
     * Constructor initializing with a specific ToolWindow and registering this container
     * in the live {@link #INSTANCES} registry.
     *
     * @param toolWindow The target ToolWindow.
     */
    public IntellijAsiContainer(ToolWindow toolWindow) {
        this();
        this.toolWindow = toolWindow;
        INSTANCES.add(this);
    }

    /**
     * Returns an immutable snapshot of the live tool-window-backed containers.
     *
     * @return the active containers in this JVM.
     */
    public static List<IntellijAsiContainer> getInstances() {
        return List.copyOf(INSTANCES);
    }

    /**
     * Removes a container from the live registry, called when its project/tool window is
     * disposed so closed projects no longer surface stale sessions to the Project-view UI.
     *
     * @param container the container to deregister.
     */
    public static void removeInstance(IntellijAsiContainer container) {
        INSTANCES.remove(container);
    }

    /**
     * Reconciles the persisted AGI template's toolkit list with the toolkits currently registered
     * in {@link IntellijAgiConfig}.
     * <p>
     * The template config is persisted via Kryo, which bypasses {@code IntellijAgiConfig}'s instance
     * initializer on restore — so newly registered toolkits never reach the stored template, and
     * therefore never appear in Preferences or in new sessions (which are cloned from the template).
     * This adds any missing toolkit classes and drops any obsolete ones, preserving all other
     * template settings, and persists the result only when it actually changed. It is idempotent and
     * safe to call on every tool-window open.
     * </p>
     */
    public void syncTemplateToolkits() {
        AsiContainerPreferences preferences = getPreferences();
        preferences.ensureTemplatesInitialized(this);

        List<Class<?>> templateTools = preferences.getAgiTemplate().getToolClasses();
        List<Class<?>> currentTools = createNewAgiConfig().getToolClasses();

        boolean changed = false;
        for (Class<?> toolClass : currentTools) {
            if (!templateTools.contains(toolClass)) {
                templateTools.add(toolClass);
                changed = true;
            }
        }
        changed |= templateTools.removeIf(toolClass -> !currentTools.contains(toolClass));

        if (changed) {
            savePreferences();
            log.info("Synced AGI template toolkits with the registered defaults ({} toolkits).", templateTools.size());
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns an IntelliJ-specific AGI configuration.
     * </p>
     */
    @Override
    public AgiConfig createNewAgiConfig() {
        return new IntellijAgiConfig(this);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Focuses or creates a dedicated tab inside the IntelliJ ToolWindow 
     * for the given AGI session.
     * </p>
     */
    @Override
    protected void focusUI(Agi agi) {
        if (toolWindow == null) {
            return;
        }

        Content content = null;
        for (Content c : toolWindow.getContentManager().getContents()) {
            if (c.getComponent() instanceof AgiPanel panel && panel.getAgi() == agi) {
                content = c;
                break;
            }
        }

        if (content == null) {
            AgiPanel agiPanel = new AgiPanel(agi);
            agiPanel.initComponents();

            ContentFactory contentFactory = ContentFactory.getInstance();
            content = contentFactory.createContent(agiPanel, agi.getDisplayName(), false);
            content.setCloseable(true);

            toolWindow.getContentManager().addContent(content);
        }

        toolWindow.getContentManager().setSelectedContent(content);
        toolWindow.show();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Closes and removes the dedicated tab inside the IntelliJ ToolWindow 
     * for the given AGI session.
     * </p>
     */
    @Override
    protected void closeUI(Agi agi) {
        if (toolWindow == null) {
            return;
        }
        for (Content c : toolWindow.getContentManager().getContents()) {
            if (c.getComponent() instanceof AgiPanel panel && panel.getAgi() == agi) {
                toolWindow.getContentManager().removeContent(c, true);
                break;
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Locates the active AgiPanel component for the given AGI session if open.
     * </p>
     */
    @Override
    public Object getUI(Agi agi) {
        if (toolWindow == null) {
            return null;
        }
        for (Content c : toolWindow.getContentManager().getContents()) {
            if (c.getComponent() instanceof AgiPanel panel && panel.getAgi() == agi) {
                return panel;
            }
        }
        return null;
    }
}
