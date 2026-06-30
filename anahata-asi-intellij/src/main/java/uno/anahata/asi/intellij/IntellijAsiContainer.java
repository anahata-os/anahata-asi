package uno.anahata.asi.intellij;

import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * Concrete implementation of the ASI Container for IntelliJ IDEA.
 * <p>
 * This container integrates the Anahata framework with the IntelliJ IDEA 
 * tool window system, managing dashboard and session tabs reactively.
 * </p>
 * 
 * @author anahata
 */
public class IntellijAsiContainer extends AbstractSwingAsiContainer {

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
     * Constructor initializing with a specific ToolWindow.
     * 
     * @param toolWindow The target ToolWindow.
     */
    public IntellijAsiContainer(ToolWindow toolWindow) {
        this();
        this.toolWindow = toolWindow;
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
