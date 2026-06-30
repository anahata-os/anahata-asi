package uno.anahata.asi.intellij;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import org.jetbrains.annotations.NotNull;
import uno.anahata.asi.swing.AsiCardsContainerPanel;

import javax.swing.JPanel;
import java.awt.BorderLayout;

/**
 * Factory for creating the Anahata ASI Tool Window in IntelliJ.
 * <p>
 * This class instantiates the {@link IntellijAsiContainer} and boots up the 
 * dashboard of cards to manage all active AI sessions from a single unified panel.
 * </p>
 * 
 * @author anahata
 */
public class AnahataToolWindowFactory implements ToolWindowFactory {

    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        IntellijAsiContainer asiContainer = new IntellijAsiContainer(toolWindow);
        
        JPanel mainView = new JPanel(new BorderLayout());
        
        // Instantiate the master dashboard of session cards and add it to the view
        AsiCardsContainerPanel dashboard = new AsiCardsContainerPanel(asiContainer);
        dashboard.startRefresh();
        
        mainView.add(dashboard, BorderLayout.CENTER);

        ContentFactory contentFactory = ContentFactory.getInstance();
        Content content = contentFactory.createContent(mainView, "Dashboard", false);
        content.setCloseable(false); // The master dashboard tab remains sticky and cannot be closed
        
        toolWindow.getContentManager().addContent(content);
    }
}
