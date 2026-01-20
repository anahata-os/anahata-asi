package uno.anahata.asi.nb.context;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.List;
import java.util.logging.Logger;
import org.netbeans.api.project.Project;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionRegistration;
import org.openide.filesystems.FileUtil;
//import uno.anahata.mavenproject9.FakeContext;

@ActionID(category = "Tools", id = "uno.anahata.mavenproject9.AddProjectToContextAction")
@ActionRegistration(displayName = "Add project(s) to context", iconBase = "icons/anahata.png", asynchronous = true)
@ActionReference(path = "Projects/Actions", position = 500)
public final class AddProjectToContextAction implements ActionListener {

    private static final Logger LOG = Logger.getLogger(AddProjectToContextAction.class.getName());
    private final List<Project> context;

    static {
        LOG.info("AddProjectToContextAction static init");
    }

    public AddProjectToContextAction(List<Project> context) {
        LOG.info("AddProjectToContextAction constructor called with context size: " + context.size());
        this.context = context;
    }

    @Override
    public void actionPerformed(ActionEvent ev) {
        LOG.info("AddProjectToContextAction.actionPerformed called");
        for (Project p : context) {
            File file = FileUtil.toFile(p.getProjectDirectory());
            if (file != null) {
                LOG.info("Adding project to context: " + file.getAbsolutePath());
                //FakeContext.add(file);
            }
        }
    }
}
