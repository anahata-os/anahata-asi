/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.nb.nodes;

import java.awt.Image;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.ImageIcon;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ui.support.NodeFactory;
import org.netbeans.spi.project.ui.support.NodeFactorySupport;
import org.netbeans.spi.project.ui.support.NodeList;
import org.openide.filesystems.FileChangeAdapter;
import org.openide.filesystems.FileEvent;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;
import org.openide.nodes.AbstractNode;
import org.openide.nodes.Children;
import org.openide.nodes.FilterNode;
import org.openide.nodes.Node;
import org.openide.util.ImageUtilities;
import org.openide.util.lookup.Lookups;

/**
 * A factory that creates the "JASI" virtual folder in the NetBeans Projects window.
 * This folder contains project-specific AI resources like {@code anahata.md}.
 * Ported from V1 to the JASI (V2) architecture.
 * 
 * @author anahata
 */
@NodeFactory.Registration(projectType = {"org-netbeans-modules-maven", "org-netbeans-modules-java-j2seproject"}, position = 500)
public class AnahataNodeFactory implements NodeFactory {

    private static final Logger log = Logger.getLogger(AnahataNodeFactory.class.getName());

    /**
     * Default constructor for the node factory.
     */
    public AnahataNodeFactory() {
        log.info("AnahataNodeFactory initialized for JASI V2.");
    }

    /**
     * {@inheritDoc}
     * Creates the "JASI" virtual node for the given project.
     */
    @Override
    public NodeList<?> createNodes(Project project) {
        log.log(Level.FINE, "Creating JASI nodes for project: {0}", project.getProjectDirectory().getName());
        AnahataFolderNode anahataNode = new AnahataFolderNode(project);
        return NodeFactorySupport.fixedNodeList(anahataNode);
    }

    /**
     * A virtual folder node named "JASI" that serves as a container for AI-related files.
     */
    private static class AnahataFolderNode extends AbstractNode {

        /** The path to the default folder icon. */
        private static final String FOLDER_ICON_PATH = "org/openide/loaders/defaultFolder.gif";
        /** The path to the default open folder icon. */
        private static final String FOLDER_OPEN_ICON_PATH = "org/openide/loaders/defaultFolderOpen.gif";
        /** 
         * The path to the Anahata overlay icon. 
         * FIXED: Renamed to avoid shadowing V1's icon in the merged resource layer.
         */
        private static final String OVERLAY_ICON_PATH = "icons/v2/anahata.png";

        /**
         * Constructs a new JASI folder node.
         * 
         * @param project The project this node belongs to.
         */
        public AnahataFolderNode(Project project) {
            super(new AnahataFileChildren(project), Lookups.singleton(project));
            setName("JASI");
            setDisplayName("JASI");
        }

        /** {@inheritDoc} */
        @Override
        public Image getIcon(int type) {
            return createMergedIcon(FOLDER_ICON_PATH);
        }

        /** {@inheritDoc} */
        @Override
        public Image getOpenedIcon(int type) {
            return createMergedIcon(FOLDER_OPEN_ICON_PATH);
        }

        /**
         * Merges the base folder icon with the Anahata overlay.
         * 
         * @param baseIconPath The path to the base icon.
         * @return The merged image.
         */
        private Image createMergedIcon(String baseIconPath) {
            Image folderIcon = ImageUtilities.loadImage(baseIconPath);
            Image overlayIcon = ImageUtilities.loadImage(OVERLAY_ICON_PATH);
            if (overlayIcon == null) {
                return folderIcon;
            }
            
            // Scale the overlay icon to 12x12
            Image scaledOverlay = overlayIcon.getScaledInstance(12, 12, Image.SCALE_SMOOTH);
            Image finalOverlay = new ImageIcon(scaledOverlay).getImage();
            // Position in the bottom-right (16-12=4)
            return ImageUtilities.mergeImages(folderIcon, finalOverlay, 4, 4);
        }
    }

    /**
     * Manages the children of the JASI folder, specifically looking for .md files.
     */
    private static class AnahataFileChildren extends Children.Keys<FileObject> {

        /** The project this children list belongs to. */
        private final Project project;
        /** The project's root directory. */
        private final FileObject projectDir;
        /** Listener for file changes in the project directory. */
        private final FileChangeAdapter fileChangeListener;

        /**
         * Constructs a new children manager for the JASI folder.
         * 
         * @param project The project to monitor.
         */
        public AnahataFileChildren(Project project) {
            super();
            this.project = project;
            this.projectDir = project.getProjectDirectory();
            this.fileChangeListener = new FileChangeAdapter() {
                @Override
                public void fileDataCreated(FileEvent fe) {
                    refreshKeys();
                }

                @Override
                public void fileDeleted(FileEvent fe) {
                    refreshKeys();
                }
            };
        }

        /** {@inheritDoc} */
        @Override
        protected void addNotify() {
            projectDir.addFileChangeListener(fileChangeListener);
            refreshKeys();
        }

        /** {@inheritDoc} */
        @Override
        protected void removeNotify() {
            projectDir.removeFileChangeListener(fileChangeListener);
            setKeys(new ArrayList<>());
        }

        /**
         * Scans the project directory for .md files and ensures anahata.md exists.
         */
        private void refreshKeys() {
            List<FileObject> mdFiles = new ArrayList<>();
            boolean anahataMdExists = false;
            for (FileObject child : projectDir.getChildren()) {
                if (child.isData() && "md".equalsIgnoreCase(child.getExt())) {
                    mdFiles.add(child);
                    if ("anahata.md".equalsIgnoreCase(child.getNameExt())) {
                        anahataMdExists = true;
                    }
                }
            }

            if (!anahataMdExists) {
                try {
                    FileObject newFile = projectDir.createData("anahata.md");
                    try (Writer writer = new OutputStreamWriter(newFile.getOutputStream())) {
                        writer.write("# Anahata Project Notes\n\nThis file is for Anahata AI Assistant's notes regarding the '"
                                + project.getProjectDirectory().getName() + "' project.\n");
                    }
                    mdFiles.add(newFile);
                } catch (IOException e) {
                    log.log(Level.SEVERE, "Failed to create anahata.md", e);
                }
            }

            setKeys(mdFiles);
        }

        /** {@inheritDoc} */
        @Override
        protected Node[] createNodes(FileObject key) {
            try {
                DataObject dob = DataObject.find(key);
                Node originalNode = dob.getNodeDelegate();

                // ALWAYS wrap in a FilterNode to avoid hierarchy conflicts with the project root.
                if ("anahata.md".equals(key.getNameExt())) {
                    return new Node[]{new AnahataMdNode(originalNode)};
                } else {
                    return new Node[]{new FilterNode(originalNode)};
                }
            } catch (DataObjectNotFoundException e) {
                log.log(Level.WARNING, "Could not find DataObject for " + key.getPath(), e);
            }
            return new Node[0];
        }
    }

    /**
     * A specialized node for the anahata.md file that uses the Anahata icon.
     */
    private static class AnahataMdNode extends FilterNode {

        /** 
         * The path to the Anahata icon. 
         * FIXED: Renamed to avoid shadowing V1's icon.
         */
        private static final String ANAHATA_ICON_PATH = "icons/v2/anahata.png";

        /**
         * Constructs a new anahata.md node.
         * 
         * @param original The original node to filter.
         */
        public AnahataMdNode(Node original) {
            super(original);
        }

        /** {@inheritDoc} */
        @Override
        public Image getIcon(int type) {
            Image icon = ImageUtilities.loadImage(ANAHATA_ICON_PATH);
            return icon != null ? icon : super.getIcon(type);
        }

        /** {@inheritDoc} */
        @Override
        public Image getOpenedIcon(int type) {
            return getIcon(type);
        }
    }
}
