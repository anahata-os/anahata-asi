/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.ide;

import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import javax.swing.SwingUtilities;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.editor.EditorRegistry;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.modules.editor.NbEditorUtilities;
import org.openide.cookies.EditorCookie;
import org.openide.cookies.LineCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.text.Line;
import org.openide.text.Line.ShowOpenType;
import org.openide.text.Line.ShowVisibilityType;
import org.openide.windows.Mode;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.AgiTopComponent;
import uno.anahata.asi.AsiTopComponent;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * Provides tools for interacting with the NetBeans editor, such as opening files,
 * closing files, and navigating to specific lines.
 *
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for interacting with the NetBeans editor.")
public class Editor extends AnahataToolkit {

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        final List<TopComponent> editors = getOpenEditors();
        if (editors.isEmpty()) {
            return;
        }

        StringBuilder sb = new StringBuilder();
        sb.append("### Open Editor Files\n");
        sb.append("| File Path | Project | Mode | Line | Offset | Modified |\n");
        sb.append("|---|---|---|---|---|---|\n");

        List<String> snippets = new ArrayList<>();

        for (TopComponent tc : editors) {
            DataObject dobj = tc.getLookup().lookup(DataObject.class);
            if (dobj == null) {
                continue;
            }

            FileObject fo = dobj.getPrimaryFile();
            Project owner = FileOwnerQuery.getOwner(fo);
            String projectName = (owner != null) ? owner.getProjectDirectory().getNameExt() : "N/A";
            
            Mode mode = WindowManager.getDefault().findMode(tc);
            String modeName = (mode != null) ? mode.getName() : "N/A";

            // Search for realized JTextComponents
            int line = 0;
            int offset = -1;
            String selection = null;
            String visibleContent = null;
            String visibleRange = null;

            for (JTextComponent comp : EditorRegistry.componentList()) {
                if (NbEditorUtilities.getDataObject(comp.getDocument()) == dobj) {
                    Document doc = comp.getDocument();
                    Element root = doc.getDefaultRootElement();
                    
                    // Caret Info
                    offset = comp.getCaretPosition();
                    line = root.getElementIndex(offset) + 1;
                    
                    // Selection
                    selection = comp.getSelectedText();
                    
                    // Visible Snippet
                    try {
                        Rectangle vis = comp.getVisibleRect();
                        int startOff = comp.viewToModel2D(new Point2D.Double(vis.getX(), vis.getY()));
                        int endOff = comp.viewToModel2D(new Point2D.Double(vis.getX() + vis.getWidth(), vis.getY() + vis.getHeight()));
                        
                        int startIdx = root.getElementIndex(startOff);
                        int endIdx = root.getElementIndex(endOff);
                        visibleRange = (startIdx + 1) + "-" + (endIdx + 1);

                        StringBuilder visContent = new StringBuilder();
                        for (int i = startIdx; i <= endIdx; i++) {
                            Element lineElem = root.getElement(i);
                            visContent.append(doc.getText(lineElem.getStartOffset(), lineElem.getEndOffset() - lineElem.getStartOffset()));
                        }
                        visibleContent = visContent.toString();
                    } catch (Exception e) {
                        log.warn("Failed to extract visible snippet for {}", fo.getNameExt(), e);
                    }
                    break;
                }
            }

            sb.append("| ").append(fo.getPath()).append(" | ")
              .append(projectName).append(" | ")
              .append(modeName).append(" | ")
              .append(line > 0 ? line : "N/A").append(" | ")
              .append(offset >= 0 ? offset : "N/A").append(" | ")
              .append(dobj.isModified() ? "Y" : "N").append(" |\n");
            
            if (selection != null && !selection.isEmpty()) {
                snippets.add("**Selection in " + fo.getNameExt() + ":**\n```\n" + selection + "\n```");
            }
            
            if (visibleContent != null && !visibleContent.isBlank()) {
                snippets.add("**Visible Lines in " + fo.getNameExt() + " (" + visibleRange + "):**\n```\n" + visibleContent + "```");
            }
        }
        
        ragMessage.addTextPart(sb.toString());
        for (String snippet : snippets) {
            ragMessage.addTextPart(snippet);
        }
    }

    /**
     * Opens a specified file in the NetBeans editor and optionally scrolls to a specific line.
     * @param filePath The absolute path of the file to open.
     * @param scrollToLine The line number to scroll to (1-based).
     * @return a message indicating the result of the operation.
     * @throws Exception if an error occurs.
     */
    @AiTool("Opens a specified file in the NetBeans editor and optionally scrolls to a specific line.")
    public String openFile(
            @AiToolParam("The absolute path of the file to open.") String filePath,
            @AiToolParam("The line number to scroll to (1-based).") Integer scrollToLine) throws Exception {
        
        if (filePath == null || filePath.trim().isEmpty()) {
            return "Error: The 'filePath' parameter was not set.";
        }
        
        File file = new File(filePath);
        if (!file.exists()) {
            return "Error: File does not exist at path: " + filePath;
        }
        
        FileObject fileObject = FileUtil.toFileObject(FileUtil.normalizeFile(file));
        if (fileObject == null) {
            return "Error: Could not find or create a FileObject for: " + filePath;
        }
        
        DataObject dataObject = DataObject.find(fileObject);
        EditorCookie editorCookie = dataObject.getLookup().lookup(EditorCookie.class);
        
        if (editorCookie != null) {
            SwingUtilities.invokeLater(() -> {
                try {
                    editorCookie.open();
                    if (scrollToLine != null && scrollToLine > 0) {
                        LineCookie lineCookie = dataObject.getLookup().lookup(LineCookie.class);
                        if (lineCookie != null) {
                            int lineIndex = scrollToLine - 1;
                            Line.Set lineSet = lineCookie.getLineSet();
                            if (lineIndex < lineSet.getLines().size()) {
                                Line line = lineSet.getLines().get(lineIndex);
                                line.show(ShowOpenType.OPEN, ShowVisibilityType.FOCUS);
                            }
                        }
                    }
                } catch (Exception e) {
                    log.error("Failed to navigate to line: " + scrollToLine, e);
                }
            });
            String scrollMessage = (scrollToLine != null) ? " and scroll to line " + scrollToLine : "";
            return "Successfully requested to open file: " + filePath + scrollMessage;
        } else {
            return "Error: The specified file is not an editable text file.";
        }
    }

    /**
     * Gets a list of all files open in the editor, including their path and unsaved changes status.
     * @return a string listing the open files.
     */
    @AiTool("Gets a list of all files open in the editor")
    public String getOpenFiles() {
        final List<TopComponent> editors = getOpenEditors();
        if (editors.isEmpty()) {
            return "No files are currently open in the editor.";
        }

        StringBuilder sb = new StringBuilder();
        for (TopComponent tc : editors) {
            DataObject dobj = tc.getLookup().lookup(DataObject.class);
            if (dobj != null) {
                boolean modified = dobj.isModified();
                FileObject fo = dobj.getPrimaryFile();
                sb.append("File: ").append(fo.getPath())
                  .append(" [unsavedChanges=").append(modified).append("]")
                  .append("\n");
            }
        }
        return sb.toString();
    }

    /**
     * Closes all files currently open in the IDE.
     * @return a summary of the operation.
     */
    @AiTool("Closes all files currently open in the IDE.")
    public String closeAllFiles() {
        final List<String> closedFiles = new ArrayList<>();
        try {
            SwingUtils.runInEDTAndWait(() -> {
                Set<TopComponent> opened = WindowManager.getDefault().getRegistry().getOpened();
                for (TopComponent tc : opened) {
                    if (isFileEditor(tc)) {
                        String name = tc.getDisplayName();
                        if (tc.close()) {
                            closedFiles.add(name);
                        }
                    }
                }
            });
        } catch (InterruptedException | InvocationTargetException e) {
            log.error("Failed to close all files", e);
            return "Error while closing files: " + e.getMessage();
        }
        return closedFiles.isEmpty() ? "No files were open to close." : "Closed the following files: " + String.join(", ", closedFiles);
    }

    private List<TopComponent> getOpenEditors() {
        final List<TopComponent> editors = new ArrayList<>();
        try {
            SwingUtils.runInEDTAndWait(() -> {
                Set<TopComponent> opened = WindowManager.getDefault().getRegistry().getOpened();
                for (TopComponent tc : opened) {
                    if (isFileEditor(tc)) {
                        editors.add(tc);
                    }
                }
            });
        } catch (InterruptedException | InvocationTargetException e) {
            log.error("Failed to get open editors", e);
        }
        return editors;
    }

    private boolean isFileEditor(TopComponent tc) {
        if (tc instanceof AgiTopComponent || tc instanceof AsiTopComponent) {
            return false;
        }

        String className = tc.getClass().getName();
        boolean isMultiview = className.contains("MultiViewCloneableTopComponent");

        Mode mode = WindowManager.getDefault().findMode(tc);
        String modeName = (mode != null) ? mode.getName() : "";

        DataObject dobj = tc.getLookup().lookup(DataObject.class);

        // Architectural rule: It's an editor if it has a DataObject AND
        // either is a multiview component OR is explicitly in the 'editor' mode.
        return dobj != null && (isMultiview || "editor".equals(modeName));
    }
}
