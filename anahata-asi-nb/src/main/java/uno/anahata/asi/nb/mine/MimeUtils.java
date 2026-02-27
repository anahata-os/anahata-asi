/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.mine;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

/**
 * Utility class for working with MIME types and file extensions in NetBeans.
 * This class provides methods to query the IDE's internal configuration to 
 * discover supported file formats and their mappings.
 * 
 * @author anahata
 */
public final class MimeUtils {

    /**
     * Private constructor for the utility class to prevent instantiation.
     */
    private MimeUtils() {
        // Utility class
    }

    /**
     * Scans the NetBeans System Filesystem's "Editors" folder to find all registered MIME types.
     * This is the most reliable method to get a complete list of MIME types the IDE is aware of.
     * 
     * <p><b>Implementation Logic:</b></p>
     * <ul>
     *   <li>Retrieves the "Editors" configuration folder via {@code FileUtil.getConfigFile("Editors")}.</li>
     *   <li>Recursively iterates through all subfolders using an {@code Enumeration}.</li>
     *   <li>Calculates the MIME type string by deriving the relative path from the "Editors" root.</li>
     *   <li>Filters for folders that contain data files (settings), ensuring that only 
     *       effectively active MIME types are returned, skipping intermediate hierarchy folders.</li>
     * </ul>
     *
     * @return A Set of all registered MIME type strings (e.g., "text/x-java", "text/xml").
     */
    public static Set<String> getAllMimeTypes() {
        Set<String> mimeTypes = new HashSet<>();
        FileObject editorsFolder = FileUtil.getConfigFile("Editors");
        if (editorsFolder != null) {
            // Recursively iterate through all folders
            Enumeration<? extends FileObject> e = editorsFolder.getFolders(true);
            while (e.hasMoreElements()) {
                FileObject mimeFolder = e.nextElement();
                // The relative path from the 'Editors' folder is the mime type
                String mimeType = FileUtil.getRelativePath(editorsFolder, mimeFolder);
                // We only care about folders that directly contain settings (files), not intermediate folders.
                boolean hasSettings = false;
                for (FileObject child : mimeFolder.getChildren()) {
                    if (child.isData()) {
                        hasSettings = true;
                        break;
                    }
                }
                if (hasSettings) {
                    mimeTypes.add(mimeType);
                }
            }
        }
        return mimeTypes;
    }
    
    /**
     * Scans the NetBeans System Filesystem to find all registered MIME types and their file extensions,
     * returning a map where the key is the extension and the value is the MIME type.
     * 
     * <p><b>Implementation Logic:</b></p>
     * <ul>
     *   <li>Calls {@link #getAllMimeTypes()} to get the baseline of known types.</li>
     *   <li>For each MIME type, uses {@code FileUtil.getMIMETypeExtensions(mimeType)} to 
     *       retrieve associated file extensions.</li>
     *   <li>Maps each extension to its MIME type. In cases where an extension is 
     *       associated with multiple types, the last one processed prevails.</li>
     * </ul>
     *
     * @return A Map of file extensions (e.g., "java", "xml") to their corresponding MIME types.
     */
    public static Map<String, String> getExtensionToMimeTypeMap() {
        Map<String, String> extensionToMimeMap = new HashMap<>();
        Set<String> allMimeTypes = getAllMimeTypes();

        for (String mimeType : allMimeTypes) {
            List<String> extensions = FileUtil.getMIMETypeExtensions(mimeType);
            if (extensions != null) {
                for (String extension : extensions) {
                    extensionToMimeMap.put(extension, mimeType);
                }
            }
        }
        return extensionToMimeMap;
    }
}
