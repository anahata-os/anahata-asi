/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.classpath;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.spi.java.classpath.ClassPathProvider;
import org.netbeans.spi.java.classpath.support.ClassPathSupport;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.lookup.ServiceProvider;
import uno.anahata.asi.nb.module.NetBeansModuleUtils;

/**
 * A global ClassPathProvider that intercepts queries for in-memory files
 * and provides the appropriate classpath based on the file's context.
 * Enables semantic highlighting and code completion for virtual snippets.
 */
@ServiceProvider(service = ClassPathProvider.class, position = 10000)
@Slf4j
public class AnahataClasspathProvider implements ClassPathProvider {

    private ClassPath anahataPluginCp;

    /**
     * Lazily initializes and returns the ClassPath representing the Anahata plugin's environment.
     * @return the anahata plugin classpath.
     */
    private synchronized ClassPath getPluginClassPath() {
        if (anahataPluginCp == null) {
            String rawCp = NetBeansModuleUtils.getNetBeansClasspath();
            List<URL> cpUrls = new ArrayList<>();
            for (String path : rawCp.split(File.pathSeparator)) {
                File f = new File(path);
                if (f.exists()) {
                    URL archiveOrDir = FileUtil.urlForArchiveOrDir(f);
                    if (archiveOrDir != null) {
                        cpUrls.add(archiveOrDir);
                    }
                }
            }
            anahataPluginCp = ClassPathSupport.createClassPath(cpUrls.toArray(new URL[0]));
        }
        return anahataPluginCp;
    }

    /**
     * {@inheritDoc}
     * <p>Detects memory-based FileObjects and attempts to resolve their classpath 
     * by checking for custom attributes (<code>anahata.customClasspath</code> 
     * or <code>anahata.contextPath</code>). If no specific context is found, 
     * it falls back to the global plugin classpath.</p>
     */
    @Override
    public ClassPath findClassPath(FileObject file, String type) {
        try {
            // MemoryFileSystem classes are package-private, so we match the simple name.
            if (file.getFileSystem().getClass().getSimpleName().equals("MemoryFileSystem")) {
                
                // 1. Check for explicitly injected custom classpath (e.g., from JavaCodeParameterRenderer)
                Object customCp = file.getAttribute("anahata.customClasspath");
                if (customCp instanceof String cpStr && (ClassPath.COMPILE.equals(type) || ClassPath.EXECUTE.equals(type))) {
                    log.info("findClassPath: Resolving custom injected classpath for memory file");
                    List<URL> cpUrls = new ArrayList<>();
                    for (String path : cpStr.split(File.pathSeparator)) {
                        File f = new File(path);
                        if (f.exists()) {
                            URL url = FileUtil.urlForArchiveOrDir(f);
                            if (url != null) {
                                cpUrls.add(url);
                            }
                        }
                    }
                    return ClassPathSupport.createClassPath(cpUrls.toArray(new URL[0]));
                }

                // 2. Check for Context Path (e.g., from createTextFile)
                Object contextPath = file.getAttribute("anahata.contextPath");
                if (contextPath != null) {
                    log.info("findClassPath: Resolving project classpath for memory file bound to: {}", contextPath);
                    File physicalFile = new File(contextPath.toString());
                    File parentDir = physicalFile.getParentFile();
                    if (parentDir != null && parentDir.exists()) {
                        FileObject parentFo = FileUtil.toFileObject(parentDir);
                        if (parentFo != null) {
                            Project p = FileOwnerQuery.getOwner(parentFo);
                            if (p != null) {
                                ClassPathProvider cpp = p.getLookup().lookup(ClassPathProvider.class);
                                if (cpp != null) {
                                    ClassPath cp = cpp.findClassPath(parentFo, type);
                                    if (cp != null) {
                                        return cp;
                                    }
                                }
                            }
                        }
                    }
                }

                // Fallback to global plugin classpath if no context hint is found
                if (ClassPath.COMPILE.equals(type) || ClassPath.EXECUTE.equals(type)) {
                    return getPluginClassPath();
                } else if (ClassPath.SOURCE.equals(type)) {
                    return ClassPathSupport.createClassPath(new URL[0]);
                } else if (ClassPath.BOOT.equals(type)) {
                    // Returning null tells JavaSource to fall back to the default platform's bootstrap libraries
                    return null;
                }
            }
        } catch (Exception ex) {
            // Silently ignore exceptions (like FileStateInvalidException) to prevent parser disruption
        }
        return null;
    }
}