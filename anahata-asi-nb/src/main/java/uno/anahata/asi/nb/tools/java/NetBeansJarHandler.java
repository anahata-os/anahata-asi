package uno.anahata.asi.nb.tools.java;

import java.io.File;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import uno.anahata.asi.toolkit.java.classpath.AbstractJarHandler;
import uno.anahata.asi.toolkit.java.classpath.JarMetadata;

/**
 * Specialized JAR handler for parsing NetBeans Platform module manifests and metadata.
 */
public class NetBeansJarHandler extends AbstractJarHandler {
    @Override
    public boolean canHandle(File jarFile) {
        String name = jarFile.getName();
        return name.startsWith("org-netbeans-") || name.startsWith("org-openide-");
    }

    @Override
    public JarMetadata extractMetadata(File jarFile) {
        Manifest mf = readManifest(jarFile);
        String version = null;
        if (mf != null) {
            Attributes attr = mf.getMainAttributes();
            version = attr.getValue("Bundle-Version");
            if (version == null) version = attr.getValue(Attributes.Name.IMPLEMENTATION_VERSION);
        }

        return JarMetadata.builder()
            .id(getBaseName(jarFile))
            .version(JarMetadata.sanitizeVersion(version))
            .vendor("Apache NetBeans")
            .build();
    }
}
