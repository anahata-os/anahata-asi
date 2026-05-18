package uno.anahata.asi.toolkit.java.classpath;

import java.io.File;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A fallback JAR handler that uses Implementation-Version manifest 
 * attributes or filename pattern matching to extract metadata.
 * <p>Implementation detail: This handler acts as the "Last Resort" in the 
 * pretty-printer chain, ensuring every library has at least a basic 
 * representation.</p>
 * @author anahata
 */
public class DefaultJarHandler extends AbstractJarHandler {
    
    /**
     * Regex to capture version-like strings at the end of a JAR name.
     * It captures everything from the first dash followed by a digit until the .jar extension.
     */
    private static final Pattern VERSION_PATTERN = Pattern.compile("-([0-9].*)\\.jar$");

    @Override
    public boolean canHandle(File jarFile) {
        return true;
    }

    @Override
    public JarMetadata extractMetadata(File jarFile) {
        String version = null;
        String vendor = null;
        Manifest mf = readManifest(jarFile);
        if (mf != null) {
            Attributes attr = mf.getMainAttributes();
            version = attr.getValue(Attributes.Name.IMPLEMENTATION_VERSION);
            if (version == null) version = attr.getValue("Bundle-Version");
            vendor = attr.getValue(Attributes.Name.IMPLEMENTATION_VENDOR);
        }
        
        // Fallback to filename version extraction if manifest failed
        if (version == null) {
            Matcher m = VERSION_PATTERN.matcher(jarFile.getName());
            if (m.find()) {
                version = m.group(1);
            }
        }

        return JarMetadata.builder()
            .id(getBaseName(jarFile))
            .version(JarMetadata.sanitizeVersion(version))
            .vendor(vendor)
            .build();
    }
    
    @Override
    protected String getBaseName(File jarFile) {
        String name = jarFile.getName();
        Matcher m = VERSION_PATTERN.matcher(name);
        if (m.find()) {
            return name.substring(0, m.start());
        }
        return super.getBaseName(jarFile);
    }
}
