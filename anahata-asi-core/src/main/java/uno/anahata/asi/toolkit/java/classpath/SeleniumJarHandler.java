package uno.anahata.asi.toolkit.java.classpath;

import java.io.File;
import java.io.StringReader;
import java.util.Properties;

/**
 * Specialized handler for Selenium libraries.
 * <p>Implementation detail: It detects Selenium JARs via filename keywords 
 * and extracts specific version info from {@code selenium-build.properties}.</p>
 * @author anahata
 */
public class SeleniumJarHandler extends AbstractJarHandler {
    @Override
    public boolean canHandle(File jarFile) {
        return jarFile.getName().contains("selenium") || readEntry(jarFile, "META-INF/selenium-build.properties") != null;
    }

    @Override
    public JarMetadata extractMetadata(File jarFile) {
        String props = readEntry(jarFile, "META-INF/selenium-build.properties");
        if (props != null) {
            try {
                Properties p = new Properties();
                p.load(new StringReader(props));
                return JarMetadata.builder()
                    .id(getBaseName(jarFile))
                    .version(p.getProperty("Selenium-Version"))
                    .vendor("SeleniumHQ")
                    .build();
            } catch (Exception e) {}
        }
        return null;
    }
}
