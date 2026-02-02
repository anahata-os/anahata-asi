/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.java;

import java.util.concurrent.atomic.AtomicReference;
import javax.lang.model.element.Element;
import lombok.Getter;
import org.netbeans.api.java.source.JavaSource;
import org.openide.filesystems.FileObject;

/**
 * A rich result object that represents the outcome of a Javadoc retrieval operation for a JavaType.
 * It leverages the polymorphic nature of JavaType to retrieve docs for classes, inner classes, and members.
 */
@Getter
public class JavaTypeDocs {

    protected final JavaType javaType;
    protected String javadoc;

    /**
     * Constructs a new JavaTypeDocs and attempts to retrieve the Javadoc for the given JavaType.
     * @param javaType the type or member to retrieve Javadoc for.
     * @throws Exception if the Javadoc cannot be retrieved.
     */
    public JavaTypeDocs(JavaType javaType) throws Exception {
        this.javaType = javaType;
        
        // 1. Prefer the source file if available, otherwise fallback to the class file.
        FileObject fileToUse;
        try {
            fileToUse = javaType.getSource().getSourceFile();
        } catch (Exception e) {
            fileToUse = javaType.getClassFileObject();
        }
        
        // 2. Create a JavaSource for the file.
        JavaSource js = JavaSource.forFileObject(fileToUse);
        if (js == null) {
            throw new Exception("Could not create JavaSource for: " + fileToUse.getPath());
        }

        final AtomicReference<String> docRef = new AtomicReference<>();
        final Exception[] taskException = new Exception[1];

        js.runUserActionTask(controller -> {
            try {
                controller.toPhase(JavaSource.Phase.RESOLVED);
                // 3. Resolve the handle to an Element
                Element element = javaType.getHandle().resolve(controller);
                if (element != null) {
                    // 4. Use the Elements utility to get the doc comment.
                    // This method is "magic": it fishes Javadocs from sources if the javadoc jar is missing.
                    String comment = controller.getElements().getDocComment(element);
                    if (comment != null) {
                        docRef.set(cleanJavadoc(comment));
                    }
                }
            } catch (Exception e) {
                taskException[0] = e;
            }
        }, true);
        
        if (taskException[0] != null) {
            throw taskException[0];
        }

        this.javadoc = docRef.get();
        
        if (this.javadoc == null) {
             throw new Exception("Javadoc not found for: " + javaType.getHandle() + ". "
                    + "If this is a Maven dependency, try using 'MavenTools.downloadProjectDependencies' or 'MavenTools.downloadDependencyArtifact' to retrieve the 'javadoc' or 'sources' classifier.");
        }
    }

    protected static String cleanJavadoc(String rawDoc) {
        if (rawDoc == null) {
            return "";
        }
        return rawDoc.trim();
    }
}
