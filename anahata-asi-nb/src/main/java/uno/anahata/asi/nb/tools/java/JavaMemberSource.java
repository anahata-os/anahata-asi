/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.Tree;
import com.sun.source.util.SourcePositions;
import java.util.concurrent.atomic.AtomicReference;
import javax.lang.model.element.Element;
import lombok.Getter;
import org.netbeans.api.java.source.JavaSource;

/**
 * A rich result object that represents the outcome of a source-finding operation for a specific JavaMember.
 * It extracts only the source code of the member itself from the containing file.
 */
@Getter
public class JavaMemberSource extends JavaTypeSource {

    /**
     * Constructs a new JavaMemberSource and attempts to extract the source code for the given JavaMember.
     * @param member the member to find source for.
     * @throws Exception if the source cannot be found or read.
     */
    public JavaMemberSource(JavaMember member) throws Exception {
        super(member);
        
        // 1. Create a JavaSource for the file (sourceFile is inherited from JavaTypeSource)
        JavaSource js = JavaSource.forFileObject(this.sourceFile);
        if (js == null) {
            this.content = null;
            return;
        }

        final AtomicReference<String> sourceRef = new AtomicReference<>();
        js.runUserActionTask(controller -> {
            controller.toPhase(JavaSource.Phase.RESOLVED);
            // 2. Resolve the handle to an Element
            Element element = member.getHandle().resolve(controller);
            if (element != null) {
                // 3. Get the Tree for the element
                Tree tree = controller.getTrees().getTree(element);
                if (tree != null) {
                    // 4. Get the source positions
                    SourcePositions sp = controller.getTrees().getSourcePositions();
                    int start = (int) sp.getStartPosition(controller.getCompilationUnit(), tree);
                    int end = (int) sp.getEndPosition(controller.getCompilationUnit(), tree);
                    
                    if (start != -1 && end != -1) {
                        // 5. Extract the substring
                        sourceRef.set(controller.getText().substring(start, end));
                    }
                }
            }
        }, true);
        
        this.content = sourceRef.get();
    }
    
    public JavaMember getMember() {
        return (JavaMember) javaType;
    }
}
