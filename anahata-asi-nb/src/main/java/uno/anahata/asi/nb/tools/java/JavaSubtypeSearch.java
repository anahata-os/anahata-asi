/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.java;

import java.net.URL;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.TypeElement;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.ClassIndex;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.CompilationInfo;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.SourceUtils;
import org.openide.filesystems.FileObject;

/**
 * A "Finder" command object that recursively searches for all subtypes 
 * (implementations and subclasses) of a given JavaType.
 */
@Slf4j
@Getter
public class JavaSubtypeSearch {

    /** The root node of the subtype hierarchy tree. */
    private final JavaHierarchyNode rootNode;

    /**
     * Performs a recursive subtype search.
     * 
     * @param rootType The starting type.
     * @param maxDepth The maximum depth to recurse.
     * @throws Exception if the search fails.
     */
    public JavaSubtypeSearch(JavaType rootType, int maxDepth) throws Exception {
        this.rootNode = JavaHierarchyNode.builder().type(rootType).build();
        
        FileObject fo = rootType.getClassFileObject();
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            js = JavaSource.create(ClasspathInfo.create(fo));
        }

        js.runUserActionTask(cc -> {
            cc.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
            TypeElement te = (TypeElement) rootType.getHandle().resolve(cc);
            if (te != null) {
                buildSubtypeTree(cc, te, rootNode, maxDepth, new HashSet<>());
            }
        }, true);
    }

    /**
     * Recursively builds the subtype tree using the NetBeans ClassIndex.
     * 
     * @param info The current compilation context.
     * @param te The type element to find implementors for.
     * @param node The current node in the hierarchy tree.
     * @param depth The remaining depth to search.
     * @param visited Set of visited FQNs to prevent cycles in broken classpaths.
     */
    private void buildSubtypeTree(CompilationInfo info, TypeElement te, JavaHierarchyNode node, int depth, Set<String> visited) {
        if (depth <= 0) {
            return;
        }
        
        String fqn = te.getQualifiedName().toString();
        if (!visited.add(fqn)) {
            return;
        }

        ElementHandle<TypeElement> handle = ElementHandle.create(te);
        Set<ElementHandle<TypeElement>> implementors = info.getClasspathInfo().getClassIndex().getElements(
            handle, 
            EnumSet.of(ClassIndex.SearchKind.IMPLEMENTORS), 
            EnumSet.of(ClassIndex.SearchScope.SOURCE, ClassIndex.SearchScope.DEPENDENCIES)
        );

        for (ElementHandle<TypeElement> kidHandle : implementors) {
            TypeElement kidTe = kidHandle.resolve(info);
            if (kidTe != null) {
                FileObject kidFo = SourceUtils.getFile(kidHandle, info.getClasspathInfo());
                URL url = null;
                try { if (kidFo != null) url = kidFo.toURL(); } catch (Exception e) {}
                
                JavaHierarchyNode kidNode = JavaHierarchyNode.builder()
                        .type(new JavaType(kidHandle, url))
                        .build();
                
                node.getSubtypes().add(kidNode);
                buildSubtypeTree(info, kidTe, kidNode, depth - 1, visited);
            }
        }
    }
}
