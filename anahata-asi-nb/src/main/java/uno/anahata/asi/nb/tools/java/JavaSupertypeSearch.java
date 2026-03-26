/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.java;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.CompilationInfo;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.SourceUtils;
import org.openide.filesystems.FileObject;

/**
 * A "Finder" command object that recursively searches for all supertypes 
 * (base classes and implemented interfaces) of a given JavaType.
 */
@Slf4j
@Getter
public class JavaSupertypeSearch {

    /** The root node of the supertype hierarchy tree. */
    private final JavaHierarchyNode rootNode;

    /**
     * Performs a recursive supertype search.
     * 
     * @param rootType The starting type.
     * @param maxDepth The maximum depth to recurse up.
     * @throws Exception if the search fails.
     */
    public JavaSupertypeSearch(JavaType rootType, int maxDepth) throws Exception {
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
                buildSupertypeTree(cc, te, rootNode, maxDepth, new HashSet<>());
            }
        }, true);
    }

    /**
     * Recursively builds the supertype tree by traversing direct supertypes (class and interfaces).
     * 
     * @param info The current compilation context.
     * @param te The type element to find parents for.
     * @param node The current node in the hierarchy tree.
     * @param depth The remaining depth to search.
     * @param visited Set of visited FQNs to prevent cycles.
     */
    private void buildSupertypeTree(CompilationInfo info, TypeElement te, JavaHierarchyNode node, int depth, Set<String> visited) {
        if (depth <= 0) {
            return;
        }

        String fqn = te.getQualifiedName().toString();
        // Stop at Object to keep the tree clean
        if (fqn.equals("java.lang.Object") || !visited.add(fqn)) {
            return;
        }

        List<? extends TypeMirror> superTypes = info.getTypes().directSupertypes(te.asType());
        for (TypeMirror tm : superTypes) {
            TypeElement superTe = (TypeElement) info.getTypes().asElement(tm);
            if (superTe != null) {
                String superFqn = superTe.getQualifiedName().toString();
                if (superFqn.equals("java.lang.Object")) {
                    continue;
                }

                ElementHandle<TypeElement> handle = ElementHandle.create(superTe);
                FileObject superFo = SourceUtils.getFile(handle, info.getClasspathInfo());
                URL url = null;
                try { if (superFo != null) url = superFo.toURL(); } catch (Exception e) {}

                JavaHierarchyNode superNode = JavaHierarchyNode.builder()
                        .type(new JavaType(handle, url))
                        .build();

                node.getSupertypes().add(superNode);
                buildSupertypeTree(info, superTe, superNode, depth - 1, visited);
            }
        }
    }
}
