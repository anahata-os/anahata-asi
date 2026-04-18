/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.ClassTree;
import com.sun.source.tree.NewClassTree;
import com.sun.source.util.TreePath;
import com.sun.source.util.TreePathScanner;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import lombok.Getter;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.JavaSource;
import org.openide.filesystems.FileObject;
import javax.lang.model.element.ExecutableElement;
import org.netbeans.api.java.source.CompilationController;
import org.netbeans.api.java.source.Task;
import uno.anahata.asi.agi.tool.AgiToolException;

/**
 * A "Finder" command object that searches for all members of a given JavaType
 * upon instantiation. It uses a context-aware ClasspathInfo derived from the
 * type's own class file.
 *
 * @author anahata
 */
@Getter
public class JavaMemberSearch {

    /**
     * The JavaType being searched.
     */
    private final JavaType type;
    
    /**
     * The list of members found for the type.
     */
    private final List<JavaMember> members;

    /**
     * Constructs a new JavaMemberSearch and performs the search for members of
     * the given JavaType.
     *
     * @param javaType the type to search members for.
     * @throws Exception if the search fails.
     */
    public JavaMemberSearch(JavaType javaType) throws Exception {
        this.type = javaType;
        FileObject classFile = javaType.getClassFileObject();
        JavaSource javaSource = JavaSource.forFileObject(classFile);
        if (javaSource == null) {
            ClasspathInfo cpInfo = ClasspathInfo.create(classFile);
            javaSource = JavaSource.create(cpInfo);
        }
        if (javaSource == null) {
            throw new AgiToolException("Could not create JavaSource for: " + classFile.getPath());
        }
        final List<JavaMember> foundMembers = new ArrayList<>();
        final URL url = javaType.getUrl();
        javaSource.runUserActionTask(controller-> {
            controller.toPhase(JavaSource.Phase.RESOLVED);
            Element resolvedElement = javaType.getHandle().resolve(controller);
            if (resolvedElement == null) {
                throw new AgiToolException("Failed to resolve ElementHandle for " + javaType.getHandle());
            }
            if (!(resolvedElement instanceof TypeElement typeElement)) {
                throw new AgiToolException("Resolved element is not a TypeElement, but a " + resolvedElement.getKind());
            }
            for (Element element : typeElement.getEnclosedElements()) {
                ElementKind kind = element.getKind();
                ElementHandle<? extends Element> handle = ElementHandle.create(element);
                String name = element.getSimpleName().toString();
                String memberFqn;
                if (element instanceof ExecutableElement ee) {
                    String simpleName = kind == ElementKind.CONSTRUCTOR ? "<init>" : name;
                    String params = ee.getParameters().stream().map(p-> {
                        String t = p.asType().toString();
                        int bracket = t.indexOf('<');
                        return bracket != -1 ? t.substring(0, bracket) : t;
                    }).collect(Collectors.joining(","));
                    memberFqn = type.getFqn() + "." + simpleName + "(" + params + ")";
                } else if (kind == ElementKind.STATIC_INIT) {
                    memberFqn = type.getFqn() + ".<clinit>()";
                } else if (kind == ElementKind.INSTANCE_INIT) {
                    memberFqn = type.getFqn() + ".<init-block>()";
                } else {
                    memberFqn = type.getFqn() + "." + name;
                }
                Set<String> modifiers = element.getModifiers().stream().map(m-> m.name().toLowerCase()).collect(Collectors.toSet());
                foundMembers.add(new JavaMember(handle, memberFqn, name, kind, url, modifiers));
            }
            TreePath typePath = controller.getTrees().getPath(typeElement);
            if (typePath != null) {
                new TreePathScanner<Void, Void>() {
                    private int anonCount = 0;

                    @Override
                    public Void visitNewClass(NewClassTree node, Void p) {
                        if (node.getClassBody() != null) {
                            anonCount++;
                            TreePath bodyPath = new TreePath(getCurrentPath(), node.getClassBody());
                            Element anonElement = controller.getTrees().getElement(bodyPath);
                            if (anonElement instanceof TypeElement anonTe) {
                                ElementHandle<TypeElement> handle = ElementHandle.create(anonTe);
                                String baseType = node.getIdentifier().toString();
                                String name = "Anonymous #" + anonCount + " (" + baseType + ")";
                                String memberFqn = type.getFqn() + "." + name;
                                Set<String> modifiers = anonTe.getModifiers().stream().map(m-> m.name().toLowerCase()).collect(Collectors.toSet());
                                foundMembers.add(new JavaMember(handle, memberFqn, name, anonTe.getKind(), url, modifiers));
                            }
                        }
                        return super.visitNewClass(node, p);
                    }

                    @Override
                    public Void visitClass(ClassTree node, Void p) {
                        if (getCurrentPath().equals(typePath)) {
                            return super.visitClass(node, p);
                        }
                        return null;
                    }
                }.scan(typePath, null);
            }
        }, true);
        this.members = Collections.unmodifiableList(foundMembers);
    }
}
