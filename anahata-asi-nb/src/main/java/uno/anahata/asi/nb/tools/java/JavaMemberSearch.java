/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. For\u00e7a Bar\u00e7a! */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.BlockTree;
import com.sun.source.tree.ClassTree;
import com.sun.source.tree.NewClassTree;
import com.sun.source.tree.Tree;
import com.sun.source.util.TreePath;
import com.sun.source.util.TreePathScanner;
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
        javaSource.runUserActionTask(controller-> {
            controller.toPhase(JavaSource.Phase.RESOLVED);
            Element resolvedElement = javaType.getHandle().resolve(controller);
            if (resolvedElement == null) {
                throw new AgiToolException("Failed to resolve ElementHandle for " + javaType.getHandle());
            }
            if (!(resolvedElement instanceof TypeElement typeElement)) {
                throw new AgiToolException("Resolved element is not a TypeElement, but a " + resolvedElement.getKind());
            }
            TreePath typePath = controller.getTrees().getPath(typeElement);
            if (typePath != null && typePath.getLeaf() instanceof ClassTree ct) {
                int staticCount = 0;
                int instanceCount = 0;
                for (Tree member : ct.getMembers()) {
                    Element e = controller.getTrees().getElement(new TreePath(typePath, member));
                    ElementKind kind;
                    String memberName;
                    String simpleName;
                    String memberFqn;
                    Set<String> modifiers;
                    ElementHandle<? extends Element> handle = null;
                    if (member.getKind() == Tree.Kind.BLOCK) {
                        BlockTree bt = (BlockTree) member;
                        boolean isStatic = bt.isStatic();
                        int index = isStatic ? ++staticCount : ++instanceCount;
                        kind = isStatic ? ElementKind.STATIC_INIT : ElementKind.INSTANCE_INIT;
                        memberName = isStatic ? "<clinit>" : "<init-block>";
                        simpleName = memberName + "#" + index + "()";
                        memberFqn = type.getFqn() + "." + simpleName;
                        modifiers = isStatic ? Set.of("static") : Collections.emptySet();
                    } else {
                        if (e == null) {
                            continue;
                        }
                        kind = e.getKind();
                        String name = e.getSimpleName().toString();
                        memberName = name;
                        if (e instanceof ExecutableElement ee) {
                            String namePart = kind == ElementKind.CONSTRUCTOR ? "<init>" : name;
                            String params = ee.getParameters().stream().map(p -> {
                                String t = p.asType().toString();
                                int bracket = t.indexOf('<');
                                return bracket != -1 ? t.substring(0, bracket) : t;
                            }).collect(Collectors.joining(","));
                            simpleName = namePart + "(" + params + ")";
                        } else {
                            simpleName = name;
                        }
                        if (kind.isClass() || kind.isInterface()) {
                            memberFqn = type.getFqn() + "$" + name;
                        } else {
                            memberFqn = type.getFqn() + "." + simpleName;
                        }
                        modifiers = e.getModifiers().stream().map(m -> m.name().toLowerCase()).collect(Collectors.toSet());
                        handle = ElementHandle.create(e);
                    }
                    foundMembers.add(new JavaMember(handle, memberFqn, memberName, kind, null, modifiers));
                }
                new TreePathScanner<Void, Void>() {

                    @Override
                    public Void visitNewClass(NewClassTree node, Void p) {
                        if (node.getClassBody() != null) {
                            TreePath bodyPath = new TreePath(getCurrentPath(), node.getClassBody());
                            Element anonElement = controller.getTrees().getElement(bodyPath);
                            if (anonElement instanceof TypeElement anonTe) {
                                ElementHandle<TypeElement> handle = ElementHandle.create(anonTe);
                                String binaryName = handle.getBinaryName();
                                Set<String> modifiers = anonTe.getModifiers().stream().map(m-> m.name().toLowerCase()).collect(Collectors.toSet());
                                foundMembers.add(new JavaMember(handle, binaryName, binaryName.substring(binaryName.lastIndexOf('$')), anonTe.getKind(), null, modifiers));
                            }
                        }
                        return super.visitNewClass(node, p);
                    }

                    @Override
                    public Void visitClass(ClassTree node, Void p) {
                        return getCurrentPath().equals(typePath) ? super.visitClass(node, p) : null;
                    }

                }.scan(typePath, null);
            } else {
                // Fallback for binaries (JARs) where no AST is available
                int staticCount = 0;
                int instanceCount = 0;
                for (Element element : typeElement.getEnclosedElements()) {
                    ElementKind kind = element.getKind();
                    ElementHandle<? extends Element> handle = ElementHandle.create(element);
                    String name = element.getSimpleName().toString();
                    String memberName = name;
                    String simpleName;
                    String memberFqn;
                    if (element instanceof ExecutableElement ee) {
                        String namePart = kind == ElementKind.CONSTRUCTOR ? "<init>" : name;
                        String params = ee.getParameters().stream().map(p -> {
                            String t = p.asType().toString();
                            int bracket = t.indexOf('<');
                            return bracket != -1 ? t.substring(0, bracket) : t;
                        }).collect(Collectors.joining(","));
                        simpleName = namePart + "(" + params + ")";
                    } else {
                        simpleName = name;
                    }
                    
                    if (kind == ElementKind.STATIC_INIT) {
                        int index = ++staticCount;
                        memberName = "<clinit>";
                        simpleName = memberName + "#" + index + "()";
                        memberFqn = type.getFqn() + "." + simpleName;
                    } else if (kind == ElementKind.INSTANCE_INIT) {
                        int index = ++instanceCount;
                        memberName = "<init-block>";
                        simpleName = memberName + "#" + index + "()";
                        memberFqn = type.getFqn() + "." + simpleName;
                    } else if (kind.isClass() || kind.isInterface()) {
                        memberFqn = type.getFqn() + "$" + name;
                    } else {
                        memberFqn = type.getFqn() + "." + simpleName;
                    }
                    
                    Set<String> modifiers = element.getModifiers().stream().map(m -> m.name().toLowerCase()).collect(Collectors.toSet());
                    foundMembers.add(new JavaMember(handle, memberFqn, memberName, kind, null, modifiers));
                }
            }
        }, true);
        this.members = Collections.unmodifiableList(foundMembers);
    }
}
