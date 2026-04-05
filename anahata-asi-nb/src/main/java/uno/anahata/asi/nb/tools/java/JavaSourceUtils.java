/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.ClassTree;
import com.sun.source.tree.Tree;
import java.io.File;
import java.io.IOException;
import java.util.List;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import org.netbeans.api.java.source.Comment;
import org.netbeans.api.java.source.CompilationController;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.GeneratorUtilities;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.Task;
import org.netbeans.api.java.source.TreePathHandle;
import org.netbeans.api.java.source.WorkingCopy;
import org.netbeans.modules.refactoring.java.api.MemberInfo;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.agi.tool.AgiToolException;

/**
 * Shared utilities for NetBeans Java Source (AST) operations.
 * <p>
 * Provides surgical helper methods for element resolution, tree handle 
 * management, and Javadoc manipulation using the NetBeans Java Source API.
 * </p>
 * 
 * @author anahata
 */
public class JavaSourceUtils {

    /**
     * Resolves a {@link FileObject} for the given absolute path.
     * 
     * @param filePath The absolute path to the file.
     * @return The corresponding FileObject.
     * @throws AgiToolException If the file does not exist or cannot be resolved.
     */
    public static FileObject getFileObject(String filePath) throws AgiToolException {
        File f = new File(filePath);
        if (!f.exists()) {
            throw new AgiToolException("File does not exist: " + filePath);
        }
        FileObject fo = FileUtil.toFileObject(f);
        if (fo == null) {
            throw new AgiToolException("Could not resolve FileObject for: " + filePath);
        }
        return fo;
    }

    /**
     * Creates a {@link TreePathHandle} for a specific member within a file.
     * 
     * @param fo The FileObject containing the class.
     * @param memberName The simple name of the member.
     * @return A TreePathHandle or null if the member is not found.
     * @throws IOException If the source cannot be parsed.
     */
    public static TreePathHandle getTreePathHandleForMember(FileObject fo, String memberName) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            return null;
        }

        final TreePathHandle[] handle = new TreePathHandle[1];
        js.runUserActionTask(new Task<CompilationController>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                TypeElement te = parameter.getTopLevelElements().isEmpty() ? null : parameter.getTopLevelElements().get(0);
                if (te != null) {
                    for (Element e : te.getEnclosedElements()) {
                        if (e.getSimpleName().contentEquals(memberName)) {
                            handle[0] = TreePathHandle.create(e, parameter);
                            break;
                        }
                    }
                }
            }
        }, true);
        return handle[0];
    }

    /**
     * Creates a {@link TreePathHandle} for the primary top-level class in a file.
     * 
     * @param fo The FileObject.
     * @return A TreePathHandle for the class or null.
     * @throws IOException If the source cannot be parsed.
     */
    public static TreePathHandle getTreePathHandleForClass(FileObject fo) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            return null;
        }

        final TreePathHandle[] handle = new TreePathHandle[1];
        js.runUserActionTask(new Task<CompilationController>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                TypeElement te = parameter.getTopLevelElements().isEmpty() ? null : parameter.getTopLevelElements().get(0);
                if (te != null) {
                    handle[0] = TreePathHandle.create(te, parameter);
                }
            }
        }, true);
        return handle[0];
    }

    /**
     * Finds a {@link Element} by its fully qualified name within a {@link WorkingCopy}.
     * Supports both types and members (including basic method signature matching).
     * 
     * @param wc The working copy.
     * @param memberFqn The FQN to search for.
     * @return The resolved Element or null.
     */
    public static Element findElement(WorkingCopy wc, String memberFqn) {
        // Handle method signature if present
        String pureFqn = memberFqn;
        if (memberFqn.contains("(")) {
            pureFqn = memberFqn.substring(0, memberFqn.indexOf('('));
        }

        TypeElement type = wc.getElements().getTypeElement(pureFqn);
        if (type != null) {
            return type;
        }
        
        int lastDot = pureFqn.lastIndexOf('.');
        if (lastDot == -1) {
            return null;
        }
        
        String parentFqn = pureFqn.substring(0, lastDot);
        String memberName = pureFqn.substring(lastDot + 1);
        
        TypeElement parent = wc.getElements().getTypeElement(parentFqn);
        if (parent == null) {
            return null;
        }
        
        for (Element e : parent.getEnclosedElements()) {
            if (e.getSimpleName().toString().equals(memberName)) {
                if (e instanceof ExecutableElement ee && memberFqn.contains("(")) {
                    if (matchSignature(ee, memberFqn)) {
                        return ee;
                    }
                } else {
                    return e;
                }
            }
        }
        return null;
    }

    /**
     * Internal helper to match a method element against a string signature.
     */
    private static boolean matchSignature(ExecutableElement ee, String methodFqn) {
        String params = methodFqn.substring(methodFqn.indexOf('(') + 1, methodFqn.lastIndexOf(')')).trim();
        if (params.isEmpty()) {
            return ee.getParameters().isEmpty();
        }
        
        String[] expectedTypes = params.split(",");
        if (expectedTypes.length != ee.getParameters().size()) {
            return false;
        }
        
        for (int i = 0; i < expectedTypes.length; i++) {
            String expected = expectedTypes[i].trim();
            String actual = ee.getParameters().get(i).asType().toString();
            // Basic matching (handle simple name vs FQN loosely)
            if (!actual.endsWith(expected)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Conveniently finds an {@link ExecutableElement} for a method FQN.
     * 
     * @param wc The working copy.
     * @param methodFqn The method FQN.
     * @return The ExecutableElement or null.
     */
    public static ExecutableElement findMethodElement(WorkingCopy wc, String methodFqn) {
        Element e = findElement(wc, methodFqn);
        return (e instanceof ExecutableElement ee) ? ee : null;
    }

    /**
     * Resolves a set of member names into {@link ElementHandle}s.
     * 
     * @param fo The FileObject.
     * @param memberNames The names to resolve.
     * @param methods Output list for resolved methods.
     * @param fields Output list for resolved fields.
     * @throws IOException If the source cannot be parsed.
     */
    public static void resolveMembers(FileObject fo, List<String> memberNames, List<ElementHandle<ExecutableElement>> methods, List<ElementHandle<VariableElement>> fields) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            return;
        }

        js.runUserActionTask(new Task<CompilationController>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                TypeElement te = parameter.getTopLevelElements().isEmpty() ? null : parameter.getTopLevelElements().get(0);
                if (te != null) {
                    for (Element e : te.getEnclosedElements()) {
                        if (memberNames.contains(e.getSimpleName().toString())) {
                            if (e instanceof ExecutableElement ee) {
                                methods.add(ElementHandle.create(ee));
                            } else if (e instanceof VariableElement ve) {
                                fields.add(ElementHandle.create(ve));
                            }
                        }
                    }
                }
            }
        }, true);
    }

    /**
     * Resolves member names into {@link MemberInfo} objects for refactoring.
     * 
     * @param fo The FileObject.
     * @param memberNames The names to resolve.
     * @param members Output list for MemberInfo objects.
     * @throws IOException If the source cannot be parsed.
     */
    @SuppressWarnings("unchecked")
    public static void resolveMemberInfos(FileObject fo, List<String> memberNames, List<MemberInfo<ElementHandle<? extends Element>>> members) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            return;
        }

        js.runUserActionTask(new Task<CompilationController>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                TypeElement te = parameter.getTopLevelElements().isEmpty() ? null : parameter.getTopLevelElements().get(0);
                if (te != null) {
                    for (Element e : te.getEnclosedElements()) {
                        if (memberNames.contains(e.getSimpleName().toString())) {
                            members.add((MemberInfo) MemberInfo.create(e, parameter));
                        }
                    }
                }
            }
        }, true);
    }

    /**
     * Structurally sets or removes the Javadoc for a given tree.
     * 
     * @param wc The working copy.
     * @param tree The tree to modify.
     * @param javadocText The Javadoc content (without markers), or null to remove.
     */
    public static void setJavadoc(WorkingCopy wc, Tree tree, String javadocText) {
        if (javadocText == null) {
            wc.getTreeMaker().removeComment(tree, 0, true);
            return;
        }
        
        String formatted = "/**\n * " + javadocText.replace("\n", "\n * ") + "\n */";
        Comment comment = Comment.create(Comment.Style.JAVADOC, -1, -1, -1, formatted);
        wc.getTreeMaker().addComment(tree, comment, true);
    }
}
