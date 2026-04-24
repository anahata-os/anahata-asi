/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.AnnotationTree;
import com.sun.source.tree.BlockTree;
import com.sun.source.tree.ClassTree;
import com.sun.source.tree.ExpressionTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.ModifiersTree;
import com.sun.source.tree.NewClassTree;
import com.sun.source.tree.Tree;
import com.sun.source.tree.VariableTree;
import com.sun.source.util.TreePath;
import com.sun.source.util.TreePathScanner;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeMirror;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.CompilationController;
import org.netbeans.api.java.source.CompilationInfo;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.Task;
import org.netbeans.api.java.source.TreeMaker;
import org.netbeans.api.java.source.TreePathHandle;
import org.netbeans.api.java.source.TreeUtilities;
import org.netbeans.api.java.source.WorkingCopy;
import org.netbeans.modules.refactoring.java.api.MemberInfo;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.agi.tool.AgiToolException;
import org.openide.loaders.DataObject;
import org.openide.cookies.EditorCookie;
import org.openide.cookies.SaveCookie;

/**
 * Shared utilities for NetBeans Java Source (AST) operations.
 * <p>
 * Provides surgical helper methods for element resolution, tree handle
 * management, and Javadoc manipulation using the NetBeans Java Source API.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class JavaSourceUtils {

    /**
     * Defines positions for structural member insertion.
     */
    public enum RelativePosition {
        START, END, BEFORE, AFTER
    }

    /**
     * Extracts the parent FQN (Class or Outer Class) from a member or nested
     * type FQN.
     *
     * @param memberFqn The FQN to parse.
     * @return The parent FQN.
     */
    public static String getParentFqn(String memberFqn) {
        int paren = memberFqn.indexOf('(');
        String namePart = paren != -1 ? memberFqn.substring(0, paren) : memberFqn;
        int lastSeparator = Math.max(namePart.lastIndexOf('.'), namePart.lastIndexOf('$'));
        if (lastSeparator == -1) {
            return "";
        }
        return namePart.substring(0, lastSeparator);
    }

    /**
     * Extracts the simple name of a member or nested type from an FQN.
     *
     * @param memberFqn The FQN to parse.
     * @return The simple name (e.g., 'myMethod' or 'InnerClass').
     */
    public static String getMemberSimpleName(String memberFqn) {
        int paren = memberFqn.indexOf('(');
        String namePart = paren != -1 ? memberFqn.substring(0, paren) : memberFqn;
        int lastSeparator = Math.max(namePart.lastIndexOf('.'), namePart.lastIndexOf('$'));
        return namePart.substring(lastSeparator + 1);
    }

    /**
     * Resolves a {@link FileObject} for the given absolute path.
     *
     * @param filePath The absolute path to the file.
     * @return The corresponding FileObject.
     * @throws AgiToolException If the file does not exist or cannot be
     * resolved.
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
     * @deprecated Use {@link #resolveMember(FileObject, String)} for
     * high-precision FQN resolution.
     */
    @Deprecated
    public static TreePathHandle getTreePathHandleForMember(FileObject fo, String memberName) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            return null;
        }

        final TreePathHandle[] handle = new TreePathHandle[1];
        js.runUserActionTask(new Task<>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                for (TypeElement te : parameter.getTopLevelElements()) {
                    for (Element e : te.getEnclosedElements()) {
                        if (e.getSimpleName().contentEquals(memberName)) {
                            handle[0] = TreePathHandle.create(e, parameter);
                            return;
                        }
                    }
                }
            }
        }, true);
        return handle[0];
    }

    /**
     * Creates a {@link TreePathHandle} for the primary top-level class in a
     * file.
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
        js.runUserActionTask(new Task<>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED);
                for (TypeElement te : parameter.getTopLevelElements()) {
                    if (te.getSimpleName().contentEquals(fo.getName())) {
                        handle[0] = TreePathHandle.create(te, parameter);
                        return;
                    }
                }
                if (!parameter.getTopLevelElements().isEmpty()) {
                    handle[0] = TreePathHandle.create(parameter.getTopLevelElements().get(0), parameter);
                }
            }
        }, true);
        return handle[0];
    }

    public static Tree findTree(CompilationInfo wc, String memberFqn) {
        String pureFqn = memberFqn;
        String indexPart = null;
        if (memberFqn.contains("#")) {
            indexPart = memberFqn.substring(memberFqn.indexOf('#') + 1, memberFqn.indexOf('('));
            pureFqn = memberFqn.substring(0, memberFqn.indexOf('#'));
        } else if (memberFqn.contains("(")) {
            pureFqn = memberFqn.substring(0, memberFqn.indexOf('('));
        }
        TypeElement type = wc.getElements().getTypeElement(pureFqn);
        if (type != null) {
            if (pureFqn.equals(memberFqn) || memberFqn.equals(type.getQualifiedName().toString())) {
                return wc.getTrees().getTree(type);
            }
        }
        int lastSeparator = Math.max(pureFqn.lastIndexOf('.'), pureFqn.lastIndexOf('$'));
        if (lastSeparator == -1) {
            return null;
        }
        String parentFqn = pureFqn.substring(0, lastSeparator);
        String memberName = pureFqn.substring(lastSeparator + 1);
        Tree parentTree = findTree(wc, parentFqn);
        if (!(parentTree instanceof ClassTree ct)) {
            return null;
        }
        if (indexPart != null) {
            int targetIndex = Integer.parseInt(indexPart);
            int currentCount = 0;
            for (Tree member : ct.getMembers()) {
                if (member.getKind() == Tree.Kind.BLOCK) {
                    BlockTree bt = (BlockTree) member;
                    String blockName = bt.isStatic() ? "<clinit>" : "<init-block>";
                    if (blockName.equals(memberName)) {
                        if (++currentCount == targetIndex) {
                            return member;
                        }
                    }
                }
            }
            return null;
        }
        for (Tree member : ct.getMembers()) {
            String name = null;
            if (member instanceof MethodTree mt) {
                name = mt.getName().toString();
                if (name.equals(memberName) || (name.equals("<init>") && memberName.equals("<init>"))) {
                    if (memberFqn.contains("(")) {
                        Element e = wc.getTrees().getElement(TreePath.getPath(wc.getCompilationUnit(), member));
                        if (e instanceof ExecutableElement ee && matchSignature(wc, ee, memberFqn)) {
                            return member;
                        }
                    } else {
                        return member;
                    }
                }
            } else if (member instanceof VariableTree vt) {
                name = vt.getName().toString();
            } else if (member instanceof ClassTree innerCt) {
                name = innerCt.getSimpleName().toString();
            }
            if (memberName.equals(name)) {
                return member;
            }
        }
        if (memberName.matches("\\d+")) {
            final Tree[] found = new Tree[1];
            final String targetBinaryName = pureFqn;
            new TreePathScanner<Void, Void>() {
                @Override
                public Void visitNewClass(NewClassTree node, Void p) {
                    if (node.getClassBody() != null) {
                        Element e = wc.getTrees().getElement(new TreePath(getCurrentPath(), node.getClassBody()));
                        if (e instanceof TypeElement te && ElementHandle.create(te).getBinaryName().equals(targetBinaryName)) {
                            found[0] = node.getClassBody();
                        }
                    }
                    return super.visitNewClass(node, p);
                }
            }.scan(TreePath.getPath(wc.getCompilationUnit(), parentTree), null);
            if (found[0] != null) {
                return found[0];
            }
        }
        return null;
    }

    /**
     * Finds a {@link Element} by its fully qualified name within a
     * {@link WorkingCopy}. Supports types, members, and packages.
     *
     * @param wc The working copy.
     * @param memberFqn The FQN to search for.
     * @return The resolved Element or null.
     */
    public static Element findElement(CompilationInfo wc, String memberFqn) {
        Tree tree = findTree(wc, memberFqn);
        return tree == null ? null : wc.getTrees().getElement(TreePath.getPath(wc.getCompilationUnit(), tree));
    }

    /**
     * Internal helper to match a method element against a string signature.
     * <p>
     * This implementation is erasure-aware. If the provided signature does not
     * contain generics, it will match against the raw AST type.
     * </p>
     */
    private static boolean matchSignature(CompilationInfo wc, ExecutableElement ee, String methodFqn) {
        String paramsPart = methodFqn.substring(methodFqn.indexOf('(') + 1, methodFqn.lastIndexOf(')')).trim();
        List<String> expectedTypes = splitParameters(paramsPart);
        if (expectedTypes.size() != ee.getParameters().size()) {
            return false;
        }
        for (int i = 0; i < expectedTypes.size(); i++) {
            String expected = expectedTypes.get(i);
            if (expected.contains(" ")) {
                String[] parts = expected.split("\\s+");
                expected = parts[parts.length - 1];
                if (expected.endsWith(">")) {
                    expected = parts[parts.length - 2];
                }
            }
            TypeMirror actualMirror = ee.getParameters().get(i).asType();
            Element actualEl = wc.getTypes().asElement(actualMirror);
            String actual = (actualEl instanceof TypeElement te)
                    ? ElementHandle.create(te).getBinaryName()
                    : actualMirror.toString();
            if (!expected.contains("<") && actual.contains("<")) {
                actual = actual.substring(0, actual.indexOf('<'));
            }
            if (!actual.endsWith(expected)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Splits a parameter string into individual types, respecting generic
     * brackets.
     */
    private static List<String> splitParameters(String params) {
        List<String> result = new ArrayList<>();
        if (params.isEmpty()) {
            return result;
        }

        StringBuilder current = new StringBuilder();
        int depth = 0;
        for (char c : params.toCharArray()) {
            if (c == '<') {
                depth++;
            }
            if (c == '>') {
                depth--;
            }

            if (c == ',' && depth == 0) {
                result.add(current.toString().trim());
                current = new StringBuilder();
            } else {
                current.append(c);
            }
        }
        if (current.length() > 0) {
            result.add(current.toString().trim());
        }
        return result;
    }

    /**
     *
     * /
     *
     **
     * @deprecated Use the signature-aware
     * {@link #findMemberIndex(WorkingCopy, List, String)}
     */
    @Deprecated
    public static int findMemberIndex(List<? extends Tree> members, String memberName) {
        return findMemberIndex(null, members, memberName);
    }

    /**
     * Finds the index of a member by its name or canonical signature.
     *
     * @param wc The working copy for resolution.
     * @param members The list of class members.
     * @param memberName The name or signature to look for.
     * @return The index, or -1 if not found.
     */
    public static int findMemberIndex(WorkingCopy wc, List<? extends Tree> members, String memberName) {
        for (int i = 0; i < members.size(); i++) {
            Tree m = members.get(i);
            String name = null;
            String signature = null;
            if (m instanceof MethodTree mt) {
                name = mt.getName().toString();
                if (wc != null) {
                    TreePath path = TreePath.getPath(wc.getCompilationUnit(), m);
                    Element e = wc.getTrees().getElement(path);
                    if (e instanceof ExecutableElement ee) {
                        String params = ee.getParameters().stream().map(p -> {
                            String t = p.asType().toString();
                            int bracket = t.indexOf('<');
                            return bracket != -1 ? t.substring(0, bracket) : t;
                        }).collect(Collectors.joining(","));
                        signature = (name.equals("<init>") ? "<init>" : name) + "(" + params + ")";
                    }
                }
            } else if (m instanceof VariableTree vt) {
                name = vt.getName().toString();
            } else if (m instanceof ClassTree ct) {
                name = ct.getSimpleName().toString();
            } else if (m.getKind() == Tree.Kind.BLOCK) {
                name = ((BlockTree) m).isStatic() ? "<clinit>" : "<init-block>";
            }
            if (memberName.equals(name) || memberName.equals(signature)) {
                return i;
            }
            if (memberName.contains("#")) {
                String typePart = memberName.substring(0, memberName.indexOf('#'));
                int targetIndex = Integer.parseInt(memberName.substring(memberName.indexOf('#') + 1, memberName.indexOf('(')));
                int currentCount = 0;
                for (Tree prev : members) {
                    String prevName = null;
                    if (prev.getKind() == Tree.Kind.BLOCK) {
                        prevName = ((BlockTree) prev).isStatic() ? "<clinit>" : "<init-block>";
                    }
                    if (typePart.equals(prevName)) {
                        if (++currentCount == targetIndex && prev == m) {
                            return i;
                        }
                    }
                }
            }
        }
        return -1;
    }

    /**
     * @deprecated Used only by DeprecatedCodeRefiner.
     */
    @Deprecated
    public static ModifiersTree buildModifiers(TreeMaker make, TreeUtilities utils, Set<Modifier> modifiers, List<String> annotations) {
        List<AnnotationTree> annos = new ArrayList<>();
        if (annotations != null && !annotations.isEmpty()) {
            for (String a : annotations) {
                String clean = a.trim().startsWith("@") ? a.trim().substring(1) : a.trim();
                if (clean.contains("(")) {
                    String type = clean.substring(0, clean.indexOf("("));
                    String args = clean.substring(clean.indexOf("(") + 1, clean.lastIndexOf(")"));
                    List<String> attrList = splitAttributes(args);
                    List<ExpressionTree> exprs = new ArrayList<>();
                    for (String attr : attrList) {
                        exprs.add(utils.parseExpression(attr, null));
                    }
                    annos.add(make.Annotation(make.Type(type), exprs));
                } else {
                    annos.add(make.Annotation(make.Type(clean), Collections.emptyList()));
                }
            }
        }
        return make.Modifiers(modifiers, annos);
    }

    /**
     * @deprecated Used only by DeprecatedCodeRefiner.
     */
    @Deprecated
    public static Set<Modifier> getModifiersSet(String modifiersStr) {
        Set<Modifier> mods = EnumSet.noneOf(Modifier.class);
        if (modifiersStr != null && !modifiersStr.isBlank()) {
            for (String m : modifiersStr.split("\\s+")) {
                try {
                    mods.add(Modifier.valueOf(m.toUpperCase()));
                } catch (IllegalArgumentException e) {
                    log.error("Could not parse modifier {}", m);
                }
            }
        }
        return mods;
    }

    private static List<String> splitAttributes(String args) {
        List<String> result = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        int depth = 0;
        for (char c : args.toCharArray()) {
            if (c == '(' || c == '{' || c == '[') {
                depth++;
            } else if (c == ')' || c == '}' || c == ']') {
                depth--;
            }

            if (c == ',' && depth == 0) {
                result.add(current.toString().trim());
                current = new StringBuilder();
            } else {
                current.append(c);
            }
        }
        if (current.length() > 0) {
            result.add(current.toString().trim());
        }
        return result;
    }

    /**
     * @deprecated Used only by DeprecatedCodeRefiner.
     */
    @Deprecated
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
    public static void resolveMembers(FileObject fo, List<String> memberFqns, List<ElementHandle<ExecutableElement>> methods, List<ElementHandle<VariableElement>> fields) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            return;
        }
        js.runUserActionTask(new Task<>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.RESOLVED);
                for (String fqn : memberFqns) {
                    Tree tree = findTree(parameter, fqn);
                    if (tree != null) {
                        Element e = parameter.getTrees().getElement(TreePath.getPath(parameter.getCompilationUnit(), tree));
                        if (e instanceof ExecutableElement ee) {
                            methods.add(ElementHandle.create(ee));
                        } else if (e instanceof VariableElement ve) {
                            fields.add(ElementHandle.create(ve));
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
    public static void resolveMemberInfos(FileObject fo, List<String> memberFqns, List<MemberInfo<ElementHandle<? extends Element>>> members) throws IOException {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            return;
        }

        js.runUserActionTask(new Task<>() {
            @Override
            public void run(CompilationController parameter) throws Exception {
                parameter.toPhase(JavaSource.Phase.RESOLVED);
                for (String fqn : memberFqns) {
                    Tree tree = findTree(parameter, fqn);
                    if (tree != null) {
                        Element e = parameter.getTrees().getElement(TreePath.getPath(parameter.getCompilationUnit(), tree));
                        if (e != null) {
                            members.add((MemberInfo) MemberInfo.create(e, parameter));
                        }
                    }
                }
            }
        }, true);
    }

    /**
     * Generates a list of canonical candidate FQNs for a given partial member
     * name. Use this when a resolution fails to provide helpful feedback.
     *
     * @param info CompilationInfo.
     * @param memberFqn The FQN that failed resolution.
     * @return A list of valid canonical FQNs that share the same base name.
     */
    public static List<String> findMemberCandidates(CompilationInfo info, String memberFqn) {
        int paren = memberFqn.indexOf("(");
        String namePart = paren != -1 ? memberFqn.substring(0, paren) : memberFqn;
        int lastSeparator = Math.max(namePart.lastIndexOf("."), namePart.lastIndexOf("$"));
        if (lastSeparator == -1) {
            return Collections.emptyList();
        }
        String parentFqn = namePart.substring(0, lastSeparator);
        String name = namePart.substring(lastSeparator + 1);
        TypeElement parent = info.getElements().getTypeElement(parentFqn);
        if (parent == null) {
            return Collections.emptyList();
        }
        List<String> candidates = new ArrayList<>();
        for (Element e : parent.getEnclosedElements()) {
            if (e.getSimpleName().contentEquals(name)) {
                if (e instanceof ExecutableElement ee) {
                    String params = ee.getParameters().stream()
                            .map(p -> {
                                String type = p.asType().toString();
                                return type.contains("<") ? type.substring(0, type.indexOf("<")) : type;
                            })
                            .collect(Collectors.joining(","));
                    String namePrefix = (e.getKind() == javax.lang.model.element.ElementKind.CONSTRUCTOR) ? "<init>" : name;
                    candidates.add(parentFqn + "." + namePrefix + "(" + params + ")");
                } else {
                    candidates.add(parentFqn + "." + name);
                }
            }
        }
        return candidates;
    }

    public static String getMemberNotFoundMessage(CompilationInfo info, String memberFqn) {
        List<String> candidates = findMemberCandidates(info, memberFqn);
        if (!candidates.isEmpty()) {
            StringBuilder sb = new StringBuilder("Member not found: ").append(memberFqn);
            sb.append("\nDid you mean one of these canonical identification FQNs?\n");
            for (String c : candidates) {
                sb.append("- ").append(c).append("\n");
            }
            return sb.toString();
        }
        return "Member not found: " + memberFqn;
    }

    /**
     * Authoritatively resolves a member FQN to a TreePathHandle, throwing a
     * detailed exception with candidates if not found.
     *
     * @param fo The file object.
     * @param memberFqn The member FQN.
     * @return The handle.
     * @throws Exception if not found or parsing fails.
     */
    public static TreePathHandle resolveMember(FileObject fo, String memberFqn) throws Exception {
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            throw new AgiToolException("Could not get JavaSource for: " + fo.getPath());
        }
        final TreePathHandle[] result = new TreePathHandle[1];
        final String[] candidatesMsg = new String[1];

        js.runUserActionTask(info -> {
            info.toPhase(JavaSource.Phase.RESOLVED);
            Tree tree = findTree(info, memberFqn);
            if (tree != null) {
                result[0] = TreePathHandle.create(TreePath.getPath(info.getCompilationUnit(), tree), info);
            } else {
                List<String> candidates = findMemberCandidates(info, memberFqn);
                if (!candidates.isEmpty()) {
                    StringBuilder sb = new StringBuilder("Member not found: ").append(memberFqn);
                    sb.append("\nDid you mean one of these canonical identification FQNs?\n");
                    for (String c : candidates) {
                        sb.append("- ").append(c).append("\n");
                    }
                    candidatesMsg[0] = sb.toString();
                }
            }
        }, true);

        if (result[0] != null) {
            return result[0];
        }

        if (candidatesMsg[0] != null) {
            throw new AgiToolException(candidatesMsg[0]);
        }

        throw new AgiToolException("Member not found: " + memberFqn);
    }

    public static void handleSave(FileObject fo) throws IOException {
        DataObject doid = DataObject.find(fo);
        EditorCookie ec = doid.getLookup().lookup(EditorCookie.class);
        if (ec != null && ec.getOpenedPanes() != null) {
            ec.saveDocument();
        }
        SaveCookie sc = doid.getLookup().lookup(SaveCookie.class);
        if (sc != null) {
            sc.save();
        }
        fo.refresh();
    }

}
