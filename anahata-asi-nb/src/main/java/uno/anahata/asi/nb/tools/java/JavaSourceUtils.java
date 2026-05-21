/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.BlockTree;
import com.sun.source.tree.ClassTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.Tree;
import com.sun.source.tree.VariableTree;
import com.sun.source.util.TreePath;
import com.sun.source.util.TreePathScanner;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.ArrayType;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.CompilationInfo;
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
     * The authoritative identification standard for all Anahata Java toolkits.
     */
    public static final String CANONICAL_FQN_STANDARD = 
              "### Anahata Canonical Identification Standard (Global)\n"
            + "All Java toolkits require members to be identified using this exact FQN syntax. **No simple names, no fallbacks**.\n"
            + "\n"
            + "| Entity | Canonical Identification FQN Syntax | Example |\n"
            + "| :--- | :--- | :--- |\n"
            + "| **Top-Level Type** | `package.sub.ClassName` | `java.util.ArrayList` |\n"
            + "| **Nested/Inner Type** | `package.sub.Outer$Inner` | `java.util.Map$Entry` |\n"
            + "| **Field** | `[ClassFQN].fieldName` | `java.lang.System.out` |\n"
            + "| **Method** | `[ClassFQN].methodName(Param1FQN,...)` | `java.lang.String.concat(java.lang.String)` |\n"
            + "| **Constructor** | `[ClassFQN].<init>(Param1FQN,...)` | `java.util.ArrayList.<init>(int)` |\n"
            + "| **Static Block** | `[ClassFQN].<clinit>#n()` | `com.foo.Bar.<clinit>#1()` |\n"
            + "| **Instance Block** | `[ClassFQN].<init-block>#n()` | `com.foo.Bar.<init-block>#1()` |\n"
            + "\n"
            + "**Rules for Identification**:\n"
            + "- **Separators**: Use `.` for packages and members. Use `$` **strictly and only** to separate nested types.\n"
            + "- **Parentheses**: **Mandatory** for all methods, constructors, and blocks, even if no-args (e.g. `toString()`).\n"
            + "- **Parameter FQNs**: Use full FQNs for all parameter types (except primitives).\n"
            + "- **Type Erasure**: Always use the raw type. **Generics (<...>) are strictly forbidden** in identification FQNs.\n"
            + "- **Arrays**: Use the `[]` suffix (e.g. `java.lang.String[]`).\n";

    /**
     * Generates the Anahata Canonical FQN for a given element.
     * @param e The element to resolve.
     * @return The canonical FQN string.
     */
    public static String getCanonicalFqn(Element e) {
        if (e == null) {
            return null;
        }
        if (e instanceof PackageElement pe) {
            return pe.getQualifiedName().toString();
        }
        Element enclosing = e.getEnclosingElement();
        if (enclosing == null || enclosing.getKind() == javax.lang.model.element.ElementKind.PACKAGE) {
            return (e instanceof TypeElement te) ? te.getQualifiedName().toString() : e.getSimpleName().toString();
        }

        String parentFqn = getCanonicalFqn(enclosing);
        if (e.getKind().isClass() || e.getKind().isInterface()) {
            return parentFqn + "$" + e.getSimpleName();
        }

        if (e instanceof ExecutableElement ee) {
            String name = (ee.getKind() == javax.lang.model.element.ElementKind.CONSTRUCTOR) ? "<init>" : ee.getSimpleName().toString();
            String params = ee.getParameters().stream()
                    .map(p -> getCanonicalFqn(p.asType()))
                    .collect(Collectors.joining(","));
            return parentFqn + "." + name + "(" + params + ")";
        }

        return parentFqn + "." + e.getSimpleName();
    }

    /**
     * Generates the Anahata Canonical FQN for a given type mirror.
     * @param m The type mirror to resolve.
     * @return The canonical FQN string.
     */
    public static String getCanonicalFqn(TypeMirror m) {
        if (m == null) {
            return null;
        }
        if (m.getKind().isPrimitive()) {
            return m.toString();
        }
        if (m.getKind() == TypeKind.ARRAY) {
            return getCanonicalFqn(((ArrayType) m).getComponentType()) + "[]";
        }
        if (m instanceof DeclaredType dt) {
            Element e = dt.asElement();
            if (e != null) {
                return getCanonicalFqn(e);
            }
        }
        String s = m.toString();
        return s.contains("<") ? s.substring(0, s.indexOf('<')) : s;
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
     * Normalizes an Anahata FQN (which uses '$' for inner classes) to a
     * standard Java FQN (using '.') for compatibility with standard APIs.
     *
     * @param fqn The FQN to normalize.
     * @return The normalized FQN.
     */
    public static String normalizeFqn(String fqn) {
        if (fqn == null) {
            return null;
        }
        return fqn.replace('$', '.');
    }

    

    /**
     * Locates a syntax tree node inside the compilation context matching the given FQN.
     * @param memberFqn The canonical FQN of the member to find.
     * @param info The compilation context.
     * @return The matching Tree node, or null if not found.
     */
    public static Tree findTree(CompilationInfo info, final String memberFqn) {
        
        final String pureFqn;
        final String indexPart;
        if (memberFqn.contains("#")) {
            int hash = memberFqn.indexOf('#');
            int paren = memberFqn.indexOf('(', hash);
            if (paren != -1) {
                indexPart = memberFqn.substring(hash + 1, paren);
                pureFqn = memberFqn.substring(0, hash);
            } else {
                indexPart = memberFqn.substring(hash + 1);
                pureFqn = memberFqn.substring(0, hash);
            }
        } else if (memberFqn.contains("(")) {
            indexPart = null;
            pureFqn = memberFqn.substring(0, memberFqn.indexOf('('));
        } else {
            indexPart = null;
            pureFqn = memberFqn;
        }
        final Tree[] found = new Tree[1];
        new TreePathScanner<Void, Void>() {
            @Override
            public Void visitClass(ClassTree node, Void p) {
                if (found[0] != null) {
                    return null;
                }
                Element el = info.getTrees().getElement(getCurrentPath());
                if (el instanceof TypeElement te) {
                    String currentFqn = te.getQualifiedName().toString();
                    String normalizedPureFqn = normalizeFqn(pureFqn);
                    // Case 1: Exact Class Match
                    if (currentFqn.equals(normalizedPureFqn) && !memberFqn.contains("(") && indexPart == null) {
                        found[0] = node;
                        return null;
                    }
                    // Case 2: Member Match inside Class
                    if (normalizedPureFqn.startsWith(currentFqn)) {
                        int currentBlockCount = 0;
                        for (Tree member : node.getMembers()) {
                            if (indexPart != null && member.getKind() == Tree.Kind.BLOCK) {
                                String blockName = ((BlockTree) member).isStatic() ? "<clinit>" : "<init-block>";
                                if (blockName.equals(getMemberSimpleName(pureFqn))) {
                                    if (++currentBlockCount == Integer.parseInt(indexPart)) {
                                        found[0] = member;
                                        return null;
                                    }
                                }
                            }
                            if (member instanceof MethodTree mt) {
                                String name = mt.getName().toString();
                                String targetName = getMemberSimpleName(pureFqn);
                                String className = te.getSimpleName().toString();
                                // Support both <init> and ClassName for constructors in the FQN
                                boolean nameMatch = name.equals(targetName) || 
                                                 (name.equals("<init>") && (targetName.equals("<init>") || targetName.equals(className)));
                                if (nameMatch) {
                                    if (memberFqn.contains("(")) {
                                        TreePath memberPath = new TreePath(getCurrentPath(), member);
                                        Element e = info.getTrees().getElement(memberPath);
                                        if (e instanceof ExecutableElement ee && matchSignature(info, ee, memberFqn)) {
                                            found[0] = member;
                                            return null;
                                        }
                                    } else if (!memberFqn.contains("#")) {
                                        found[0] = member;
                                        return null;
                                    }
                                }
                            }
                            if (member instanceof VariableTree vt && vt.getName().contentEquals(getMemberSimpleName(pureFqn))) {
                                if (currentFqn.equals(normalizeFqn(getParentFqn(pureFqn)))) {
                                    found[0] = member;
                                    return null;
                                }
                            }
                        }
                    }
                }
                return super.visitClass(node, p);
            }
        }.scan(new TreePath(info.getCompilationUnit()), null);
        return found[0];
    }
    // Case 1: Exact Class Match
    // Case 2: Member Match inside Class
    // Support both <init> and ClassName for constructors in the FQN
        // Support both <init> and ClassName for constructors
        
    /**
     * Internal helper to match a method element against a string signature.
     * <p>This implementation is erasure-aware. If the provided signature does not
     * contain generics, it will match against the raw AST type.</p>
     * @param methodFqn The signature to compare against.
     * @param ee The executable element to match.
     * @param info The compilation context.
     * @return true if the signature matches, false otherwise.
     */
    private static boolean matchSignature(CompilationInfo info, ExecutableElement ee, String methodFqn) {
        String actualFqn = getCanonicalFqn(ee).replaceAll("\\s+", "");
        String expectedFqn = methodFqn.replaceAll("\\s+", "");
        
        // Normalization: remove generics from both sides to ensure raw signature matching
        if (expectedFqn.contains("<")) {
            expectedFqn = expectedFqn.replaceAll("<[^>]*>", "");
        }
        if (actualFqn.contains("<")) {        
            actualFqn = actualFqn.replaceAll("<[^>]*>", "");
        }
        // Special case: Constructor name normalization (<init> vs ClassName)
        if (ee.getKind() == ElementKind.CONSTRUCTOR) {
            String className = ee.getEnclosingElement().getSimpleName().toString();
            if (expectedFqn.contains("." + className + "(")) {
                expectedFqn = expectedFqn.replace("." + className + "(", ".<init>(");
            }
        }
        return actualFqn.equals(expectedFqn);
    }

    /**
     * Splits a parameter string into individual types, respecting generic brackets.
     * @param params The raw parameters string.
     * @return A list of individual parameter types.
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
        
        // For lookup we must normalize to dots
        TypeElement parent = info.getElements().getTypeElement(normalizeFqn(parentFqn));
        if (parent == null) {
            return Collections.emptyList();
        }
        
        List<String> candidates = new ArrayList<>();
        for (Element e : parent.getEnclosedElements()) {
            if (e.getSimpleName().contentEquals(name) || (name.equals("<init>") && e.getKind() == javax.lang.model.element.ElementKind.CONSTRUCTOR)) {
                candidates.add(getCanonicalFqn(e));
            }
        }
        return candidates;
    }

    
    

    /**
     * Forces the IDE to save and flush any open document buffers for the given file.
     * @param fo The NetBeans file object to save.
     * @throws java.io.IOException If saving fails.
     */
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
