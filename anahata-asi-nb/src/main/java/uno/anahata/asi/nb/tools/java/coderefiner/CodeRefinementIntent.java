/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java.coderefiner;

import com.sun.source.tree.*;
import com.sun.source.util.SourcePositions;
import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.util.TreePath;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.nb.tools.java.BatchCodeRefiner;

/**
 * Represents a single structural AST modification instruction in a flattened format.
 * <p>
 * This intent is processed by the {@link BatchCodeRefiner} to perform surgical, AST-guided text replacements on Java source files.
 * </p>
 */
@Data
@Slf4j
@NoArgsConstructor
@Schema(description = "Represents a single structural AST modification instruction in a flattened format.")
public class CodeRefinementIntent implements Serializable {

    /**
     * Defines the types of structural modifications available for a CodeRefinementIntent.
     */
    public enum Type {
        /** Inserts a new member (method, field, or inner type). */
        @Schema(description = "Inserts a brand-new member (method, field, constructor, enum constant, or nested type). "
            + "Requires: 'classFqn' of the container class, 'declaration' for the signature, and 'position' (plus 'anchorMemberName' if using BEFORE or AFTER).")
        INSERT,
        /** Updates an existing member's signature or body. */
        @Schema(description = "Surgically updates an existing member. Requires: 'memberFqn' of the target. "
            + "1) To update signature: provide new 'declaration'. "
            + "2) To update logic/value: provide 'innerBlockOrInitializer'. "
            + "3) To update Javadoc only: provide 'javadoc' (and leave other fields null).")
        UPDATE,
        /** Deletes an existing member. */
        @Schema(description = "Deletes an existing member (method, field, or nested class) from the AST. "
            + "Requires ONLY: 'memberFqn'. Other fields should be left null.")
        DELETE,
        /** Moves an existing member to a new position within its class. */
        @Schema(description = "Relocates an existing member to a new position in the same class. "
            + "Requires: 'memberFqn' of the target, 'position', and 'anchorMemberName' (if BEFORE or AFTER).")
        MOVE
    }

    /**
     * The type of structural modification to perform (INSERT, UPDATE, DELETE, MOVE).
     */
    @Schema(description = "The operation type.", required = true)
    private Type type;

    /**
     * The fully qualified name of the target class container. 
     * Used for resolving the container during structural inserts. 
     * Nested types should use the '$' separator.
     */
    @Schema(description = "The FQN of the target class container (e.g. 'com.foo.Bar'). "
        + "Mandatory for 'INSERT' and 'MOVE' of methods, fields, or constructors. "
        + "Nested classes must use '$' (e.g. 'com.foo.Bar$InnerHelper'). "
        + "Leave empty ONLY if you are inserting a brand-new top-level class/interface at the file level.")
     private String classFqn;

    /**
     * The absolute fully qualified name of the target member to perform the operation on.
     * Supports methods, fields, and inner classes. 
     * Method FQNs must include full parameter types (type erasure applied).
     */
    @Schema(description = "The absolute FQN of the target member to UPDATE, DELETE, or MOVE. "
        + "Examples: Methods: 'com.foo.Bar.myMethod(java.lang.String,int)'. "
        + "Fields: 'com.foo.Bar.myField'. Constructors: 'com.foo.Bar.<init>()'. "
        + "Enum Constants: 'com.foo.Bar$MyEnum.CONSTANT'. "
        + "Type erasure applies: DO NOT include generics/parameters with '<...>', use raw classes.")
    private String memberFqn;

    /**
     * The exact string declaration (signature) of the member.
     * This should include modifiers, return types, and parameter lists 
     * but NOT the body or Javadoc.
     */
    @Schema(description = "The exact member signature or header (everything to the LEFT of the first '{' or '='), including annotations and modifiers, but WITHOUT any Javadocs (e.g., '@Override public void foo()'). "
        + "Mandatory for 'INSERT'. Optional for 'UPDATE' (provide only if changing modifiers or the signature). "
        + "WARNING: Never put Javadocs in this field! Use the structured 'javadoc' property instead.")
    private String declaration;

    /**
     * The code body or initializer expression.
     * For methods, this is the code inside the braces. 
     * For fields, it is the expression following the assignment operator.
     */
    @Schema(description = "Provides the inner body code or initializer value depending on the member type:\n"
        + "1) For Methods & Constructors: The pure block statements inside the braces (excluding the method signature).\n"
        + "2) For Fields: The initializer expression following the '=' operator (e.g. '\"123\"' or 'false').\n"
        + "3) For Inner/Nested Types (Classes, Records, Interfaces): The full list of member definitions (methods, fields, nested classes) inside the type's braces.\n"
        + "4) For Enum Constants: The constructor arguments (e.g., '\"arg\"' or '1, \"arg\"') or anonymous body.\n"
        + "**WARNING: Do not duplicate the signature or declaration in this field! E.g., no 'public void' or '@Override' should start this field.**")
    private String innerBlockOrInitializer;

    /**
     * The relative position constraint for insertion or moving operations.
     * Requires an anchor member if using BEFORE or AFTER.
     */
    @Schema(description = "The placement constraint. Mandatory for 'INSERT' and 'MOVE'. "
        + "If BEFORE or AFTER, you must specify 'anchorMemberName'. If START or END, 'anchorMemberName' is ignored.")
    private RelativePosition position;

    /**
     * The simple name or signature of the anchor member to position against.
     * Used in conjunction with {@link RelativePosition}.
     */
    @Schema(description = "The simple name or signature of the member to position against (e.g., 'myField' or 'foo(java.lang.String)'). "
        + "Mandatory if 'position' is BEFORE or AFTER.")
    private String anchorMemberName;

    /**
     * A human-readable reason explaining the intent. 
     * Visible in the UI during the refinement review phase.
     */
    @Schema(description = "A concise, developer-friendly explanation of why this modification is being made. "
        + "Will be rendered as an AI comment above the diff in the NetBeans review panel.")
    private String reason;

    /**
     * The structured Javadoc configuration to apply to the member.
     * Allows for synchronous documentation and code updates.
     */
    @Schema(description = "Optional Javadoc to apply to the member. If updating a member and left null, the existing Javadoc is preserved. WARNING: When used in an UPDATE intent, providing this object will completely replace the existing Javadoc. You MUST provide all @param, @return, and @throws fields if they should be preserved.")
    private JavadocIntent javadoc;

    public void validate() throws AgiToolException {
        if (type == Type.INSERT || type == Type.MOVE) {
                    String decl = declaration != null ? declaration.trim() : "";
                    boolean isTopLevelType = decl.contains("class ") || decl.contains("interface ") || decl.contains("enum ") || decl.contains("record ");
                    
                    if (!isTopLevelType && (classFqn == null || classFqn.trim().isEmpty())) {
                        throw new AgiToolException("Validation Failed: 'classFqn' is mandatory when inserting/moving methods, fields, or constructors. "
                                + "Leaving 'classFqn' empty is only permitted for top-level types (e.g. inserting 'class Helper' at file-level).");
                    }
                }
    }
    /**
     * Generates a formatted diagnostic string detailing the intent's configuration.
     * @return a multi-line diagnostic string.
     */
    public String toDiagnosticString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\n-type                     : ").append(type);
        sb.append("\n-classFqn                 : ").append(classFqn);
        sb.append("\n-memberFqn                : ").append(memberFqn);
        sb.append("\n-position                 : ").append(position);
        sb.append("\n-anchorMemberName         : ").append(anchorMemberName);
        sb.append("\n-reason                   : ").append(reason);
        sb.append("\n-declaration              : [").append(abbreviate(declaration, 256)).append("]");
        sb.append("\n-innerBlockOrInitializer  : [").append(abbreviate(innerBlockOrInitializer, 256)).append("]");
        sb.append("\n-javadoc                  : ").append(javadoc != null ? "Present" : "null");
        return sb.toString();
    }

    /**
     * Abbreviates a string for compact logging.
     * @param s the string to abbreviate.
     * @param max the maximum allowed length.
     * @return the abbreviated string.
     */
    private String abbreviate(String s, int max) {
        if (s == null) return "null";
        if (s.length() <= max) return s;
        int half = max / 2 - 2;
        return s.substring(0, half) + "..." + s.substring(s.length() - half);
    }

    /**
     * Executes the V4 AST-Guided text replacement, calculating exact bounds via the NetBeans compiler API.
     * @param cc the compilation controller.
     * @param currentContent the raw text content of the file.
     * @return the updated text content.
     * @throws java.lang.Exception if parsing or string replacement fails.
     */
    public String applyToText(org.netbeans.api.java.source.CompilationController cc, String currentContent) throws Exception {
        CompilationUnitTree cut = cc.getCompilationUnit();
        SourcePositions sp = cc.getTrees().getSourcePositions();

        Tree member = null;
        if (type != Type.INSERT) {
            member = BatchCodeRefiner.findMemberInWorkingCopy(cc, memberFqn);
            if (member == null) {
                BatchCodeRefiner.throwMemberNotFound(cc, memberFqn);
            }
        }

        if (type == Type.UPDATE) {
            long startPos = sp.getStartPosition(cut, member);
            long endPos = sp.getEndPosition(cut, member);
            
            if (endPos < 0) {
                int tempEnd = (int)startPos;
                while (tempEnd < currentContent.length()) {
                    char c = currentContent.charAt(tempEnd);
                    if (c == ',' || c == ';' || c == '{' || c == '=' || c == '(' || c == ')') {
                        break;
                    }
                    tempEnd++;
                }
                endPos = tempEnd;
            }

            long docStart = startPos;
            for (org.netbeans.api.java.source.Comment comm : cc.getTreeUtilities().getComments(member, true)) {
                if (comm.isDocComment() && comm.pos() < docStart) {
                    docStart = comm.pos();
                }
            }

            long bodyStart = endPos;
            long bodyEnd = endPos;
            long initStart = -1;
            long initEnd = -1;
            if (member instanceof MethodTree mt && mt.getBody() != null) {
                bodyStart = sp.getStartPosition(cut, mt.getBody());
                bodyEnd = sp.getEndPosition(cut, mt.getBody());
            } else if (member instanceof VariableTree vt) {
                initStart = vt.getInitializer() != null ? sp.getStartPosition(cut, vt.getInitializer()) : -1;
                initEnd = vt.getInitializer() != null ? sp.getEndPosition(cut, vt.getInitializer()) : -1;
                if (initStart >= 0 && initEnd >= 0) {
                    bodyStart = initStart;
                    bodyEnd = initEnd;
                    String textToInit = currentContent.substring((int)startPos, (int)bodyStart);
                    int eq = textToInit.lastIndexOf('=');
                    if (eq != -1) {
                        bodyStart = startPos + eq + 1;
                    } else {
                        int paren = textToInit.indexOf('(');
                        if (paren != -1) {
                            bodyStart = startPos + paren;
                        }
                    }
                } else {
                    bodyStart = endPos;
                    if (endPos > 0 && currentContent.charAt((int)endPos - 1) == ';') {
                        bodyStart = endPos - 1;
                        bodyEnd = endPos - 1;
                    }
                }
            } else if (member instanceof BlockTree bt) {
                bodyStart = sp.getStartPosition(cut, bt);
                bodyEnd = sp.getEndPosition(cut, bt);
            } else if (member instanceof MethodTree mt && mt.getBody() == null) {
                bodyStart = endPos;
                if (currentContent.charAt((int)endPos - 1) == ';') {
                    bodyStart = endPos - 1;
                    bodyEnd = endPos - 1;
                }
            } else if (member instanceof ClassTree ct) {
                long cStart = sp.getStartPosition(cut, ct);
                int brace = currentContent.indexOf('{', (int)cStart);
                if (brace != -1) {
                    bodyStart = brace;
                    bodyEnd = endPos;
                }
            }

            long declEnd = bodyStart;

            String oldDoc = currentContent.substring((int)docStart, (int)startPos);
            String oldDecl = currentContent.substring((int)startPos, (int)declEnd);
            String oldBody = currentContent.substring((int)bodyStart, (int)bodyEnd);

            int lsDecl = currentContent.lastIndexOf('\n', (int)startPos);
            int firstNonWs = lsDecl + 1;
            while (firstNonWs < currentContent.length() && (currentContent.charAt(firstNonWs) == ' ' || currentContent.charAt(firstNonWs) == '\t')) {
                firstNonWs++;
            }
            String baseIndent = currentContent.substring(lsDecl + 1, firstNonWs);

            String newDocStr = oldDoc;
            if (javadoc != null) {
                newDocStr = javadoc.generateString() + "\n";
                newDocStr = newDocStr.replace("\n", "\n" + baseIndent);
                if (!newDocStr.endsWith(baseIndent)) newDocStr += baseIndent;
            }

            String newDeclStr = oldDecl;
            if (declaration != null) {
                newDeclStr = declaration.replace("\n", "\n" + baseIndent);
                if (member instanceof MethodTree || member instanceof ClassTree) {
                    newDeclStr += " ";
                }
            }

            String newBodyStr = oldBody;
            if (innerBlockOrInitializer != null) {
                if (member instanceof VariableTree) {
                    if (innerBlockOrInitializer.isBlank()) {
                        newBodyStr = "";
                        int eq = newDeclStr.lastIndexOf('=');
                        if (eq != -1) newDeclStr = newDeclStr.substring(0, eq).trim();
                    } else {
                        newBodyStr = (newDeclStr.trim().endsWith("=") ? " " : " = ") + innerBlockOrInitializer;
                    }
                } else if (member instanceof MethodTree || member instanceof ClassTree || member instanceof BlockTree) {
                    String bodyIndent = baseIndent + "    ";
                    String[] lines = innerBlockOrInitializer.split("\\r?\\n", -1);
                    boolean hasBraces = innerBlockOrInitializer.trim().startsWith("{") && innerBlockOrInitializer.trim().endsWith("}");
                    StringBuilder sb = new StringBuilder();
                    if (!hasBraces) {
                        sb.append("{\n");
                    }
                    for (int i = 0; i < lines.length; i++) {
                        String line = lines[i];
                        if (hasBraces && i == 0) {
                            sb.append(line.trim()).append("\n");
                        } else if (hasBraces && i == lines.length - 1) {
                            sb.append(baseIndent).append(line.trim());
                        } else {
                            sb.append(line.isEmpty() ? "" : bodyIndent + line).append("\n");
                        }
                    }
                    if (!hasBraces) {
                        sb.append(baseIndent).append("}");
                    } else if (sb.length() > 0 && sb.charAt(sb.length() - 1) == '\n') {
                        sb.setLength(sb.length() - 1);
                    }
                    newBodyStr = sb.toString();
                }
            } else {
                if (member instanceof VariableTree && initStart >= 0 && initEnd >= 0) {
                    if (declaration != null && !newDeclStr.trim().endsWith("=")) {
                        newBodyStr = " = " + oldBody.trim();
                    }
                }
            }

            return currentContent.substring(0, (int)docStart) +
                   newDocStr + newDeclStr + newBodyStr +
                   currentContent.substring((int)bodyEnd);
        }

        if (type == Type.DELETE || type == Type.MOVE) {
            long startPos = sp.getStartPosition(cut, member);
            long endPos = sp.getEndPosition(cut, member);

            if (endPos < 0) {
                int tempEnd = (int)startPos;
                while (tempEnd < currentContent.length()) {
                    char c = currentContent.charAt(tempEnd);
                    if (c == ',' || c == ';' || c == '{' || c == '=' || c == '(' || c == ')') {
                        break;
                    }
                    tempEnd++;
                }
                endPos = tempEnd;
            }

            long docStart = startPos;
            for (org.netbeans.api.java.source.Comment comm : cc.getTreeUtilities().getComments(member, true)) {
                if (comm.pos() > 0 && comm.pos() < docStart) docStart = comm.pos();
            }
            
            int end = (int)endPos;
            while (end < currentContent.length() && (currentContent.charAt(end) == ' ' || currentContent.charAt(end) == '\t' || currentContent.charAt(end) == ';')) end++;
            
            int start = (int)docStart;
            int lsDel = currentContent.lastIndexOf('\n', start);
            if (lsDel != -1 && currentContent.substring(lsDel + 1, start).trim().isEmpty()) {
                start = lsDel + 1;
            }
            if (end < currentContent.length() && currentContent.charAt(end) == '\n') end++;

            String memberText = currentContent.substring((int)docStart, (int)endPos);
            String formattedMemberText = currentContent.substring(start, end);
            
            if (type == Type.DELETE) {
                return currentContent.substring(0, start) + currentContent.substring(end);
            }

            if (javadoc != null) {
                String oldDoc = currentContent.substring((int)docStart, (int)startPos);
                
                int lsDecl = currentContent.lastIndexOf('\n', (int)startPos);
                int firstNonWs = lsDecl + 1;
                while (firstNonWs < currentContent.length() && (currentContent.charAt(firstNonWs) == ' ' || currentContent.charAt(firstNonWs) == '\t')) {
                    firstNonWs++;
                }
                String baseIndent = currentContent.substring(lsDecl + 1, firstNonWs);

                String newDocStr = javadoc.generateString() + "\n";
                newDocStr = newDocStr.replace("\n", "\n" + baseIndent);
                if (!newDocStr.endsWith(baseIndent)) newDocStr += baseIndent;
                
                formattedMemberText = formattedMemberText.replace(oldDoc, newDocStr);
            }

            Tree container = (classFqn != null && !classFqn.isBlank()) ? BatchCodeRefiner.findMemberInWorkingCopy(cc, classFqn) : cut;
            if (container == null) throw new AgiToolException("Target container not found: " + classFqn);

            int insertOffset = -1;
            if (container instanceof ClassTree ct) {
                List<Tree> members = new ArrayList<>(ct.getMembers());
                members.remove(member);
                int index = BatchCodeRefiner.getInsertIndex(cc, members, position, anchorMemberName);
                if (members.isEmpty()) {
                    insertOffset = currentContent.indexOf('{', (int)sp.getStartPosition(cut, ct)) + 1;
                } else if (index == members.size()) {
                    insertOffset = (int)sp.getEndPosition(cut, ct) - 1;
                } else {
                    Tree anchor = members.get(index);
                    long aStart = sp.getStartPosition(cut, anchor);
                    for (org.netbeans.api.java.source.Comment comm : cc.getTreeUtilities().getComments(anchor, true)) {
                        if (comm.pos() > 0 && comm.pos() < aStart) aStart = comm.pos();
                    }
                    int ls = currentContent.lastIndexOf('\n', (int)aStart);
                    if (ls != -1 && currentContent.substring(ls + 1, (int)aStart).trim().isEmpty()) {
                        insertOffset = ls + 1;
                    } else {
                        insertOffset = (int)aStart;
                    }
                }
            } else if (container instanceof CompilationUnitTree cutContainer) {
                List<Tree> decls = new ArrayList<>(cutContainer.getTypeDecls());
                decls.remove(member);
                int index = BatchCodeRefiner.getInsertIndex(cc, decls, position, anchorMemberName);
                if (decls.isEmpty() || index == decls.size()) {
                    insertOffset = currentContent.length();
                } else {
                    Tree anchor = decls.get(index);
                    long aStart = sp.getStartPosition(cut, anchor);
                    for (org.netbeans.api.java.source.Comment comm : cc.getTreeUtilities().getComments(anchor, true)) {
                        if (comm.pos() > 0 && comm.pos() < aStart) aStart = comm.pos();
                    }
                    int ls = currentContent.lastIndexOf('\n', (int)aStart);
                    if (ls != -1 && currentContent.substring(ls + 1, (int)aStart).trim().isEmpty()) {
                        insertOffset = ls + 1;
                    } else {
                        insertOffset = (int)aStart;
                    }
                }
            }

            if (insertOffset > start) {
                insertOffset -= (end - start);
            }
            
            String textWithoutMember = currentContent.substring(0, start) + currentContent.substring(end);
            return textWithoutMember.substring(0, insertOffset) + formattedMemberText + textWithoutMember.substring(insertOffset);
        }

        if (type == Type.INSERT) {
            Tree container = (classFqn != null && !classFqn.isBlank()) ? BatchCodeRefiner.findMemberInWorkingCopy(cc, classFqn) : cut;
            if (container == null) throw new AgiToolException("Target container not found: " + classFqn);

            int insertOffset = -1;
            String indent = "";
            int index = -1;

            if (container instanceof ClassTree ct) {
                TreePath ctPath = TreePath.getPath(cut, ct);
                List<Tree> members = new ArrayList<>();
                for (Tree m : ct.getMembers()) {
                    if (!cc.getTreeUtilities().isSynthetic(new TreePath(ctPath, m))) members.add(m);
                }
                index = BatchCodeRefiner.getInsertIndex(cc, members, position, anchorMemberName);
                
                if (members.isEmpty()) {
                    long cStart = sp.getStartPosition(cut, ct);
                    int brace = currentContent.indexOf('{', (int)cStart);
                    int nextNl = currentContent.indexOf('\n', brace);
                    insertOffset = (nextNl != -1 && nextNl < sp.getEndPosition(cut, ct)) ? nextNl + 1 : brace + 1;
                    int ls = currentContent.lastIndexOf('\n', (int)cStart);
                    int firstNonWs = ls + 1;
                    while (firstNonWs < currentContent.length() && (currentContent.charAt(firstNonWs) == ' ' || currentContent.charAt(firstNonWs) == '\t')) firstNonWs++;
                    indent = (ls != -1 ? currentContent.substring(ls + 1, firstNonWs) : "") + "    ";
                } else if (index == members.size()) {
                    long cEnd = sp.getEndPosition(cut, ct);
                    int brace = currentContent.lastIndexOf('}', (int)cEnd);
                    int ls = currentContent.lastIndexOf('\n', brace);
                    insertOffset = (ls != -1 && currentContent.substring(ls + 1, brace).trim().isEmpty()) ? ls + 1 : brace;
                    Tree lastMem = members.get(members.size() - 1);
                    long lastStart = sp.getStartPosition(cut, lastMem);
                    int lastLs = currentContent.lastIndexOf('\n', (int)lastStart);
                    int firstNonWs = lastLs + 1;
                    while (firstNonWs < currentContent.length() && (currentContent.charAt(firstNonWs) == ' ' || currentContent.charAt(firstNonWs) == '\t')) firstNonWs++;
                    indent = lastLs != -1 ? currentContent.substring(lastLs + 1, firstNonWs) : "    ";
                } else {
                    Tree anchor = members.get(index);
                    long aStart = sp.getStartPosition(cut, anchor);
                    for (org.netbeans.api.java.source.Comment comm : cc.getTreeUtilities().getComments(anchor, true)) {
                        if (comm.pos() > 0 && comm.pos() < aStart) aStart = comm.pos();
                    }
                    int ls = currentContent.lastIndexOf('\n', (int)aStart);
                    if (ls != -1 && currentContent.substring(ls + 1, (int)aStart).trim().isEmpty()) {
                        insertOffset = ls + 1;
                        indent = currentContent.substring(ls + 1, (int)aStart);
                    } else {
                        insertOffset = (int)aStart;
                        indent = "    ";
                    }
                }
            } else if (container instanceof CompilationUnitTree cutContainer) {
                List<? extends Tree> decls = cutContainer.getTypeDecls();
                index = BatchCodeRefiner.getInsertIndex(cc, decls, position, anchorMemberName);
                if (decls.isEmpty() || index == decls.size()) {
                    insertOffset = currentContent.length();
                } else {
                    Tree anchor = decls.get(index);
                    long aStart = sp.getStartPosition(cut, anchor);
                    for (org.netbeans.api.java.source.Comment comm : cc.getTreeUtilities().getComments(anchor, true)) {
                        if (comm.pos() > 0 && comm.pos() < aStart) aStart = comm.pos();
                    }
                    insertOffset = (int)aStart;
                }
                indent = "";
            }

            String declStr = declaration != null ? declaration.trim() : "";
            boolean isMethod = declStr.contains("(");
            boolean isClass = declStr.contains("class ") || declStr.contains("interface ") || declStr.contains("enum ") || declStr.contains("record ");
            boolean isBlock = declStr.equals("static") || declStr.isEmpty();
            boolean isField = !isMethod && !isClass && !isBlock;

            if (isField && innerBlockOrInitializer != null && !innerBlockOrInitializer.isEmpty() && declStr.endsWith(";")) {
                declStr = declStr.substring(0, declStr.length() - 1).trim();
            }

            StringBuilder memberBuilder = new StringBuilder();
            if (javadoc != null) {
                String doc = javadoc.generateString() + "\n";
                memberBuilder.append(doc.replace("\n", "\n" + indent));
            }
            memberBuilder.append(declStr.replace("\n", "\n" + indent));
            
            if (innerBlockOrInitializer != null && !innerBlockOrInitializer.isEmpty()) {
                if (isField) {
                    memberBuilder.append(" = ").append(innerBlockOrInitializer).append(";");
                } else {
                    boolean hasBraces = innerBlockOrInitializer.trim().startsWith("{") && innerBlockOrInitializer.trim().endsWith("}");
                    String bodyIndent = indent + "    ";
                    String[] lines = innerBlockOrInitializer.split("\\r?\\n", -1);
                    if (!hasBraces) {
                        memberBuilder.append(" {\n");
                    } else {
                        memberBuilder.append(" ");
                    }
                    for (int i = 0; i < lines.length; i++) {
                        String line = lines[i];
                        if (hasBraces && i == 0) {
                            memberBuilder.append(line.trim()).append("\n");
                        } else if (hasBraces && i == lines.length - 1) {
                            memberBuilder.append(indent).append(line.trim());
                        } else {
                            memberBuilder.append(line.isEmpty() ? "" : bodyIndent + line).append("\n");
                        }
                    }
                    if (!hasBraces) {
                        memberBuilder.append(indent).append("}");
                    } else if (memberBuilder.length() > 0 && memberBuilder.charAt(memberBuilder.length() - 1) == '\n') {
                        memberBuilder.setLength(memberBuilder.length() - 1);
                    }
                }
            } else if (!declStr.endsWith(";") && !declStr.endsWith("}")) {
                memberBuilder.append(";");
            }
            
            int blankLinesBefore = 1;
            int blankLinesAfter = 1;
            try {
                org.netbeans.api.java.source.CodeStyle cs = null;
                javax.swing.text.Document doc = cc.getDocument();
                if (doc != null) {
                    cs = org.netbeans.api.java.source.CodeStyle.getDefault(doc);
                } else if (cc.getFileObject() != null) {
                    cs = org.netbeans.api.java.source.CodeStyle.getDefault(cc.getFileObject());
                }
                if (cs != null) {
                    if (isMethod) {
                        blankLinesBefore = cs.getBlankLinesBeforeMethods();
                        blankLinesAfter = cs.getBlankLinesAfterMethods();
                    } else if (isClass) {
                        blankLinesBefore = cs.getBlankLinesBeforeClass();
                        blankLinesAfter = cs.getBlankLinesAfterClass();
                    } else {
                        blankLinesBefore = cs.getBlankLinesBeforeFields();
                        blankLinesAfter = cs.getBlankLinesAfterFields();
                    }
                    if (container instanceof ClassTree ct && index == ct.getMembers().size()) {
                        blankLinesAfter = cs.getBlankLinesBeforeClassClosingBrace();
                    }
                }
            } catch (Exception e) {
                // Ignore
            }

            int existingNewlinesBefore = 0;
            for (int i = insertOffset - 1; i >= 0; i--) {
                char c = currentContent.charAt(i);
                if (c == '\n') existingNewlinesBefore++;
                else if (c != ' ' && c != '\t' && c != '\r') break;
            }
            
            int neededBefore = Math.max(0, blankLinesBefore + 1 - existingNewlinesBefore);
            StringBuilder prefixBuilder = new StringBuilder();
            for (int i = 0; i < neededBefore; i++) {
                prefixBuilder.append("\n");
            }
            String prefix = prefixBuilder.toString();
            
            int existingNewlinesAfter = 0;
            for (int i = insertOffset; i < currentContent.length(); i++) {
                char c = currentContent.charAt(i);
                if (c == '\n') existingNewlinesAfter++;
                else if (c != ' ' && c != '\t' && c != '\r') break;
            }
            
            int neededAfter = Math.max(0, blankLinesAfter + 1 - existingNewlinesAfter);
            StringBuilder suffixBuilder = new StringBuilder();
            for (int i = 0; i < neededAfter; i++) {
                suffixBuilder.append("\n");
            }
            String suffix = suffixBuilder.toString();
            
            return currentContent.substring(0, insertOffset) +
                   prefix + indent + memberBuilder.toString() + suffix +
                   currentContent.substring(insertOffset);
        }

        return currentContent;
    }



    /**
     * Generates a rich HTML representation of this intent for UI rendering.
     * @return an HTML-formatted string.
     */
    public String getHtmlDisplay() {
        String color = switch (type) {
            case INSERT -> "#4CAF50";
            case UPDATE -> "#2196F3";
            case DELETE -> "#F44336";
            case MOVE -> "#FF9800";
        };
        String icon = switch (type) {
            case INSERT -> "[+]";
            case UPDATE -> "[*]";
            case DELETE -> "[-]";
            case MOVE -> "[M]";
        };

        String targetName = (memberFqn != null) ? getSimpleName(memberFqn) : "New Member";
        if (type == Type.INSERT && declaration != null) {
            targetName = getSimpleNameFromDeclaration(declaration);
        }

        StringBuilder sb = new StringBuilder("<font color='").append(color).append("'>").append(icon).append("</font> ");
        sb.append("<b>").append(type.toString().toUpperCase()).append("</b> <code>").append(targetName).append("</code>");

        if (position != null) {
            sb.append(" ").append(position);
            if (anchorMemberName != null) {
                sb.append(" ").append(getSimpleName(anchorMemberName));
            }
        }

        if (reason != null && !reason.isBlank()) {
            sb.append(" <i style='color: #888888;'>(").append(reason).append(")</i>");
        }

        return sb.toString();
    }

    /**
     * Calculates the expected fully qualified name of the member after this intent is applied.
     * @return the resulting FQN, or null if deleted.
     */
    public String getResultingMemberFqn() {
        if (type == Type.DELETE) {
            return null;
        }
        if (type == Type.INSERT) {
            String name = getSimpleNameFromDeclaration(declaration);
            if (classFqn == null || classFqn.isBlank()) {
                return name;
            }
            return classFqn + "." + name;
        }
        return memberFqn;
    }

    /**
     * Extracts the simple name from a fully qualified name.
     * @param fqn the fully qualified name.
     * @return the simple name.
     */
    private String getSimpleName(String fqn) {
        if (fqn == null || fqn.isBlank()) {
            return "Unknown";
        }
        int paren = fqn.indexOf('(');
        String namePart = (paren == -1) ? fqn : fqn.substring(0, paren);
        int lastDot = Math.max(namePart.lastIndexOf('.'), namePart.lastIndexOf('$'));
        return (lastDot == -1) ? namePart : namePart.substring(lastDot + 1);
    }

    /**
     * Parses a member declaration string to extract its simple name.
     * @param decl the declaration string.
     * @return the simple name.
     */
    private String getSimpleNameFromDeclaration(String decl) {
        if (decl == null) return "Unknown";
        String clean = decl.trim();
        while (clean.startsWith("@")) {
            int space = clean.indexOf(' ');
            if (space == -1) {
                break;
            }
            clean = clean.substring(space).trim();
        }
        int paren = clean.indexOf('(');
        int end = (paren != -1) ? paren : (clean.endsWith(";") ? clean.length() - 1 : clean.length());
        int start = clean.lastIndexOf(' ', end - 1);
        if (start == -1) start = 0;
        String name = clean.substring(start + 1, end).trim();
        return (paren != -1) ? name + "()" : name;
    }
}