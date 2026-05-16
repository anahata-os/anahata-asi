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

@Data
@Slf4j
@NoArgsConstructor
@Schema(description = "Represents a single structural AST modification instruction in a flattened format.")
public class CodeRefinementIntent implements Serializable {

    public enum Type {
        @Schema(description = "Inserts a new member (method, field, or inner type).")
        INSERT,
        @Schema(description = "Updates an existing member's signature or body.")
        UPDATE,
        @Schema(description = "Deletes an existing member.")
        DELETE,
        @Schema(description = "Moves an existing member to a new position within its class.")
        MOVE
    }

    @Schema(description = "The operation type.", required = true)
    private Type type;

    @Schema(description = "The FQN of the target class (e.g. 'com.foo.Bar'). Mandatory for 'INSERT' inside a class. Use '$' for nested types. Leave empty for file-level.")
    private String classFqn;

    @Schema(description = "The ABSOLUTE FQN of the member to operation on (e.g. 'com.foo.Bar.myMethod(java.util.List)'). FQNs are preferred for parameters. Generic brackets '<...>' are not required and will be ignored during matching.")
    private String memberFqn;

    @Schema(description = "The member signature or header (everything to the LEFT of the first '{' or '=')' without javadoc. (e.g. '@Override public void setItems(List<String> items)'). Mandatory for 'INSERT', optional for 'UPDATE' (only if you want to change the declaration). Do not provide Javadocs here. Will cause the tool to fail or corrupt the java source file.")
    private String declaration;

    @Schema(description = "For methods, The WHOLE body code, the logic inside the braces. For fields, the initializer expression (part after '=') or can be blank if there is no initializer expression. FOr use with 'INSERT' and 'UPDATE'. **Do not include the method signature or field declaration in this 'body' field. i.e. ths field cannot start with annotations like @Override or modifiers like 'public void '**")
    private String body;

    @Schema(description = "Position relative to the anchor member. **Mandatory for 'INSERT' and 'MOVE'**.")
    private RelativePosition position;

    @Schema(description = "Anchor member name relative to class (e.g. 'myMethod()'). Mandatory for BEFORE/AFTER positions in 'INSERT' and 'MOVE'.")
    private String anchorMemberName;

    @Schema(description = "The reason for this structural change. Will be displayed in the UI.")
    private String reason;

    @Schema(description = "Optional Javadoc to apply to the member. If updating a member and left null, the existing Javadoc is preserved.")
    private JavadocIntent javadoc;

    public String toDiagnosticString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\n-type             : ").append(type);
        sb.append("\n-classFqn         : ").append(classFqn);
        sb.append("\n-memberFqn        : ").append(memberFqn);
        sb.append("\n-position         : ").append(position);
        sb.append("\n-anchorMemberName : ").append(anchorMemberName);
        sb.append("\n-reason           : ").append(reason);
        sb.append("\n-declaration      : [").append(abbreviate(declaration, 256)).append("]");
        sb.append("\n-body             : [").append(abbreviate(body, 256)).append("]");
        sb.append("\n-javadoc          : ").append(javadoc != null ? "Present" : "null");
        return sb.toString();
    }

    private String abbreviate(String s, int max) {
        if (s == null) return "null";
        if (s.length() <= max) return s;
        int half = max / 2 - 2;
        return s.substring(0, half) + "..." + s.substring(s.length() - half);
    }

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
            
            long docStart = startPos;
            for (org.netbeans.api.java.source.Comment comm : cc.getTreeUtilities().getComments(member, true)) {
                if (comm.isDocComment() && comm.pos() < docStart) {
                    docStart = comm.pos();
                }
            }

            long bodyStart = endPos;
            long bodyEnd = endPos;
            if (member instanceof MethodTree mt && mt.getBody() != null) {
                bodyStart = sp.getStartPosition(cut, mt.getBody());
                bodyEnd = sp.getEndPosition(cut, mt.getBody());
            } else if (member instanceof VariableTree vt) {
                if (vt.getInitializer() != null) {
                    bodyStart = sp.getStartPosition(cut, vt.getInitializer());
                    bodyEnd = sp.getEndPosition(cut, vt.getInitializer());
                    String textToInit = currentContent.substring((int)startPos, (int)bodyStart);
                    int eq = textToInit.lastIndexOf('=');
                    if (eq != -1) {
                        bodyStart = startPos + eq + 1;
                    }
                } else {
                    bodyStart = endPos;
                    if (currentContent.charAt((int)endPos - 1) == ';') {
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
            if (body != null) {
                if (member instanceof VariableTree) {
                    if (body.isBlank()) {
                        newBodyStr = "";
                        int eq = newDeclStr.lastIndexOf('=');
                        if (eq != -1) newDeclStr = newDeclStr.substring(0, eq).trim();
                    } else {
                        newBodyStr = (newDeclStr.trim().endsWith("=") ? " " : " = ") + body;
                    }
                } else if (member instanceof MethodTree || member instanceof ClassTree || member instanceof BlockTree) {
                    String bodyIndent = baseIndent + "    ";
                    String[] lines = body.split("\\r?\\n", -1);
                    boolean hasBraces = body.trim().startsWith("{") && body.trim().endsWith("}");
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
            }

            return currentContent.substring(0, (int)docStart) +
                   newDocStr + newDeclStr + newBodyStr +
                   currentContent.substring((int)bodyEnd);
        }

        if (type == Type.DELETE || type == Type.MOVE) {
            long startPos = sp.getStartPosition(cut, member);
            long endPos = sp.getEndPosition(cut, member);

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

            StringBuilder memberBuilder = new StringBuilder();
            if (javadoc != null) {
                String doc = javadoc.generateString() + "\n";
                memberBuilder.append(doc.replace("\n", "\n" + indent));
            }
            memberBuilder.append(declaration.replace("\n", "\n" + indent));
            
            if (body != null && !body.isEmpty()) {
                boolean hasBraces = body.trim().startsWith("{") && body.trim().endsWith("}");
                String bodyIndent = indent + "    ";
                String[] lines = body.split("\\r?\\n", -1);
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
            } else if (!declaration.trim().endsWith(";") && !declaration.trim().endsWith("}")) {
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
                    boolean isMethod = declaration.contains("(");
                    boolean isClass = declaration.contains("class ") || declaration.contains("interface ") || declaration.contains("enum ") || declaration.contains("record ");
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

    private String getSimpleName(String fqn) {
        if (fqn == null || fqn.isBlank()) {
            return "Unknown";
        }
        int paren = fqn.indexOf('(');
        String namePart = (paren == -1) ? fqn : fqn.substring(0, paren);
        int lastDot = Math.max(namePart.lastIndexOf('.'), namePart.lastIndexOf('$'));
        return (lastDot == -1) ? namePart : namePart.substring(lastDot + 1);
    }

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