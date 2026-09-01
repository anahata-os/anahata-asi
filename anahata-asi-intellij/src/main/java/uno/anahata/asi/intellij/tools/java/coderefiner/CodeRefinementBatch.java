/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java.coderefiner;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.intellij.lang.java.JavaLanguage;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.codeStyle.JavaCodeStyleManager;
import com.intellij.psi.util.PsiTreeUtil;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.intellij.internal.JavaPsi;
import uno.anahata.asi.toolkit.resources.text.AbstractTextResourceWrite;
import uno.anahata.asi.toolkit.resources.text.LineComment;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * An atomic batch of member-level {@link CodeRefinementIntent}s applied to a single Java
 * file by {@code BatchCodeRefiner#refine}.
 * <p>
 * Extends {@link AbstractTextResourceWrite} to participate in the interactive diff UI
 * lifecycle, optimistic locking, and manual override capture.
 * </p>
 *
 * @author anahata
 */
@Data
@Slf4j
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Schema(description = "An atomic batch of member-level structural AST modifications for a single Java file.")
public class CodeRefinementBatch extends AbstractTextResourceWrite {

    /**
     * The absolute path of the Java file to modify (optional if resourceUuid is provided).
     */
    @Schema(description = "The absolute path of the Java file to modify.")
    private String filePath;

    /**
     * The ordered list of structural modifications to apply.
     */
    @Schema(description = "The ordered list of member-level modifications to apply atomically.", required = true)
    private List<CodeRefinementIntent> intents = new ArrayList<>();

    /**
     * Whether to optimize imports (shorten FQNs, remove unused) after applying the intents.
     */
    @Schema(description = "Whether to optimize imports after applying the intents.")
    private boolean optimizeImports = true;

    /**
     * Whether to persist the file to disk after applying the intents.
     */
    @Schema(description = "Whether to save the file to disk after applying the intents.")
    private boolean save = true;

    /**
     * Optional calculated line comments for UI commentary rendering.
     */
    @JsonIgnore
    @Schema(hidden = true)
    private List<LineComment> calculatedComments = new ArrayList<>();

    /**
     * Minimal constructor for standard tool invocation.
     *
     * @param uuid         the resource UUID.
     * @param lastModified optimistic locking timestamp.
     */
    public CodeRefinementBatch(String uuid, long lastModified) {
        super(uuid, lastModified);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Replays the refinement intents in-memory on a dummy {@link PsiJavaFile} created via
     * {@link PsiFileFactory}, shortens class references, optionally optimizes imports, and
     * reformats code without mutating workspace files until committed.
     * </p>
     */
    @Override
    protected String doCalculateResultingContent(Agi agi) throws Exception {
        if (originalContent == null) {
            captureOriginalContent(agi);
        }

        Project hostProject = resolveProject(agi);
        String name = originalResourceName != null ? originalResourceName : "Temp.java";
        String baseSource = originalContent.replace("\r\n", "\n");

        return ReadAction.compute(() -> {
            PsiFileFactory fileFactory = PsiFileFactory.getInstance(hostProject);
            PsiFile file = fileFactory.createFileFromText(name, JavaLanguage.INSTANCE, baseSource);
            if (!(file instanceof PsiJavaFile dummyFile)) {
                return baseSource;
            }
            PsiElementFactory elementFactory = JavaPsiFacade.getElementFactory(hostProject);

            for (CodeRefinementIntent intent : intents) {
                applyIntentToPsi(hostProject, elementFactory, dummyFile, intent);
            }

            JavaCodeStyleManager styleManager = JavaCodeStyleManager.getInstance(hostProject);
            styleManager.shortenClassReferences(dummyFile);
            if (optimizeImports) {
                styleManager.optimizeImports(dummyFile);
            }
            CodeStyleManager.getInstance(hostProject).reformat(dummyFile);

            return dummyFile.getText();
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void validate(Agi agi) throws Exception {
        if (resourceUuid == null && filePath != null) {
            for (Resource r : agi.getResourceManager().getResources().values()) {
                String path = null;
                if (r.getHandle() instanceof uno.anahata.asi.agi.resource.handle.PathHandle ph) {
                    path = ph.getPath();
                }
                if (filePath.equals(path) || filePath.equals(r.getHandle().getUri().toString())) {
                    this.resourceUuid = r.getUuid();
                    this.lastModified = r.getHandle().getLastModified();
                    break;
                }
            }
        }
        super.validate(agi);

        if (intents != null) {
            for (CodeRefinementIntent intent : intents) {
                if (intent.getType() == null) {
                    throw new AgiToolException("Intent type cannot be null.");
                }
            }
        }

        if (intents == null || intents.isEmpty()) {
            throw new AgiToolException("Refinement batch is empty. You must provide at least one structural member intent.");
        }

        if (Objects.equals(originalContent, calculateResultingContent(agi))) {
            throw new AgiToolException("Update rejected: The resulting content is identical to the current file content on disk.");
        }
    }

    /**
     * Resolves the hosting IntelliJ project for AST parsing and styling.
     *
     * @param agi the parent agi session.
     * @return the hosting project or default project.
     */
    private Project resolveProject(Agi agi) {
        if (filePath != null) {
            VirtualFile vf = JavaPsi.findVirtualFile(filePath);
            if (vf != null) {
                Project p = JavaPsi.findHostProject(vf);
                if (p != null) {
                    return p;
                }
            }
        }
        if (resourceUuid != null) {
            Resource r = agi.getResourceManager().get(resourceUuid);
            if (r != null && r.getHandle() instanceof uno.anahata.asi.agi.resource.handle.PathHandle ph) {
                VirtualFile vf = JavaPsi.findVirtualFile(ph.getPath());
                if (vf != null) {
                    Project p = JavaPsi.findHostProject(vf);
                    if (p != null) {
                        return p;
                    }
                }
            }
        }
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        return open.length > 0 ? open[0] : ProjectManager.getInstance().getDefaultProject();
    }

    /**
     * Applies a structural intent to the target PSI file in-memory.
     *
     * @param project the project.
     * @param factory the element factory.
     * @param file    the Java PSI file.
     * @param intent  the structural intent.
     * @throws AgiToolException if an intent target cannot be resolved.
     */
    public static void applyIntentToPsi(Project project, PsiElementFactory factory, PsiJavaFile file, CodeRefinementIntent intent) throws AgiToolException {
        switch (intent.getType()) {
            case INSERT -> {
                PsiClass target = findClassInFile(file, intent.getClassFqn());
                if (target == null) {
                    throw new AgiToolException("INSERT target class not found: " + intent.getClassFqn());
                }
                PsiMember member = parseMember(factory, intent.getDeclaration(), target);
                insertMember(target, member, intent.getPosition(), intent.getAnchorMemberName());
            }
            case UPDATE -> {
                PsiElement existing = requireMemberInFile(file, intent.getMemberFqn());
                PsiMember replacement = parseMember(factory, intent.getDeclaration(), existing);
                existing.replace(replacement);
            }
            case DELETE -> requireMemberInFile(file, intent.getMemberFqn()).delete();
            case MOVE -> {
                PsiElement existing = requireMemberInFile(file, intent.getMemberFqn());
                PsiClass parent = PsiTreeUtil.getParentOfType(existing, PsiClass.class);
                if (parent == null) {
                    throw new AgiToolException("MOVE member has no enclosing class: " + intent.getMemberFqn());
                }
                PsiElement copy = existing.copy();
                existing.delete();
                insertMember(parent, (PsiMember) copy, intent.getPosition(), intent.getAnchorMemberName());
            }
            default -> throw new AgiToolException("Unknown intent type: " + intent.getType());
        }
    }

    /**
     * Locates a class by FQN or simple name within the PSI file.
     *
     * @param file     the Java PSI file.
     * @param classFqn the class FQN or simple name.
     * @return the resolved class, or first class if unspecified.
     */
    private static PsiClass findClassInFile(PsiJavaFile file, String classFqn) {
        if (classFqn == null || classFqn.isBlank()) {
            return file.getClasses().length > 0 ? file.getClasses()[0] : null;
        }
        for (PsiClass cls : file.getClasses()) {
            if (classFqn.equals(cls.getQualifiedName()) || classFqn.equals(cls.getName())) {
                return cls;
            }
            for (PsiClass inner : cls.getAllInnerClasses()) {
                if (classFqn.equals(inner.getQualifiedName()) || classFqn.equals(inner.getName())) {
                    return inner;
                }
            }
        }
        return file.getClasses().length > 0 ? file.getClasses()[0] : null;
    }

    /**
     * Locates a member by canonical FQN within the PSI file.
     *
     * @param file      the Java PSI file.
     * @param memberFqn the member FQN.
     * @return the matching member.
     * @throws AgiToolException if member is absent.
     */
    private static PsiElement requireMemberInFile(PsiJavaFile file, String memberFqn) throws AgiToolException {
        boolean isMethod = memberFqn.contains("(");
        String beforeParen = isMethod ? memberFqn.substring(0, memberFqn.indexOf('(')) : memberFqn;
        int lastDot = beforeParen.lastIndexOf('.');
        String simpleName = lastDot > 0 ? beforeParen.substring(lastDot + 1) : beforeParen;

        for (PsiClass cls : file.getClasses()) {
            PsiElement found = searchClassForMember(cls, memberFqn, simpleName, isMethod);
            if (found != null) {
                return found;
            }
        }
        throw new AgiToolException("Member not found in file: " + memberFqn);
    }

    private static PsiElement searchClassForMember(PsiClass cls, String memberFqn, String simpleName, boolean isMethod) {
        if (isMethod) {
            for (PsiMethod m : cls.getMethods()) {
                if (m.getName().equals(simpleName) || memberFqn.equals(JavaPsi.methodFqn(cls, m))) {
                    return m;
                }
            }
        } else {
            for (PsiField f : cls.getFields()) {
                if (f.getName().equals(simpleName)) {
                    return f;
                }
            }
        }
        for (PsiClass inner : cls.getInnerClasses()) {
            PsiElement found = searchClassForMember(inner, memberFqn, simpleName, isMethod);
            if (found != null) {
                return found;
            }
        }
        return null;
    }

    private static PsiMember parseMember(PsiElementFactory factory, String declaration, PsiElement context) throws AgiToolException {
        PsiClass holder = factory.createClassFromText(declaration, context);
        if (holder.getMethods().length > 0) {
            return holder.getMethods()[0];
        }
        if (holder.getFields().length > 0) {
            return holder.getFields()[0];
        }
        if (holder.getInnerClasses().length > 0) {
            return holder.getInnerClasses()[0];
        }
        if (holder.getInitializers().length > 0) {
            return holder.getInitializers()[0];
        }
        throw new AgiToolException("Could not parse a member from declaration: " + declaration);
    }

    private static void insertMember(PsiClass target, PsiMember member, RelativePosition position, String anchorName) throws AgiToolException {
        RelativePosition pos = position != null ? position : RelativePosition.END;
        switch (pos) {
            case END -> target.add(member);
            case START -> {
                PsiElement lBrace = target.getLBrace();
                if (lBrace != null) {
                    target.addAfter(member, lBrace);
                } else {
                    target.add(member);
                }
            }
            case BEFORE -> target.addBefore(member, requireAnchor(target, anchorName));
            case AFTER -> target.addAfter(member, requireAnchor(target, anchorName));
        }
    }

    private static PsiMember requireAnchor(PsiClass target, String anchorName) throws AgiToolException {
        for (PsiMethod method : target.getMethods()) {
            if (method.getName().equals(anchorName)) {
                return method;
            }
        }
        for (PsiField field : target.getFields()) {
            if (field.getName().equals(anchorName)) {
                return field;
            }
        }
        for (PsiClass inner : target.getInnerClasses()) {
            if (anchorName.equals(inner.getName())) {
                return inner;
            }
        }
        throw new AgiToolException("Anchor member not found in " + target.getName() + ": " + anchorName);
    }
}
