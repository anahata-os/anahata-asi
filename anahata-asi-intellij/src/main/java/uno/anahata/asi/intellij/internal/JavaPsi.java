/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.internal;

import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.JavaPsiFacade;
import uno.anahata.asi.agi.tool.AgiToolException;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiField;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiJavaFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.PsiModifierListOwner;
import com.intellij.psi.PsiParameterList;
import com.intellij.psi.search.GlobalSearchScope;

import java.nio.file.Path;

/**
 * Shared, stateless PSI/VFS resolution utilities for the IntelliJ toolkits.
 * <p>
 * Consolidates the file/project/member resolution logic that would otherwise be
 * duplicated across {@code CodeModel}, {@code CodeRefiner}, {@code Refactor} and other
 * Java toolkits, and pins down the single canonical member-FQN scheme
 * ({@code pkg.Type.name(erasedArg,...)} for methods, {@code pkg.Type.field} for fields).
 * </p>
 * <p>
 * <b>Threading:</b> the VFS lookups ({@link #findVirtualFile}, {@link #findHostProject})
 * are thread-safe, but every method that returns or traverses live PSI
 * ({@link #findPsiFile}, {@link #findClass}, {@link #primaryClass}, {@link #findMember},
 * {@link #findAnnotatable}, {@link #methodFqn}) MUST be invoked inside a read or write
 * action; the returned PSI must not escape that action.
 * </p>
 *
 * @author anahata
 */
public final class JavaPsi {

    /**
     * Maximum time to wait for the IDE to finish indexing before a tool gives up.
     */
    private static final long SMART_MODE_WAIT_MS = 60_000L;

    /**
     * Non-instantiable utility holder.
     */
    private JavaPsi() {
    }

    /**
     * Ensures the project's indexes are available before a PSI/search operation, blocking the
     * calling (background tool) thread for a bounded time if indexing is in progress.
     * <p>
     * PSI stub indexes, short-name caches, references search and refactorings all read the
     * indexes and either throw {@code IndexNotReadyException} or return incomplete results while
     * the IDE is in dumb mode. Tool methods call this first (never on the EDT or inside a read
     * action) so that, rather than failing opaquely, they either proceed on fresh indexes or
     * report a clean, retryable message.
     * </p>
     *
     * @param project the project whose indexes are needed.
     * @throws AgiToolException if the IDE is still indexing after {@link #SMART_MODE_WAIT_MS}.
     */
    public static void requireSmart(Project project) throws AgiToolException {
        DumbService service = DumbService.getInstance(project);
        if (!service.isDumb()) {
            return;
        }
        if (!service.waitForSmartMode(SMART_MODE_WAIT_MS)) {
            throw new AgiToolException("The IDE is still indexing project '" + project.getName()
                    + "'. Retry once indexing has completed.");
        }
    }

    /**
     * Ensures indexes are available for every open project (used by tools that operate across
     * the whole workspace rather than a single project).
     *
     * @throws AgiToolException if any open project is still indexing after the bounded wait.
     */
    public static void requireSmartForOpenProjects() throws AgiToolException {
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            requireSmart(project);
        }
    }

    /**
     * Resolves an absolute path to a {@link VirtualFile}, refreshing the VFS if needed.
     *
     * @param filePath the absolute filesystem path.
     * @return the virtual file, or {@code null} if it does not exist.
     */
    public static VirtualFile findVirtualFile(String filePath) {
        return VfsUtil.findFile(Path.of(filePath), true);
    }

    /**
     * Resolves the open project whose content roots contain the given file, falling
     * back to the first open project so that files outside any project remain usable.
     *
     * @param file the file to host.
     * @return a hosting project, or {@code null} if no projects are open.
     */
    public static Project findHostProject(VirtualFile file) {
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        for (Project project : open) {
            if (ProjectRootManager.getInstance(project).getFileIndex().isInContent(file)) {
                return project;
            }
        }
        return open.length > 0 ? open[0] : null;
    }

    /**
     * Resolves a PSI file for a virtual file within a project. Must run inside a read action.
     *
     * @param project the host project.
     * @param file    the virtual file.
     * @return the PSI file, or {@code null} if none.
     */
    public static PsiFile findPsiFile(Project project, VirtualFile file) {
        return PsiManager.getInstance(project).findFile(file);
    }

    /**
     * Finds a class by fully-qualified name across the whole project scope. Nested-type
     * {@code $} separators are normalized to {@code .}. Must run inside a read action.
     *
     * @param project the host project.
     * @param fqn     the fully-qualified type name.
     * @return the class, or {@code null} if not found.
     */
    public static PsiClass findClass(Project project, String fqn) {
        return JavaPsiFacade.getInstance(project).findClass(fqn.replace('$', '.'), GlobalSearchScope.allScope(project));
    }

    /**
     * Returns the primary (first top-level) class of a Java source file. Must run inside
     * a read action.
     *
     * @param project the host project.
     * @param file    the virtual file (expected to be Java source).
     * @return the primary class, or {@code null} if the file is not Java source or has no class.
     */
    public static PsiClass primaryClass(Project project, VirtualFile file) {
        PsiFile psiFile = findPsiFile(project, file);
        if (psiFile instanceof PsiJavaFile javaFile && javaFile.getClasses().length > 0) {
            return javaFile.getClasses()[0];
        }
        return null;
    }

    /**
     * Resolves a canonical member FQN to its PSI method or field. Must run inside a read
     * action.
     *
     * @param project   the host project.
     * @param memberFqn the canonical FQN of a method ({@code ...name(argType,...)}) or field.
     * @return the resolved method or field, or {@code null} if none matches.
     */
    public static PsiElement findMember(Project project, String memberFqn) {
        boolean isMethod = memberFqn.contains("(");
        String beforeParen = isMethod ? memberFqn.substring(0, memberFqn.indexOf('(')) : memberFqn;
        int lastDot = beforeParen.lastIndexOf('.');
        if (lastDot <= 0) {
            return null;
        }
        PsiClass cls = findClass(project, beforeParen.substring(0, lastDot));
        if (cls == null) {
            return null;
        }
        if (isMethod) {
            for (PsiMethod method : cls.getMethods()) {
                if (memberFqn.equals(methodFqn(cls, method))) {
                    return method;
                }
            }
            return null;
        }
        String simpleName = beforeParen.substring(lastDot + 1);
        for (PsiField field : cls.getFields()) {
            if (simpleName.equals(field.getName())) {
                return field;
            }
        }
        return null;
    }

    /**
     * Resolves a canonical FQN to an annotation target: the whole class when the FQN is
     * itself a class, otherwise the referenced method or field. Must run inside a read action.
     *
     * @param project the host project.
     * @param fqn     the canonical FQN of a class, method, or field.
     * @return the annotatable owner, or {@code null} if none matches.
     */
    public static PsiModifierListOwner findAnnotatable(Project project, String fqn) {
        if (!fqn.contains("(")) {
            PsiClass cls = findClass(project, fqn);
            if (cls != null) {
                return cls;
            }
        }
        PsiElement member = findMember(project, fqn);
        return member instanceof PsiModifierListOwner owner ? owner : null;
    }

    /**
     * Builds the canonical method FQN ({@code pkg.Type.name(erasedArg,...)}; constructors
     * use {@code <init>}). Must run inside a read action.
     *
     * @param cls    the declaring class.
     * @param method the method.
     * @return the canonical method FQN.
     */
    public static String methodFqn(PsiClass cls, PsiMethod method) {
        StringBuilder sb = new StringBuilder(cls.getQualifiedName()).append(".");
        sb.append(method.isConstructor() ? "<init>" : method.getName()).append("(");
        PsiParameterList parameters = method.getParameterList();
        for (int i = 0; i < parameters.getParametersCount(); i++) {
            sb.append(parameters.getParameter(i).getType().getCanonicalText());
            if (i < parameters.getParametersCount() - 1) {
                sb.append(",");
            }
        }
        return sb.append(")").toString();
    }
}
