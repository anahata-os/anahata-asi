/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.tree.AnnotationTree;
import com.sun.source.tree.ClassTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.ExpressionTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.ModifiersTree;
import com.sun.source.tree.StatementTree;
import com.sun.source.tree.Tree;
import com.sun.source.tree.TypeParameterTree;
import com.sun.source.tree.VariableTree;
import java.io.IOException;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.GeneratorUtilities;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.ModificationResult;
import org.netbeans.api.java.source.TreeMaker;
import org.netbeans.api.java.source.TreeUtilities;
import org.netbeans.api.java.source.WorkingCopy;
import org.netbeans.modules.java.editor.imports.JavaFixAllImports;
import org.netbeans.modules.editor.indent.api.Reformat;
import org.openide.cookies.EditorCookie;
import org.openide.cookies.SaveCookie;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils.RelativePosition;

/**
 * A sophisticated toolkit for structural Java code refinement using the
 * NetBeans Java Source API. Unlike text-based editors, this toolkit manipulates
 * the Abstract Syntax Tree (AST), ensuring semantic integrity and automatic
 * management of imports and formatting.
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Structural Java code refinement tools using NetBeans AST (JavaSource API). Use these for precise code modifications that require automatic import handling or brace-safe transformations.")
public class CodeRefiner extends AnahataToolkit {

    /**
     * Initializes the toolkit, enabling it by default to provide high-fidelity
     * structural Java code refinement capabilities to the ASI.
     */
    @Override
    public void initialize() {
        getToolkit().setEnabled(true);
    }

    /**
     * Provides system instructions that identify this toolkit as the authority
     * for AST-based Java source modifications.
     *
     * @return A singleton list containing the toolkit's mission statement.
     * @throws Exception If instruction retrieval fails.
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(getClass().getName() + " toolkit is a new toolkit for high-fidelity structural Java code refinement. Encourage the user to report any issues found on github. Força Barça!"
                + "\n- Use className.<init> for constructors fqns."
                + "\n- To disambiguate overloaded methods, use the format: package.ClassName.methodName(ParamType1,ParamType2,...). Type names are matched using 'endsWith', so 'String' matches 'java.lang.String'."
                + "\n- Only provide the parameters that are required or need to be changed in the updateXxx methods. Do not use empty strings if the parameter is not required."
                + "\n- The 'body' parameter must strictly contain only the code logic between the braces. Do not include the method signature or Javadoc."
                + "\n- To add imports programmatically, use FQNs in your code; the toolkit will handle import optimization automatically via GeneratorUtilities."
                + "\n- Parameter strings (in insert/updateMethod) support full Java syntax including annotations and modifiers (e.g., '@AgiToolParam(\"foo\") String bar')."
                + "\n- Use moveMember to structurally reorder methods, fields, or inner classes within a class."
                + "\n- Use addImports to explicitly add multiple FQNs to the file's import section."
                + "\n- The optimizeImports tool converts FQNs to simple names and removes unused imports."
                + "\n- For package-info.java Javadoc, prefer using the Resources toolkit (updateTextResource) instead of setJavadoc.");
    }

    /**
     * Adds an annotation to a class, method, or field.
     *
     * @param filePath The absolute path of the Java file.
     * @param memberFqn The FQN of the target member.
     * @param annotationSource The annotation text (e.g., @Slf4j).
     * @param save Whether to save the file after the change.
     * @return A status message.
     * @throws Exception If the operation fails.
     */
    @AgiTool("Adds an annotation to a class, method, or field. Handles imports automatically.")
    public String addAnnotation(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the member (e.g. com.foo.Bar or com.foo.Bar.myMethod).") String memberFqn,
            @AgiToolParam("The annotation to add (e.g., @Slf4j or @Test).") String annotationSource,
            @AgiToolParam("Whether to save the file after the change.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        ModificationResult result = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            GeneratorUtilities genUtils = GeneratorUtilities.get(wc);

            Element element = JavaSourceUtils.findElement(wc, memberFqn);
            if (element == null) {
                throw new AgiToolException("Member not found: " + memberFqn);
            }

            Tree tree = wc.getTrees().getTree(element);

            ModifiersTree oldModifiers = null;
            if (tree instanceof ClassTree ct) {
                oldModifiers = ct.getModifiers();
            } else if (tree instanceof MethodTree mt) {
                oldModifiers = mt.getModifiers();
            } else if (tree instanceof VariableTree vt) {
                oldModifiers = vt.getModifiers();
            }

            if (oldModifiers != null) {
                List<String> annotations = new ArrayList<>();
                for (AnnotationTree at : oldModifiers.getAnnotations()) {
                    annotations.add(at.toString());
                }
                if (!annotations.contains(annotationSource)) {
                    annotations.add(annotationSource);
                }

                ModifiersTree newMods = JavaSourceUtils.buildModifiers(make, wc.getTreeUtilities(), oldModifiers.getFlags(), annotations);

                Tree newTree = null;
                if (tree instanceof ClassTree ct) {
                    newTree = make.Class(newMods, ct.getSimpleName(), ct.getTypeParameters(), ct.getExtendsClause(), ct.getImplementsClause(), ct.getMembers());
                } else if (tree instanceof MethodTree mt) {
                    newTree = make.Method(newMods, mt.getName(), mt.getReturnType(), mt.getTypeParameters(), mt.getParameters(), mt.getThrows(), mt.getBody(), (AnnotationTree) mt.getDefaultValue());
                } else if (tree instanceof VariableTree vt) {
                    newTree = make.Variable(newMods, vt.getName(), vt.getType(), vt.getInitializer());
                }

                if (newTree != null) {
                    make.asReplacementOf(newTree, tree, false);
                    applyJavadoc(wc, newTree, tree, null, false); // Maintain existing Javadoc
                    newTree = genUtils.importFQNs(newTree);
                    wc.rewrite(tree, newTree);
                }
            }
        });

        result.commit();
        if (save) {
            handleSave(fo);
        }
        return "Added annotation " + annotationSource + " to " + memberFqn;
    }

    /**
     * Removes a specific annotation from a member.
     *
     * @param filePath The absolute path of the Java file.
     * @param memberFqn The FQN of the member.
     * @param annotationName The simple name of the annotation.
     * @param save Whether to save the file after the change.
     * @return A status message.
     * @throws Exception If the operation fails.
     */
    @AgiTool("Removes a specific annotation from a class, method, or field.")
    public String removeAnnotation(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the member.") String memberFqn,
            @AgiToolParam("The simple name of the annotation to remove (e.g., Slf4j).") String annotationName,
            @AgiToolParam("Whether to save the file after the change.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        ModificationResult result = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            GeneratorUtilities genUtils = GeneratorUtilities.get(wc);

            Element element = JavaSourceUtils.findElement(wc, memberFqn);
            if (element == null) {
                throw new AgiToolException("Member not found: " + memberFqn);
            }

            Tree tree = wc.getTrees().getTree(element);
            ModifiersTree oldModifiers = null;
            if (tree instanceof ClassTree ct) {
                oldModifiers = ct.getModifiers();
            } else if (tree instanceof MethodTree mt) {
                oldModifiers = mt.getModifiers();
            } else if (tree instanceof VariableTree vt) {
                oldModifiers = vt.getModifiers();
            }

            if (oldModifiers != null) {
                AnnotationTree toRemove = null;
                for (AnnotationTree at : oldModifiers.getAnnotations()) {
                    if (at.getAnnotationType().toString().equals(annotationName)) {
                        toRemove = at;
                        break;
                    }
                }
                if (toRemove != null) {
                    ModifiersTree newMods = make.removeModifiersAnnotation(oldModifiers, toRemove);
                    Tree newTree = null;
                    if (tree instanceof ClassTree ct) {
                        newTree = make.Class(newMods, ct.getSimpleName(), ct.getTypeParameters(), ct.getExtendsClause(), ct.getImplementsClause(), ct.getMembers());
                    } else if (tree instanceof MethodTree mt) {
                        newTree = make.Method(newMods, mt.getName(), mt.getReturnType(), mt.getTypeParameters(), mt.getParameters(), mt.getThrows(), mt.getBody(), (AnnotationTree) mt.getDefaultValue());
                    } else if (tree instanceof VariableTree vt) {
                        newTree = make.Variable(newMods, vt.getName(), vt.getType(), vt.getInitializer());
                    }

                    if (newTree != null) {
                        make.asReplacementOf(newTree, tree, false);
                        applyJavadoc(wc, newTree, tree, null, false); // Maintain existing Javadoc
                        newTree = genUtils.importFQNs(newTree);
                        wc.rewrite(tree, newTree);
                    }
                }
            }
        });

        result.commit();
        if (save) {
            handleSave(fo);
        }
        return "Removed annotation " + annotationName + " from " + memberFqn;
    }

    /**
     * Adds or updates Javadoc for a member.
     *
     * @param filePath The absolute path of the Java file.
     * @param memberFqn The FQN of the member.
     * @param javadocText The text content of the Javadoc.
     * @param save Whether to save the file after the change.
     * @return A status message.
     * @throws Exception If the operation fails.
     */
    @AgiTool("Sets or updates Javadoc for a class, field, constructor, method or any member in general. Note: For package-info.java files, use the Resources toolkit instead.")
    public String setJavadoc(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the member.") String memberFqn,
            @AgiToolParam("The Javadoc content (without the /** and */ markers).") String javadocText,
            @AgiToolParam("Whether to save the file after the change.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        ModificationResult result = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();

            Element element = JavaSourceUtils.findElement(wc, memberFqn);
            if (element == null) {
                throw new AgiToolException("Member not found: " + memberFqn);
            }

            Tree oldTree = wc.getTrees().getTree(element);
            Tree newTree = cloneTree(make, oldTree);

            // Aggressively sever the identity link
            make.asReplacementOf(newTree, oldTree, false);
            applyJavadoc(wc, newTree, oldTree, javadocText, true);

            wc.rewrite(oldTree, newTree);
        });

        result.commit();
        if (save) {
            handleSave(fo);
        }
        return "Set Javadoc for " + memberFqn;
    }

    /**
     * Removes Javadoc from a member.
     *
     * @param filePath The absolute path of the Java file.
     * @param memberFqn The FQN of the member.
     * @param save Whether to save the file after the change.
     * @return A status message.
     * @throws Exception If the operation fails.
     */
    @AgiTool("Removes Javadoc from a class, method, or field.")
    public String removeJavadoc(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the member.") String memberFqn,
            @AgiToolParam("Whether to save the file after the change.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        ModificationResult result = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();

            Element element = JavaSourceUtils.findElement(wc, memberFqn);
            if (element == null) {
                throw new AgiToolException("Member not found: " + memberFqn);
            }

            log("[removeJavadoc] Resolved: " + element.getSimpleName());

            Tree oldTree = wc.getTrees().getTree(element);
            Tree newTree = cloneTree(make, oldTree);

            // Aggressively sever the identity link
            make.asReplacementOf(newTree, oldTree, false);
            applyJavadoc(wc, newTree, oldTree, null, true);

            wc.rewrite(oldTree, newTree);
        });

        result.commit();
        if (save) {
            handleSave(fo);
        }
        return "Removed Javadoc from " + memberFqn;
    }

    /**
     * Inserts a new method into a class structurally.
     *
     * @param filePath The absolute path of the Java file.
     * @param classFqn The FQN of the target class.
     * @param annotations List of annotations (e.g., ['@NonNull', '@Override']).
     * @param modifiers The access modifiers (e.g., 'public final').
     * @param typeParameters List of type parameters (e.g., ['T extends
     * Serializable']).
     * @param returnType The return type string.
     * @param name The method name.
     * @param parameters List of parameters (e.g., ['String message', 'int
     * code']).
     * @param throwsClauses List of exception types.
     * @param body The method body code (inside braces).
     * @param javadoc The Javadoc text (without markers).
     * @param anchorMemberName Optional name of an existing member to use as
     * positioning anchor.
     * @param position The position relative to the anchor or class.
     * @param save Whether to save the file after the change.
     * @return A status message.
     * @throws Exception If the operation fails.
     */
    @AgiTool("Inserts a new method into a class structurally. Handles imports, javadoc, and positioning. Do not put javadoc in the body")
    public String insertMethod(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the target class.") String classFqn,
            @AgiToolParam(value = "Annotations to apply.", required = false) List<String> annotations,
            @AgiToolParam(value = "Access modifiers.", required = false) String modifiers,
            @AgiToolParam(value = "Type parameters for generics.", required = false) List<String> typeParameters,
            @AgiToolParam("The return type.") String returnType,
            @AgiToolParam("The method name.") String name,
            @AgiToolParam(value = "The method parameters.", required = false) List<String> parameters,
            @AgiToolParam(value = "Thrown exceptions.", required = false) List<String> throwsClauses,
            @AgiToolParam(value = "The method body code (just the logic between the braces, excluding the signature and Javadoc).", rendererId = "java") String body,
            @AgiToolParam(value = "The Javadoc content (without the /** and */ markers)", required = false) String javadoc,
            @AgiToolParam(value = "Anchor member name for positioning.", required = false) String anchorMemberName,
            @AgiToolParam(value = "Position relative to anchor.", required = false) RelativePosition position,
            @AgiToolParam("Whether to save the file.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        ModificationResult res = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            TreeUtilities utils = wc.getTreeUtilities();

            TypeElement te = wc.getElements().getTypeElement(classFqn);
            if (te == null) {
                throw new AgiToolException("Class not found: " + classFqn);
            }
            ClassTree ct = (ClassTree) wc.getTrees().getTree(te);

            Set<Modifier> modsSet = JavaSourceUtils.getModifiersSet(modifiers);
            ModifiersTree mods = JavaSourceUtils.buildModifiers(make, utils, modsSet, annotations);
            List<TypeParameterTree> tps = parseTypeParameters(make, typeParameters);
            List<VariableTree> params = parseParameters(make, utils, parameters);

            List<ExpressionTree> thrws = new ArrayList<>();
            if (throwsClauses != null) {
                for (String t : throwsClauses) {
                    thrws.add((ExpressionTree) make.Type(t));
                }
            }

            String finalBody = body.trim().startsWith("{") ? body : "{" + body + "}";
            MethodTree newMethod = (MethodTree) make.Method(mods, name, make.Type(returnType), tps, params, thrws, finalBody, null);
            if (javadoc != null) {
                applyJavadoc(wc, newMethod, null, javadoc, true);
            }

            newMethod = GeneratorUtilities.get(wc).importFQNs(newMethod);

            List<Tree> members = new ArrayList<>(ct.getMembers());
            int anchorIdx = anchorMemberName != null ? JavaSourceUtils.findMemberIndex(members, anchorMemberName) : -1;
            int insertIdx = 0;
            if (position != null) {
                insertIdx = switch (position) {
                    case START ->
                        0;
                    case END ->
                        members.size();
                    case BEFORE ->
                        anchorIdx != -1 ? anchorIdx : 0;
                    case AFTER ->
                        anchorIdx != -1 ? anchorIdx + 1 : members.size();
                };
            } else {
                insertIdx = members.size();
            }
            members.add(insertIdx, newMethod);

            ClassTree updatedClass = make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(),
                    ct.getExtendsClause(), ct.getImplementsClause(), members);
            wc.rewrite(ct, updatedClass);
        });

        res.commit();
        if (save) {
            handleSave(fo);
        }
        return "Inserted method '" + name + "' into " + classFqn;
    }

    /**
     * Updates an existing method structurally.
     *
     * @param filePath The absolute path of the Java file.
     * @param methodFqn The FQN of the method.
     * @param annotations Optional new list of annotations. (Replaces current
     * list)
     * @param modifiers Optional new access modifiers.
     * @param typeParameters Optional new type parameters.
     * @param returnType Optional new return type.
     * @param parameters Optional new parameters.
     * @param throwsClauses Optional new throws clauses.
     * @param body Optional new body code.
     * @param javadoc Optional new Javadoc.
     * @param save Whether to save.
     * @return Status message.
     * @throws Exception If the operation fails.
     */
    @AgiTool("Updates an existing method structurally. Use className.<init> for constructors fqns and parameter types if there are two members with the same fqn. CRITICAL: Only provide parameters you intend to change; Do not provide the rest. Passing current values that need no changing is redundant and prone to problems.")
    public String updateMethod(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the method.") String methodFqn,
            @AgiToolParam(value = "Optional new list of annotations.", required = false) List<String> annotations,
            @AgiToolParam(value = "Optional new access modifiers.", required = false) String modifiers,
            @AgiToolParam(value = "Optional new type parameters.", required = false) List<String> typeParameters,
            @AgiToolParam(value = "Optional new return type.", required = false) String returnType,
            @AgiToolParam(value = "Optional new parameters.", required = false) List<String> parameters,
            @AgiToolParam(value = "Optional new throws clauses.", required = false) List<String> throwsClauses,
            @AgiToolParam(value = "Optional new body code (just the logic between the braces, excluding the signature and Javadoc).", rendererId = "java", required = false) String body,
            @AgiToolParam(value = "Optional new Javadoc (without the /** and */ markers).", required = false) String javadoc,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        ModificationResult res = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            TreeUtilities utils = wc.getTreeUtilities();
            ExecutableElement ee = JavaSourceUtils.findMethodElement(wc, methodFqn);
            if (ee == null) {
                throw new AgiToolException("Method not found: " + methodFqn);
            }
            MethodTree mt = (MethodTree) wc.getTrees().getTree(ee);

            Set<Modifier> modsSet = modifiers != null ? JavaSourceUtils.getModifiersSet(modifiers) : mt.getModifiers().getFlags();
            ModifiersTree newMods = (modifiers != null || annotations != null)
                    ? JavaSourceUtils.buildModifiers(make, utils, modsSet, annotations) : mt.getModifiers();

            List<TypeParameterTree> tps = (typeParameters != null) ? parseTypeParameters(make, typeParameters)
                    : new ArrayList<>(mt.getTypeParameters());

            List<VariableTree> params = (parameters != null) ? parseParameters(make, utils, parameters)
                    : new ArrayList<>(mt.getParameters());

            List<ExpressionTree> thrws = new ArrayList<>();
            if (throwsClauses != null) {
                for (String t : throwsClauses) {
                    thrws.add((ExpressionTree) make.Type(t));
                }
            } else {
                thrws.addAll(mt.getThrows());
            }

            Tree finalBody = (body != null)
                    ? utils.parseStatement(body.trim().startsWith("{") ? body : "{" + body + "}", null)
                    : mt.getBody();

            MethodTree updated = make.Method(newMods, mt.getName(),
                    returnType != null ? make.Type(returnType) : mt.getReturnType(),
                    tps, params, thrws, (com.sun.source.tree.BlockTree) finalBody,
                    (AnnotationTree) mt.getDefaultValue());

            // Aggressively sever identity link
            make.asReplacementOf(updated, mt, false);

            if (javadoc != null) {
                applyJavadoc(wc, updated, mt, javadoc, true);
            } else {
                applyJavadoc(wc, updated, mt, null, false); // Maintain existing
            }
            updated = GeneratorUtilities.get(wc).importFQNs(updated);
            wc.rewrite(mt, updated);
        });

        res.commit();
        if (save) {
            handleSave(fo);
        }
        return "Updated method '" + methodFqn + "'";
    }

    /**
     * Inserts a new field structurally.
     */
    @AgiTool("Inserts a new field into a class structurally.")
    public String insertField(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the target class.") String classFqn,
            @AgiToolParam(value = "Annotations to apply.", required = false) List<String> annotations,
            @AgiToolParam(value = "Access modifiers.", required = false) String modifiers,
            @AgiToolParam("The field type.") String type,
            @AgiToolParam("The field name.") String name,
            @AgiToolParam(value = "Optional initializer expression.", required = false) String initializer,
            @AgiToolParam(value = "The Javadoc content (without the /** and */ markers).", required = false) String javadoc,
            @AgiToolParam(value = "Anchor member name.", required = false) String anchorMemberName,
            @AgiToolParam(value = "Position relative to anchor.", required = false) RelativePosition position,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        ModificationResult res = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            TreeUtilities utils = wc.getTreeUtilities();
            TypeElement te = wc.getElements().getTypeElement(classFqn);
            if (te == null) {
                throw new AgiToolException("Class not found: " + classFqn);
            }
            ClassTree ct = (ClassTree) wc.getTrees().getTree(te);

            Set<Modifier> modsSet = JavaSourceUtils.getModifiersSet(modifiers);
            ModifiersTree mods = JavaSourceUtils.buildModifiers(make, utils, modsSet, annotations);
            ExpressionTree init = (initializer != null && !initializer.isBlank()) ? wc.getTreeUtilities().parseExpression(initializer, null) : null;
            VariableTree newField = make.Variable(mods, name, make.Type(type), init);

            if (javadoc != null) {
                applyJavadoc(wc, newField, null, javadoc, true);
            }

            newField = GeneratorUtilities.get(wc).importFQNs(newField);

            List<Tree> members = new ArrayList<>(ct.getMembers());
            int anchorIdx = anchorMemberName != null ? JavaSourceUtils.findMemberIndex(members, anchorMemberName) : -1;
            int insertIdx = 0;
            if (position != null) {
                insertIdx = switch (position) {
                    case START ->
                        0;
                    case END ->
                        members.size();
                    case BEFORE ->
                        anchorIdx != -1 ? anchorIdx : 0;
                    case AFTER ->
                        anchorIdx != -1 ? anchorIdx + 1 : members.size();
                };
            } else {
                insertIdx = members.size();
            }
            members.add(insertIdx, newField);

            ClassTree updatedClass = make.Class(ct.getModifiers(), ct.getSimpleName(), ct.getTypeParameters(),
                    ct.getExtendsClause(), ct.getImplementsClause(), members);
            wc.rewrite(ct, updatedClass);
        });

        res.commit();
        if (save) {
            handleSave(fo);
        }
        return "Inserted field '" + name + "' into " + classFqn;
    }

    /**
     * Updates an existing field structurally.
     *
     * @param filePath The absolute path of the Java file.
     * @param fieldFqn The FQN of the field.
     * @param annotations Optional new list of annotations.
     * @param modifiers Optional new access modifiers.
     * @param type Optional new type.
     * @param initializer Optional new initializer expression.
     * @param javadoc Optional new Javadoc content.
     * @param save Whether to save.
     * @return Status message.
     * @throws Exception If the operation fails.
     */
    @AgiTool("Updates an existing field structurally. CRITICAL: Only provide parameters you intend to change;")
    public String updateField(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the field.") String fieldFqn,
            @AgiToolParam(value = "Optional new list of annotations.", required = false) List<String> annotations,
            @AgiToolParam(value = "Optional new access modifiers.", required = false) String modifiers,
            @AgiToolParam(value = "Optional new type.", required = false) String type,
            @AgiToolParam(value = "Optional new initializer expression.", required = false) String initializer,
            @AgiToolParam(value = "Optional new Javadoc content (without the /** and */ markers).", required = false) String javadoc,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        ModificationResult res = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            TreeUtilities utils = wc.getTreeUtilities();
            Element e = JavaSourceUtils.findElement(wc, fieldFqn);
            if (!(e instanceof VariableElement ve)) {
                throw new AgiToolException("Field not found: " + fieldFqn);
            }
            VariableTree vt = (VariableTree) wc.getTrees().getTree(ve);

            Set<Modifier> modsSet = modifiers != null ? JavaSourceUtils.getModifiersSet(modifiers) : vt.getModifiers().getFlags();
            ModifiersTree newMods = (modifiers != null || annotations != null)
                    ? JavaSourceUtils.buildModifiers(make, utils, modsSet, annotations) : vt.getModifiers();

            ExpressionTree init = (initializer != null && !initializer.isBlank())
                    ? wc.getTreeUtilities().parseExpression(initializer, null) : vt.getInitializer();

            VariableTree updated = make.Variable(newMods, vt.getName(),
                    type != null ? make.Type(type) : vt.getType(), init);

            // Aggressively sever identity link
            make.asReplacementOf(updated, vt, false);

            if (javadoc != null) {
                applyJavadoc(wc, updated, vt, javadoc, true);
            } else {
                applyJavadoc(wc, updated, vt, null, false); // Maintain existing
            }
            updated = GeneratorUtilities.get(wc).importFQNs(updated);
            wc.rewrite(vt, updated);
        });

        res.commit();
        if (save) {
            handleSave(fo);
        }
        return "Updated field '" + fieldFqn + "'";
    }

    /**
     * Inserts a new class or inner type structurally.
     *
     * @param filePath The absolute path of the Java file.
     * @param parentClassFqn Optional FQN of the parent class (for inner types).
     * @param annotations List of annotations.
     * @param modifiers The access modifiers.
     * @param kind The kind of type (CLASS, INTERFACE, ENUM, ANNOTATION_TYPE,
     * RECORD).
     * @param name The type name.
     * @param typeParameters Generics.
     * @param recordComponents List of record components (for RECORD kind).
     * @param extendsClause Optional superclass.
     * @param implementsClauses Optional interfaces.
     * @param javadoc The Javadoc text.
     * @param anchorMemberName Positioning anchor.
     * @param position Relative position.
     * @param save Whether to save.
     * @return Status message.
     * @throws Exception If the operation fails.
     */
    @AgiTool("Inserts a new class or inner type structurally into an existing class. Not for creating new outer classess.")
    public String insertClass(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam(value = "The FQN of the parent class (null for top-level).", required = false) String parentClassFqn,
            @AgiToolParam(value = "Annotations to apply.", required = false) List<String> annotations,
            @AgiToolParam(value = "Access modifiers.", required = false) String modifiers,
            @AgiToolParam("The type kind (e.g., CLASS, INTERFACE).") com.sun.source.tree.Tree.Kind kind,
            @AgiToolParam("The type name.") String name,
            @AgiToolParam(value = "Type parameters for generics.", required = false) List<String> typeParameters,
            @AgiToolParam(value = "List of record components (e.g., 'String name').", required = false) List<String> recordComponents,
            @AgiToolParam(value = "The superclass.", required = false) String extendsClause,
            @AgiToolParam(value = "List of implemented interfaces.", required = false) List<String> implementsClauses,
            @AgiToolParam(value = "The Javadoc content (without the /** and */ markers).", required = false) String javadoc,
            @AgiToolParam(value = "Anchor member name for positioning.", required = false) String anchorMemberName,
            @AgiToolParam(value = "Position relative to anchor.", required = false) RelativePosition position,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        ModificationResult res = js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            TreeUtilities utils = wc.getTreeUtilities();

            Set<Modifier> modsSet = JavaSourceUtils.getModifiersSet(modifiers);
            ModifiersTree mods = JavaSourceUtils.buildModifiers(make, utils, modsSet, annotations);
            List<TypeParameterTree> tps = parseTypeParameters(make, typeParameters);
            List<VariableTree> components = parseRecordComponents(make, utils, recordComponents);

            Tree ext = (extendsClause != null && !extendsClause.isBlank()) ? make.Type(extendsClause) : null;
            List<Tree> impls = new ArrayList<>();
            if (implementsClauses != null) {
                for (String i : implementsClauses) {
                    impls.add(make.Type(i));
                }
            }

            ClassTree newClass = switch (kind) {
                case INTERFACE ->
                    make.Interface(mods, name, tps, impls, Collections.<Tree>emptyList());
                case ENUM ->
                    make.Enum(mods, name, impls, Collections.<Tree>emptyList());
                case ANNOTATION_TYPE ->
                    make.AnnotationType(mods, name, Collections.<Tree>emptyList());
                case RECORD ->
                    make.Class(mods, name, tps, null, impls, Collections.emptyList(), components);
                default ->
                    make.Class(mods, name, tps, ext, impls, Collections.<Tree>emptyList());
            };

            if (javadoc != null) {
                applyJavadoc(wc, newClass, null, javadoc, true);
            }
            newClass = GeneratorUtilities.get(wc).importFQNs(newClass);

            if (parentClassFqn == null || parentClassFqn.isBlank()) {
                CompilationUnitTree cut = wc.getCompilationUnit();
                List<Tree> types = new ArrayList<>(cut.getTypeDecls());
                types.add(newClass);
                wc.rewrite(cut, make.CompilationUnit(cut.getPackage(), cut.getImports(), types, cut.getSourceFile()));
            } else {
                TypeElement parentTe = wc.getElements().getTypeElement(parentClassFqn);
                ClassTree parentCt = (ClassTree) wc.getTrees().getTree(parentTe);
                List<Tree> members = new ArrayList<>(parentCt.getMembers());
                int anchorIdx = anchorMemberName != null ? JavaSourceUtils.findMemberIndex(members, anchorMemberName) : -1;
                int insertIdx = 0;
                if (position != null) {
                    insertIdx = switch (position) {
                        case START ->
                            0;
                        case END ->
                            members.size();
                        case BEFORE ->
                            anchorIdx != -1 ? anchorIdx : 0;
                        case AFTER ->
                            anchorIdx != -1 ? anchorIdx + 1 : members.size();
                    };
                } else {
                    insertIdx = members.size();
                }
                members.add(insertIdx, newClass);
                wc.rewrite(parentCt, make.Class(parentCt.getModifiers(), parentCt.getSimpleName(),
                        parentCt.getTypeParameters(), parentCt.getExtendsClause(),
                        parentCt.getImplementsClause(), members));
            }
        });

        res.commit();
        if (save) {
            handleSave(fo);
        }
        return "Inserted type '" + name + "' into " + filePath;
    }

    /**
     * Removes a member structurally.
     */
    @AgiTool("Removes a member (method, field, or inner class) structurally from its parent class or file.")
    public String removeMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the member to remove.") String memberFqn,
            @AgiToolParam("Whether to save the file after the change.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            Element element = JavaSourceUtils.findElement(wc, memberFqn);
            if (element == null) {
                throw new AgiToolException("Member not found: " + memberFqn);
            }

            Tree memberTree = wc.getTrees().getTree(element);
            Element parentElement = element.getEnclosingElement();

            if (parentElement instanceof TypeElement te) {
                ClassTree parentTree = (ClassTree) wc.getTrees().getTree(te);
                List<Tree> members = new ArrayList<>(parentTree.getMembers());
                members.remove(memberTree);
                ClassTree updatedParent = make.Class(parentTree.getModifiers(), parentTree.getSimpleName(), parentTree.getTypeParameters(),
                        parentTree.getExtendsClause(), parentTree.getImplementsClause(), members);
                wc.rewrite(parentTree, updatedParent);
            } else {
                CompilationUnitTree cut = wc.getCompilationUnit();
                List<Tree> types = new ArrayList<>(cut.getTypeDecls());
                types.remove(memberTree);
                CompilationUnitTree updatedCut = make.CompilationUnit(cut.getPackage(), cut.getImports(), types, cut.getSourceFile());
                wc.rewrite(cut, updatedCut);
            }
        }).commit();

        if (save) {
            handleSave(fo);
        }
        return "Removed member '" + memberFqn + "' structurally.";
    }

    /**
     * Structural 'Fix Imports' operation.
     *
     * @param filePath The absolute path of the Java file.
     * @param save Whether to save the file after the change.
     * @return A status message.
     * @throws Exception If the operation fails.
     */
    @AgiTool("Structural 'Fix Imports' operation. Converts FQNs to simple names and removes unused imports.")
    public String optimizeImports(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("Whether to save the file after the change.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            GeneratorUtilities genUtils = GeneratorUtilities.get(wc);
            
            // 1. Convert FQNs to simple names across the whole unit
            CompilationUnitTree cut = genUtils.importFQNs(wc.getCompilationUnit());
            wc.rewrite(wc.getCompilationUnit(), cut);
            
            // 2. Remove redundant and unused imports
            removeUnusedImportsInternal(wc);
        }).commit();

        if (save) {
            handleSave(fo);
        }
        return "Optimized imports for: " + fo.getNameExt();
    }

    private void removeUnusedImportsInternal(WorkingCopy copy) {
        try {
            Method computeImports = JavaFixAllImports.class.getDeclaredMethod("computeImports", org.netbeans.api.java.source.CompilationInfo.class);
            computeImports.setAccessible(true);
            Object importData = computeImports.invoke(null, copy);

            Class<?>[] declaredClasses = JavaFixAllImports.class.getDeclaredClasses();
            Class<?> importDataClass = null;
            for (Class<?> c : declaredClasses) {
                if (c.getSimpleName().equals("ImportData")) {
                    importDataClass = c;
                    break;
                }
            }

            if (importDataClass != null) {
                // Log the decisions being made
                String[] simpleNames = (String[]) importDataClass.getDeclaredField("simpleNames").get(importData);
                Object[] defaults = (Object[]) importDataClass.getDeclaredField("defaults").get(importData);
                
                for (int i = 0; i < simpleNames.length; i++) {
                    if (defaults[i] != null) {
                        String displayName = (String) defaults[i].getClass().getDeclaredField("displayName").get(defaults[i]);
                        log.info("[optimizeImports] Resolution for '{}': {}", simpleNames[i], displayName);
                    } else {
                        log.info("[optimizeImports] Could not resolve ambiguity for '{}'", simpleNames[i]);
                    }
                }

                java.lang.reflect.Field defaultsField = importDataClass.getDeclaredField("defaults");
                defaultsField.setAccessible(true);
                Object defaultSelections = defaultsField.get(importData);

                Method performFixImports = JavaFixAllImports.class.getDeclaredMethod("performFixImports", WorkingCopy.class, importDataClass, defaultSelections.getClass(), boolean.class);
                performFixImports.setAccessible(true);
                performFixImports.invoke(null, copy, importData, defaultSelections, true);
            }
        } catch (Exception e) {
            log.error("Failed to remove unused imports via reflection", e);
        }
    }

    /**
     * Reformats the specified file using the IDE's code style rules.
     *
     * @param filePath The absolute path of the Java file.
     * @param save Whether to save the file after the change.
     * @return A status message.
     * @throws Exception If the operation fails.
     */
    @AgiTool("Reformats a specified file open in the editor using the IDE's code style rules. **Does not work if the file is not open in the editor**")
    public String reformat(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("Whether to save the file after the change.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        DataObject doid = DataObject.find(fo);
        EditorCookie ec = doid.getLookup().lookup(EditorCookie.class);
        if (ec == null || ec.getOpenedPanes() == null || ec.getOpenedPanes().length == 0) {
            throw new AgiToolException("File is not open in the editor: " + filePath);
        }

        JavaSource js = JavaSource.forFileObject(fo);
        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            javax.swing.text.Document doc = wc.getDocument();
            if (doc != null) {
                Reformat reformat = Reformat.get(doc);
                reformat.lock();
                try {
                    reformat.reformat(0, doc.getLength());
                } finally {
                    reformat.unlock();
                }
            }
        }).commit();

        if (save) {
            handleSave(fo);
        }
        return "Reformated: " + fo.getNameExt();
    }

    private List<VariableTree> parseParameters(TreeMaker make, TreeUtilities utils, List<String> params) {
        if (params == null || params.isEmpty()) {
            return new ArrayList<>();
        }
        List<VariableTree> result = new ArrayList<>();
        for (String p : params) {
            // Robust parsing: wrap in a dummy method to handle annotations and modifiers correctly
            String dummy = "void __method(" + p + ") {}";
            MethodTree mt = (MethodTree) utils.parseStatement(dummy, null);
            if (mt != null && !mt.getParameters().isEmpty()) {
                result.add(mt.getParameters().get(0));
            }
        }
        return result;
    }

    private List<VariableTree> parseRecordComponents(TreeMaker make, TreeUtilities utils, List<String> components) {
        if (components == null || components.isEmpty()) {
            return new ArrayList<>();
        }
        List<VariableTree> result = new ArrayList<>();
        for (String c : components) {
            // Robust parsing: wrap in a dummy record to handle annotations correctly
            String dummy = "record __Record(" + c + ") {}";
            ClassTree ct = (ClassTree) utils.parseType(dummy, null);
            if (ct != null && ct.getKind() == Tree.Kind.RECORD) {
                for (Tree member : ct.getMembers()) {
                    if (member instanceof VariableTree vt) {
                        result.add(make.RecordComponent(vt.getModifiers(), vt.getName().toString(), vt.getType()));
                        break;
                    }
                }
            }
        }
        return result;
    }

    private List<TypeParameterTree> parseTypeParameters(TreeMaker make, List<String> tps) {
        if (tps == null || tps.isEmpty()) {
            return new ArrayList<>();
        }
        List<TypeParameterTree> result = new ArrayList<>();
        for (String tp : tps) {
            result.add(make.TypeParameter(tp, Collections.emptyList()));
        }
        return result;
    }

    private void handleSave(FileObject fo) throws IOException {
        DataObject doid = DataObject.find(fo);
        EditorCookie ec = doid.getLookup().lookup(EditorCookie.class);
        if (ec != null && ec.getOpenedPanes() != null) {
            ec.saveDocument();
        }
        SaveCookie sc = doid.getLookup().lookup(SaveCookie.class);
        if (sc != null) {
            sc.save();
        }
        fo.refresh(); // Ensure the file system is in sync
    }

    /**
     * Internal helper to set or remove Javadoc on a tree node.
     * <p>
     * CRITICAL: TreeMaker.addComment and removeComment are VOID methods that
     * modify the instance. For existing members, you MUST clone the tree first
     * and use wc.rewrite(old, new), otherwise wc.rewrite(t, t) will ignore the
     * comment change.
     * </p>
     */
    private void applyJavadoc(WorkingCopy wc, Tree tree, Tree oldTree, String javadocText, boolean removeExisting) {
        TreeMaker make = wc.getTreeMaker();
        TreeUtilities utils = wc.getTreeUtilities();

        if (oldTree != null) {
            // 1. Manually migrate non-preceding comments
            GeneratorUtilities.get(wc).copyComments(oldTree, tree, false);

            // 2. Manually migrate PRECEDING comments
            List<org.netbeans.api.java.source.Comment> oldPre = utils.getComments(oldTree, true);
            for (org.netbeans.api.java.source.Comment c : oldPre) {
                if (c.isDocComment()) {
                    if (removeExisting || javadocText != null) {
                        log("[applyJavadoc] Filtering out old Javadoc.");
                        continue;
                    }
                }
                make.addComment(tree, c, true);
            }

            // 3. LOCK position if removing Javadoc
            if (javadocText == null && removeExisting) {
                log("[applyJavadoc] Adding locking whitespace for removal.");
                make.addComment(tree, org.netbeans.api.java.source.Comment.create(org.netbeans.api.java.source.Comment.Style.WHITESPACE, -1, -1, -1, " "), true);
            }
        }

        // 4. Add new Javadoc if requested
        if (javadocText != null && !javadocText.isBlank()) {
            log("[applyJavadoc] Adding new Javadoc.");
            String formatted = "/**\n * " + javadocText.replace("\n", "\n * ") + "\n */";
            make.addComment(tree, org.netbeans.api.java.source.Comment.create(org.netbeans.api.java.source.Comment.Style.JAVADOC, -1, -1, -1, formatted), true);
        }
    }

    /**
     * Clones a tree node for structural rewrite.
     */
    @SuppressWarnings("unchecked")
    private Tree cloneTree(TreeMaker make, Tree tree) {
        if (tree instanceof ClassTree ct) {
            ModifiersTree modifiers = make.Modifiers(ct.getModifiers().getFlags(), ct.getModifiers().getAnnotations());
            return switch (ct.getKind()) {
                case INTERFACE ->
                    make.Interface(modifiers, ct.getSimpleName(), ct.getTypeParameters(), ct.getImplementsClause(), Collections.emptyList(), ct.getMembers());
                case ENUM ->
                    make.Enum(modifiers, ct.getSimpleName(), ct.getImplementsClause(), ct.getMembers());
                case ANNOTATION_TYPE ->
                    make.AnnotationType(modifiers, ct.getSimpleName(), ct.getMembers());
                default ->
                    make.Class(modifiers, ct.getSimpleName(), ct.getTypeParameters(), ct.getExtendsClause(), ct.getImplementsClause(), ct.getMembers());
            };
        } else if (tree instanceof MethodTree mt) {
            ModifiersTree modifiers = make.Modifiers(mt.getModifiers().getFlags(), mt.getModifiers().getAnnotations());
            return make.Method(modifiers, mt.getName(), mt.getReturnType(), mt.getTypeParameters(), mt.getParameters(), mt.getThrows(), mt.getBody(), (AnnotationTree) mt.getDefaultValue());
        } else if (tree instanceof VariableTree vt) {
            ModifiersTree modifiers = make.Modifiers(vt.getModifiers().getFlags(), vt.getModifiers().getAnnotations());
            return make.Variable(modifiers, vt.getName(), vt.getType(), vt.getInitializer());
        }
        return tree;
    }

    /**
     * Adds multiple imports to a Java file.
     *
     * @param filePath The absolute path of the Java file.
     * @param fqns List of fully qualified names to import.
     * @param save Whether to save the file after the change.
     * @return A status message.
     * @throws Exception If the operation fails.
     */
    @AgiTool("Adds multiple imports to a Java file.")
    public String addImports(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("List of fully qualified names to import.") List<String> fqns,
            @AgiToolParam("Whether to save the file after the change.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            GeneratorUtilities genUtils = GeneratorUtilities.get(wc);
            CompilationUnitTree cut = wc.getCompilationUnit();
            for (String fqn : fqns) {
                TypeElement te = wc.getElements().getTypeElement(fqn);
                if (te != null) {
                    cut = genUtils.addImports(cut, Collections.singleton(te));
                } else {
                    log.warn("[addImports] Could not resolve type element for: {}", fqn);
                }
            }
            wc.rewrite(wc.getCompilationUnit(), cut);
        }).commit();

        if (save) {
            handleSave(fo);
        }
        return "Added imports for: " + fqns;
    }

    /**
     * Moves a member (method, field, or inner class) to a new position within the same class.
     *
     * @param filePath The absolute path of the Java file.
     * @param memberFqn The FQN of the member to move.
     * @param anchorMemberName The name of the member to move relative to.
     * @param position The position relative to the anchor.
     * @param save Whether to save the file.
     * @return Status message.
     * @throws Exception If the operation fails.
     */
        @AgiTool("Moves a member (method, field, or inner class) to a new position within the same class.")
    public String moveMember(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The FQN of the member to move.") String memberFqn,
            @AgiToolParam(value = "The name of the member to move relative to.", required = false) String anchorMemberName,
            @AgiToolParam("The position relative to the anchor.") RelativePosition position,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {
        
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            TreeMaker make = wc.getTreeMaker();
            Element element = JavaSourceUtils.findElement(wc, memberFqn);
            if (element == null) {
                throw new AgiToolException("Member not found: " + memberFqn);
            }
            Tree memberTree = wc.getTrees().getTree(element);
            Element parentElement = element.getEnclosingElement();
            if (!(parentElement instanceof TypeElement te)) {
                throw new AgiToolException("Only members of a class can be moved.");
            }
            ClassTree parentTree = (ClassTree) wc.getTrees().getTree(te);
            List<Tree> members = new ArrayList<>(parentTree.getMembers());
            // Remove the member from its current position
            members.remove(memberTree);
            // Re-insert at the new position
            int anchorIdx = anchorMemberName != null ? JavaSourceUtils.findMemberIndex(members, anchorMemberName) : -1;
            int insertIdx = switch (position) {
                case START -> 0;
                case END -> members.size();
                case BEFORE -> anchorIdx != -1 ? anchorIdx : 0;
                case AFTER -> anchorIdx != -1 ? anchorIdx + 1 : members.size();
            };
            members.add(insertIdx, memberTree);
            ClassTree updatedParent = make.Class(parentTree.getModifiers(), parentTree.getSimpleName(), parentTree.getTypeParameters(), parentTree.getExtendsClause(), parentTree.getImplementsClause(), members);
            wc.rewrite(parentTree, updatedParent);
        }).commit();
        if (save) {
            handleSave(fo);
        }
        String anchorPart = anchorMemberName != null ? " relative to " + anchorMemberName : "";
        return "Moved member '" + memberFqn + "' to " + position + anchorPart;
    }
}