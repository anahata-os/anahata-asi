/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import com.sun.source.doctree.DocCommentTree;
import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.ReferenceTree;
import com.sun.source.tree.*;
import com.sun.source.util.DocTrees;
import com.sun.source.util.TreePath;
import java.io.IOException;
import java.util.*;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.source.*;
import org.openide.filesystems.FileObject;
import uno.anahata.asi.agi.tool.*;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils;

/**
 * A specialized toolkit for managing Java Javadocs structurally.
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Structural Javadoc management. Allows updating descriptions, authors, tags, and parameters using the AST.")
@Deprecated
public class Javadocs extends AnahataToolkit {

    /**
     * Terrible, don't enable it, rewrites the whole class
     */
    @Override
    public void initialize() {
        getToolkit().setEnabled(false);
    }

    
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(JavaSourceUtils.CANONICAL_FQN_STANDARD
                + "\n"
                + "Javadocs Toolkit: Not working, dont enable it.\n"
                + "- Always use the Anahata Canonical FQN to identify the target member.\n"
                + "- Descriptions support Markdown and HTML.\n"
        );
    }

    /**
     * Sets or updates Javadoc for a class, field, constructor, method or any
     * member in general.
     *
     * @param filePath the absolute path of the Java file
     * @param memberFqn the ABSOLUTE FQN of the member
     * @param description the main Javadoc description
     * @param authors list of author names
     * @param since list of since versions
     * @param params map of parameter names to descriptions
     * @param returns the return value description
     * @param throwsList map of exception FQNs to descriptions
     * @param tags optional list of additional block tags
     * @param save whether to save the file
     * @return a success message
     * @throws Exception if Javadoc update fails
     */
    @AgiTool("Sets or updates Javadoc for a class, field, constructor, method or any member in general.")
    public String setJavadoc(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member.") String memberFqn,
            @AgiToolParam(value = "The main Javadoc description (Markdown or HTML). Supports inline tags like {@inheritDoc}, {@link}, {@code}.", required = false) String description,
            @AgiToolParam(value = "List of author names.", required = false) List<String> authors,
            @AgiToolParam(value = "List of since versions.", required = false) List<String> since,
            @AgiToolParam(value = "Map of parameter names to their descriptions.", required = false) Map<String, String> params,
            @AgiToolParam(value = "The return value description.", required = false) String returns,
            @AgiToolParam(value = "Map of exception FQNs to their descriptions.", required = false) Map<String, String> throwsList,
            @AgiToolParam(value = "Optional list of additional block tags (e.g. ['version 1.0', 'see OtherClass']).", required = false) List<String> tags,
            @AgiToolParam("Whether to save.") boolean save) throws Exception {

        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);

        js.runModificationTask(wc -> {
            wc.toPhase(JavaSource.Phase.RESOLVED);
            Tree oldTree = JavaSourceUtils.findTree(wc, memberFqn);
            if (oldTree == null) {
                throw new AgiToolException("Member not found: " + memberFqn);
            }

            applyJavadocStructural(wc, oldTree, description, authors, since, params, returns, throwsList, tags);
        }).commit();

        if (save) {
            JavaSourceUtils.handleSave(fo);
        }
        return "Set Javadoc for " + memberFqn;
    }

    private void applyJavadocStructural(WorkingCopy wc, Tree tree, String description,
            List<String> authors, List<String> since, Map<String, String> params,
            String returns, Map<String, String> throwsList, List<String> tags) {

        TreeMaker make = wc.getTreeMaker();
        DocTrees docTrees = wc.getDocTrees();
        CompilationUnitTree cut = wc.getCompilationUnit();
        TreePath path = wc.getTrees().getPath(cut, tree);

        DocCommentTree oldDoc = docTrees.getDocCommentTree(path);

        List<DocTree> bodyNodes = new ArrayList<>();
        if (description != null && !description.isBlank()) {
            String[] lines = description.split("\n");
            for (int i = 0; i < lines.length; i++) {
                bodyNodes.addAll(parseInlineTags(make, lines[i]));
                if (i < lines.length - 1) {
                    bodyNodes.add(make.Text("\n"));
                }
            }
        }

        List<DocTree> docTags = new ArrayList<>();

        if (authors != null) {
            for (String author : authors) {
                docTags.add(make.Author(Collections.singletonList(make.Text(author))));
            }
        }

        if (since != null) {
            for (String s : since) {
                docTags.add(make.Since(Collections.singletonList(make.Text(s))));
            }
        }

        if (params != null) {
            params.forEach((name, desc) -> {
                docTags.add(make.Param(false, make.DocIdentifier(name), parseInlineTags(make, desc)));
            });
        }

        if (returns != null && !returns.isBlank()) {
            docTags.add(make.DocReturn(parseInlineTags(make, returns)));
        }

        if (throwsList != null) {
            throwsList.forEach((ex, desc) -> {
                docTags.add(make.Throws(make.Reference(null, ex, null), parseInlineTags(make, desc)));
            });
        }

        if (tags != null) {
            for (String input : tags) {
                int firstSpace = input.indexOf(' ');
                String tagName = (firstSpace != -1) ? input.substring(0, firstSpace) : input;
                String content = (firstSpace != -1) ? input.substring(firstSpace + 1) : "";

                switch (tagName) {
                    case "see" ->
                        docTags.add(make.See(parseInlineTags(make, content)));
                    case "version" ->
                        docTags.add(make.Version(parseInlineTags(make, content)));
                    default ->
                        docTags.add(make.UnknownBlockTag(tagName, parseInlineTags(make, content)));
                }
            }
        }

        DocCommentTree newDoc = make.DocComment(bodyNodes, docTags);
        wc.rewrite(tree, oldDoc, newDoc);
    }

    private List<DocTree> parseInlineTags(TreeMaker make, String text) {
        List<DocTree> nodes = new ArrayList<>();
        if (text == null) {
            return nodes;
        }
        parseInlineTags(make, text, nodes);
        return nodes;
    }

    private void parseInlineTags(TreeMaker make, String text, List<DocTree> nodes) {
        int start = 0;
        while (true) {
            int tagStart = text.indexOf("{@", start);
            if (tagStart == -1) {
                String remainder = text.substring(start);
                if (!remainder.isEmpty()) {
                    nodes.add(make.Text(remainder));
                }
                break;
            }
            String prefix = text.substring(start, tagStart);
            if (!prefix.isEmpty()) {
                nodes.add(make.Text(prefix));
            }

            int tagEnd = text.indexOf("}", tagStart);
            if (tagEnd == -1) {
                nodes.add(make.Text(text.substring(tagStart)));
                break;
            }

            String fullTag = text.substring(tagStart + 2, tagEnd);
            int firstSpace = fullTag.indexOf(' ');
            String tagName = (firstSpace != -1) ? fullTag.substring(0, firstSpace) : fullTag;
            String tagContent = (firstSpace != -1) ? fullTag.substring(firstSpace + 1) : "";

            switch (tagName) {
                case "inheritDoc" ->
                    nodes.add(make.InheritDoc());
                case "code" ->
                    nodes.add(make.Code(make.Text(tagContent)));
                case "link", "linkplain" -> {
                    boolean plain = "linkplain".equals(tagName);
                    int labelSpace = tagContent.indexOf(' ');
                    String ref = (labelSpace != -1) ? tagContent.substring(0, labelSpace) : tagContent;
                    String label = (labelSpace != -1) ? tagContent.substring(labelSpace + 1) : "";
                    ReferenceTree refTree = make.Reference(null, ref, null);
                    List<DocTree> labelNodes = label.isEmpty() ? Collections.emptyList() : Collections.singletonList(make.Text(label));
                    nodes.add(plain ? make.LinkPlain(refTree, labelNodes) : make.Link(refTree, labelNodes));
                }
                default ->
                    nodes.add(make.UnknownInlineTag(tagName, Collections.singletonList(make.Text(tagContent))));
            }
            start = tagEnd + 1;
        }
    }
}
