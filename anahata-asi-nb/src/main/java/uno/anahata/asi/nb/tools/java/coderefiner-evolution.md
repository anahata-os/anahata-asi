/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
# CodeRefiner Evolution Plan (V3)

This document outlines the architectural shift for structural Java manipulation, moving from "blind" path-based execution to "Resource-Centric" batch refinement using Pure AST.

## Turn: 213 Findings

**The Collapse of Text-Splicing (V2-Hybrid):**
The hybrid approach of using AST for coordinates and `String.substring` for text application failed because:
1. `SourcePositions` for members only returns the start of the modifiers, completely omitting the `DocCommentTree` (Javadoc). Attempting to manually parse the Javadoc using `indexOf` led to accidental duplication when splicing.
2. Relying on `indexOf` and `.trim()` for calculating text boundaries of complex Java constructs is mathematically doomed.
3. Attempting to manually fix indentation led to exponential formatting drift.

**The AST Redemption (V3-Pure AST):**
1. We originally feared `CasualDiff` because it threw an uncatchable `NullPointerException` during `importFQNs`. However, reading the NetBeans source code revealed that `CasualDiff` itself is incredibly robust for member replacements! The NPE was specifically caused by `GeneratorUtilities.importFQNs` aggressively rewriting the compilation unit and collapsing generics, completely desyncing the Lexer's `TokenSequence` from the AST.
2. By using `UnusedImports` instead of `importFQNs`, we bypass the bug entirely. Ideally we would like `GeneratorUtilities.importFQNs` to automatically resolve and convert FQNs to imports in our generated trees, but we can work around this by surgically managing imports ourselves since NetBeans has issues (or we can raise an issue on GitHub).
3. `WorkingCopy.rewrite(Tree oldTree, Tree newTree)` automatically preserves `//` and `/*` comments using `CommentHandlerService`.
4. NetBeans `WorkingCopy.rewrite(Tree tree, DocTree oldDoc, DocTree newDoc)` DOES work, but requires exact mapping and careful application to avoid `docChanges` assertions. Javadoc updates are much safer to apply by generating a full string (Javadoc + declaration), parsing it into a new Tree, and replacing the member entirely.

**The Missing Link: Blank Lines and Inline Comments**
Replacing a method body using pure AST (`wc.rewrite(oldTree, newTree)`) often causes NetBeans' `VeryPretty` to reformat the new tree according to the IDE's `CodeStyle`. Initially, we thought this would strip intentional blank lines within the method body provided by the LLM. 
**The Revelation:** We discovered two different native solutions depending on the operation:
1. **For Method Body Updates (`createMethodBody`)**: NetBeans `TreeMaker` provides a special extension method `make.createMethodBody(MethodTree old, String body)`. This bypasses AST parsing completely and injects the raw string directly into the diff engine, guaranteeing 100% preservation of all LLM formatting, inline comments, and blank lines within the method!
2. **For Full Insertions (`parseMember`)**: NetBeans `CommentHandlerService` actually treats blank lines as a special type of comment (`Comment.Style.WHITESPACE`). When we parse the LLM's snippet into a temporary `JavaSource`, we use `GeneratorUtilities.get(wc).importComments(t, innerWc.getCompilationUnit())` to perfectly capture all blank lines and `//` comments into the AST node before injecting it into the main file!

**The Final V3 Architecture:**
- Parse incoming LLM strings for full members into detached AST `Tree` nodes using `BatchCodeRefiner.parseMember` (which uses `importComments`).
- Update existing method bodies using `make.createMethodBody` to bypass formatting drift.
- Perform pure AST replacements using `WorkingCopy.rewrite(oldTree, newTree)`.
- Use `GeneratorUtilities.copyComments` to merge existing leading/trailing comments.
- Rely entirely on `CasualDiff` and `Reformatter` to natively handle text generation and formatting.

## Turn 232: Comprehensive Test Suite (`CodeRefinementBatchTest.java`)
To validate the robust V3 AST manipulation without regressions, we have implemented a programmatic test suite covering the following edge cases:
- Inserting / updating / moving a method with `@SneakyThrows`.
- Updating the Javadoc of a member only.
- Guaranteeing preservation of blank lines in method bodies.
- Creating an inner class with 3 members.
- Updating a member of an inner class (insert/move/delete).
- Class-level Javadocs.
- Inner-class Javadocs.
- Checking annotations are correctly parsed in insert/update/statements.
- Inserting methods and inner classes with complex generics (to guarantee CasualDiff doesn't crash on <T, R>).
- Updating methods with generics and preserving blank lines in the body.
- Chained Anchoring: Inserting `methodB` AFTER `methodA` (which was just inserted in the same batch), and `methodC` AFTER `methodB`.

This ensures that NetBeans formatting (`VeryPretty` & `CasualDiff`) and comment injection perfectly respect our structural edits.

## Import Resolution Strategy
To bypass the buggy `GeneratorUtilities.importFQNs()`, we have introduced `importsToAdd` and `importsToRemove` directly into the `CodeRefinementBatch`. We resolve the elements via `WorkingCopy.getElements().getTypeElement()` and manually ask `GeneratorUtilities` to add them to the AST. This is highly stable. Additionally, we avoid using `Reformatter.reformat` directly because it requires the file to be actively open in an IDE `Document` to apply `CodeStyle` correctly. By sticking to `CasualDiff` and `WorkingCopy.rewrite`, we get perfect, headless formatting!

## Turn 263: NetBeans Bug Reports & Workarounds
During the development of the V3 Pure AST architecture, we uncovered two significant bugs in the NetBeans compiler/AST manipulation APIs that should be reported upstream to Geertjan and the NetBeans team:

1. **`CasualDiff` NullPointerException on Generics (`GeneratorUtilities.importFQNs`)**
   - **Bug:** When `GeneratorUtilities.importFQNs(CompilationUnitTree)` structurally rewrites a `CompilationUnit` containing complex generics (e.g., `<T, R>`), the AST translation loses synchronization with the Lexer. Later, `CasualDiff.diff()` advances the `TokenSequence` out of bounds during the formatting pass, and blindly calls `tokenSequence.token().id()` without checking if `tokens.token()` is null. This crashes the background parser with an uncatchable `NullPointerException`.
   - **Workaround:** We bypass `importFQNs` entirely. Instead, we manually resolve FQNs to `TypeElement`s and use `GeneratorUtilities.addImports(...)` to surgically inject explicit imports, leaving the rest of the AST untouched. *(Update: Exhaustive standalone testing with heavily nested generics and FQNs failed to reproduce the crash in isolation. The NPE may be a side-effect of mixed AST node identities resulting from aggressive manual `WorkingCopy.rewrite` operations we were doing in V2/V3. Regardless, the V4 Text Splicing approach safely sidesteps the entire issue).*

2. **`CasualDiff` Hard-Copies Preceding Text on Aligned Replacements**
   - **Bug:** When replacing an existing member via `WorkingCopy.rewrite(oldTree, newTree)`, `CasualDiff` aligns the old and new elements based on signature. When traversing into `diffClassDef` or `diffMethodDef`, it uses `copyTo(localPointer, pos)` to hard-copy the raw text *preceding* the modifiers directly from the original source file! This means any new Javadoc injected into `newTree`'s `CommentSet` is completely ignored, and the OLD Javadoc is brutally hard-copied back into the file.
   - **Workaround:** We explicitly disable `javadoc` modifications for `UPDATE` intents in `BatchCodeRefiner` and throw a fast-failing `AgiToolException`. `INSERT` intents still support Javadoc injection because newly inserted members are printed from scratch by `VeryPretty` (which correctly renders the `CommentSet`). For updating Javadocs of existing members, the agent must use the dedicated `Javadocs` toolkit or standard text replacement.

## Turn 246: Structural Removal Identity Bug & Anchoring
**The Identity Bug:**
When chaining multiple intents in a batch, early intents (like an `INSERT` or `UPDATE`) cause the `ClassTree` container to be rebuilt. When a subsequent `DELETE` or `MOVE` intent attempts to remove an old member, it looks up the original member tree and tries to call `members.remove(oldMember)`. Because the container was rebuilt, its `members` list contains entirely new object instances! `remove()` fails silently because Java `Tree` nodes do not override `.equals()`.
**The Fix:** If `remove(oldMember)` returns false, we fall back to comparing the `StartPosition` of the old member against the `StartPosition` of all members in the new list, as `WorkingCopy` preserves original source coordinates for cloned nodes.

**The Anchoring Bug:**
If a batch contains an `INSERT` (e.g. `methodA()`) and a subsequent `INSERT` tries to use `methodA()` as its anchor (`BEFORE` or `AFTER`), the second intent must be able to find the newly inserted method. Since the newly inserted method hasn't been attributed by the compiler, `info.getTrees().getElement()` returns null. However, our `BatchCodeRefiner.getMemberSignature()` regex-based fallback perfectly extracts the signature from the un-attributed `MethodTree`, allowing relative anchoring to work seamlessly within the same batch!

### Loaded NetBeans Sources in Context
To achieve these insights, we loaded the following NetBeans compiler and AST internals into our context:
- `org.netbeans.api.java.source.WorkingCopy`
- `org.netbeans.api.java.source.TreeMaker`
- `org.netbeans.api.java.source.GeneratorUtilities`
- `org.netbeans.api.java.source.TreeUtilities`
- `org.netbeans.modules.java.editor.base.imports.UnusedImports`
- `org.netbeans.api.java.source.support.ReferencesCount`
- `org.netbeans.modules.java.source.save.CasualDiff`
- `org.netbeans.modules.java.source.save.Reformatter`
- `org.netbeans.api.java.source.Comment`
- `org.netbeans.modules.java.source.builder.CommentHandlerService`
- `org.netbeans.modules.java.source.builder.CommentSetImpl`
- `org.netbeans.modules.java.source.query.CommentHandler`
- `org.netbeans.modules.java.source.query.CommentSet`
- `org.netbeans.modules.java.source.transform.ImmutableDocTreeTranslator`
- `org.netbeans.modules.java.source.transform.ImmutableTreeTranslator`
- `org.netbeans.modules.java.source.transform.TreeDuplicator`
- `org.netbeans.api.java.source.support.ErrorAwareTreeScanner`
- `org.netbeans.api.java.source.support.ErrorAwareTreePathScanner`
- `com.sun.source.util.DocTrees`
- `com.sun.source.util.Trees`
- `com.sun.source.util.TreePath`
- `com.sun.tools.javac.tree.TreeMaker`
- `com.sun.tools.javac.api.JavacTaskImpl`
- `com.sun.tools.javac.api.JavacTrees`
- `com.sun.tools.javac.tree.JCTree`
- `org.netbeans.modules.java.source.pretty.VeryPretty`
- `org.netbeans.modules.java.source.save.DiffUtilities`
- `org.netbeans.modules.java.source.save.PositionEstimator`
- `org.netbeans.modules.java.source.save.EstimatorFactory`
- `org.netbeans.modules.java.source.parsing.JavacParser`
- `org.netbeans.modules.java.source.parsing.FileObjects`
- `org.netbeans.modules.java.source.save.DiffContext`
- `org.netbeans.api.java.source.JavaSource`
- `org.netbeans.api.java.source.ModificationResult`
- `org.netbeans.api.java.source.ClasspathInfo`
- `org.netbeans.api.java.source.Task`
- `org.openide.filesystems.FileObject`
- `org.openide.filesystems.FileUtil`
- `org.openide.filesystems.FileSystem`
- `org.netbeans.api.java.source.CodeStyle`
- `org.netbeans.api.java.source.CompilationInfo`
- `org.netbeans.api.java.source.CompilationController`
- `uno.anahata.asi.nb.tools.java.JavaSourceUtils`

## Turn 288: Analysis of V3 Test Results and The Shift to V4

### The Remaining Bugs in V3 (Pure AST)
After successfully executing the comprehensive test suite, we analyzed the final output and discovered two critical flaws stemming from NetBeans' code generation engines (`CasualDiff` and `VeryPretty`):

1. **The Javadoc Erasure Bug (Test 5):** 
   In Test 3, `riskyMethod` was inserted without a Javadoc. In Test 5, we issued an `UPDATE` intent strictly to add a Javadoc. The AST parsed perfectly and `CommentHandlerService` attached the Javadoc to the tree. However, in the final output, the Javadoc was missing! 
   *Root Cause:* `CasualDiff.diffMethodDef` checks if the method modifiers changed. Because they didn't, it assumed the preamble was identical and used `copyTo(...)` to hard-copy the raw text from the original file, completely ignoring our newly attached Javadoc AST node.

2. **The Method Body Whitespace Erasure (Test 10):**
   In Test 10, we updated a method body and explicitly provided intentional blank lines. In the final output, the blank lines were collapsed into single newlines.
   *Root Cause:* When a completely new AST block is provided, `CasualDiff` delegates to `VeryPretty` to format it from scratch. `VeryPretty` ruthlessly applies the IDE's `CodeStyle` (which strips excessive blank lines inside method bodies), thereby destroying the LLM's intentional formatting.

### The "V4 Katana Sword" Architecture: AST-Guided Text Replacement
Because these limitations are hardcoded features of the IDE's code generator, we cannot achieve 100% formatting fidelity using `WorkingCopy.rewrite`. 
To forge the true Katana Sword, `BatchCodeRefiner` will pivot to **AST-Guided Text Replacement**.

**How V4 works:**
1. We use a read-only AST pass (`JavaSource.runUserActionTask`) to extract the exact character offsets (`startPos`, `endPos`, and `docStartPos`) of members.
2. We construct precise text replacement strings (e.g., extracting the original declaration and prepending the new Javadoc).
3. We apply these replacements from bottom-to-top directly onto the raw file string.

This hybrid approach leverages the 100% targeting accuracy of the AST with the 100% formatting fidelity of raw text splicing.

## Turn 301: V4 Auto-Indentation & The "public" Glitch
When transitioning to V4's raw text splicing, we assumed responsibility for horizontal whitespace (indentation) since we bypassed `VeryPretty`. 
Our initial approach attempted to capture the indentation by taking the substring from the previous newline up to the AST's `startPos`. However, we discovered that `SourcePositions.getStartPosition()` can sometimes point *after* modifiers (e.g., pointing to `class` instead of `public`). This caused the text-slicer to extract `"public "` as the indentation string, leading to catastrophic formatting corruption (e.g., `public public static class`).

**The Fix:** We implemented a strict, intelligent `baseIndent` scanner in `CodeRefinementIntent.java`. It starts at the previous newline and scans forward, strictly accepting only space (` `) and tab (`\t`) characters. It then splits the LLM's raw `body` string and prepends this computed `baseIndent` to every line, seamlessly formatting the injected code without corrupting the surrounding AST text.

## Turn 306: Perfecting V4 Formatting Math
While the V4 text splicing worked, three subtle math bugs caused formatting quirks:
1. **Zero Indent Bug:** When inserting into an "empty" class, `members.isEmpty()` returned false because Javac always generates a hidden synthetic default constructor `<init>`. Our code incorrectly read the indentation of this synthetic constructor (which is zero). **Fix:** We filter out synthetic members using `TreeUtilities.isSynthetic()` before calculating indentation.
2. **Orphaned Anchor Bug:** When inserting `BEFORE` an anchor, `insertOffset` was exactly at the anchor's first character, pushing the existing indentation to the right. **Fix:** For `BEFORE` inserts, `insertOffset` is moved backwards to the preceding newline, inserting the new code *above* the existing indentation.
3. **Glued Braces Bug:** When deleting a member, our bounds logic was consuming *both* the preceding and trailing newlines, destroying the blank line that separated the class brace from the next member. **Fix:** Deletion bounds are now constrained to consume exactly one newline.

## Turn 302: V4 Architecture Stabilized
With the "public" glitch resolved, the V4 "AST-Guided Text Replacement" architecture successfully passed the comprehensive test suite (all 11 test cases). 
- **Intelligent Auto-Indentation:** The `baseIndent` scanner perfectly formats inserted member bodies, preserving LLM-provided blank lines while conforming to the surrounding AST's indentation level.
- **Javadoc and Modifier Preservation:** The AST read-only pass correctly identifies exact boundaries of existing Javadocs and declarations, allowing seamless in-place updates without triggering NetBeans' `CasualDiff` hard-copying bugs.
- **Generics and Imports:** Surgical text insertion combined with explicit `importsToAdd` resolutions fully bypasses the `CasualDiff` NPE related to `GeneratorUtilities.importFQNs`.

## Turn 286: The Limits of CasualDiff and the V4 Architecture

**The Role of VeryPretty and CasualDiff:**
- `VeryPretty` is a code formatter. Its job is to take an AST node and print it as text, strictly enforcing the IDE's `CodeStyle`.
- `CasualDiff` is a diff engine. Its job is to compare the old AST with the new AST and generate surgical text replacements, preserving the original text (and its custom formatting/whitespace) for any nodes that did not change.

**The Method Body Whitespace Issue:**
NetBeans stores blank lines as `Comment.Style.WHITESPACE` attached to AST nodes via `CommentHandlerService`. However, when we supply a newly parsed AST node for a method body, `CasualDiff` delegates to `VeryPretty` to format it from scratch. `VeryPretty` strictly enforces the IDE's `CodeStyle` (which typically strips excessive blank lines inside method bodies), thereby destroying the LLM's intentional blank lines.

**The Javadoc Update Bug (NetBeans Bug #2 Confirmed):**
When updating a Javadoc via `wc.rewrite(member, oldDoc, newDoc)`, the AST correctly registers the change in `tree2Doc`. However, `CasualDiff.diffMethodDef` has an optimization: if the method's modifiers (e.g., `public static`) haven't changed, it assumes the entire preamble is identical and uses `copyTo(...)` to hard-copy the raw text from the original file. This completely ignores the `tree2Doc` mapping and the new Javadoc is never printed.

**The Resolution: AST-Guided Text Replacement (V4):**
To achieve 100% targeting accuracy and 100% formatting fidelity, `BatchCodeRefiner` will abandon `WorkingCopy.rewrite`. Instead, it will use a read-only AST pass (`runUserActionTask`) to extract the exact character offsets (`startPos`, `endPos`) of members, blocks, and Javadocs. It will then construct surgical text replacements and apply them from bottom-to-top (descending offset order), completely bypassing the bugs and formatting constraints of `CasualDiff` and `VeryPretty`.

## Turn 310: Complete Context Acquisition & CodeStyle Integration
To ensure the V4 math respects all user formatting rules (such as blank lines between members), we acquired the final missing NetBeans AST API files into our context:
- `org.netbeans.api.java.source.CodeStyle`
- `org.netbeans.api.java.source.CompilationInfo`
- `org.netbeans.api.java.source.CompilationController`
- `uno.anahata.asi.nb.tools.java.JavaSourceUtils` (our own utility!)

With these in context, we determined that **Yes, V4 officially bypasses all NetBeans formatting rules (VeryPretty and CasualDiff) for the exact string it splices.** NetBeans no longer dictates our internal method spacing. 
However, to keep the output beautiful, we query `CodeStyle.getDefault(doc)` manually inside `CodeRefinementIntent.java`. We dynamically calculate `blankLinesBefore` and `blankLinesAfter` based on the user's IDE preferences and intelligently inject exact `\n` characters around our `INSERT` splices by counting the existing surrounding whitespace! 

We also fixed the final math bug in `MOVE`/`DELETE` where the anchor `insertOffset` was lacking the same backward-scan for indentation that `INSERT` used, which caused shifted alignment when moving members.

## Turn 323: Enum Constant Bounds Discovery
While applying Javadocs to the AST components in `CodeRefinementIntent.java`, we discovered a critical edge case in the `Javac` AST mapping. 
For Enum Constants without bodies or arguments (e.g., `INSERT,`), `SourcePositions.getEndPosition()` returns `-1`.
Because the V4 text-splicing architecture relies heavily on precise character offsets, this `-1` propagated through the string slicing math and threw a `StringIndexOutOfBoundsException` (Range [1246, -1) out of bounds), rejecting the batch.

**The Fix:** We implemented an active forward-scanner fallback in `CodeRefinementIntent.java`. If `endPos < 0` is detected, V4 scans forward from `startPos` until it encounters a logical terminator (`,`, `;`, `{`, `=`, `(`, or `)`). This guarantees robust bound calculation even for AST nodes lacking compiler-defined end positions.

## Turn 328: Synthetic Initializer Blind Spot
During Test 13 (Updating an Enum Constant Javadoc), we encountered another `StringIndexOutOfBoundsException` (Range [1360, -1)). 
**Root Cause:** In the Javac AST, Enum Constants are represented as `VariableTree` nodes and are always assigned a synthetic `JCNewClass` initializer, even if they have no explicit arguments. While `SourcePositions` returns a valid start position (the start of the constant), it returns `-1` for the end position if there are no explicit arguments. Our `UPDATE` logic checked `if (initStart >= 0)` (which evaluated to true because the start position was valid) and blindly used the `-1` end position for substring math.
**The Fix:** We updated `CodeRefinementIntent` to verify both `initStart >= 0 && initEnd >= 0` before assuming the initializer is explicitly declared in the source text. If it lacks a valid end position, it safely falls back to the standard variable extraction logic.

## Turn 354: Field Initializer Support for V4 INSERT
During field insertion via the `body` parameter, V4's `INSERT` logic mistakenly assumed all members with a `body` were methods or classes and wrapped the initializer in `{ }` braces. 
**The Fix:** Added heuristic detection (`!isMethod && !isClass && !isBlock`) in `CodeRefinementIntent.java` to detect field declarations during `INSERT`. If a field is detected, the `body` string is appended as an initializer using ` = ` instead of `{ }`.

## Turn 378 & 379: The Truth About importFQNs
Following the stabilization of the V4 AST-Guided architecture, we launched a mission to investigate the `GeneratorUtilities.importFQNs` mystery. Did it actually have a bug that crashed `CasualDiff` when handling complex generics, or was it a phantom side-effect of our manual AST mutilation in V3? 

We constructed a bulletproof test case using an in-memory `JavaSource` bound to the real project's `ClasspathInfo`. 

**The Verdict:** `importFQNs` successfully resolved and shortened complex generic FQNs (`<T extends java.lang.Number & java.io.Serializable>`) without throwing a `NullPointerException` in `CasualDiff`! 
The historical crashes were caused by our V3 architecture handing `importFQNs` a manually hacked, un-attributed AST. Because V4 uses pure text-splicing, we can now safely use `importFQNs` on an isolated memory file to restore the `optimize=true` functionality!

Go Anahata!
