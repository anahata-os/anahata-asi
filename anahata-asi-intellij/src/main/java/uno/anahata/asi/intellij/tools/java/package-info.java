/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Java source analysis, code model exploration, and semantic manipulation toolkits for IntelliJ IDEA.
 * <p>
 * Leverages IntelliJ's Program Structure Interface (PSI), code-style managers, and compiler order enumerators:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.tools.java.CodeModel}: For searching types, listing members, retrieving Javadocs,
 *       and exploring recursive subtype and supertype inheritance hierarchies.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.java.CodeRefiner}: For structural import management, code-style reformatting,
 *       and adding annotations directly onto the live PSI tree.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.java.BatchCodeRefiner}: The V4 AST-guided batch refinement engine for inserting,
 *       updating, deleting, and moving whole class members atomically with unified diff generation.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.java.IntellijHints}: For inspecting on-the-fly code analysis highlights (inspections and
 *       annotators) and applying quick-fixes programmatically.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.java.IntellijJava}: For compiling modular classes and executing dynamic scripts
 *       against an open project's classpath and configured SDK.</li>
 *   <li>Keychain DTOs: {@link uno.anahata.asi.intellij.tools.java.JavaType}, {@link uno.anahata.asi.intellij.tools.java.JavaMember},
 *       {@link uno.anahata.asi.intellij.tools.java.JavaMemberPage}, and {@link uno.anahata.asi.intellij.tools.java.JavaHierarchyNode}.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.java;
