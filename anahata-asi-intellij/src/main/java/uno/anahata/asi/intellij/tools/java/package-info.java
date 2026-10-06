/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Java source analysis, code model exploration, and semantic manipulation toolkits for IntelliJ IDEA.
 * <p>
 * Leverages IntelliJ's Program Structure Interface (PSI), code-style managers, and compiler order enumerators:
 * </p>
 * <ul>
 *   <li>{@link CodeModel}: Universal JVM Code Model for searching types, listing members,
 *       retrieving source declarations and Javadocs, and exploring recursive inheritance hierarchies across Java, Kotlin, Groovy, and Scala.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.java.CodeRefiner}: For structural import management, code-style reformatting,
 *       and adding annotations directly onto the live PSI tree.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.java.BatchCodeRefiner}: The V4 AST-guided batch refinement engine for inserting,
 *       updating, deleting, and moving whole class members atomically with unified diff generation.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.java.IntellijHints}: For inspecting on-the-fly code analysis highlights (inspections and
 *       annotators) and applying quick-fixes programmatically.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.java.IntellijJava}: For compiling modular classes and executing dynamic scripts
 *       against an open project's classpath and configured SDK.</li>
 *   <li>Universal CodeModel2 DTOs: {@link uno.anahata.asi.intellij.tools.java.codemodel.TypeItem}, {@link uno.anahata.asi.intellij.tools.java.codemodel.MemberItem},
 *       {@link uno.anahata.asi.intellij.tools.java.codemodel.MemberPage}, and {@link uno.anahata.asi.intellij.tools.java.codemodel.HierarchyNode}.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.java;

import uno.anahata.asi.intellij.tools.java.codemodel.CodeModel;