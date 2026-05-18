/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
/**
 * The V4 AST-Guided Structural Refinement Engine.
 * <p>
 * This package contains the domain model for structural code modifications. 
 * Unlike text-based search and replace, this engine leverages the NetBeans 
 * compiler API (Javac) to identify exact AST nodes, calculate their 
 * physical bounds in the source file, and perform surgical replacements. 
 * This ensures that comments, indentation, and unrelated code remain 
 * perfectly preserved during complex refactorings.
 * </p>
 * 
 * @author anahata
 */
package uno.anahata.asi.nb.tools.java.coderefiner;
