/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java.coderefiner;

import lombok.Getter;
import lombok.SneakyThrows;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Base Test Class for AST refinement in IntelliJ IDEA (mirrors NetBeans SmallTestClass).
 *
 * @author anahata
 */
@ToString
@Slf4j
public abstract class SmallTestClass {

    /**
     * Inner Class Doc.
     */
    public static class InnerTest {

        private String b;

        private final String description = "123";

        public void foo() {
        }

    }

    /**
     * This method is extremely risky.
     */
    @SneakyThrows
    public void riskyMethod() {
        System.out.println("A");

        // Space!
        System.out.println("B");
    }

    /**
     * Processes generic numbers.
     */
    public <T extends Number, R> List<R> processGenerics(Map<String, T> input) {
        List<R> list = new ArrayList<>();

        // Look at this beautiful blank line!
        return list;
    }

    public static class GenericInner<X, Y> {

        private X first;
        private Y second;
    }

    public void methodA() {
        System.out.println("A");
    }

    /**
     * Refined methodB with enhanced logging and diagnostics.
     */
    public void methodB() {
        log.info("Executing refined methodB in IntelliJ AST");
        System.out.println("B - Refined by Anahata ASI");
    }

    public void methodC() {
        System.out.println("C");
    }

    /**
     * Demonstrates an inserted method via IntelliJ BatchCodeRefiner AST.
     *
     * @return confirmation message.
     */
    public String executeRefinedBatchOperation() {
        log.info("Batch refinement operation successfully executed.");
        return "Refinement Success";
    }

    /**
     * A test enum.
     */
    public enum TestEnum {
        /**
         * First doc
         */
        FIRST,
        /**
         * Second doc
         */
        SECOND,
        /**
         * The third constant.
         */
        THIRD;
    }

    @Getter
    public enum TestEnum2 {
        FIRST("first"),
        /**
         * Second doc
         */
        SECOND("second"),
        /**
         * The third constant with args.
         */
        THIRD("third");

        /**
         * First doc
         */
        private TestEnum2(String displayValue) {
            this.displayValue = displayValue;
        }

        String displayValue;
    }

    public void testMethodWithEnum(TestEnum val) {
        System.out.println("Updated: " + val);
    }

    /**
     * Gets the source files for types specified by their fully qualified names
     * and registers them as resources.
     */
    public void complexStringMethod() {
        String s = "cat.eat.the.dog";
        String msg = "Invalid member FQN: Type.member or Type$NestedType";
        System.out.println(s + msg);
    }

    public void methodWithFqns() {
        AbstractCollection c = null;
        ConcurrentHashMap<String, Object> map = new ConcurrentHashMap<>();
        System.out.println(c);
    }

    public void testSlf4jLogging() {
        Collections.emptyList();
        log.info("Testing log.info {}", "arg");
        log.warn("Testing log.warn {}", "arg2");
    }

    public abstract void abstractTarget();
}
