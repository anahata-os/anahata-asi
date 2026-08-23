package uno.anahata.asi.nb.tools.java.coderefiner;

import java.util.AbstractCollection;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import lombok.Getter;
import lombok.SneakyThrows;

/**
 * Base Test Class for AST (Updated with ToString).
 */
@lombok.ToString
public class SmallTestClass {

    /**
     * Inner Class Doc.
     */
    public static class InnerTest {

        private String b;

        private final String description = "123";
        public void foo() {
        }
        @Deprecated
        public void bar() {
            System.out.println("bar");
        }
    }

    /**
     * This method is extremely risky.
     */
    @lombok.SneakyThrows
    public void riskyMethod() {
        System.out.println("A");

        // Space!
        System.out.println("B");
    }

    /**
     * Processes generic numbers.
     */
    public <T extends Number, R> java.util.List<R> processGenerics(java.util.Map<String, T> input) {
        java.util.List<R> list = new java.util.ArrayList<>();

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

    public void methodB() {
        System.out.println("B");
    }

    public void methodC() {
        System.out.println("C");
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

    @lombok.Getter
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
}
