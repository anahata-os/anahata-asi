package uno.anahata.asi.nb.tools.java.coderefiner;

import java.io.File;
import java.util.LinkedList;

/**
 * Base Test Class for AST.
 */
public class SmallTestClass {

    /**
     * Inner Class Doc.
     */
    public static class InnerTest {
        private String b;

        private final String description = "123";
        public void foo() {}
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
        /** First doc */
        FIRST,
        /** Second doc */
        SECOND,
        /**
         * The third constant.
         */
        THIRD;
    }

    @lombok.Getter
    public enum TestEnum2 {
        FIRST ("first"),
        /** Second doc */
        SECOND ("second"),
        /**
         * The third constant with args.
         */
        THIRD ("third");

        /** First doc */
        private TestEnum2(String displayValue) {
        this.displayValue = displayValue;
        }
        String displayValue;
    }

    public void testMethodWithEnum(TestEnum val) {
        System.out.println("Updated: " + val);
    }
}
