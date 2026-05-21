package uno.anahata.asi.nb.tools.java.coderefiner;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.SneakyThrows;


/**
 * Base Test Class for AST.
 */
public class SmallTestClass {

    /**
     * Inner Class Doc.
     */
    public static class InnerTest {
        /** Basic string property b. */
        private String b;
        /** Basic description string property with default value. */
        private String description = "123";
        /** Basic empty foo method. */
        public void foo() {}
        /** Deprecated bar printing helper method. */
        @Deprecated
        public void bar() {
            System.out.println("bar");
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
     *
     * @param <T> The numeric lower bound type parameter.
     * @param <R> The mapped return generic collection element type parameter.
     * @param input The generic number mapper container.
     * @return A list of processed generics.
     */
    public <T extends Number, R> List<R> processGenerics(Map<String, T> input) {
        List<R> list = new ArrayList<>();
        return list;
    }

    /**
     * Generic inner class representing coordinate/pair types.
     *
     * @param <X> First generic variable type.
     * @param <Y> Second generic variable type.
     */
    public static class GenericInner<X, Y> {
        /** First generic coordinate field. */
        private X first;
        /** Second generic coordinate field. */
        private Y second;
    }

    /** Printing wrapper method A. */
    public void methodA() {
        System.out.println("A");
    }

    /** Printing wrapper method B. */
    public void methodB() {
        System.out.println("B");
    }

    /** Printing wrapper method C. */
    public void methodC() {
        System.out.println("C");
    }

    /**
     * A test enum.
     */
    public enum TestEnum {
        /** The first index enum constant. */
        FIRST,
        //comment
        /** The second index enum constant. */
        SECOND,
        /** The third index enum constant. */
        THIRD;
    }
}
