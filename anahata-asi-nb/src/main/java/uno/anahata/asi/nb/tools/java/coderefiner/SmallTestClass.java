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
        private String b;
        private String description = "123";
        public void foo() {}
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
     */
    public <T extends Number, R> List<R> processGenerics(Map<String, T> input) {
        List<R> list = new ArrayList<>();
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
        FIRST,
        //comment
        SECOND,
        THIRD;
    }
}
