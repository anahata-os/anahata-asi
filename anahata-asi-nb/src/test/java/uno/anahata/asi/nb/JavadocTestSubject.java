package uno.anahata.asi.nb;


/**
 * A high-fidelity test subject used to verify Javadoc injection and
 * replacement logic within the {@code CodeRefiner} toolkit.
 * <p>
 * This class contains varied member types and visibility levels to ensure
 * that documentation can be surgically updated without affecting
 * neighboring annotations or code logic.
 * </p>
 */
public class JavadocTestSubject {

    
    /**
     * Computes the length of the provided prefix.
     * Used to verify that methods with suppress-warning annotations can
     * still be correctly documented.
     * @param prefix The string to measure.
     * @return The length of the prefix.
     */
    @SuppressWarnings("unchecked")
    public int theBeginning(String prefix) {
        return prefix.length();
    }
    /**
     * This is a brand new Javadoc that should replace the old one while keeping the annotation.
     */
    private int id = 108;

    /**
     * The name of the test subject.
     */
    private String name;

    /**
     * Updates the name of the test subject.
     * @param name The new name to set.
     * @deprecated This method is retained only for testing deprecation-handling
     * logic in the refiner. Use the direct field access for tests that
     * don't require annotation verification.
     */
    @Deprecated(since = "1.0", forRemoval = true)
    public void setName(String name) {
        this.name = name;
    }


    /**
     * Internal test class used to verify documentation of nested static
     * member types.
     */
    @SuppressWarnings("all")
    @Deprecated
    public static class InnerSubject {

        /**
         * A nested inner class for deep testing.
         */
        public static class NestedInnerSubject {
        }
    }

}
