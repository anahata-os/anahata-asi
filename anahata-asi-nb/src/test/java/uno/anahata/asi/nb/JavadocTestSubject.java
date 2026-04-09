package uno.anahata.asi.nb;

/**
 * A test subject for Javadoc manipulation.
 */
public class JavadocTestSubject {

    /**
     * This is the updated beginning.
     */
    public int theBeginning(String prefix) {
        return prefix.length();
    }

    /**
     * The name of the test subject.
     */
    private String name;

    /**
     * Gets the name.
     */
    @SuppressWarnings("unchecked")
    public String getName() {
        return name;
    }

    /**
     * Sets the name.
     */
    @Deprecated(since = "1.0", forRemoval = true)
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Clears the name field.
     */
    public void clearName() {
        this.name = null;
    }

    @SuppressWarnings("all")
    @Deprecated
    public static class InnerSubject {

        /**
         * A nested inner class for deep testing.
         */
        public static class NestedInnerSubject {
        }
    }

    /**
     * The final method of the class.
     */
    public void theFinalCountdown() {
        // Last method logic
        return;
    }
}
