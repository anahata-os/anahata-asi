package uno.anahata.asi.nb;


public class JavadocTestSubject {

    
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
     *
     * @param name
     * @deprecated
     */
    @Deprecated(since = "1.0", forRemoval = true)
    public void setName(String name) {
        this.name = name;
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

}
