package uno.anahata.asi.nb;

public class CodeRefinerTestSubject<T extends java.lang.Number & java.io.Serializable, R> {
    private java.util.Map<java.lang.String, java.util.List<T>> complexMap;
    
    @lombok.SneakyThrows
    public <X> java.util.Set<R> process(java.util.function.Function<T, R> mapper, java.util.List<X> extra) {
        java.util.function.Supplier<java.util.List<java.lang.Integer>> supplier = java.util.ArrayList<java.lang.Integer>::new;
        return new java.util.HashSet<R>();
    }
    
    public record ComplexRecord<Z>(java.util.Optional<Z> opt) {}
}
