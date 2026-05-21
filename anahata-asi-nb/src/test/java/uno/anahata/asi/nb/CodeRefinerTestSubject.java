package uno.anahata.asi.nb;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;
import lombok.SneakyThrows;

public class CodeRefinerTestSubject<T extends Number, 
      Serializabl, R> {
    private Map<String, List<T>> complexMap;
    
    @SneakyThrows
    public <X> Set<R> process(Function<T, R> mapper, List<X> extra) {
        Supplier<List<Integer>> supplier = ArrayList<Integer>::new;
        return new HashSet<R>();
    }
    
    public record ComplexRecord<Z>(Optional<Z> opt) {}
}
