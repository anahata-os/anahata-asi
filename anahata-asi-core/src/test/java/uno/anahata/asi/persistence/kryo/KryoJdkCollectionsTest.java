/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.persistence.kryo;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Unit tests verifying that all JDK immutable, unmodifiable, singleton, and empty collection types
 * safely round-trip through Kryo serialization and deserialization via {@link KryoUtils}.
 *
 * @author anahata
 */
public class KryoJdkCollectionsTest {

    /**
     * Test record containing various JDK collection types.
     */
    public record SampleRecord(
            String id,
            List<String> list,
            Set<Integer> set,
            Map<String, String> map
    ) {}

    @Test
    public void testImmutableListSerialization() {
        List<String> empty = List.of();
        List<String> clonedEmpty = KryoUtils.clone(empty);
        Assertions.assertEquals(empty, clonedEmpty);

        List<String> single = List.of("one");
        List<String> clonedSingle = KryoUtils.clone(single);
        Assertions.assertEquals(single, clonedSingle);

        List<String> multiple = List.of("a", "b", "c", "d");
        List<String> clonedMultiple = KryoUtils.clone(multiple);
        Assertions.assertEquals(multiple, clonedMultiple);

        List<String> subList = multiple.subList(1, 3);
        List<String> clonedSubList = KryoUtils.clone(subList);
        Assertions.assertEquals(subList, clonedSubList);
    }

    @Test
    public void testImmutableSetSerialization() {
        Set<String> empty = Set.of();
        Set<String> clonedEmpty = KryoUtils.clone(empty);
        Assertions.assertEquals(empty, clonedEmpty);

        Set<String> single = Set.of("apple");
        Set<String> clonedSingle = KryoUtils.clone(single);
        Assertions.assertEquals(single, clonedSingle);

        Set<String> multiple = Set.of("x", "y", "z");
        Set<String> clonedMultiple = KryoUtils.clone(multiple);
        Assertions.assertEquals(multiple, clonedMultiple);
    }

    @Test
    public void testImmutableMapSerialization() {
        Map<String, Integer> empty = Map.of();
        Map<String, Integer> clonedEmpty = KryoUtils.clone(empty);
        Assertions.assertEquals(empty, clonedEmpty);

        Map<String, Integer> single = Map.of("key", 100);
        Map<String, Integer> clonedSingle = KryoUtils.clone(single);
        Assertions.assertEquals(single, clonedSingle);

        Map<String, Integer> multiple = Map.of("k1", 1, "k2", 2, "k3", 3);
        Map<String, Integer> clonedMultiple = KryoUtils.clone(multiple);
        Assertions.assertEquals(multiple, clonedMultiple);
    }

    @Test
    public void testCollectionsSingletonSerialization() {
        List<String> singletonList = Collections.singletonList("only");
        List<String> clonedList = KryoUtils.clone(singletonList);
        Assertions.assertEquals(singletonList, clonedList);

        Set<String> singletonSet = Collections.singleton("onlySet");
        Set<String> clonedSet = KryoUtils.clone(singletonSet);
        Assertions.assertEquals(singletonSet, clonedSet);

        Map<String, String> singletonMap = Collections.singletonMap("k", "v");
        Map<String, String> clonedMap = KryoUtils.clone(singletonMap);
        Assertions.assertEquals(singletonMap, clonedMap);
    }

    @Test
    public void testCollectionsEmptySerialization() {
        Assertions.assertEquals(Collections.emptyList(), KryoUtils.clone(Collections.emptyList()));
        Assertions.assertEquals(Collections.emptySet(), KryoUtils.clone(Collections.emptySet()));
        Assertions.assertEquals(Collections.emptyMap(), KryoUtils.clone(Collections.emptyMap()));
        Assertions.assertEquals(Collections.emptyNavigableSet(), KryoUtils.clone(Collections.emptyNavigableSet()));
        Assertions.assertEquals(Collections.emptyNavigableMap(), KryoUtils.clone(Collections.emptyNavigableMap()));
        Assertions.assertEquals(Collections.emptySortedSet(), KryoUtils.clone(Collections.emptySortedSet()));
        Assertions.assertEquals(Collections.emptySortedMap(), KryoUtils.clone(Collections.emptySortedMap()));
    }

    @Test
    public void testUnmodifiableWrappersSerialization() {
        List<String> rawList = Arrays.asList("alpha", "beta");
        List<String> unmodifiableList = Collections.unmodifiableList(rawList);
        List<String> clonedList = KryoUtils.clone(unmodifiableList);
        Assertions.assertEquals(unmodifiableList, clonedList);

        Set<Integer> rawSet = new HashSet<>(Arrays.asList(1, 2, 3));
        Set<Integer> unmodifiableSet = Collections.unmodifiableSet(rawSet);
        Set<Integer> clonedSet = KryoUtils.clone(unmodifiableSet);
        Assertions.assertEquals(unmodifiableSet, clonedSet);

        Map<String, String> rawMap = new HashMap<>();
        rawMap.put("foo", "bar");
        Map<String, String> unmodifiableMap = Collections.unmodifiableMap(rawMap);
        Map<String, String> clonedMap = KryoUtils.clone(unmodifiableMap);
        Assertions.assertEquals(unmodifiableMap, clonedMap);
    }

    @Test
    public void testRecordHoldingImmutableCollections() {
        SampleRecord record = new SampleRecord(
                "rec-108",
                List.of("alpha", "beta"),
                Set.of(1, 2, 3),
                Map.of("lang", "Java", "version", "25")
        );

        SampleRecord cloned = KryoUtils.clone(record);
        Assertions.assertNotNull(cloned);
        Assertions.assertEquals(record.id(), cloned.id());
        Assertions.assertEquals(record.list(), cloned.list());
        Assertions.assertEquals(record.set(), cloned.set());
        Assertions.assertEquals(record.map(), cloned.map());
    }
}
