/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.persistence.kryo;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.Serializer;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;

/**
 * Provides dedicated Kryo serializers and registration helpers for JDK collection types.
 * <p>
 * Specifically handles JDK 9+ immutable collections ({@link List#of()}, {@link Set#of()}, {@link Map#of()}),
 * {@link java.util.Arrays#asList(Object[])}, {@link java.util.Collections} unmodifiable wrappers,
 * singletons, and empty collections which otherwise throw {@link UnsupportedOperationException}
 * during Kryo deserialization due to mutation in default serializers.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class JdkCollectionsSerializers {

    /**
     * Registers all JDK collection serializers with the given Kryo instance.
     *
     * @param kryo The Kryo instance to register serializers with.
     */
    public static void register(Kryo kryo) {
        // 1. Immutable Collections (JDK 9+ List.of, Set.of, Map.of, Map.ofEntries)
        Serializer<List<?>> immutableListSerializer = new ImmutableListSerializer();
        registerIfNotNull(kryo, List.of().getClass(), immutableListSerializer);
        registerIfNotNull(kryo, List.of(1).getClass(), immutableListSerializer);
        registerIfNotNull(kryo, List.of(1, 2).getClass(), immutableListSerializer);
        registerIfNotNull(kryo, List.of(1, 2, 3).getClass(), immutableListSerializer);
        registerIfNotNull(kryo, List.of(1, 2, 3).subList(0, 1).getClass(), immutableListSerializer);

        Serializer<Set<?>> immutableSetSerializer = new ImmutableSetSerializer();
        registerIfNotNull(kryo, Set.of().getClass(), immutableSetSerializer);
        registerIfNotNull(kryo, Set.of(1).getClass(), immutableSetSerializer);
        registerIfNotNull(kryo, Set.of(1, 2).getClass(), immutableSetSerializer);
        registerIfNotNull(kryo, Set.of(1, 2, 3).getClass(), immutableSetSerializer);

        Serializer<Map<?, ?>> immutableMapSerializer = new ImmutableMapSerializer();
        registerIfNotNull(kryo, Map.of().getClass(), immutableMapSerializer);
        registerIfNotNull(kryo, Map.of(1, 1).getClass(), immutableMapSerializer);
        registerIfNotNull(kryo, Map.of(1, 1, 2, 2).getClass(), immutableMapSerializer);
        registerIfNotNull(kryo, Map.of(1, 1, 2, 2, 3, 3).getClass(), immutableMapSerializer);

        // 2. Arrays.asList fixed-size list
        registerIfNotNull(kryo, Arrays.asList(1, 2).getClass(), new ArraysListSerializer());

        // 3. java.util.Collections singletons
        registerIfNotNull(kryo, Collections.singletonList(1).getClass(), new SingletonListSerializer());
        registerIfNotNull(kryo, Collections.singleton(1).getClass(), new SingletonSetSerializer());
        registerIfNotNull(kryo, Collections.singletonMap(1, 1).getClass(), new SingletonMapSerializer());

        // 4. java.util.Collections empties
        registerIfNotNull(kryo, Collections.emptyList().getClass(), new EmptyListSerializer());
        registerIfNotNull(kryo, Collections.emptySet().getClass(), new EmptySetSerializer());
        registerIfNotNull(kryo, Collections.emptyMap().getClass(), new EmptyMapSerializer());
        registerIfNotNull(kryo, Collections.emptyNavigableSet().getClass(), new EmptyNavigableSetSerializer());
        registerIfNotNull(kryo, Collections.emptyNavigableMap().getClass(), new EmptyNavigableMapSerializer());
        registerIfNotNull(kryo, Collections.emptySortedSet().getClass(), new EmptySortedSetSerializer());
        registerIfNotNull(kryo, Collections.emptySortedMap().getClass(), new EmptySortedMapSerializer());

        // 5. java.util.Collections unmodifiable wrappers
        Serializer<List<?>> unmodifiableListSerializer = new UnmodifiableListSerializer();
        registerIfNotNull(kryo, Collections.unmodifiableList(new ArrayList<>()).getClass(), unmodifiableListSerializer);
        registerIfNotNull(kryo, Collections.unmodifiableList(new java.util.LinkedList<>()).getClass(), unmodifiableListSerializer);

        Serializer<Set<?>> unmodifiableSetSerializer = new UnmodifiableSetSerializer();
        registerIfNotNull(kryo, Collections.unmodifiableSet(new HashSet<>()).getClass(), unmodifiableSetSerializer);

        Serializer<Map<?, ?>> unmodifiableMapSerializer = new UnmodifiableMapSerializer();
        registerIfNotNull(kryo, Collections.unmodifiableMap(new HashMap<>()).getClass(), unmodifiableMapSerializer);
    }

    /**
     * Safely registers a class with Kryo if the class reference is non-null and not already registered.
     *
     * @param kryo The Kryo instance.
     * @param clazz The class to register.
     * @param serializer The serializer to associate.
     */
    private static void registerIfNotNull(Kryo kryo, Class<?> clazz, Serializer<?> serializer) {
        if (clazz != null && kryo != null && serializer != null) {
            kryo.register(clazz, serializer);
        }
    }

    /**
     * Kryo serializer for JDK 9+ {@link List#of()} and immutable lists.
     */
    public static class ImmutableListSerializer extends Serializer<List<?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public ImmutableListSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the size followed by each list element.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, List<?> list) {
            output.writeInt(list.size(), true);
            for (Object item : list) {
                kryo.writeClassAndObject(output, item);
            }
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes elements and reconstructs an immutable list preserving contract fidelity.</p>
         */
        @Override
        public List<?> read(Kryo kryo, Input input, Class<? extends List<?>> type) {
            int size = input.readInt(true);
            if (size == 0) {
                return List.of();
            }
            Object[] array = new Object[size];
            boolean hasNull = false;
            for (int i = 0; i < size; i++) {
                array[i] = kryo.readClassAndObject(input);
                if (array[i] == null) {
                    hasNull = true;
                }
            }
            if (hasNull) {
                return Collections.unmodifiableList(Arrays.asList(array));
            }
            return List.of(array);
        }
    }

    /**
     * Kryo serializer for JDK 9+ {@link Set#of()} and immutable sets.
     */
    public static class ImmutableSetSerializer extends Serializer<Set<?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public ImmutableSetSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the size followed by each set element.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, Set<?> set) {
            output.writeInt(set.size(), true);
            for (Object item : set) {
                kryo.writeClassAndObject(output, item);
            }
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes elements and reconstructs an immutable set preserving contract fidelity.</p>
         */
        @Override
        public Set<?> read(Kryo kryo, Input input, Class<? extends Set<?>> type) {
            int size = input.readInt(true);
            if (size == 0) {
                return Set.of();
            }
            Object[] array = new Object[size];
            boolean hasNull = false;
            for (int i = 0; i < size; i++) {
                array[i] = kryo.readClassAndObject(input);
                if (array[i] == null) {
                    hasNull = true;
                }
            }
            if (hasNull) {
                Set<Object> s = new LinkedHashSet<>(Arrays.asList(array));
                return Collections.unmodifiableSet(s);
            }
            return Set.of(array);
        }
    }

    /**
     * Kryo serializer for JDK 9+ {@link Map#of()} and immutable maps.
     */
    public static class ImmutableMapSerializer extends Serializer<Map<?, ?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public ImmutableMapSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the size followed by key-value pairs.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, Map<?, ?> map) {
            output.writeInt(map.size(), true);
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                kryo.writeClassAndObject(output, entry.getKey());
                kryo.writeClassAndObject(output, entry.getValue());
            }
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes key-value pairs and reconstructs an immutable map.</p>
         */
        @Override
        public Map<?, ?> read(Kryo kryo, Input input, Class<? extends Map<?, ?>> type) {
            int size = input.readInt(true);
            if (size == 0) {
                return Map.of();
            }
            Map<Object, Object> temp = new LinkedHashMap<>(size);
            boolean hasNull = false;
            for (int i = 0; i < size; i++) {
                Object key = kryo.readClassAndObject(input);
                Object value = kryo.readClassAndObject(input);
                if (key == null || value == null) {
                    hasNull = true;
                }
                temp.put(key, value);
            }
            if (hasNull) {
                return Collections.unmodifiableMap(temp);
            }
            return Map.copyOf(temp);
        }
    }

    /**
     * Kryo serializer for {@link java.util.Arrays#asList(Object[])}.
     */
    public static class ArraysListSerializer extends Serializer<List<?>> {

        /**
         * Default constructor.
         */
        public ArraysListSerializer() {
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the size followed by each list element.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, List<?> list) {
            output.writeInt(list.size(), true);
            for (Object item : list) {
                kryo.writeClassAndObject(output, item);
            }
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes elements and returns an {@link Arrays#asList(Object[])} wrapper.</p>
         */
        @Override
        public List<?> read(Kryo kryo, Input input, Class<? extends List<?>> type) {
            int size = input.readInt(true);
            Object[] array = new Object[size];
            for (int i = 0; i < size; i++) {
                array[i] = kryo.readClassAndObject(input);
            }
            return Arrays.asList(array);
        }
    }

    /**
     * Kryo serializer for {@link Collections#singletonList(Object)}.
     */
    public static class SingletonListSerializer extends Serializer<List<?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public SingletonListSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the single element in the list.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, List<?> object) {
            kryo.writeClassAndObject(output, object.get(0));
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes the single element and reconstructs a singleton list.</p>
         */
        @Override
        public List<?> read(Kryo kryo, Input input, Class<? extends List<?>> type) {
            Object item = kryo.readClassAndObject(input);
            return Collections.singletonList(item);
        }
    }

    /**
     * Kryo serializer for {@link Collections#singleton(Object)}.
     */
    public static class SingletonSetSerializer extends Serializer<Set<?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public SingletonSetSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the single element in the set.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, Set<?> object) {
            kryo.writeClassAndObject(output, object.iterator().next());
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes the single element and reconstructs a singleton set.</p>
         */
        @Override
        public Set<?> read(Kryo kryo, Input input, Class<? extends Set<?>> type) {
            Object item = kryo.readClassAndObject(input);
            return Collections.singleton(item);
        }
    }

    /**
     * Kryo serializer for {@link Collections#singletonMap(Object, Object)}.
     */
    public static class SingletonMapSerializer extends Serializer<Map<?, ?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public SingletonMapSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the single key and value pair in the map.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, Map<?, ?> object) {
            Map.Entry<?, ?> entry = object.entrySet().iterator().next();
            kryo.writeClassAndObject(output, entry.getKey());
            kryo.writeClassAndObject(output, entry.getValue());
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes the key and value and reconstructs a singleton map.</p>
         */
        @Override
        public Map<?, ?> read(Kryo kryo, Input input, Class<? extends Map<?, ?>> type) {
            Object key = kryo.readClassAndObject(input);
            Object value = kryo.readClassAndObject(input);
            return Collections.singletonMap(key, value);
        }
    }

    /**
     * Kryo serializer for {@link Collections#emptyList()}.
     */
    public static class EmptyListSerializer extends Serializer<List<?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public EmptyListSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>No-op write as empty list holds no state.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, List<?> object) {
        }

        /**
         * {@inheritDoc}
         * <p>Returns singleton empty list instance.</p>
         */
        @Override
        public List<?> read(Kryo kryo, Input input, Class<? extends List<?>> type) {
            return Collections.emptyList();
        }
    }

    /**
     * Kryo serializer for {@link Collections#emptySet()}.
     */
    public static class EmptySetSerializer extends Serializer<Set<?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public EmptySetSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>No-op write as empty set holds no state.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, Set<?> object) {
        }

        /**
         * {@inheritDoc}
         * <p>Returns singleton empty set instance.</p>
         */
        @Override
        public Set<?> read(Kryo kryo, Input input, Class<? extends Set<?>> type) {
            return Collections.emptySet();
        }
    }

    /**
     * Kryo serializer for {@link Collections#emptyMap()}.
     */
    public static class EmptyMapSerializer extends Serializer<Map<?, ?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public EmptyMapSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>No-op write as empty map holds no state.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, Map<?, ?> object) {
        }

        /**
         * {@inheritDoc}
         * <p>Returns singleton empty map instance.</p>
         */
        @Override
        public Map<?, ?> read(Kryo kryo, Input input, Class<? extends Map<?, ?>> type) {
            return Collections.emptyMap();
        }
    }

    /**
     * Kryo serializer for {@link Collections#emptyNavigableSet()}.
     */
    public static class EmptyNavigableSetSerializer extends Serializer<java.util.NavigableSet<?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public EmptyNavigableSetSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>No-op write.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, java.util.NavigableSet<?> object) {
        }

        /**
         * {@inheritDoc}
         * <p>Returns singleton empty navigable set.</p>
         */
        @Override
        public java.util.NavigableSet<?> read(Kryo kryo, Input input, Class<? extends java.util.NavigableSet<?>> type) {
            return Collections.emptyNavigableSet();
        }
    }

    /**
     * Kryo serializer for {@link Collections#emptyNavigableMap()}.
     */
    public static class EmptyNavigableMapSerializer extends Serializer<java.util.NavigableMap<?, ?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public EmptyNavigableMapSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>No-op write.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, java.util.NavigableMap<?, ?> object) {
        }

        /**
         * {@inheritDoc}
         * <p>Returns singleton empty navigable map.</p>
         */
        @Override
        public java.util.NavigableMap<?, ?> read(Kryo kryo, Input input, Class<? extends java.util.NavigableMap<?, ?>> type) {
            return Collections.emptyNavigableMap();
        }
    }

    /**
     * Kryo serializer for {@link Collections#emptySortedSet()}.
     */
    public static class EmptySortedSetSerializer extends Serializer<java.util.SortedSet<?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public EmptySortedSetSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>No-op write.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, java.util.SortedSet<?> object) {
        }

        /**
         * {@inheritDoc}
         * <p>Returns singleton empty sorted set.</p>
         */
        @Override
        public java.util.SortedSet<?> read(Kryo kryo, Input input, Class<? extends java.util.SortedSet<?>> type) {
            return Collections.emptySortedSet();
        }
    }

    /**
     * Kryo serializer for {@link Collections#emptySortedMap()}.
     */
    public static class EmptySortedMapSerializer extends Serializer<java.util.SortedMap<?, ?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public EmptySortedMapSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>No-op write.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, java.util.SortedMap<?, ?> object) {
        }

        /**
         * {@inheritDoc}
         * <p>Returns singleton empty sorted map.</p>
         */
        @Override
        public java.util.SortedMap<?, ?> read(Kryo kryo, Input input, Class<? extends java.util.SortedMap<?, ?>> type) {
            return Collections.emptySortedMap();
        }
    }

    /**
     * Kryo serializer for {@link Collections#unmodifiableList(List)}.
     */
    public static class UnmodifiableListSerializer extends Serializer<List<?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public UnmodifiableListSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the size followed by each list element.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, List<?> list) {
            output.writeInt(list.size(), true);
            for (Object item : list) {
                kryo.writeClassAndObject(output, item);
            }
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes elements and returns an unmodifiable list wrapper.</p>
         */
        @Override
        public List<?> read(Kryo kryo, Input input, Class<? extends List<?>> type) {
            int size = input.readInt(true);
            List<Object> list = new ArrayList<>(size);
            for (int i = 0; i < size; i++) {
                list.add(kryo.readClassAndObject(input));
            }
            return Collections.unmodifiableList(list);
        }
    }

    /**
     * Kryo serializer for {@link Collections#unmodifiableSet(Set)}.
     */
    public static class UnmodifiableSetSerializer extends Serializer<Set<?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public UnmodifiableSetSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the size followed by each set element.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, Set<?> set) {
            output.writeInt(set.size(), true);
            for (Object item : set) {
                kryo.writeClassAndObject(output, item);
            }
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes elements and returns an unmodifiable set wrapper.</p>
         */
        @Override
        public Set<?> read(Kryo kryo, Input input, Class<? extends Set<?>> type) {
            int size = input.readInt(true);
            Set<Object> set = new LinkedHashSet<>(size);
            for (int i = 0; i < size; i++) {
                set.add(kryo.readClassAndObject(input));
            }
            return Collections.unmodifiableSet(set);
        }
    }

    /**
     * Kryo serializer for {@link Collections#unmodifiableMap(Map)}.
     */
    public static class UnmodifiableMapSerializer extends Serializer<Map<?, ?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public UnmodifiableMapSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the size followed by each map key-value entry.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, Map<?, ?> map) {
            output.writeInt(map.size(), true);
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                kryo.writeClassAndObject(output, entry.getKey());
                kryo.writeClassAndObject(output, entry.getValue());
            }
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes key-value entries and returns an unmodifiable map wrapper.</p>
         */
        @Override
        public Map<?, ?> read(Kryo kryo, Input input, Class<? extends Map<?, ?>> type) {
            int size = input.readInt(true);
            Map<Object, Object> map = new LinkedHashMap<>(size);
            for (int i = 0; i < size; i++) {
                Object key = kryo.readClassAndObject(input);
                Object value = kryo.readClassAndObject(input);
                map.put(key, value);
            }
            return Collections.unmodifiableMap(map);
        }
    }
}
