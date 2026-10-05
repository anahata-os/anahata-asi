/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.persistence.kryo;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.Serializer;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import com.esotericsoftware.kryo.serializers.ImmutableCollectionsSerializers;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.RandomAccess;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
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
        // 1. Immutable Collections (delegated to Kryo's built-in ImmutableCollectionsSerializers)
        ImmutableCollectionsSerializers.registerSerializers(kryo);

        // 2. java.util.Collections empty navigable/sorted collections (not provided by Kryo)
        registerIfNotNull(kryo, Collections.emptyNavigableSet().getClass(), new EmptyNavigableSetSerializer());
        registerIfNotNull(kryo, Collections.emptyNavigableMap().getClass(), new EmptyNavigableMapSerializer());
        registerIfNotNull(kryo, Collections.emptySortedSet().getClass(), new EmptySortedSetSerializer());
        registerIfNotNull(kryo, Collections.emptySortedMap().getClass(), new EmptySortedMapSerializer());

        // 3. java.util.Collections unmodifiable wrappers (not provided by Kryo)
        Serializer<Collection<?>> unmodifiableCollectionSerializer = new UnmodifiableCollectionSerializer();
        registerIfNotNull(kryo, Collections.unmodifiableCollection(new ArrayList<>()).getClass(), unmodifiableCollectionSerializer);

        Serializer<List<?>> unmodifiableListSerializer = new UnmodifiableListSerializer();
        registerIfNotNull(kryo, Collections.unmodifiableList(new ArrayList<>()).getClass(), unmodifiableListSerializer);
        registerIfNotNull(kryo, Collections.unmodifiableList(new LinkedList<>()).getClass(), unmodifiableListSerializer);

        Serializer<Set<?>> unmodifiableSetSerializer = new UnmodifiableSetSerializer();
        registerIfNotNull(kryo, Collections.unmodifiableSet(new HashSet<>()).getClass(), unmodifiableSetSerializer);

        Serializer<Map<?, ?>> unmodifiableMapSerializer = new UnmodifiableMapSerializer();
        registerIfNotNull(kryo, Collections.unmodifiableMap(new HashMap<>()).getClass(), unmodifiableMapSerializer);

        // 4. java.util.Collections synchronized wrappers (not provided by Kryo)
        Serializer<Collection<?>> synchronizedCollectionSerializer = new SynchronizedCollectionSerializer();
        registerIfNotNull(kryo, Collections.synchronizedCollection(new ArrayList<>()).getClass(), synchronizedCollectionSerializer);

        Serializer<List<?>> synchronizedListSerializer = new SynchronizedListSerializer();
        registerIfNotNull(kryo, Collections.synchronizedList(new ArrayList<>()).getClass(), synchronizedListSerializer);
        registerIfNotNull(kryo, Collections.synchronizedList(new LinkedList<>()).getClass(), synchronizedListSerializer);

        Serializer<Set<?>> synchronizedSetSerializer = new SynchronizedSetSerializer();
        registerIfNotNull(kryo, Collections.synchronizedSet(new HashSet<>()).getClass(), synchronizedSetSerializer);

        Serializer<Map<?, ?>> synchronizedMapSerializer = new SynchronizedMapSerializer();
        registerIfNotNull(kryo, Collections.synchronizedMap(new HashMap<>()).getClass(), synchronizedMapSerializer);
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
     * Kryo serializer for {@link Collections#emptyNavigableSet()}.
     */
    public static class EmptyNavigableSetSerializer extends Serializer<NavigableSet<?>> {

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
        public void write(Kryo kryo, Output output, NavigableSet<?> object) {
        }

        /**
         * {@inheritDoc}
         * <p>Returns singleton empty navigable set.</p>
         */
        @Override
        public NavigableSet<?> read(Kryo kryo, Input input, Class<? extends NavigableSet<?>> type) {
            return Collections.emptyNavigableSet();
        }
    }

    /**
     * Kryo serializer for {@link Collections#emptyNavigableMap()}.
     */
    public static class EmptyNavigableMapSerializer extends Serializer<NavigableMap<?, ?>> {

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
        public void write(Kryo kryo, Output output, NavigableMap<?, ?> object) {
        }

        /**
         * {@inheritDoc}
         * <p>Returns singleton empty navigable map.</p>
         */
        @Override
        public NavigableMap<?, ?> read(Kryo kryo, Input input, Class<? extends NavigableMap<?, ?>> type) {
            return Collections.emptyNavigableMap();
        }
    }

    /**
     * Kryo serializer for {@link Collections#emptySortedSet()}.
     */
    public static class EmptySortedSetSerializer extends Serializer<SortedSet<?>> {

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
        public void write(Kryo kryo, Output output, SortedSet<?> object) {
        }

        /**
         * {@inheritDoc}
         * <p>Returns singleton empty sorted set.</p>
         */
        @Override
        public SortedSet<?> read(Kryo kryo, Input input, Class<? extends SortedSet<?>> type) {
            return Collections.emptySortedSet();
        }
    }

    /**
     * Kryo serializer for {@link Collections#emptySortedMap()}.
     */
    public static class EmptySortedMapSerializer extends Serializer<SortedMap<?, ?>> {

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
        public void write(Kryo kryo, Output output, SortedMap<?, ?> object) {
        }

        /**
         * {@inheritDoc}
         * <p>Returns singleton empty sorted map.</p>
         */
        @Override
        public SortedMap<?, ?> read(Kryo kryo, Input input, Class<? extends SortedMap<?, ?>> type) {
            return Collections.emptySortedMap();
        }
    }

    /**
     * Kryo serializer for {@link Collections#unmodifiableCollection(Collection)}.
     */
    public static class UnmodifiableCollectionSerializer extends Serializer<Collection<?>> {

        /**
         * Default constructor marking serializer as immutable.
         */
        public UnmodifiableCollectionSerializer() {
            setImmutable(true);
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the size followed by each element.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, Collection<?> col) {
            output.writeInt(col.size(), true);
            for (Object item : col) {
                kryo.writeClassAndObject(output, item);
            }
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes elements and returns an unmodifiable collection wrapper.</p>
         */
        @Override
        public Collection<?> read(Kryo kryo, Input input, Class<? extends Collection<?>> type) {
            int size = input.readInt(true);
            List<Object> list = new ArrayList<>(size);
            for (int i = 0; i < size; i++) {
                list.add(kryo.readClassAndObject(input));
            }
            return Collections.unmodifiableCollection(list);
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
         * <p>Deserializes elements and returns an unmodifiable list wrapper, preserving RandomAccess capability.</p>
         */
        @Override
        public List<?> read(Kryo kryo, Input input, Class<? extends List<?>> type) {
            int size = input.readInt(true);
            boolean isRandomAccess = RandomAccess.class.isAssignableFrom(type);
            List<Object> list = isRandomAccess ? new ArrayList<>(size) : new LinkedList<>();
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

    /**
     * Kryo serializer for {@link Collections#synchronizedCollection(Collection)}.
     */
    public static class SynchronizedCollectionSerializer extends Serializer<Collection<?>> {

        /**
         * Default constructor.
         */
        public SynchronizedCollectionSerializer() {
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the size followed by each element within a synchronized block.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, Collection<?> col) {
            synchronized (col) {
                output.writeInt(col.size(), true);
                for (Object item : col) {
                    kryo.writeClassAndObject(output, item);
                }
            }
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes elements and returns a synchronized collection wrapper.</p>
         */
        @Override
        public Collection<?> read(Kryo kryo, Input input, Class<? extends Collection<?>> type) {
            int size = input.readInt(true);
            List<Object> list = new ArrayList<>(size);
            for (int i = 0; i < size; i++) {
                list.add(kryo.readClassAndObject(input));
            }
            return Collections.synchronizedCollection(list);
        }

        /**
         * {@inheritDoc}
         * <p>Creates a thread-safe copy of the synchronized collection.</p>
         */
        @Override
        public Collection<?> copy(Kryo kryo, Collection<?> original) {
            synchronized (original) {
                List<Object> copy = new ArrayList<>(original.size());
                for (Object item : original) {
                    copy.add(kryo.copy(item));
                }
                return Collections.synchronizedCollection(copy);
            }
        }
    }

    /**
     * Kryo serializer for {@link Collections#synchronizedList(List)}.
     */
    public static class SynchronizedListSerializer extends Serializer<List<?>> {

        /**
         * Default constructor.
         */
        public SynchronizedListSerializer() {
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the size followed by each list element within a synchronized block.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, List<?> list) {
            synchronized (list) {
                output.writeInt(list.size(), true);
                for (Object item : list) {
                    kryo.writeClassAndObject(output, item);
                }
            }
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes elements and returns a synchronized list wrapper, preserving RandomAccess capability.</p>
         */
        @Override
        public List<?> read(Kryo kryo, Input input, Class<? extends List<?>> type) {
            int size = input.readInt(true);
            boolean isRandomAccess = RandomAccess.class.isAssignableFrom(type);
            List<Object> list = isRandomAccess ? new ArrayList<>(size) : new LinkedList<>();
            for (int i = 0; i < size; i++) {
                list.add(kryo.readClassAndObject(input));
            }
            return Collections.synchronizedList(list);
        }

        /**
         * {@inheritDoc}
         * <p>Creates a thread-safe copy of the synchronized list.</p>
         */
        @Override
        public List<?> copy(Kryo kryo, List<?> original) {
            synchronized (original) {
                boolean isRandomAccess = original instanceof RandomAccess;
                List<Object> copy = isRandomAccess ? new ArrayList<>(original.size()) : new LinkedList<>();
                for (Object item : original) {
                    copy.add(kryo.copy(item));
                }
                return Collections.synchronizedList(copy);
            }
        }
    }

    /**
     * Kryo serializer for {@link Collections#synchronizedSet(Set)}.
     */
    public static class SynchronizedSetSerializer extends Serializer<Set<?>> {

        /**
         * Default constructor.
         */
        public SynchronizedSetSerializer() {
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the size followed by each set element within a synchronized block.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, Set<?> set) {
            synchronized (set) {
                output.writeInt(set.size(), true);
                for (Object item : set) {
                    kryo.writeClassAndObject(output, item);
                }
            }
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes elements and returns a synchronized set wrapper.</p>
         */
        @Override
        public Set<?> read(Kryo kryo, Input input, Class<? extends Set<?>> type) {
            int size = input.readInt(true);
            Set<Object> set = new LinkedHashSet<>(size);
            for (int i = 0; i < size; i++) {
                set.add(kryo.readClassAndObject(input));
            }
            return Collections.synchronizedSet(set);
        }

        /**
         * {@inheritDoc}
         * <p>Creates a thread-safe copy of the synchronized set.</p>
         */
        @Override
        public Set<?> copy(Kryo kryo, Set<?> original) {
            synchronized (original) {
                Set<Object> copy = new LinkedHashSet<>(original.size());
                for (Object item : original) {
                    copy.add(kryo.copy(item));
                }
                return Collections.synchronizedSet(copy);
            }
        }
    }

    /**
     * Kryo serializer for {@link Collections#synchronizedMap(Map)}.
     */
    public static class SynchronizedMapSerializer extends Serializer<Map<?, ?>> {

        /**
         * Default constructor.
         */
        public SynchronizedMapSerializer() {
        }

        /**
         * {@inheritDoc}
         * <p>Serializes the size followed by each map key-value entry within a synchronized block.</p>
         */
        @Override
        public void write(Kryo kryo, Output output, Map<?, ?> map) {
            synchronized (map) {
                output.writeInt(map.size(), true);
                for (Map.Entry<?, ?> entry : map.entrySet()) {
                    kryo.writeClassAndObject(output, entry.getKey());
                    kryo.writeClassAndObject(output, entry.getValue());
                }
            }
        }

        /**
         * {@inheritDoc}
         * <p>Deserializes key-value entries and returns a synchronized map wrapper.</p>
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
            return Collections.synchronizedMap(map);
        }

        /**
         * {@inheritDoc}
         * <p>Creates a thread-safe copy of the synchronized map.</p>
         */
        @Override
        public Map<?, ?> copy(Kryo kryo, Map<?, ?> original) {
            synchronized (original) {
                Map<Object, Object> copy = new LinkedHashMap<>(original.size());
                for (Map.Entry<?, ?> entry : original.entrySet()) {
                    copy.put(kryo.copy(entry.getKey()), kryo.copy(entry.getValue()));
                }
                return Collections.synchronizedMap(copy);
            }
        }
    }
}
