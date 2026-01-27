/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.internal.kryo;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import com.esotericsoftware.kryo.util.DefaultInstantiatorStrategy;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import lombok.extern.slf4j.Slf4j;
import org.objenesis.strategy.StdInstantiatorStrategy;

/**
 * A utility class for thread-safe Kryo serialization and deserialization.
 * <p>
 * This class uses a {@link ThreadLocal} to manage Kryo instances. This is the standard,
 * enterprise-grade pattern for using Kryo in a multi-threaded environment because Kryo
 * instances are **not thread-safe**.
 * </p>
 */
@Slf4j
public class KryoUtils {

    /**
     * The core mechanism for managing thread-safe Kryo instances.
     */
    private static final ThreadLocal<Kryo> kryoThreadLocal = ThreadLocal.withInitial(() -> {
        Kryo kryo = new Kryo();
        // Use Objenesis for classes that lack a no-arg constructor.
        kryo.setInstantiatorStrategy(new DefaultInstantiatorStrategy(new StdInstantiatorStrategy()));
        kryo.setRegistrationRequired(false); 
        kryo.setReferences(true); 

        // Register common JDK types
        kryo.register(ArrayList.class);
        kryo.register(HashMap.class);
        kryo.register(LinkedHashMap.class);
        kryo.register(ConcurrentHashMap.class);
        kryo.register(CopyOnWriteArrayList.class);
        kryo.register(Optional.class, new OptionalSerializer()); 

        // Register Atomic types with custom serializers to avoid JPMS access issues in java.base
        kryo.register(AtomicBoolean.class, new AtomicBooleanSerializer());
        kryo.register(AtomicInteger.class, new AtomicIntegerSerializer());
        kryo.register(AtomicLong.class, new AtomicLongSerializer());

        // Register Path serializer to avoid JPMS issues with UnixPath/WindowsPath
        kryo.addDefaultSerializer(Path.class, new PathSerializer());

        // Set the global factory for automated Rebindable support
        kryo.setDefaultSerializer(new RebindableSerializerFactory());

        return kryo;
    });

    /**
     * Retrieves the Kryo instance for the currently executing thread.
     *
     * @return A thread-safe Kryo instance.
     */
    public static Kryo getKryo() {
        return kryoThreadLocal.get();
    }

    /**
     * Serializes an object into a byte array.
     *
     * @param object The object to serialize.
     * @return A byte array representing the serialized object.
     */
    public static byte[] serialize(Object object) {
        long start = System.currentTimeMillis();
        Kryo kryo = getKryo();
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        try (Output output = new Output(byteArrayOutputStream)) {
            kryo.writeObject(output, object);
        }
        byte[] bytes = byteArrayOutputStream.toByteArray();
        long end = System.currentTimeMillis();
        log.info("Kryo serialization of {} took {} ms, size: {} bytes", object.getClass().getSimpleName(), (end - start), bytes.length);
        return bytes;
    }

    /**
     * Deserializes a byte array into an object.
     *
     * @param <T>   The type of the object to deserialize.
     * @param bytes The byte array to deserialize.
     * @param clazz The class of the object.
     * @return The deserialized object.
     */
    public static <T> T deserialize(byte[] bytes, Class<T> clazz) {
        long start = System.currentTimeMillis();
        Kryo kryo = getKryo();
        ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(bytes);
        try (Input input = new Input(byteArrayInputStream)) {
            T object = kryo.readObject(input, clazz);
            long end = System.currentTimeMillis();
            log.info("Kryo deserialization of {} took {} ms, size: {} bytes", clazz.getSimpleName(), (end - start), bytes.length);
            return object;
        }
    }
}
