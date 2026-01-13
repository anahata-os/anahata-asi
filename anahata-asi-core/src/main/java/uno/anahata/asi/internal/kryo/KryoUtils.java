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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import org.objenesis.strategy.StdInstantiatorStrategy;

/**
 * A utility class for thread-safe Kryo serialization and deserialization.
 * <p>
 * This class uses a {@link ThreadLocal} to manage Kryo instances. This is the standard,
 * enterprise-grade pattern for using Kryo in a multi-threaded environment because Kryo
 * instances are **not thread-safe**.
 * <p>
 * Using a {@code ThreadLocal} provides two key benefits:
 * <ul>
 *     <li><b>Thread Safety:</b> Each thread gets its own isolated Kryo instance, preventing
 *         race conditions and data corruption.</li>
 *     <li><b>Performance:</b> Each thread reuses its own instance, avoiding the significant
 *         performance overhead of creating a new Kryo object for every operation.</li>
 * </ul>
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
        kryo.setRegistrationRequired(false); // Keep it simple for the core module

        // Register common JDK types
        kryo.register(ArrayList.class);
        kryo.register(HashMap.class);
        kryo.register(Optional.class, new OptionalSerializer()); // Use custom serializer for Optional

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
     * Serializes an object into a byte array using the current thread's Kryo instance.
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
     * Deserializes a byte array into an object of the specified class using the current thread's Kryo instance.
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
