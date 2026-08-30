/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.persistence.kryo;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import com.esotericsoftware.kryo.util.DefaultInstantiatorStrategy;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
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

        // Register JDK immutable, unmodifiable, singleton, and empty collection serializers
        JdkCollectionsSerializers.register(kryo);

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
     * Creates a deep clone of the given object using a serialization-deserialization cycle.
     * <p>
     * <b>Technical Purity:</b> This approach is preferred over {@code kryo.copy()} because it 
     * authoritatively respects the {@code transient} modifier, ensuring that environmental 
     * references (like the AsiContainer or ThreadPools) are not accidentally cloned, which 
     * prevents circular dependencies and access violations in JDK 17+.
     * </p>
     * 
     * @param <T> The type of the object.
     * @param object The object to clone.
     * @return A deep clone of the object.
     */
    public static <T> T clone(T object) {
        if (object == null) {
            throw new IllegalArgumentException ("Cannot clone a null object.");
        }
        byte[] bytes = serialize(object);
        return (T) deserialize(bytes, object.getClass());
    }

    /**
     * Serializes an object with Kryo and writes it atomically to the target file on disk using a temporary file.
     *
     * @param object The object to serialize and save.
     * @param targetFile The final destination path.
     * @throws IOException If serialization or writing fails.
     */
    public static void saveToFile(Object object, Path targetFile) throws IOException {
        Path parent = targetFile.getParent();
        if (parent != null && !Files.exists(parent)) {
            Files.createDirectories(parent);
        }
        Path tmpFile = targetFile.resolveSibling(targetFile.getFileName().toString() + ".tmp");
        byte[] data = serialize(object);
        Files.write(tmpFile, data);
        try {
            Files.move(tmpFile, targetFile, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
        } catch (java.nio.file.AtomicMoveNotSupportedException e) {
            Files.move(tmpFile, targetFile, StandardCopyOption.REPLACE_EXISTING);
        }
    }

    /**
     * Reads a file from disk and deserializes it with Kryo.
     *
     * @param <T> The target object type.
     * @param file The file to read.
     * @param clazz The target class.
     * @return The deserialized object.
     * @throws IOException If reading the file fails.
     */
    public static <T> T loadFromFile(Path file, Class<T> clazz) throws IOException {
        byte[] data = Files.readAllBytes(file);
        return deserialize(data, clazz);
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
