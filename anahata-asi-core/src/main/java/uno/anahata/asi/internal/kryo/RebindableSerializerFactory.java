/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.internal.kryo;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.Serializer;
import com.esotericsoftware.kryo.SerializerFactory;
import com.esotericsoftware.kryo.SerializerFactory.FieldSerializerFactory;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import lombok.extern.slf4j.Slf4j;

/**
 * A custom {@link SerializerFactory} that automatically wraps all created 
 * serializers with a {@link RebindableWrapperSerializer}.
 * <p>
 * This ensures that the {@code rebind()} hook is consistently applied across 
 * the entire object graph during deserialization.
 * </p>
 * <p>
 * <b>Resilience:</b> This factory also detects "hidden classes" (e.g., Java 17+ 
 * lambdas or internal library proxies) and provides a safe null-serializer 
 * to prevent Kryo from crashing during the serialization of complex object graphs.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class RebindableSerializerFactory implements SerializerFactory {

    /** The base factory used to create the standard serializers. */
    private final SerializerFactory delegateFactory = new FieldSerializerFactory();

    /** {@inheritDoc} */
    @Override
    public boolean isSupported(Class type) {
        return delegateFactory.isSupported(type);
    }

    /** {@inheritDoc} */
    @Override
    public Serializer newSerializer(Kryo kryo, Class type) {
        // CRITICAL FIX: Skip hidden classes (like Selenium lambdas) which Kryo cannot handle.
        // These classes often lack stable field offsets or are incompatible with reflection-based serialization.
        if (type.isHidden()) {
            log.warn("Detected hidden class during serialization: {}. Using safe null-serializer.", type.getName());
            return createNullSerializer();
        }
        
        try {
            Serializer s = delegateFactory.newSerializer(kryo, type);
            return new RebindableWrapperSerializer(s);
        } catch (Throwable t) {
            log.error("Failed to create serializer for type: {}. Falling back to null-serializer.", type.getName(), t);
            return createNullSerializer();
        }
    }
    
    /**
     * Creates a minimal serializer that writes nothing and returns null on read.
     * Used as a safety fallback for problematic classes.
     */
    private Serializer createNullSerializer() {
        return new Serializer() {
            @Override
            public void write(Kryo kryo, Output output, Object object) {
                // Do nothing, effectively skipping this object in the stream
            }

            @Override
            public Object read(Kryo kryo, Input input, Class type) {
                return null;
            }
        };
    }
}
