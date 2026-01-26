/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.internal.kryo;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.Serializer;
import com.esotericsoftware.kryo.SerializerFactory;
import com.esotericsoftware.kryo.SerializerFactory.FieldSerializerFactory;

/**
 * A custom {@link SerializerFactory} that automatically wraps all created 
 * serializers with a {@link RebindableWrapperSerializer}.
 * <p>
 * This ensures that the {@code rebind()} hook is consistently applied across 
 * the entire object graph during deserialization.
 * </p>
 * 
 * @author anahata
 */
public class RebindableSerializerFactory implements SerializerFactory {

    /** The base factory used to create the standard serializers. */
    private final SerializerFactory delegateFactory = new FieldSerializerFactory();

    @Override
    public boolean isSupported(Class type) {
        return delegateFactory.isSupported(type);
    }

    @Override
    public Serializer newSerializer(Kryo kryo, Class type) {
        Serializer s = delegateFactory.newSerializer(kryo, type);
        return new RebindableWrapperSerializer(s);
    }
}
