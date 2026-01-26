/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.internal.kryo;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.Serializer;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import uno.anahata.asi.model.core.Rebindable;

/**
 * A decorator serializer that adds automated {@link Rebindable} support to any 
 * existing Kryo serializer.
 * <p>
 * After the delegate serializer completes the {@code read} operation, this 
 * wrapper checks if the resulting object implements {@link Rebindable} and, 
 * if so, calls its {@code rebind()} method.
 * </p>
 * 
 * @param <T> The type of object being serialized.
 * @author anahata
 */
public class RebindableWrapperSerializer<T> extends Serializer<T> {
    
    /** The underlying serializer to delegate to. */
    private final Serializer<T> delegate;

    /**
     * Constructs a new wrapper.
     * 
     * @param delegate The serializer to wrap.
     */
    public RebindableWrapperSerializer(Serializer<T> delegate) {
        this.delegate = delegate;
        setAcceptsNull(delegate.getAcceptsNull());
        setImmutable(delegate.isImmutable());
    }

    @Override
    public void write(Kryo kryo, Output output, T object) {
        delegate.write(kryo, output, object);
    }

    @Override
    public T read(Kryo kryo, Input input, Class<? extends T> type) {
        T obj = delegate.read(kryo, input, type);
        if (obj instanceof Rebindable rebindable) {
            rebindable.rebind();
        }
        return obj;
    }

    @Override
    public T copy(Kryo kryo, T original) {
        return delegate.copy(kryo, original);
    }
}
