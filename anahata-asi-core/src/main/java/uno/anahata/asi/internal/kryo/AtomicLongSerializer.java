/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.internal.kryo;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.Serializer;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import java.util.concurrent.atomic.AtomicLong;

/**
 * A specialized Kryo serializer for {@link AtomicLong}.
 * <p>
 * This serializer avoids the {@code InaccessibleObjectException} caused by the 
 * Java Module System (JPMS) when using reflection on internal JDK classes 
 * in {@code java.base}.
 * </p>
 * 
 * @author anahata
 */
public class AtomicLongSerializer extends Serializer<AtomicLong> {

    @Override
    public void write(Kryo kryo, Output output, AtomicLong object) {
        output.writeLong(object.get());
    }

    @Override
    public AtomicLong read(Kryo kryo, Input input, Class<? extends AtomicLong> type) {
        return new AtomicLong(input.readLong());
    }
}
