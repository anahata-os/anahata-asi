/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.internal.kryo;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.Serializer;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * A specialized Kryo serializer for {@link AtomicInteger}.
 * <p>
 * This serializer avoids the {@code InaccessibleObjectException} caused by the 
 * Java Module System (JPMS) when using reflection on internal JDK classes 
 * in {@code java.base}.
 * </p>
 * 
 * @author anahata
 */
public class AtomicIntegerSerializer extends Serializer<AtomicInteger> {

    @Override
    public void write(Kryo kryo, Output output, AtomicInteger object) {
        output.writeInt(object.get());
    }

    @Override
    public AtomicInteger read(Kryo kryo, Input input, Class<? extends AtomicInteger> type) {
        return new AtomicInteger(input.readInt());
    }
}
