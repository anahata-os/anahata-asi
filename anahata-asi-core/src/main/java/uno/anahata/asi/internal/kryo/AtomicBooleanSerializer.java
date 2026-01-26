/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.internal.kryo;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.Serializer;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * A specialized Kryo serializer for {@link AtomicBoolean}.
 * <p>
 * This serializer avoids the {@code InaccessibleObjectException} caused by the 
 * Java Module System (JPMS) when using reflection on internal JDK classes 
 * in {@code java.base}.
 * </p>
 * 
 * @author anahata
 */
public class AtomicBooleanSerializer extends Serializer<AtomicBoolean> {

    @Override
    public void write(Kryo kryo, Output output, AtomicBoolean object) {
        output.writeBoolean(object.get());
    }

    @Override
    public AtomicBoolean read(Kryo kryo, Input input, Class<? extends AtomicBoolean> type) {
        return new AtomicBoolean(input.readBoolean());
    }
}
