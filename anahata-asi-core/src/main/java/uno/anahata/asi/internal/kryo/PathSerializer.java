/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.internal.kryo;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.Serializer;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * A Kryo serializer for {@link java.nio.file.Path} objects.
 * <p>
 * This serializer avoids JPMS access issues by serializing the path as a 
 * simple String and reconstructing it using {@link Paths#get(String)}.
 * </p>
 * 
 * @author anahata
 */
public class PathSerializer extends Serializer<Path> {

    @Override
    public void write(Kryo kryo, Output output, Path path) {
        output.writeString(path.toAbsolutePath().toString());
    }

    @Override
    public Path read(Kryo kryo, Input input, Class<? extends Path> type) {
        String pathString = input.readString();
        return Paths.get(pathString);
    }
}
