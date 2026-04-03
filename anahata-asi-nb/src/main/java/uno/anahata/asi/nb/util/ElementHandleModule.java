/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.util;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.module.SimpleModule;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.lang.model.element.ElementKind;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.modules.java.source.ElementHandleAccessor;

/**
 * A Jackson module that provides custom serialization and deserialization for NetBeans {@link ElementHandle}.
 * It uses the {@link ElementHandleAccessor} to interact with the opaque handle.
 */
public class ElementHandleModule extends SimpleModule {

    /**
     * Default constructor for the ElementHandleModule.
     * Registers serializers and deserializers for {@link ElementHandle}.
     */
    public ElementHandleModule() {
        super("ElementHandleModule");
        addSerializer(ElementHandle.class, new ElementHandleSerializer());
        addDeserializer(ElementHandle.class, new ElementHandleDeserializer());
    }

    /**
     * Custom serializer for NetBeans {@link ElementHandle}.
     * Extracts kind and JVM signatures to facilitate JSON interoperability.
     */
    private static class ElementHandleSerializer extends JsonSerializer<ElementHandle> {
        /**
         * {@inheritDoc}
         * <p>Serializes the {@link ElementHandle} by writing its kind and JVM signatures 
         * obtained through the {@link ElementHandleAccessor}.</p>
         */
        @Override
        public void serialize(ElementHandle value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
            gen.writeStartObject();
            gen.writeStringField("kind", value.getKind().name());
            gen.writeArrayFieldStart("signatures");
            for (String sig : ElementHandleAccessor.getInstance().getJVMSignature(value)) {
                gen.writeString(sig);
            }
            gen.writeEndArray();
            gen.writeEndObject();
        }
    }

    /**
     * Custom deserializer for NetBeans {@link ElementHandle}.
     * Reconstructs handles from kind and signature information using 
     * {@link ElementHandleAccessor}.
     */
    private static class ElementHandleDeserializer extends JsonDeserializer<ElementHandle> {
        /**
         * {@inheritDoc}
         * <p>Deserializes the {@link ElementHandle} by reconstructing it from kind 
         * and signature information using the {@link ElementHandleAccessor}.</p>
         */
        @Override
        public ElementHandle deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
            JsonNode node = p.getCodec().readTree(p);
            if (node.isObject()) {
                JsonNode kindNode = node.get("kind");
                JsonNode sigsNode = node.get("signatures");
                
                if (kindNode != null && sigsNode != null && sigsNode.isArray()) {
                    ElementKind kind = ElementKind.valueOf(kindNode.asText());
                    List<String> sigs = new ArrayList<>();
                    sigsNode.forEach(n -> sigs.add(n.asText()));
                    return ElementHandleAccessor.getInstance().create(kind, sigs.toArray(new String[0]));
                }
            }
            return null;
        }
    }
}
