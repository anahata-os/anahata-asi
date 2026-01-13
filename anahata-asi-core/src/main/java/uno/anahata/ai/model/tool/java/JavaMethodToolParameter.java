/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool.java;

import java.lang.reflect.Parameter;
import java.lang.reflect.Type;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.model.tool.AbstractToolParameter;
import uno.anahata.ai.tool.schema.SchemaProvider;
import uno.anahata.ai.tool.AiToolParam;

/**
 * A subclass of AbstractToolParameter that holds Java-specific reflection
 * information, namely the full generic Type of the parameter.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class JavaMethodToolParameter extends AbstractToolParameter<JavaMethodTool> {

    /**
     * The full Java reflection Type, preserving generics.
     */
    @NonNull
    private final Type javaType;

    private JavaMethodToolParameter(
            @NonNull JavaMethodTool tool,
            @NonNull String name,
            @NonNull String description,
            @NonNull String jsonSchema,
            boolean required,
            String rendererId,
            @NonNull Type javaType) {
        super(tool, name, description, jsonSchema, required, rendererId);
        this.javaType = javaType;
    }

    /**
     * The definitive factory method for creating a JavaMethodToolParameter from
     * a reflection Parameter. This method encapsulates all the logic for
     * parsing annotations and generating the schema.
     *
     * @param tool The parent JavaMethodTool.
     * @param p The reflection Parameter to parse.
     * @return A new, fully configured JavaMethodToolParameter.
     * @throws Exception if schema generation fails.
     */
    public static JavaMethodToolParameter of(JavaMethodTool tool, Parameter p) throws Exception {
        AiToolParam paramAnnotation = p.getAnnotation(AiToolParam.class);

        String description;
        boolean required;
        String rendererId;

        if (paramAnnotation != null) {
            description = paramAnnotation.value();
            required = paramAnnotation.required();
            rendererId = paramAnnotation.rendererId();
        } else {
            // Sensible defaults if the annotation is missing
            description = p.getName(); // Use the parameter name as a default description
            required = true;          // Assume required by default
            rendererId = "";
        }

        String jsonSchema = SchemaProvider.generateInlinedSchemaString(p.getParameterizedType());
        if (jsonSchema == null) {
            throw new IllegalArgumentException("Could not generate schema for parameter " + p.getName() + " in method " + p.getDeclaringExecutable().getName());
        }

        return new JavaMethodToolParameter(
            tool,
            p.getName(),
            description,
            jsonSchema,
            required,
            rendererId,
            p.getParameterizedType()
        );
    }
}
