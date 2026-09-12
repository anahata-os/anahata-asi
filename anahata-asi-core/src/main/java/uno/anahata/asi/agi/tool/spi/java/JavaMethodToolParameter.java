/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.tool.spi.java;

import java.lang.reflect.Parameter;
import java.lang.reflect.Type;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.asi.agi.tool.spi.AbstractToolParameter;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;
import uno.anahata.asi.agi.tool.AgiToolParam;

/**
 * A subclass of AbstractToolParameter that holds Java-specific reflection
 * information, namely the full generic Type of the parameter.
 *
 * @author anahata-gemini-pro-2.5
 */
public class JavaMethodToolParameter extends AbstractToolParameter<JavaMethodTool> {

    /**
     * The full Java reflection Type, preserving generics.
     */
    private transient Type javaType;

    /**
     * The index of this parameter in the method's parameter list.
     */
    @Getter
    private final int index;

    /**
     * Private constructor for initializing a JavaMethodToolParameter.
     *
     * @param tool The parent JavaMethodTool.
     * @param name The parameter name.
     * @param description A descriptive label/instruction for the parameter.
     * @param jsonSchema The raw OpenAPI 3 JSON schema.
     * @param required Whether this parameter is mandatory.
     * @param rendererId An optional custom renderer ID.
     * @param javaType The native Java reflection type.
     * @param index The parameter position.
     */
    private JavaMethodToolParameter(
            @NonNull JavaMethodTool tool,
            @NonNull String name,
            @NonNull String description,
            @NonNull String jsonSchema,
            boolean required,
            String rendererId,
            @NonNull Type javaType,
            int index) {
        super(tool, name, description, jsonSchema, required, rendererId);
        this.javaType = javaType;
        this.index = index;
    }

    /**
     * Returns the full Java reflection Type, restoring it lazily if necessary.
     * @return The parameter's Java type.
     */
    public Type getJavaType() {
        if (javaType == null) {
            javaType = getTool().getMethod().getGenericParameterTypes()[index];
        }
        return javaType;
    }

    /**
     * The definitive factory method for creating a JavaMethodToolParameter from
     * a reflection Parameter. This method encapsulates all the logic for
     * parsing annotations and generating the schema.
     *
     * @param tool The parent JavaMethodTool.
     * @param p The reflection Parameter to parse.
     * @param index The index of the parameter.
     * @return A new, fully configured JavaMethodToolParameter.
     * @throws Exception if schema generation fails.
     */
    public static JavaMethodToolParameter of(JavaMethodTool tool, Parameter p, int index) throws Exception {
        AgiToolParam paramAnnotation = p.getAnnotation(AgiToolParam.class);

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

        if (p.getType().isPrimitive() && !required) {
            throw new IllegalArgumentException("Method parameter '" + p.getName() + "' in tool '" + tool.getName()
                    + "' is a primitive type ('" + p.getType().getName()
                    + "') but marked as required=false. Primitive types cannot be optional because omitting them passes null to reflection, causing a NullPointerException. Change it to its boxed wrapper (e.g. Boolean, Integer, Long, Double) or mark it as required=true.");
        }

        return new JavaMethodToolParameter(
            tool,
            p.getName(),
            description,
            jsonSchema,
            required,
            rendererId,
            p.getParameterizedType(),
            index
        );
    }
}
