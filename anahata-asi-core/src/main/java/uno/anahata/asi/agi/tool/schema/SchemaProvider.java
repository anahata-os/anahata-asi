/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.tool.schema;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.module.mrbean.MrBeanModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import io.swagger.v3.core.converter.AnnotatedType;
import io.swagger.v3.core.converter.ModelConverters;
import io.swagger.v3.core.jackson.ModelResolver;
import io.swagger.v3.oas.annotations.media.Schema.RequiredMode;
import io.swagger.v3.oas.models.media.Schema;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.WildcardType;
import java.util.*;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.JacksonUtils;

/**
 * A clean, focused provider for generating OpenAPI/Swagger compliant JSON
 * schemas from Java types. This provider's key feature is its deep,
 * reflection-based analysis to enrich the schema with precise Java type
 * information, embedding the "beautiful" fully qualified type name into the
 * {@code title} field of every object, property, and array item. It correctly
 * handles complex generic types and recursive data structures, and performs
 * inlining to produce a single, self-contained schema object suitable for AI
 * models.
 *
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
public class SchemaProvider {

    /**
     * The centrally configured Jackson ObjectMapper for JSON operations.
     */
    public static final ObjectMapper OBJECT_MAPPER = new ObjectMapper()
            .registerModule(new ParameterNamesModule())
            .registerModule(new MrBeanModule())
            .setSerializationInclusion(JsonInclude.Include.NON_NULL)
            .setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY)
            .setVisibility(PropertyAccessor.GETTER, JsonAutoDetect.Visibility.NONE)
            .setVisibility(PropertyAccessor.IS_GETTER, JsonAutoDetect.Visibility.NONE);

    /**
     * A secondary mapper that respects getters, used specifically for
     * converting Swagger's internal Schema objects which are not standard
     * POJOs.
     */
    private static final ObjectMapper INTERNAL_MAPPER = new ObjectMapper()
            .setSerializationInclusion(JsonInclude.Include.NON_NULL);

    /**
     * Generates a complete, inlined JSON schema for a wrapper type, but with
     * the schema for a specific 'result' type surgically injected into its
     * 'result' property.
     *
     * @param wrapperType The container type (e.g.,
     * JavaMethodToolResponse.class).
     * @param attributeName The name of the property in the wrapper to replace.
     * @param wrappedType The specific type of the result to inject (e.g.,
     * Tree.class or void.class).
     * @return A complete, final JSON schema string.
     * @throws JsonProcessingException if schema generation fails.
     */
    public static String generateInlinedSchemaString(Type wrapperType, String attributeName, Type wrappedType) throws JsonProcessingException {
        if (wrapperType == null) {
            return null;
        }

        // 1. Generate the standard, non-inlined schema for the wrapper
        ObjectNode mutableRoot = generateStandardSchemaNode(wrapperType);
        if (mutableRoot == null) {
            return null;
        }

        // 2. Handle the wrappedType (the result)
        if (wrappedType == null || wrappedType.equals(void.class) || wrappedType.equals(Void.class)) {
            ObjectNode properties = (ObjectNode) mutableRoot.get("properties");
            if (properties != null) {
                properties.remove(attributeName);
            }
            removeRequired(mutableRoot, attributeName);
        } else {
            // 3. Generate the standard schema for the wrapped type
            ObjectNode wrappedRootNode = generateStandardSchemaNode(wrappedType);
            if (wrappedRootNode == null) {
                // Fallback for types that Swagger might completely ignore (like Object.class)
                wrappedRootNode = OBJECT_MAPPER.createObjectNode();
                wrappedRootNode.put("type", "object");
                wrappedRootNode.put("title", getTypeName(wrappedType));
            }

            // 4. Merge the definitions ('components.schemas') from the wrapped schema into the base schema
            mergeComponents(mutableRoot, wrappedRootNode);

            // 5. Inject the wrapped schema into the 'properties' of the base schema
            ObjectNode properties = (ObjectNode) mutableRoot.get("properties");
            if (properties == null) {
                properties = mutableRoot.putObject("properties");
            }

            ObjectNode wrappedSchemaObject = (ObjectNode) wrappedRootNode.deepCopy();
            wrappedSchemaObject.remove("components");
            properties.set(attributeName, wrappedSchemaObject);

            // 6. Ensure the attribute is in the 'required' list for the AI model
            addRequired(mutableRoot, attributeName);
        }

        // 7. Perform a single recursive inlining pass on the combined structure
        JsonNode definitions = mutableRoot.path("components").path("schemas");
        JsonNode inlinedNode = inlineDefinitionsRecursive(mutableRoot, definitions, new HashSet<>());

        // 8. Clean up and return the final schema string
        if (inlinedNode instanceof ObjectNode objectNode) {
            objectNode.remove("components");
        }

        // 9. Final Purification: Remove all framework metadata and Swagger-internal fields
        JacksonUtils.purifySchema(inlinedNode);

        return OBJECT_MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(inlinedNode);
    }

    /**
     * Generates a complete, inlined JSON schema for a given Java type. This
     * method is used for generating schemas for tool parameters and simple
     * return types.
     *
     * @param type The Java type to generate the schema for.
     * @return A complete, final JSON schema string, or {@code null} for void
     * types.
     * @throws JsonProcessingException if schema generation fails.
     */
    public static String generateInlinedSchemaString(Type type) throws JsonProcessingException {
        if (type == null || type.equals(void.class) || type.equals(Void.class)) {
            return null;
        }
        ObjectNode rootNode = generateStandardSchemaNode(type);
        if (rootNode == null) {
            // Fallback for types that Swagger might completely ignore (like Class.class)
            rootNode = OBJECT_MAPPER.createObjectNode();
            rootNode.put("type", "object");
            rootNode.put("title", getTypeName(type));
        }

        JsonNode definitions = rootNode.path("components").path("schemas");
        JsonNode inlinedNode = inlineDefinitionsRecursive(rootNode, definitions, new HashSet<>());

        if (inlinedNode instanceof ObjectNode objectNode) {
            objectNode.remove("components");
        }

        // Final Purification: Remove all framework metadata and Swagger-internal fields
        JacksonUtils.purifySchema(inlinedNode);

        return OBJECT_MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(inlinedNode);
    }

    /**
     * Helper that generates a standard, non-inlined OpenAPI Schema as a mutable ObjectNode.
     * 
     * This parses the JSON schema string produced by {@link #generateStandardSchema(Type)} into a tree representation that is suitable for structural mutations.
     * @param type The Java type to generate the schema for.
     * @return The schema represented as an ObjectNode, or null if the schema cannot be generated.
     * @throws com.fasterxml.jackson.core.JsonProcessingException If the generated schema JSON string cannot be parsed.
     */
    private static ObjectNode generateStandardSchemaNode(Type type) throws JsonProcessingException {
        String json = generateStandardSchema(type);
        return json == null ? null : (ObjectNode) OBJECT_MAPPER.readTree(json);
    }

    /**
     * Merges the 'components.schemas' from the source node into the target
     * node. This method is null-safe and robust against missing nodes.
     *
     * @param target The target schema node.
     * @param source The source schema node.
     */
    private static void mergeComponents(ObjectNode target, ObjectNode source) {
        if (target == null || source == null) {
            return;
        }

        JsonNode sourceDefinitions = source.path("components").path("schemas");
        if (sourceDefinitions.isObject()) {
            JsonNode componentsNode = target.path("components");
            ObjectNode components = componentsNode.isObject() ? (ObjectNode) componentsNode : target.putObject("components");

            JsonNode targetDefsNode = components.path("schemas");
            ObjectNode targetDefinitions = targetDefsNode.isObject() ? (ObjectNode) targetDefsNode : components.putObject("schemas");

            ObjectNode finalTargetDefinitions = targetDefinitions;
            sourceDefinitions.fields().forEachRemaining(entry -> {
                finalTargetDefinitions.set(entry.getKey(), entry.getValue());
            });
        }
    }

    /**
     * Adds a property name to the 'required' array of a schema node.
     * 
     * This method is idempotent and ensures that the property name is added to the 'required' array of the node without duplicating existing entries.
     * @param node The mutable ObjectNode schema representation.
     * @param propertyName The name of the property to mark as required.
     */
    private static void addRequired(ObjectNode node, String propertyName) {
        JsonNode requiredNode = node.get("required");
        ArrayNode required;
        if (requiredNode == null || !requiredNode.isArray()) {
            required = node.putArray("required");
        } else {
            required = (ArrayNode) requiredNode;
        }

        boolean exists = false;
        for (JsonNode item : required) {
            if (item.asText().equals(propertyName)) {
                exists = true;
                break;
            }
        }
        if (!exists) {
            required.add(propertyName);
        }
    }

    /**
     * Removes a property name from the 'required' array of a schema node.
     * 
     * If the 'required' array becomes empty as a result of the removal, the 'required' field is completely stripped from the node.
     * @param node The mutable ObjectNode schema representation.
     * @param propertyName The name of the property to remove from the required list.
     */
    private static void removeRequired(ObjectNode node, String propertyName) {
        JsonNode requiredNode = node.get("required");
        if (requiredNode != null && requiredNode.isArray()) {
            ArrayNode required = (ArrayNode) requiredNode;
            for (int i = 0; i < required.size(); i++) {
                if (required.get(i).asText().equals(propertyName)) {
                    required.remove(i);
                    break;
                }
            }
            if (required.isEmpty()) {
                node.remove("required");
            }
        }
    }

    /**
     * Generates a standard, non-inlined JSON schema for a given Java type.
     *
     * @param type The Java type.
     * @return The JSON schema string.
     * @throws JsonProcessingException if generation fails.
     */
    private static String generateStandardSchema(Type type) throws JsonProcessingException {
        if (type instanceof ParameterizedType) {
            ParameterizedType pt = (ParameterizedType) type;
            Type raw = pt.getRawType();
            if (raw.equals(List.class) || raw.equals(Collection.class) || raw.equals(Set.class)) {
                Type[] args = pt.getActualTypeArguments();
                if (args.length == 1) {
                    String itemSchemaJson = generateStandardSchema(args[0]);
                    if (itemSchemaJson != null) {
                        return buildArraySchema(type, itemSchemaJson);
                    }
                }
            } else if (raw.equals(Map.class)) {
                Type[] args = pt.getActualTypeArguments();
                if (args.length == 2 && args[0].equals(String.class)) {
                    String valueSchemaJson = generateStandardSchema(args[1]);
                    if (valueSchemaJson != null) {
                        return buildMapSchema(type, valueSchemaJson);
                    }
                }
            }
        } else if (type instanceof Class<?> clazz && clazz.isArray()) {
            // Handle T[] arrays
            Type componentType = clazz.getComponentType();
            String itemSchemaJson = generateStandardSchema(componentType);
            if (itemSchemaJson != null) {
                return buildArraySchema(type, itemSchemaJson);
            }
        }

        String simpleSchema = handleSimpleTypeSchema(type);
        if (simpleSchema != null) {
            return simpleSchema;
        }

        // 1. Discover all reachable types (including polymorphic subtypes)
        Map<String, Type> discoveredTypes = new HashMap<>();
        Map<Type, List<Type>> polymorphismMap = new HashMap<>();
        findAllTypesRecursive(type, discoveredTypes, polymorphismMap, new HashSet<>());

        ModelConverters converters = new ModelConverters();
        // CRITICAL: Use OBJECT_MAPPER (getters disabled) for type discovery.
        converters.addConverter(new ModelResolver(OBJECT_MAPPER));

        // 2. Generate schemas for ALL discovered types to ensure polymorphic branches are in the components map
        Map<String, Schema> swaggerSchemas = new HashMap<>();
        for (Type t : discoveredTypes.values()) {
            swaggerSchemas.putAll(converters.readAll(new AnnotatedType(t)));
        }

        if (swaggerSchemas.isEmpty()) {
            return null;
        }

        postProcessAndEnrichSchemas(type, swaggerSchemas, discoveredTypes, polymorphismMap);

        // 3. Inject Discriminator Enums based on Jackson annotations
        injectDiscriminatorEnums(swaggerSchemas, discoveredTypes);

        Schema rootSchema = createRootSchema(type, swaggerSchemas);

        // CRITICAL: Use INTERNAL_MAPPER to convert Swagger objects to Map, 
        // as they rely on getters which are disabled in OBJECT_MAPPER.
        Map<String, Object> finalSchemaMap = new LinkedHashMap<>(INTERNAL_MAPPER.convertValue(rootSchema, Map.class));
        Map<String, Object> components = new LinkedHashMap<>();
        Map<String, Object> componentSchemas = new LinkedHashMap<>();
        swaggerSchemas.forEach((key, schema) -> componentSchemas.put(key, INTERNAL_MAPPER.convertValue(schema, Map.class)));
        components.put("schemas", componentSchemas);
        finalSchemaMap.put("components", components);

        return INTERNAL_MAPPER.writeValueAsString(finalSchemaMap);
    }

    /**
     * Handles schema generation for simple Java types (String, Number, Boolean,
     * Enum).
     *
     * @param type The Java type.
     * @return The JSON schema string, or null if not a simple type.
     * @throws JsonProcessingException if generation fails.
     */
    private static String handleSimpleTypeSchema(Type type) throws JsonProcessingException {
        if (!(type instanceof Class)) {
            return null;
        }
        Class<?> clazz = (Class<?>) type;
        Map<String, Object> schemaMap = new LinkedHashMap<>();
        schemaMap.put("title", getTypeName(type));

        var schemaAnnotation = getSchemaAnno(clazz);
        if (schemaAnnotation != null && !schemaAnnotation.description().isEmpty()) {
            schemaMap.put("description", schemaAnnotation.description());
        }

        if (clazz.equals(String.class) || clazz.equals(Class.class)) {
            schemaMap.put("type", "string");
        } else if (clazz.equals(Integer.class) || clazz.equals(int.class) || clazz.equals(Long.class) || clazz.equals(long.class)) {
            schemaMap.put("type", "integer");
        } else if (Number.class.isAssignableFrom(clazz) || (clazz.isPrimitive() && (clazz.equals(float.class) || clazz.equals(double.class)))) {
            schemaMap.put("type", "number");
        } else if (clazz.equals(Boolean.class) || clazz.equals(boolean.class)) {
            schemaMap.put("type", "boolean");
        } else if (clazz.isEnum()) {
            schemaMap.put("type", "string");
            Object[] constants = clazz.getEnumConstants();
            List<String> enumValues = new ArrayList<>();
            StringBuilder descBuilder = new StringBuilder();

            String existingDesc = (String) schemaMap.get("description");
            var classAnno = getSchemaAnno(clazz);
            String classDesc = (classAnno != null && !classAnno.description().isEmpty()) ? classAnno.description() : null;

            if (existingDesc != null && !existingDesc.isBlank()) {
                descBuilder.append(existingDesc);
                if (classDesc != null && !existingDesc.contains(classDesc)) {
                    descBuilder.append("\n\n").append(classDesc);
                }
            } else if (classDesc != null) {
                descBuilder.append(classDesc);
            }

            String currentDescSoFar = descBuilder.toString();
            if (!currentDescSoFar.contains("(Details: Values:") && !currentDescSoFar.contains("Available Values:") && !currentDescSoFar.contains("Values:")) {
                if (!currentDescSoFar.isEmpty()) {
                    descBuilder.append("\n\nAvailable Values:");
                } else {
                    descBuilder.append("Available Values:");
                }
            }

            for (Object obj : constants) {
                String name = obj.toString();
                enumValues.add(name);
                descBuilder.append("\n- `").append(name).append("` ");
                try {
                    Field field = clazz.getField(name);
                    var fieldAnnos = getSchemaAnno(field);
                    if (fieldAnnos != null && !fieldAnnos.description().isEmpty()) {
                        descBuilder.append(": ").append(fieldAnnos.description());
                    }
                } catch (Exception e) {
                    // Fallback to name if field reflection fails
                }
            }
            schemaMap.put("enum", enumValues);
            schemaMap.put("description", descBuilder.toString());
        } else {
            return null;
        }

        return INTERNAL_MAPPER.writeValueAsString(schemaMap);
    }

    /**
     * Builds a JSON schema for an array or collection type.
     *
     * @param arrayType The array or collection type.
     * @param itemSchemaJson The JSON schema for the items.
     * @return The JSON schema string for the array.
     * @throws JsonProcessingException if generation fails.
     */
    private static String buildArraySchema(Type arrayType, String itemSchemaJson) throws JsonProcessingException {
        JsonNode itemSchema = OBJECT_MAPPER.readTree(itemSchemaJson);

        Map<String, Object> arraySchema = new LinkedHashMap<>();
        arraySchema.put("type", "array");
        arraySchema.put("title", getTypeName(arrayType));  // e.g., "java.lang.String[]" or "java.util.List<java.lang.String>"
        arraySchema.put("items", INTERNAL_MAPPER.treeToValue(itemSchema, Map.class));

        Map<String, Object> finalMap = new LinkedHashMap<>();
        finalMap.putAll(arraySchema);

        // Carry over components if the item is a complex object
        JsonNode itemComponents = itemSchema.path("components").path("schemas");
        if (itemComponents.isObject() && itemComponents.size() > 0) {
            Map<String, Object> components = new LinkedHashMap<>();
            components.put("schemas", INTERNAL_MAPPER.treeToValue(itemComponents, Map.class));
            finalMap.put("components", components);
        }

        return INTERNAL_MAPPER.writeValueAsString(finalMap);
    }

    /**
     * Builds a JSON schema for a Map&lt;String, T&gt; type.
     *
     * @param mapType The map type.
     * @param valueSchemaJson The JSON schema for the values.
     * @return The JSON schema string for the map.
     * @throws JsonProcessingException if generation fails.
     */
    private static String buildMapSchema(Type mapType, String valueSchemaJson) throws JsonProcessingException {
        JsonNode valueSchema = OBJECT_MAPPER.readTree(valueSchemaJson);

        Map<String, Object> mapSchema = new LinkedHashMap<>();
        mapSchema.put("type", "object");
        mapSchema.put("title", getTypeName(mapType));
        mapSchema.put("additionalProperties", INTERNAL_MAPPER.treeToValue(valueSchema, Map.class));

        Map<String, Object> finalMap = new LinkedHashMap<>();
        finalMap.putAll(mapSchema);

        // Carry over components if the value is a complex object
        JsonNode valueComponents = valueSchema.path("components").path("schemas");
        if (valueComponents.isObject() && valueComponents.size() > 0) {
            Map<String, Object> components = new LinkedHashMap<>();
            components.put("schemas", INTERNAL_MAPPER.treeToValue(valueComponents, Map.class));
            finalMap.put("components", components);
        }

        return INTERNAL_MAPPER.writeValueAsString(finalMap);
    }

    /**
     * Creates the root schema object for a given type.
     *
     * @param type The Java type.
     * @param swaggerSchemas The map of generated schemas.
     * @return The root Schema object.
     */
    private static Schema createRootSchema(Type type, Map<String, Schema> swaggerSchemas) {
        if (type instanceof ParameterizedType && ((ParameterizedType) type).getRawType().equals(List.class)) {
            Schema listSchema = new Schema().type("array");
            listSchema.setTitle(getTypeName(type));
            String refName = findRootSchemaName(swaggerSchemas, ((ParameterizedType) type).getActualTypeArguments()[0]);
            listSchema.setItems(new Schema().$ref("#/components/schemas/" + refName));
            return listSchema;
        } else {
            String rootSchemaName = findRootSchemaName(swaggerSchemas, type);
            Schema rootSchema = swaggerSchemas.get(rootSchemaName);
            if (rootSchema != null && rootSchema.getTitle() == null) {
                rootSchema.setTitle(getTypeName(type));
            }
            return rootSchema;
        }
    }

    /**
     * Surgically injects 'enum' constraints into discriminator properties by reading Jackson's {@code @JsonSubTypes}. This ensures the AI model knows the exact string values required for polymorphic dispatch.
     * @param allSchemas The map of all generated schemas across the entire type tree.
     * @param discoveredTypes The map of discovered Java types indexed by their simple names.
     */
    private static void injectDiscriminatorEnums(Map<String, Schema> allSchemas, Map<String, Type> discoveredTypes) {
        for (Map.Entry<String, Type> entry : discoveredTypes.entrySet()) {
            Class<?> clazz = getRawClass(entry.getValue());
            if (clazz == null) {
                continue;
            }

            JsonSubTypes subTypesAnno = clazz.getAnnotation(JsonSubTypes.class);
            JsonTypeInfo typeInfoAnno = clazz.getAnnotation(JsonTypeInfo.class);

            if (subTypesAnno != null && typeInfoAnno != null && typeInfoAnno.include() == JsonTypeInfo.As.PROPERTY) {
                String typePropertyName = typeInfoAnno.property();
                Schema baseSchema = allSchemas.get(clazz.getSimpleName());

                if (baseSchema != null && baseSchema.getProperties() != null) {
                    Schema typePropSchema = (Schema) baseSchema.getProperties().get(typePropertyName);
                    if (typePropSchema != null) {
                        List<String> validNames = Arrays.stream(subTypesAnno.value())
                                .map(JsonSubTypes.Type::name)
                                .collect(Collectors.toList());
                        typePropSchema.setEnum(validNames);

                        String desc = typePropSchema.getDescription();
                        String enumHint = "Valid values: " + validNames.stream().map(n -> "`" + n + "`").collect(Collectors.joining(", "));
                        typePropSchema.setDescription(desc == null ? enumHint : desc + "\n\n" + enumHint);
                    }
                }
            }
        }
    }

    /**
     * Post-processes and enriches the generated schemas with type information.
     *
     * @param rootType The root Java type.
     * @param allSchemas The map of all generated schemas.
     * @param discoveredTypes The map of all discovered Java types.
     */
    private static void postProcessAndEnrichSchemas(Type rootType, Map<String, Schema> allSchemas, Map<String, Type> discoveredTypes, Map<Type, List<Type>> polymorphismMap) {
        for (Map.Entry<String, Schema> entry : allSchemas.entrySet()) {
            String schemaName = entry.getKey();
            Schema schema = entry.getValue();
            Type type = discoveredTypes.get(schemaName);
            if (type != null) {
                // Manual Polymorphism Linkage: 
                // If Swagger didn't create a oneOf, but we know this type has subtypes, force it.
                List<Type> subtypes = polymorphismMap.get(type);
                if (subtypes != null && !subtypes.isEmpty() && schema.getOneOf() == null) {
                    List<Schema> oneOf = new ArrayList<>();
                    for (Type st : subtypes) {
                        Class<?> rawSt = getRawClass(st);
                        if (rawSt != null) {
                            oneOf.add(new Schema().$ref("#/components/schemas/" + rawSt.getSimpleName()));
                        }
                    }
                    schema.setOneOf(oneOf);
                }

                addTitleToSchemaRecursive(schema, type, allSchemas, discoveredTypes, new HashSet<>());
            }
        }
    }

    /**
     * Recursively finds all types reachable from a given type.
     *
     * @param type The starting type.
     * @param foundTypes The map to store found types.
     * @param visited The set of visited types.
     */
    private static void findAllTypesRecursive(Type type, Map<String, Type> foundTypes, Map<Type, List<Type>> polymorphismMap, Set<Type> visited) {
        if (type == null || !visited.add(type)) {
            return;
        }
        if (type instanceof Class) {
            Class<?> clazz = (Class<?>) type;
            if (!isJdkClass(clazz)) {
                foundTypes.put(clazz.getSimpleName(), clazz);

                // Polymorphism support: Discover subclasses from Jackson annotations
                JsonSubTypes subTypes = clazz.getAnnotation(JsonSubTypes.class);
                if (subTypes != null) {
                    List<Type> children = new ArrayList<>();
                    for (JsonSubTypes.Type subType : subTypes.value()) {
                        children.add(subType.value());
                        findAllTypesRecursive(subType.value(), foundTypes, polymorphismMap, visited);
                    }
                    polymorphismMap.put(type, children);
                }

                for (Field field : getAllFields(clazz)) {
                    findAllTypesRecursive(field.getGenericType(), foundTypes, polymorphismMap, visited);
                }
            }
        } else if (type instanceof ParameterizedType) {
            ParameterizedType pt = (ParameterizedType) type;
            findAllTypesRecursive(pt.getRawType(), foundTypes, polymorphismMap, visited);
            for (Type arg : pt.getActualTypeArguments()) {
                findAllTypesRecursive(arg, foundTypes, polymorphismMap, visited);
            }
        } else if (type instanceof WildcardType) {
            WildcardType wt = (WildcardType) type;
            Arrays.stream(wt.getUpperBounds()).forEach(b -> findAllTypesRecursive(b, foundTypes, polymorphismMap, visited));
            Arrays.stream(wt.getLowerBounds()).forEach(b -> findAllTypesRecursive(b, foundTypes, polymorphismMap, visited));
        }
    }

    /**
     * Recursively adds titles (FQN) to schemas and their properties.
     *
     * @param schema The schema to enrich.
     * @param type The corresponding Java type.
     * @param allSchemas The map of all schemas.
     * @param discoveredTypes The map of all discovered Java types.
     * @param visited The set of visited types.
     */
    private static void addTitleToSchemaRecursive(Schema schema, Type type, Map<String, Schema> allSchemas, Map<String, Type> discoveredTypes, Set<Type> visited) {
        if (schema == null || !visited.add(type)) {
            return;
        }
        try {
            if (type != null && schema.getTitle() == null) {
                schema.setTitle(getTypeName(type));
            }

            if (type instanceof Class<?> clazz && clazz.isEnum()) {
                enrichEnumDescription(schema, clazz);
            }

            // Handle Polymorphic branches
            if (schema.getOneOf() != null) {
                for (Object sub : schema.getOneOf()) {
                    Schema subSchema = (Schema) sub;
                    Type subType = resolveTypeFromSchema(subSchema, discoveredTypes);
                    addTitleToSchemaRecursive(subSchema, subType, allSchemas, discoveredTypes, visited);
                }
            }
            if (schema.getAnyOf() != null) {
                for (Object sub : schema.getAnyOf()) {
                    Schema subSchema = (Schema) sub;
                    Type subType = resolveTypeFromSchema(subSchema, discoveredTypes);
                    addTitleToSchemaRecursive(subSchema, subType, allSchemas, discoveredTypes, visited);
                }
            }
            if (schema.getAllOf() != null) {
                for (Object sub : schema.getAllOf()) {
                    Schema subSchema = (Schema) sub;
                    Type subType = resolveTypeFromSchema(subSchema, discoveredTypes);
                    addTitleToSchemaRecursive(subSchema, subType, allSchemas, discoveredTypes, visited);
                }
            }

            Class<?> rawClass = getRawClass(type);
            if (rawClass == null || isJdkClass(rawClass)) {
                return;
            }

            if (schema.getProperties() != null) {
                for (Map.Entry<String, Schema> propEntry : (Set<Map.Entry<String, Schema>>) schema.getProperties().entrySet()) {
                    Field field = findField(rawClass, propEntry.getKey());
                    if (field != null) {
                        Schema propSchema = propEntry.getValue();
                        Type fieldType = field.getGenericType();

                        // Manually capture 'required' and 'description' status from @Schema annotation 
                        // as ModelConverters might miss it due to Visibility settings or $ref sibling rules.
                        var fieldAnno = getSchemaAnno(field);
                        if (fieldAnno != null) {
                            if (!fieldAnno.description().isEmpty()) {
                                propSchema.setDescription(fieldAnno.description());
                            }
                            if (isFieldRequired(field)) {
                                if (schema.getRequired() == null) {
                                    schema.setRequired(new ArrayList<>());
                                }
                                if (!schema.getRequired().contains(propEntry.getKey())) {
                                    schema.getRequired().add(propEntry.getKey());
                                }
                            }
                        }

                        if (propSchema.get$ref() != null) {
                            String refName = propSchema.get$ref().substring(propSchema.get$ref().lastIndexOf('/') + 1);
                            addTitleToSchemaRecursive(allSchemas.get(refName), fieldType, allSchemas, discoveredTypes, visited);
                        } else if ("array".equals(propSchema.getType()) && propSchema.getItems() != null) {
                            propSchema.setTitle(getTypeName(fieldType));
                            Type itemType = null;
                            if (fieldType instanceof ParameterizedType) {
                                itemType = ((ParameterizedType) fieldType).getActualTypeArguments()[0];
                            } else if (fieldType instanceof Class && ((Class<?>) fieldType).isArray()) {
                                itemType = ((Class<?>) fieldType).getComponentType();
                            }

                            if (itemType != null) {
                                addTitleToSchemaRecursive(propSchema.getItems(), itemType, allSchemas, discoveredTypes, visited);
                            }
                        } else {
                            addTitleToSchemaRecursive(propSchema, fieldType, allSchemas, discoveredTypes, visited);
                        }
                    }
                }
            }
        } finally {
            if (type != null) {
                visited.remove(type);
            }
        }
    }

    /**
     * Attempts to resolve a Java type from a Schema branch, usually via its ref pointer.
     * 
     * Parses the schema's ref path to extract the simple name, and looks it up in the map of discovered types.
     * @param schema The Schema object to extract the type ref from.
     * @param discoveredTypes The map of discovered types reachable from the root.
     * @return The resolved Java Type, or null if no ref pointer exists.
     */
    private static Type resolveTypeFromSchema(Schema schema, Map<String, Type> discoveredTypes) {
        if (schema.get$ref() != null) {
            String refName = schema.get$ref().substring(schema.get$ref().lastIndexOf('/') + 1);
            return discoveredTypes.get(refName);
        }
        return null;
    }

    /**
     * Recursively inlines definitions into the schema.
     *
     * @param currentNode The current node in the schema tree.
     * @param definitions The map of definitions.
     * @param visitedRefs The set of visited references.
     * @return The inlined JsonNode.
     */
    private static JsonNode inlineDefinitionsRecursive(JsonNode currentNode, JsonNode definitions, Set<String> visitedRefs) {
        if (currentNode.isObject()) {
            ObjectNode objectNode = (ObjectNode) currentNode.deepCopy();
            if (objectNode.hasNonNull("$ref")) {
                String refPath = objectNode.get("$ref").asText();
                if (visitedRefs.contains(refPath)) {
                    return createRecursiveReferenceNode(refPath, definitions);
                }
                visitedRefs.add(refPath);
                String refName = refPath.substring(refPath.lastIndexOf('/') + 1);
                JsonNode definition = definitions.path(refName);
                ObjectNode mergedNode = objectNode.deepCopy();
                mergedNode.remove("$ref");
                if (definition.isObject()) {
                    ((ObjectNode) definition).fields().forEachRemaining(entry -> {
                        if (!mergedNode.has(entry.getKey())) {
                            mergedNode.set(entry.getKey(), entry.getValue());
                        } else if (entry.getKey().equals("description")) {
                            String parentDesc = mergedNode.get("description").asText();
                            String childDesc = entry.getValue().asText();
                            if (!parentDesc.contains(childDesc)) {
                                mergedNode.put("description", parentDesc + "\n\n" + childDesc);
                            }
                        }
                    });
                }
                JsonNode result = inlineDefinitionsRecursive(mergedNode, definitions, visitedRefs);
                visitedRefs.remove(refPath);
                return result;
            } else {
                objectNode.fields().forEachRemaining(field -> field.setValue(inlineDefinitionsRecursive(field.getValue(), definitions, visitedRefs)));
                return objectNode;
            }
        } else if (currentNode.isArray()) {
            List<JsonNode> newItems = new ArrayList<>();
            currentNode.forEach(item -> newItems.add(inlineDefinitionsRecursive(item, definitions, visitedRefs)));
            return OBJECT_MAPPER.createArrayNode().addAll(newItems);
        }
        return currentNode;
    }

    /**
     * Creates a node representing a recursive reference.
     *
     * @param refPath The reference path.
     * @param definitions The map of definitions.
     * @return The recursive reference ObjectNode.
     */
    private static ObjectNode createRecursiveReferenceNode(String refPath, JsonNode definitions) {
        String refName = refPath.substring(refPath.lastIndexOf('/') + 1);
        JsonNode definition = definitions.path(refName);
        String fqn = definition.path("title").isMissingNode() ? "N/A" : definition.path("title").asText();
        String originalDescription = definition.path("description").asText("");
        String newDescription = "Recursive reference to " + fqn + (originalDescription.isEmpty() ? "" : ". " + originalDescription);
        ObjectNode recursiveNode = OBJECT_MAPPER.createObjectNode();
        recursiveNode.put("type", "object");
        recursiveNode.put("title", fqn);
        recursiveNode.put("description", newDescription);
        return recursiveNode;
    }

    /**
     * Finds the name of the root schema for a given type.
     *
     * @param schemas The map of generated schemas.
     * @param type The Java type.
     * @return The name of the root schema.
     */
    private static String findRootSchemaName(Map<String, Schema> schemas, Type type) {
        String preferredName = getRawClass(type) != null ? getRawClass(type).getSimpleName() : type.getTypeName();
        if (schemas.size() == 1) {
            return schemas.keySet().iterator().next();
        }
        if (schemas.containsKey(preferredName) && schemas.get(preferredName).get$ref() == null) {
            return preferredName;
        }
        return schemas.entrySet().stream()
                .filter(e -> e.getValue().get$ref() == null)
                .map(Map.Entry::getKey)
                .findFirst()
                .orElse(preferredName);
    }

    /**
     * Gets the fully qualified name of a Java type.
     *
     * @param type The Java type.
     * @return The FQN string.
     */
    private static String getTypeName(Type type) {
        if (type instanceof Class) {
            return ((Class<?>) type).getCanonicalName();
        }
        if (type instanceof ParameterizedType) {
            ParameterizedType pt = (ParameterizedType) type;
            String args = Arrays.stream(pt.getActualTypeArguments()).map(SchemaProvider::getTypeName).collect(Collectors.joining(", "));
            return getTypeName(pt.getRawType()) + "<" + args + ">";
        }
        return type.getTypeName();
    }

    /**
     * Finds a field in a class or its superclasses.
     *
     * @param clazz The class to search.
     * @param name The name of the field.
     * @return The Field object, or null if not found.
     */
    private static Field findField(Class<?> clazz, String name) {
        for (Class<?> c = clazz; c != null; c = c.getSuperclass()) {
            try {
                return c.getDeclaredField(name);
            } catch (NoSuchFieldException e) {
                /* continue */ }
        }
        return null;
    }

    /**
     * Gets all fields of a class, including those from superclasses.
     *
     * @param clazz The class to inspect.
     * @return A list of all fields.
     */
    private static List<Field> getAllFields(Class<?> clazz) {
        List<Field> fields = new ArrayList<>();
        for (Class<?> c = clazz; c != null; c = c.getSuperclass()) {
            fields.addAll(Arrays.asList(c.getDeclaredFields()));
        }
        return fields;
    }

    /**
     * Gets the raw Class object for a given Type.
     *
     * @param type The Java type.
     * @return The raw Class object, or null if not applicable.
     */
    private static Class<?> getRawClass(Type type) {
        if (type instanceof Class) {
            return (Class<?>) type;
        }
        if (type instanceof ParameterizedType) {
            return (Class<?>) ((ParameterizedType) type).getRawType();
        }
        return null;
    }

    /**
     * Checks if a class belongs to the JDK (java.* package or primitive).
     *
     * @param clazz The class to check.
     * @return true if it's a JDK class.
     */
    private static boolean isJdkClass(Class<?> clazz) {
        return clazz != null && (clazz.isPrimitive() || (clazz.getPackage() != null && clazz.getPackage().getName().startsWith("java.")) || clazz.getName().startsWith("java."));
    }

    /**
     * Enriches an enum schema's description with detailed information about its constant values.
     * 
     * This checks for class-level and constant-level {@code @Schema} annotations and appends a descriptive markdown list detailing every constant alongside its description, providing high-precision context for AI tool-calling.
     * @param clazz The concrete Java Enum class to inspect.
     * @param schema The Schema model to enrich.
     */
    private static void enrichEnumDescription(Schema schema, Class<?> clazz) {
        Object[] constants = clazz.getEnumConstants();
        StringBuilder descBuilder = new StringBuilder();
        String existingDesc = schema.getDescription();

        var classAnno = getSchemaAnno(clazz);
        String classDesc = (classAnno != null && !classAnno.description().isEmpty()) ? classAnno.description() : null;

        if (existingDesc != null && !existingDesc.isBlank()) {
            descBuilder.append(existingDesc);
            if (classDesc != null && !existingDesc.contains(classDesc)) {
                descBuilder.append("\n\n").append(classDesc);
            }
        } else if (classDesc != null) {
            descBuilder.append(classDesc);
        }

        String currentDescSoFar = descBuilder.toString();
        boolean hasValues = currentDescSoFar.contains("(Details: Values:") || currentDescSoFar.contains("Available Values:");
        if (!hasValues) {
            if (!currentDescSoFar.isEmpty()) {
                descBuilder.append("\n\n(Details: Values:");
            } else {
                descBuilder.append("(Details: Values:");
            }
            for (Object obj : constants) {
                String name = obj.toString();
                descBuilder.append("\n- `").append(name).append("` ");
                try {
                    Field field = clazz.getField(name);
                    var fieldAnnos = getSchemaAnno(field);
                    if (fieldAnnos != null && !fieldAnnos.description().isEmpty()) {
                        descBuilder.append(": ").append(fieldAnnos.description());
                    }
                } catch (Exception e) {
                }
            }
            descBuilder.append(")");
            schema.setDescription(descBuilder.toString());
        }
    }

    /**
     * Retrieves the Swagger {@code @Schema} annotation defined at the class level.
     * @param clazz The class to inspect.
     * @return The Schema annotation instance, or null if not annotated.
     */
    private static io.swagger.v3.oas.annotations.media.Schema getSchemaAnno(Class<?> clazz) {
        return clazz.getAnnotation(io.swagger.v3.oas.annotations.media.Schema.class);
    }

    /**
     * Retrieves the Swagger {@code @Schema} annotation defined on a Field.
     * @param field The field to inspect.
     * @return The Schema annotation instance, or null if not annotated.
     */
    private static io.swagger.v3.oas.annotations.media.Schema getSchemaAnno(Field field) {
        return field.getAnnotation(io.swagger.v3.oas.annotations.media.Schema.class);
    }

    /**
     * Determines whether a Field is marked as required based on its {@code @Schema} annotation.
     * 
     * Checks both the legacy {@code required()} boolean flag and the newer {@code requiredMode()} enum value matching {@code REQUIRED}.
     * @param field The field to inspect.
     * @return true if the field is explicitly marked as required, otherwise false.
     */
    private static boolean isFieldRequired(Field field) {
        var anno = getSchemaAnno(field);
        return anno != null && (anno.required() || anno.requiredMode() == RequiredMode.REQUIRED);
    }
}
