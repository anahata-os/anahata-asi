/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.tool.schema;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.module.mrbean.MrBeanModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import io.swagger.v3.core.converter.AnnotatedType;
import io.swagger.v3.core.converter.ModelConverters;
import io.swagger.v3.core.jackson.ModelResolver;
import io.swagger.v3.oas.models.media.Schema;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.WildcardType;
import java.util.*;
import java.util.stream.Collectors;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

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
public class SchemaProvider {

    /** The centrally configured Jackson ObjectMapper for JSON operations. */
    public static final ObjectMapper OBJECT_MAPPER = new ObjectMapper()
            .registerModule(new ParameterNamesModule())
            .registerModule(new MrBeanModule())
            .setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY)
            .setVisibility(PropertyAccessor.GETTER, JsonAutoDetect.Visibility.NONE)
            .setVisibility(PropertyAccessor.IS_GETTER, JsonAutoDetect.Visibility.NONE);

    /** The centrally configured Gson instance for pretty-printing JSON. */
    private static final Gson GSON = new GsonBuilder().setPrettyPrinting().create();

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
        String baseSchemaJson = generateStandardSchema(wrapperType);
        if (baseSchemaJson == null) {
            return null;
        }

        // 2. Parse the base schema into a mutable JsonNode
        JsonNode rootNode = OBJECT_MAPPER.readTree(baseSchemaJson);
        ObjectNode mutableRoot = (ObjectNode) rootNode;

        // 3. Get the properties node from the base schema
        JsonNode propertiesNode = mutableRoot.path("properties");
        if (!propertiesNode.isObject()) {
            return GSON.toJson(OBJECT_MAPPER.treeToValue(mutableRoot, Map.class));
        }
        ObjectNode properties = (ObjectNode) propertiesNode;

        // 4. Handle the wrappedType
        if (wrappedType == null || wrappedType.equals(void.class) || wrappedType.equals(Void.class)) {
            properties.remove(attributeName);
        } else {
            String wrappedSchemaJson = generateStandardSchema(wrappedType);
            if (wrappedSchemaJson != null && properties.has(attributeName)) {
                JsonNode wrappedRootNode = OBJECT_MAPPER.readTree(wrappedSchemaJson);

                // Merge the definitions ('components.schemas') from the wrapped schema into the base schema
                JsonNode wrappedDefinitions = wrappedRootNode.path("components").path("schemas");
                ObjectNode baseDefinitions = (ObjectNode) mutableRoot.path("components").path("schemas");

                if (wrappedDefinitions.isObject()) {
                    wrappedDefinitions.fields().forEachRemaining(entry -> {
                        baseDefinitions.set(entry.getKey(), entry.getValue());
                    });
                }

                // Now, inject the main part of the wrapped schema (without its components)
                ObjectNode wrappedSchemaObject = (ObjectNode) wrappedRootNode.deepCopy();
                wrappedSchemaObject.remove("components");
                properties.set(attributeName, wrappedSchemaObject);
            }
        }

        // 5. Now, with the schemas combined and definitions merged, perform a single recursive inlining pass.
        JsonNode definitions = mutableRoot.path("components").path("schemas");
        JsonNode inlinedNode = inlineDefinitionsRecursive(mutableRoot, definitions, new HashSet<>());

        // 6. Clean up and return the final schema string
        if (inlinedNode instanceof ObjectNode) {
            ((ObjectNode) inlinedNode).remove("components");
        }
        return GSON.toJson(OBJECT_MAPPER.treeToValue(inlinedNode, Map.class));
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
        String standardSchemaJson = generateStandardSchema(type);
        if (standardSchemaJson == null) {
            return null;
        }
        JsonNode rootNode = OBJECT_MAPPER.readTree(standardSchemaJson);
        JsonNode definitions = rootNode.path("components").path("schemas");
        JsonNode inlinedNode = inlineDefinitionsRecursive(rootNode, definitions, new HashSet<>());
        if (inlinedNode instanceof ObjectNode) {
            ((ObjectNode) inlinedNode).remove("components");
        }
        return GSON.toJson(OBJECT_MAPPER.treeToValue(inlinedNode, Map.class));
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

        ModelConverters converters = new ModelConverters();
        converters.addConverter(new ModelResolver(OBJECT_MAPPER));
        Map<String, Schema> swaggerSchemas = converters.readAll(new AnnotatedType(type));
        if (swaggerSchemas.isEmpty()) {
            return null;
        }
        postProcessAndEnrichSchemas(type, swaggerSchemas);
        Schema rootSchema = createRootSchema(type, swaggerSchemas);
        Map<String, Object> finalSchemaMap = new LinkedHashMap<>(OBJECT_MAPPER.convertValue(rootSchema, Map.class));
        Map<String, Object> components = new LinkedHashMap<>();
        Map<String, Object> componentSchemas = new LinkedHashMap<>();
        swaggerSchemas.forEach((key, schema) -> componentSchemas.put(key, OBJECT_MAPPER.convertValue(schema, Map.class)));
        components.put("schemas", componentSchemas);
        finalSchemaMap.put("components", components);
        return GSON.toJson(finalSchemaMap);
    }

    /**
     * Handles schema generation for simple Java types (String, Number, Boolean, Enum).
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

        io.swagger.v3.oas.annotations.media.Schema schemaAnnotation = clazz.getAnnotation(io.swagger.v3.oas.annotations.media.Schema.class);
        if (schemaAnnotation != null && !schemaAnnotation.description().isEmpty()) {
            schemaMap.put("description", schemaAnnotation.description());
        }

        if (clazz.equals(String.class)) {
            schemaMap.put("type", "string");
        } else if (Number.class.isAssignableFrom(clazz) || clazz.isPrimitive() && (clazz.equals(int.class) || clazz.equals(long.class) || clazz.equals(float.class) || clazz.equals(double.class))) {
            schemaMap.put("type", "number");
        } else if (clazz.equals(Boolean.class) || clazz.equals(boolean.class)) {
            schemaMap.put("type", "boolean");
        } else if (clazz.isEnum()) {
            schemaMap.put("type", "string");
            List<String> enumValues = Arrays.stream(clazz.getEnumConstants()).map(Object::toString).collect(Collectors.toList());
            schemaMap.put("enum", enumValues);
        } else {
            return null;
        }

        return GSON.toJson(schemaMap);
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
        arraySchema.put("items", OBJECT_MAPPER.treeToValue(itemSchema, Map.class));

        Map<String, Object> finalMap = new LinkedHashMap<>();
        finalMap.putAll(arraySchema);

        // Carry over components if the item is a complex object
        JsonNode itemComponents = itemSchema.path("components").path("schemas");
        if (itemComponents.isObject() && itemComponents.size() > 0) {
            Map<String, Object> components = new LinkedHashMap<>();
            components.put("schemas", OBJECT_MAPPER.treeToValue(itemComponents, Map.class));
            finalMap.put("components", components);
        }

        return GSON.toJson(finalMap);
    }

    /**
     * Builds a JSON schema for a Map<String, T> type.
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
        mapSchema.put("additionalProperties", OBJECT_MAPPER.treeToValue(valueSchema, Map.class));

        Map<String, Object> finalMap = new LinkedHashMap<>();
        finalMap.putAll(mapSchema);

        // Carry over components if the value is a complex object
        JsonNode valueComponents = valueSchema.path("components").path("schemas");
        if (valueComponents.isObject() && valueComponents.size() > 0) {
            Map<String, Object> components = new LinkedHashMap<>();
            components.put("schemas", OBJECT_MAPPER.treeToValue(valueComponents, Map.class));
            finalMap.put("components", components);
        }

        return GSON.toJson(finalMap);
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
     * Post-processes and enriches the generated schemas with type information.
     * 
     * @param rootType The root Java type.
     * @param allSchemas The map of all generated schemas.
     */
    private static void postProcessAndEnrichSchemas(Type rootType, Map<String, Schema> allSchemas) {
        Map<String, Type> discoveredTypes = new HashMap<>();
        findAllTypesRecursive(rootType, discoveredTypes, new HashSet<>());
        for (Map.Entry<String, Schema> entry : allSchemas.entrySet()) {
            String schemaName = entry.getKey();
            Schema schema = entry.getValue();
            Type type = discoveredTypes.get(schemaName);
            if (type != null) {
                addTitleToSchemaRecursive(schema, type, allSchemas, new HashSet<>());
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
    private static void findAllTypesRecursive(Type type, Map<String, Type> foundTypes, Set<Type> visited) {
        if (type == null || !visited.add(type)) {
            return;
        }
        if (type instanceof Class) {
            Class<?> clazz = (Class<?>) type;
            if (!isJdkClass(clazz)) {
                foundTypes.put(clazz.getSimpleName(), clazz);
                for (Field field : getAllFields(clazz)) {
                    findAllTypesRecursive(field.getGenericType(), foundTypes, visited);
                }
            }
        } else if (type instanceof ParameterizedType) {
            ParameterizedType pt = (ParameterizedType) type;
            findAllTypesRecursive(pt.getRawType(), foundTypes, visited);
            for (Type arg : pt.getActualTypeArguments()) {
                findAllTypesRecursive(arg, foundTypes, visited);
            }
        } else if (type instanceof WildcardType) {
            WildcardType wt = (WildcardType) type;
            Arrays.stream(wt.getUpperBounds()).forEach(b -> findAllTypesRecursive(b, foundTypes, visited));
            Arrays.stream(wt.getLowerBounds()).forEach(b -> findAllTypesRecursive(b, foundTypes, visited));
        }
    }

    /**
     * Recursively adds titles (FQN) to schemas and their properties.
     * 
     * @param schema The schema to enrich.
     * @param type The corresponding Java type.
     * @param allSchemas The map of all schemas.
     * @param visited The set of visited types.
     */
    private static void addTitleToSchemaRecursive(Schema schema, Type type, Map<String, Schema> allSchemas, Set<Type> visited) {
        if (schema == null || type == null || !visited.add(type)) {
            return;
        }
        try {
            if (schema.getTitle() == null) {
                schema.setTitle(getTypeName(type));
            }
            Class<?> rawClass = getRawClass(type);
            if (rawClass == null || isJdkClass(rawClass) || schema.getProperties() == null) {
                return;
            }
            for (Map.Entry<String, Schema> propEntry : (Set<Map.Entry<String, Schema>>) schema.getProperties().entrySet()) {
                Field field = findField(rawClass, propEntry.getKey());
                if (field != null) {
                    Schema propSchema = propEntry.getValue();
                    Type fieldType = field.getGenericType();
                    if (propSchema.get$ref() != null) {
                        String refName = propSchema.get$ref().substring(propSchema.get$ref().lastIndexOf('/') + 1);
                        addTitleToSchemaRecursive(allSchemas.get(refName), fieldType, allSchemas, visited);
                    } else if ("array".equals(propSchema.getType()) && propSchema.getItems() != null) {
                        propSchema.setTitle(getTypeName(fieldType));
                        Type itemType = null;
                        if (fieldType instanceof ParameterizedType) {
                            itemType = ((ParameterizedType) fieldType).getActualTypeArguments()[0];
                        } else if (fieldType instanceof Class && ((Class<?>) fieldType).isArray()) {
                            itemType = ((Class<?>) fieldType).getComponentType();
                        }
                        
                        if (itemType != null) {
                            addTitleToSchemaRecursive(propSchema.getItems(), itemType, allSchemas, visited);
                        }
                    } else {
                        propSchema.setTitle(getTypeName(fieldType));
                    }
                }
            }
        } finally {
            visited.remove(type);
        }
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
            if (objectNode.has("$ref")) {
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
        String fqn = definition.path("title").asText("N/A");
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
}
