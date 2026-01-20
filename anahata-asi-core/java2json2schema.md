# Java to JSON Schema Generation (`SchemaProvider`)

The `SchemaProvider` is a core component of the Anahata ASI framework responsible for bridging the gap between Java's rich type system and the JSON-based tool definitions required by LLMs (like Gemini).

## 1. Core Objective

LLMs require tool parameters and return types to be defined using JSON Schema (typically OpenAPI 3.0 compliant). `SchemaProvider` automates this by reflecting on Java `Type` objects and generating a self-contained, inlined JSON schema.

## 2. Key Features

-   **FQN Enrichment**: Every object, property, and array item in the generated schema includes a `title` field containing the Fully Qualified Name (FQN) of the corresponding Java type. This provides the model with precise semantic context.
-   **Deep Inlining**: Unlike standard Swagger/OpenAPI generators that use `$ref` to a `components/schemas` section, `SchemaProvider` recursively inlines all definitions. This results in a single, flat JSON object that is easier for LLMs to process in a single turn.
-   **Recursive Type Handling**: It correctly detects and handles recursive data structures (e.g., a `TreeNode` containing a list of `TreeNode`s) by injecting a descriptive "Recursive reference" string instead of causing an infinite loop.
-   **Generic Type Support**: It preserves and renders generic type information (e.g., `java.util.List<java.lang.String>`) in the `title` fields.
-   **Map Support**: (New) It handles `java.util.Map<String, T>` by generating an `object` with `additionalProperties` matching the schema of `T`.

## 3. Generation Logic

1.  **Simple Types**: Primitives, `String`, `Number`, `Boolean`, and `Enum` are handled directly with standard JSON types.
2.  **Collections & Arrays**: `List`, `Set`, `Collection`, and arrays (`T[]`) are mapped to the `array` JSON type, with the item schema generated recursively.
3.  **Maps**: `Map<String, T>` is mapped to the `object` JSON type with `additionalProperties`.
4.  **Complex Objects**: For custom POJOs, it leverages `ModelConverters` (from Swagger Core) to generate the base schema and then post-processes it to add FQN titles and perform inlining.

## 4. Usage in the Framework

-   **`JavaMethodToolParameter`**: Uses `SchemaProvider` to generate the schema for each method parameter.
-   **`JavaMethodTool`**: Uses it to generate the schema for the return type (often wrapped in `JavaMethodToolResponse`).

## 5. Example: `Map<String, String>`

A `Map<String, String>` parameter will produce a schema like:

```json
{
  "type": "object",
  "title": "java.util.Map<java.lang.String, java.lang.String>",
  "additionalProperties": {
    "type": "string",
    "title": "java.lang.String"
  }
}
```
