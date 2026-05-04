# Using tools

import {
  File,
  Functions,
  ImageSquare,
  Code,
} from "@components/react/oai/platform/ui/Icon.react";










When generating model responses or building agents, you can extend capabilities using built‑in tools, function calling, tool search, and remote MCP servers. These enable the model to search the web, retrieve from your files, load deferred tool definitions at runtime, call your own functions, or access third‑party services. Only `gpt-5.4` and later models support `tool_search`.



<div data-content-switcher-pane data-value="web-search">
    <div class="hidden">Web search</div>
    </div>
  <div data-content-switcher-pane data-value="file-search" hidden>
    <div class="hidden">File search</div>
    Search your files in a response

```python
from openai import OpenAI
client = OpenAI()

response = client.responses.create(
    model="gpt-5.5",
    input="What is deep research by OpenAI?",
    tools=[{
        "type": "file_search",
        "vector_store_ids": ["<vector_store_id>"]
    }]
)
print(response)
```

```javascript
import OpenAI from "openai";
const openai = new OpenAI();

const response = await openai.responses.create({
    model: "gpt-5.5",
    input: "What is deep research by OpenAI?",
    tools: [
        {
            type: "file_search",
            vector_store_ids: ["<vector_store_id>"],
        },
    ],
});
console.log(response);
```

```csharp
using OpenAI.Responses;

string key = Environment.GetEnvironmentVariable("OPENAI_API_KEY")!;
OpenAIResponseClient client = new(model: "gpt-5.5", apiKey: key);

ResponseCreationOptions options = new();
options.Tools.Add(ResponseTool.CreateFileSearchTool(["<vector_store_id>"]));

OpenAIResponse response = (OpenAIResponse)client.CreateResponse([
    ResponseItem.CreateUserMessageItem([
        ResponseContentPart.CreateInputTextPart("What is deep research by OpenAI?"),
    ]),
], options);

Console.WriteLine(response.GetOutputText());
```

  </div>
  <div data-content-switcher-pane data-value="tool-search" hidden>
    <div class="hidden">Tool search</div>
    </div>
  <div data-content-switcher-pane data-value="function-calling" hidden>
    <div class="hidden">Function calling</div>
    </div>
  <div data-content-switcher-pane data-value="remote-mcp" hidden>
    <div class="hidden">Remote MCP</div>
    Call a remote MCP server

```bash
curl https://api.openai.com/v1/responses \\ 
-H "Content-Type: application/json" \\ 
-H "Authorization: Bearer $OPENAI_API_KEY" \\ 
-d '{
  "model": "gpt-5.5",
    "tools": [
      {
        "type": "mcp",
        "server_label": "dmcp",
        "server_description": "A Dungeons and Dragons MCP server to assist with dice rolling.",
        "server_url": "https://dmcp-server.deno.dev/sse",
        "require_approval": "never"
      }
    ],
    "input": "Roll 2d4+1"
  }'
```

```javascript
import OpenAI from "openai";
const client = new OpenAI();

const resp = await client.responses.create({
  model: "gpt-5.5",
  tools: [
    {
      type: "mcp",
      server_label: "dmcp",
      server_description: "A Dungeons and Dragons MCP server to assist with dice rolling.",
      server_url: "https://dmcp-server.deno.dev/sse",
      require_approval: "never",
    },
  ],
  input: "Roll 2d4+1",
});

console.log(resp.output_text);
```

```python
from openai import OpenAI

client = OpenAI()

resp = client.responses.create(
    model="gpt-5.5",
    tools=[
        {
            "type": "mcp",
            "server_label": "dmcp",
            "server_description": "A Dungeons and Dragons MCP server to assist with dice rolling.",
            "server_url": "https://dmcp-server.deno.dev/sse",
            "require_approval": "never",
        },
    ],
    input="Roll 2d4+1",
)

print(resp.output_text)
```

```csharp
using OpenAI.Responses;

string key = Environment.GetEnvironmentVariable("OPENAI_API_KEY")!;
OpenAIResponseClient client = new(model: "gpt-5.5", apiKey: key);

ResponseCreationOptions options = new();
options.Tools.Add(ResponseTool.CreateMcpTool(
    serverLabel: "dmcp",
    serverUri: new Uri("https://dmcp-server.deno.dev/sse"),
    toolCallApprovalPolicy: new McpToolCallApprovalPolicy(GlobalMcpToolCallApprovalPolicy.NeverRequireApproval)
));

OpenAIResponse response = (OpenAIResponse)client.CreateResponse([
    ResponseItem.CreateUserMessageItem([
        ResponseContentPart.CreateInputTextPart("Roll 2d4+1")
    ])
], options);

Console.WriteLine(response.GetOutputText());
```

  </div>



## Available tools

Here's an overview of the tools available in the OpenAI platform—select one of them for further guidance on usage.

<a href="/api/docs/guides/function-calling">
  

<span slot="icon">
      </span>
    Call custom code to give the model access to additional data and
    capabilities.


</a>

<a href="/api/docs/guides/tools-web-search">
  

<span slot="icon">
      </span>
    Include data from the Internet in model response generation.


</a>

<a href="/api/docs/guides/tools-connectors-mcp">
  

<span slot="icon">
      </span>
    Give the model access to new capabilities via Model Context Protocol (MCP)
    servers.


</a>

<a href="/api/docs/guides/tools-skills">
  

<span slot="icon">
      </span>
    Upload and reuse versioned skill bundles in hosted shell environments.


</a>

<a href="/api/docs/guides/tools-shell">
  

<span slot="icon">
      </span>
    Run shell commands in hosted containers or in your own local runtime.


</a>

<a href="/api/docs/guides/tools-computer-use">
  

<span slot="icon">
      </span>
    Create agentic workflows that enable a model to control a computer
    interface.


</a>

<a href="/api/docs/guides/tools-image-generation">
  

<span slot="icon">
      </span>
    Generate or edit images using GPT Image.


</a>

<a href="/api/docs/guides/tools-file-search">
  

<span slot="icon">
      </span>
    Search the contents of uploaded files for context when generating a
    response.


</a>

<a href="/api/docs/guides/tools-tool-search">
  

<span slot="icon">
      </span>
    Dynamically load relevant tools into the model’s context to optimize token
    usage.


</a>

## Usage in the API

When making a request to generate a [model response](https://developers.openai.com/api/docs/api-reference/responses/create), you usually enable tool access by specifying configurations in the `tools` parameter. Each tool has its own unique configuration requirements—see the [Available tools](#available-tools) section for detailed instructions.

Based on the provided [prompt](https://developers.openai.com/api/docs/guides/text), the model automatically decides whether to use a configured tool. For instance, if your prompt requests information beyond the model's training cutoff date and web search is enabled, the model will typically invoke the web search tool to retrieve relevant, up-to-date information.

Some advanced workflows can also load more tool definitions during the interaction. For example, [tool search](https://developers.openai.com/api/docs/guides/tools-tool-search) can defer function definitions until the model decides they're needed.

You can explicitly control or guide this behavior by setting the `tool_choice` parameter [in the API request](https://developers.openai.com/api/docs/api-reference/responses/create).

## Usage in the Agents SDK

In the Agents SDK, the tool semantics stay the same, but the wiring moves into the agent definition and workflow design rather than a single Responses API request.

- Attach hosted tools, function tools, or hosted MCP tools directly on the agent when one specialist should call them itself.
- Expose a specialist as a tool when a manager should stay in control of the user-facing reply.
- Keep shell, apply patch, and computer-use harnesses in your runtime even when the SDK models the tool decision.

Wrap local logic as a function tool

```typescript
import { tool } from "@openai/agents";
import { z } from "zod";

const getWeatherTool = tool({
  name: "get_weather",
  description: "Get the weather for a given city.",
  parameters: z.object({ city: z.string() }),
  async execute({ city }) {
    return \`The weather in \${city} is sunny.\`;
  },
});
```

```python
from agents import function_tool


@function_tool
def get_weather(city: str) -> str:
    """Get the weather for a given city."""
    return f"The weather in {city} is sunny."
```


Expose a specialist as a tool

```typescript
import { Agent } from "@openai/agents";

const summarizer = new Agent({
  name: "Summarizer",
  instructions: "Generate a concise summary of the supplied text.",
});

const mainAgent = new Agent({
  name: "Research assistant",
  tools: [
    summarizer.asTool({
      toolName: "summarize_text",
      toolDescription: "Generate a concise summary of the supplied text.",
    }),
  ],
});
```

```python
from agents import Agent

summarizer = Agent(
    name="Summarizer",
    instructions="Generate a concise summary of the supplied text.",
)

main_agent = Agent(
    name="Research assistant",
    tools=[
        summarizer.as_tool(
            tool_name="summarize_text",
            tool_description="Generate a concise summary of the supplied text.",
        )
    ],
)
```


Use [Agent definitions](https://developers.openai.com/api/docs/guides/agents/define-agents) when you are shaping a single specialist, [Orchestration and handoffs](https://developers.openai.com/api/docs/guides/agents/orchestration) when tools affect ownership, [Guardrails and human review](https://developers.openai.com/api/docs/guides/agents/guardrails-approvals) when tools affect approvals, and [Integrations and observability](https://developers.openai.com/api/docs/guides/agents/integrations-observability#mcp) when the capability comes from MCP.