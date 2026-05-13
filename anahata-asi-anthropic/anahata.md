# Project Instructions: /home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-anthropic

This file contains project-specific system instructions for the **/home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-anthropic** project.

**Note**: This is a **Sub-module** of **anahata-asi-parent**. These instructions are intended to extend the shared context provided by the parent project's `anahata.md`.

From: https://platform.minimax.io/docs/api-reference/api-overview

> ## Documentation Index
> Fetch the complete documentation index at: https://platform.minimax.io/docs/llms.txt
> Use this file to discover all available pages before exploring further.

<AgentInstructions>

## Submitting Feedback

If you encounter incorrect, outdated, or confusing documentation on this page, submit feedback:

POST https://platform.minimax.io/docs/feedback

```json
{
  "path": "/api-reference/api-overview",
  "feedback": "Description of the issue"
}
```

Only submit feedback when you have something specific and actionable to report.

</AgentInstructions>

# API Overview

> Overview of MiniMax API capabilities including text, speech, video, image, music, and file management.

## Get API Key

* **Pay-as-you-go**：Visit [API Keys > Create new secret key](https://platform.minimax.io/user-center/basic-information/interface-key) to get your **API Key**
  <Note>Pay-as-you-go supports all modality models, including Text, Video, Speech, and Image.</Note>

* **Token Plan**：Visit [Billing > Token Plan](https://platform.minimax.io/user-center/payment/token-plan) to view your **Token Plan Key**
  <Note>The Token Plan Key is used for Token Plan quotas and Credits. It is separate from pay-as-you-go API Keys. See [Token Plan Overview](https://platform.minimax.io/docs/token-plan/intro) for details.</Note>

***

## Text Generation

The text generation API uses **MiniMax M2.7**, **MiniMax M2.7 highspeed**, **MiniMax M2.5**, **MiniMax M2.5 highspeed**, **MiniMax M2.1**, **MiniMax M2.1 highspeed**, **MiniMax M2** to generate conversational content and trigger tool calls based on the provided context.

It can be accessed via **HTTP requests**, the **Anthropic SDK** (Recommended), or the **OpenAI SDK**.

### Supported Models

| Model Name             | Context Window | Description                                                                                                                                   |
| :--------------------- | :------------- | :-------------------------------------------------------------------------------------------------------------------------------------------- |
| MiniMax-M2.7           | 204,800        | **Beginning the journey of recursive self-improvement. (output speed approximately 60 tps)**                                                  |
| MiniMax-M2.7-highspeed | 204,800        | **M2.7 highspeed: Same performance, faster and more agile (output speed approximately 100 tps)**                                              |
| MiniMax-M2.5           | 204,800        | **Peak Performance. Ultimate Value. Master the Complex (output speed approximately 60 tps)**                                                  |
| MiniMax-M2.5-highspeed | 204,800        | **M2.5 highspeed: Same performance, faster and more agile (output speed approximately 100 tps)**                                              |
| MiniMax-M2.1           | 204,800        | **Powerful Multi-Language Programming Capabilities with Comprehensively Enhanced Programming Experience (output speed approximately 60 tps)** |
| MiniMax-M2.1-highspeed | 204,800        | **Faster and More Agile (output speed approximately 100 tps)**                                                                                |
| MiniMax-M2             | 204,800        | **Agentic capabilities, Advanced reasoning**                                                                                                  |

Please note: The maximum token count refers to the total number of input and output tokens.

<Columns cols={2}>
  <Card title="Anthropic API Compatible (Recommended)" icon="book-open" href="/api-reference/text-anthropic-api" cta="View Docs">
    Use Anthropic SDK with MiniMax models
  </Card>

  <Card title="OpenAI API Compatible" icon="book-open" href="/api-reference/text-openai-api" cta="View Docs">
    Use OpenAI SDK with MiniMax models
  </Card>
</Columns>

***

## Text to Speech (T2A)

This API provides synchronous text-to-speech (T2A) generation, supporting up to **10,000** characters per request.
The interface is stateless: each call only processes the provided input without involving business logic, and the model does not store any user data.

**Key Features**

1. Access to 300+ system voices and custom cloned voices.
2. Adjustable volume, pitch, speed, and output formats.
3. Support for proportional audio mixing.
4. Configurable fixed time intervals.
5. Multiple audio formats and specifications supported: `mp3`, `pcm`, `flac`, `wav` (*wav is supported only in non-streaming mode*).
6. Support for streaming output.

**Typical Use Cases:** short text generation, voice chat, online social interactions.

### Supported Models

| Model            | Description                                                                                              |
| :--------------- | :------------------------------------------------------------------------------------------------------- |
| speech-2.8-hd    | Latest HD model. Ultra-realistic quality featuring sound tags.                                           |
| speech-2.8-turbo | Latest Turbo model. Seamless speed meets natural flow.                                                   |
| speech-2.6-hd    | HD model with outstanding prosody and excellent cloning similarity.                                      |
| speech-2.6-turbo | Turbo model with support for 40 languages.                                                               |
| speech-02-hd     | Superior rhythm and stability, with outstanding performance in replication similarity and sound quality. |
| speech-02-turbo  | Superior rhythm and stability, with enhanced multilingual capabilities and excellent performance.        |

### Available Interfaces

Synchronous speech synthesis provides two interfaces. Choose based on your needs:

* HTTP T2A API
* WebSocket T2A API

### Supported Languages

MiniMax speech synthesis models offer robust multilingual capability, supporting **40 widely used languages** worldwide.

| Support Languages |               |               |
| ----------------- | ------------- | ------------- |
| 1. Chinese        | 15. Turkish   | 28. Malay     |
| 2. Cantonese      | 16. Dutch     | 29. Persian   |
| 3. English        | 17. Ukrainian | 30. Slovak    |
| 4. Spanish        | 18. Thai      | 31. Swedish   |
| 5. French         | 19. Polish    | 32. Croatian  |
| 6. Russian        | 20. Romanian  | 33. Filipino  |
| 7. German         | 21. Greek     | 34. Hungarian |
| 8. Portuguese     | 22. Czech     | 35. Norwegian |
| 9. Arabic         | 23. Finnish   | 36. Slovenian |
| 10. Italian       | 24. Hindi     | 37. Catalan   |
| 11. Japanese      | 25. Bulgarian | 38. Nynorsk   |
| 12. Korean        | 26. Danish    | 39. Tamil     |
| 13. Indonesian    | 27. Hebrew    | 40. Afrikaans |
| 14. Vietnamese    |               |               |

<Columns cols={2}>
  <Card title="HTTP T2A API" icon="globe" href="/api-reference/speech-t2a-http" cta="View Docs">
    Synchronous speech synthesis via HTTP
  </Card>

  <Card title="WebSocket T2A API" icon="plug" href="/api-reference/speech-t2a-websocket" cta="View Docs">
    Streaming speech synthesis via WebSocket
  </Card>
</Columns>

***

## Asynchronous Long-Text Speech Generation (T2A Async)

This API supports asynchronous text-to-speech generation. Each request can handle up to **1 million characters**, and the resulting audio can be retrieved asynchronously.

Features supported:

1. Choose from 100+ system voices and cloned voices.
2. Customize pitch, speed, volume, bitrate, sample rate, and output format.
3. Retrieve audio metadata, such as duration and file size.
4. Retrieve precise sentence-level timestamps (subtitles).
5. Input text directly as a string or via `file_id` after uploading a text file.
6. Detect illegal characters:
   * If illegal characters are **≤10%**, audio is generated normally, with the ratio returned.
   * If illegal characters are **>10%**, no audio will be generated (an error code will be returned).

**Note:** The returned audio URL is valid for **9 hours** (32,400 seconds) from the time it is issued. After expiration, the URL becomes invalid and the generated data will be lost.

**Use Case:** Converting entire books or other long texts into audio.

### Supported Models

| Model            | Description                                                                                              |
| :--------------- | :------------------------------------------------------------------------------------------------------- |
| speech-2.8-hd    | Latest HD model. Ultra-realistic quality featuring sound tags.                                           |
| speech-2.8-turbo | Latest Turbo model. Seamless speed meets natural flow.                                                   |
| speech-2.6-hd    | HD model with outstanding prosody and excellent cloning similarity.                                      |
| speech-2.6-turbo | Turbo model with support for 40 languages.                                                               |
| speech-02-hd     | Superior rhythm and stability, with outstanding performance in replication similarity and sound quality. |
| speech-02-turbo  | Superior rhythm and stability, with enhanced multilingual capabilities and excellent performance.        |

### API Overview

This feature includes **two APIs**:

1. Create a speech generation task (returns `task_id`).
2. Query the speech generation task status using `task_id`.
3. If the task succeeds, use the returned `file_id` with the **File API** to view and download the result.

<Columns cols={2}>
  <Card title="Create Async Task" icon="circle-play" href="/api-reference/speech-t2a-async-create" cta="View Docs">
    Create a long-text speech generation task
  </Card>

  <Card title="Query Task Status" icon="search" href="/api-reference/speech-t2a-async-query" cta="View Docs">
    Query speech generation task status
  </Card>
</Columns>

***

## Voice Cloning

This API supports cloning voices from user-uploaded audio files along with optional sample audio to enhance cloning quality.

**Use cases:** fast replication of a target timbre (IP voice recreation, voice cloning) where you need to quickly clone a specific voice.

The API supports cloning from mono or stereo audio and can rapidly reproduce speech that matches the timbre of a provided reference file.

### Supported Models

| Model            | Description                                                                                              |
| :--------------- | :------------------------------------------------------------------------------------------------------- |
| speech-2.8-hd    | Latest HD model. Ultra-realistic quality featuring sound tags.                                           |
| speech-2.8-turbo | Latest Turbo model. Seamless speed meets natural flow.                                                   |
| speech-2.6-hd    | HD model with real-time response, intelligent parsing, fluent LoRA voice                                 |
| speech-2.6-turbo | Turbo model. Ultimate Value, 40 Languages                                                                |
| speech-02-hd     | Superior rhythm and stability, with outstanding performance in replication similarity and sound quality. |
| speech-02-turbo  | Superior rhythm and stability, with enhanced multilingual capabilities and excellent performance.        |

### Notes

* Using this API to clone a voice **does not** immediately incur a cloning fee. The fee is charged the **first time** you synthesize speech with the cloned voice in a T2A synthesis API.
* Voices produced via this rapid cloning API are **temporary**. To keep a cloned voice permanently, call **any** T2A speech synthesis API with that voice **within 168 hours (7 days)**.

<Columns cols={2}>
  <Card title="Upload Clone Audio" icon="upload" href="/api-reference/voice-cloning-uploadcloneaudio" cta="View Docs">
    Upload audio file to clone
  </Card>

  <Card title="Clone Voice" icon="mic" href="/api-reference/voice-cloning-clone" cta="View Docs">
    Execute voice cloning
  </Card>
</Columns>

***

## Voice Design

This API supports generating personalized custom voices based on user-provided voice description prompts.

The generated voices (voice\_id) can then be used in the T2A API and the T2A Async API for speech generation.

### Supported Models

> It is recommended to use **speech-02-hd** for the best results.

| Model            | Description                                                                                              |
| :--------------- | :------------------------------------------------------------------------------------------------------- |
| speech-2.8-hd    | Latest HD model. Ultra-realistic quality featuring sound tags.                                           |
| speech-2.8-turbo | Latest Turbo model. Seamless speed meets natural flow.                                                   |
| speech-2.6-hd    | HD model with real-time response, intelligent parsing, fluent LoRA voice                                 |
| speech-2.6-turbo | Turbo model. Ultimate Value, 40 Languages                                                                |
| speech-02-hd     | Superior rhythm and stability, with outstanding performance in replication similarity and sound quality. |
| speech-02-turbo  | Superior rhythm and stability, with enhanced multilingual capabilities and excellent performance.        |

### Notes

> * Using this API to generate a voice does not immediately incur a fee. The generation fee will be charged upon the first use of the generated voice in speech synthesis.
> * Voices generated through this API are temporary. If you wish to keep a voice permanently, you must use it in any speech synthesis API within 168 hours (7 days).

<Card title="Voice Design API" icon="wand-magic-sparkles" href="/api-reference/voice-design-design" cta="View Docs">
  Generate personalized voices from descriptions
</Card>

***

## Video Generation

This API supports generating videos based on user-provided text, images (including first frame, last frame, or reference images).

### Supported Models

| Model                   | Description                                                                                                             |
| :---------------------- | :---------------------------------------------------------------------------------------------------------------------- |
| MiniMax-Hailuo-2.3      | New video generation model, breakthroughs in body movement, facial expressions, physical realism, and prompt adherence. |
| MiniMax-Hailuo-2.3-Fast | New Image-to-video model, for value and efficiency.                                                                     |
| MiniMax-Hailuo-02       | Video generation model supporting higher resolution (1080P), longer duration (10s), and stronger adherence to prompts.  |

### API Usage Guide

Video generation is asynchronous and consists of three APIs: **Create Video Generation Task**, **Query Video Generation Task Status**, and **File Management**. Steps are as follows:

1. Use the **Create Video Generation Task API** to start a task. On success, it will return a `task_id`.
2. Use the **Query Video Generation Task Status API** with the `task_id` to check progress. When the status is `success`, a file ID (`file_id`) will be returned.
3. Use the **Download the Video File API** with the `file_id` to view and download the generated video.

<Columns cols={2}>
  <Card title="Text to Video" icon="file-text" href="/api-reference/video-generation-t2v" cta="View Docs">
    Generate video from text description
  </Card>

  <Card title="Image to Video" icon="image-plus" href="/api-reference/video-generation-i2v" cta="View Docs">
    Generate video from image
  </Card>
</Columns>

***

## Video Generation Agent

This API supports video generation tasks based on user-selected video agent templates and inputs.

### Overview

The Video Agent API works asynchronously and includes two endpoints: **Create Video Agent Task** and **Query Video Agent Task Status**.

**Usage steps:**

1. Use the **Create Video Agent Task** API to create a task and obtain a `task_id`.
2. Use the **Query Video Agent Task Status** API with the `task_id` to check the task status. Once the status is `Success`, you can retrieve the corresponding file download URL.

### Template List

For details and examples, refer to the [Video Agent Template List](/faq/video-agent-templates).

| Template ID        | Template Name       | Description                                                                                                           | media\_inputs | text\_inputs |
| :----------------- | :------------------ | :-------------------------------------------------------------------------------------------------------------------- | :------------ | :----------- |
| 392747428568649728 | Diving              | Upload a picture to generate a video of the subject in the picture completing a perfect dive                          | Required      | /            |
| 393769180141805569 | Run for Life        | Upload a photo of your pet and enter a type of wild beast to generate a survival video of your pet in the wilderness. | Required      | Required     |
| 397087679467597833 | Transformers        | Upload a photo of a car to generate a transforming car mecha video.                                                   | Required      | /            |
| 393881433990066176 | Still rings routine | Upload your photo to generate a video of the subject performing a perfect still rings routine.                        | Required      | /            |
| 393498001241890824 | Weightlifting       | Upload a photo of your pet to generate a video where the subject performs a perfect weightlifting move.               | Required      | /            |
| 393488336655310850 | Climbing            | Upload a picture to generate a video of the subject in the picture completing a perfect sport climbing                | Required      | /            |

<Columns cols={2}>
  <Card title="Create Video Agent Task" icon="circle-play" href="/api-reference/video-agent-create" cta="View Docs">
    Create a video agent task
  </Card>

  <Card title="Query Task Status" icon="search" href="/api-reference/video-agent-query" cta="View Docs">
    Query video agent task status
  </Card>
</Columns>

***

## Image Generation

This API supports images generations from text or references, allowing custom aspect ratios and resolutions for diverse needs.

### API Description

You can generate images by creating an image generation task using text prompts and/or reference images.

### Model List

| Model    | Description                                                                                                                                                              |
| :------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| image-01 | A high-quality image generation model that produces fine-grained details. Supports both text-to-image and image-to-image generation (with subject reference for people). |

<Columns cols={2}>
  <Card title="Text to Image" icon="file-text" href="/api-reference/image-generation-t2i" cta="View Docs">
    Generate image from text description
  </Card>

  <Card title="Image to Image" icon="image-plus" href="/api-reference/image-generation-i2i" cta="View Docs">
    Generate image from reference image
  </Card>
</Columns>

***

## Music Generation

This API generates a vocal song based on a music description (prompt) and lyrics.

### Models

| Model     | Usage                                                                                                                  |
| :-------- | :--------------------------------------------------------------------------------------------------------------------- |
| music-2.6 | The latest music generation model. Supports user-provided musical inspiration and lyrics to create AI-generated music. |

<Card title="Music Generation API" icon="music" href="/api-reference/music-generation" cta="View Docs">
  Generate music from description and lyrics
</Card>

***

## File Management

This API is for file management and is used with other MiniMax APIs.

### API Description

This API includes 5 endpoints: **Upload**, **List**, **Retrieve**, **Retrieve Content**, **Delete**.

### Supported File Formats

| Type     | Format                        |
| :------- | :---------------------------- |
| Document | `pdf`, `docx`, `txt`, `jsonl` |
| Audio    | `mp3`, `m4a`, `wav`           |

### Capacity and Limits

| Item                 | Limit |
| :------------------- | :---- |
| Total Capacity       | 100GB |
| Single Document Size | 512MB |

<Columns cols={2}>
  <Card title="Upload File" icon="upload" href="/api-reference/file-management-upload" cta="View Docs">
    Upload files to the platform
  </Card>

  <Card title="List Files" icon="list" href="/api-reference/file-management-list" cta="View Docs">
    Get list of uploaded files
  </Card>
</Columns>

***

## Official MCP

MiniMax provides official Model Context Protocol (MCP) server implementations:

* [Python version](https://github.com/MiniMax-AI/MiniMax-MCP)
* [JavaScript version](https://github.com/MiniMax-AI/MiniMax-MCP-JS)

Both support speech synthesis, voice cloning, video generation, and music generation. For details, refer to the [MiniMax MCP User Guide](/guides/mcp-guide).


From: https://platform.minimax.io/docs/api-reference/text-chat-anthropic

> ## Documentation Index
> Fetch the complete documentation index at: https://platform.minimax.io/docs/llms.txt
> Use this file to discover all available pages before exploring further.

<AgentInstructions>

## Submitting Feedback

If you encounter incorrect, outdated, or confusing documentation on this page, submit feedback:

POST https://platform.minimax.io/docs/feedback

```json
{
  "path": "/api-reference/text-chat-anthropic",
  "feedback": "Description of the issue"
}
```

Only submit feedback when you have something specific and actionable to report.

</AgentInstructions>

# Text Chat (Compatible Anthropic API)

> Use the Anthropic API compatible format to call MiniMax models, supporting role-playing, multi-turn conversations and other dialogue scenarios. Supports rich role settings (system, user_system, group, etc.) and example dialogue learning.



## OpenAPI

````yaml /api-reference/text/api/openapi-chat-anthropic.json POST /anthropic/v1/messages
openapi: 3.1.0
info:
  title: MiniMax Text API Anthropic
  description: >-
    MiniMax text generation API with support for chat completion and streaming
    output
  license:
    name: MIT
  version: 1.0.0
servers:
  - url: https://api.minimax.io
security:
  - apiKeyAuth: []
paths:
  /anthropic/v1/messages:
    post:
      tags:
        - Text Generation
      summary: Text Generation Anthropic
      operationId: chatCompletionAnthropic
      parameters:
        - name: Content-Type
          in: header
          required: true
          description: >-
            Media type of the request body, should be set to `application/json`
            to ensure JSON format
          schema:
            type: string
            enum:
              - application/json
            default: application/json
      requestBody:
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/CreateMessageReq'
            examples:
              Request:
                value:
                  model: MiniMax-M2.7
                  messages:
                    - role: user
                      content: Hello
              Stream:
                value:
                  model: MiniMax-M2.7
                  stream: true
                  messages:
                    - role: user
                      content: Hello
        required: true
      responses:
        '200':
          description: ''
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/CreateMessageResp'
              examples:
                Request:
                  value:
                    id: 06379fa1dfdd9047604b8abc088ea75c
                    type: message
                    role: assistant
                    model: MiniMax-M2.7
                    content:
                      - thinking: >
                          The user says "Hello". This is a simple greeting. We
                          should respond politely, greet them back, maybe ask
                          how we can help.
                        signature: >-
                          1c3a0ae890922669e9815a201f9b645abdaafe8d8b5a65a5e48f90830c6e0750
                        type: thinking
                      - text: Hello! How can I help you today?
                        type: text
                    usage:
                      input_tokens: 39
                      output_tokens: 40
                      cache_creation_input_tokens: 0
                      cache_read_input_tokens: 0
                    stop_reason: end_turn
                    base_resp:
                      status_code: 0
                      status_msg: success
                Stream:
                  value:
                    - type: message_start
                      message:
                        id: 06379fc52f6880f96d6c2102eea03cc9
                        type: message
                        role: assistant
                        content: []
                        model: MiniMax-M2.7
                        stop_reason: null
                        stop_sequence: null
                        usage:
                          input_tokens: 3
                          output_tokens: 0
                          cache_creation_input_tokens: 0
                          cache_read_input_tokens: 0
                        service_tier: standard
                    - type: ping
                    - type: content_block_start
                      index: 0
                      content_block:
                        type: thinking
                        thinking: ''
                    - type: content_block_delta
                      index: 0
                      delta:
                        type: thinking_delta
                        thinking: >+
                          The user says "Hello". The conversation starts. The
                          user presumably wants a greeting and interaction. The
                          system instructions are to be helpful and the
                          developer says anything is allowed. So I should
                          respond with a friendly greeting, maybe ask how I can
                          help. There's no conflict with policies. Just respond.


                    - type: content_block_delta
                      index: 0
                      delta:
                        type: signature_delta
                        signature: >-
                          48f709926bbaa022cc567c6d64805fd0d5306458d2b60e2f815ffcbe13b67593
                    - type: content_block_stop
                      index: 0
                    - type: content_block_start
                      index: 1
                      content_block:
                        type: text
                        text: ''
                    - type: content_block_delta
                      index: 1
                      delta:
                        type: text_delta
                        text: |-


                          Hello! How can I help you today?
                    - type: content_block_stop
                      index: 1
                    - type: message_delta
                      delta:
                        stop_reason: end_turn
                      usage:
                        input_tokens: 7
                        output_tokens: 71
                        cache_creation_input_tokens: 0
                        cache_read_input_tokens: 32
                    - type: message_stop
            text/event-stream:
              schema:
                $ref: '#/components/schemas/StreamEvent'
              examples:
                Stream:
                  value:
                    - type: message_start
                      message:
                        id: 06369732447536c3c6b051a4f612aae3
                        type: message
                        role: assistant
                        content: []
                        model: MiniMax-M2.7
                        stop_reason: null
                        stop_sequence: null
                        usage:
                          input_tokens: 0
                          output_tokens: 0
                        service_tier: standard
                    - type: ping
                    - type: content_block_start
                      index: 0
                      content_block:
                        type: text
                        text: ''
                    - type: content_block_delta
                      index: 0
                      delta:
                        type: text_delta
                        text: Hello
                    - type: content_block_delta
                      index: 0
                      delta:
                        type: text_delta
                        text: >-
                          ! I'm MiniMax-M2.7, nice to meet you! Is there
                          anything
                    - type: content_block_delta
                      index: 0
                      delta:
                        type: text_delta
                        text: ' I can help you with?'
                    - type: content_block_stop
                      index: 0
                    - type: message_delta
                      delta:
                        stop_reason: end_turn
                      usage:
                        input_tokens: 176
                        output_tokens: 17
                    - type: message_stop
components:
  schemas:
    CreateMessageReq:
      type: object
      required:
        - model
        - messages
      properties:
        model:
          type: string
          description: Model ID
          enum:
            - MiniMax-M2.7
            - MiniMax-M2.7-highspeed
            - MiniMax-M2.5
            - MiniMax-M2.1
        system:
          description: Set the role and behavior of the model
          oneOf:
            - type: string
              description: Plain text system prompt
            - type: array
              description: System prompt in content block array format
              items:
                type: object
                properties:
                  type:
                    type: string
                    enum:
                      - text
                    description: Content block type
                  text:
                    type: string
                    description: Text content
                required:
                  - type
                  - text
        messages:
          type: array
          description: A list of messages containing the conversation history
          items:
            $ref: '#/components/schemas/Message'
        stream:
          type: boolean
          description: >-
            Whether to use streaming output, defaults to `false`. When set to
            `true`, the response will be returned in chunks
          default: false
        max_tokens:
          type: integer
          format: int64
          description: >-
            Specifies the upper limit for generated content length (in tokens),
            maximum is 204800. Content exceeding the limit will be truncated. If
            generation stops due to `length`, try increasing this value
          minimum: 1
        temperature:
          type: number
          format: double
          description: >-
            Temperature coefficient, affects output randomness, value range (0,
            1], default value for MiniMax model is 1.0. Higher values produce
            more random output; lower values produce more deterministic output
          minimum: 0
          exclusiveMinimum: 0
          maximum: 1
          default: 1
        top_p:
          type: number
          format: double
          description: >-
            Sampling strategy, affects output randomness, value range (0, 1],
            default value for MiniMax model is 0.95
          minimum: 0
          exclusiveMinimum: 0
          maximum: 1
          default: 0.95
    CreateMessageResp:
      type: object
      properties:
        id:
          type: string
          description: Unique ID of this response
        type:
          type: string
          description: Object type, fixed as `message`
          enum:
            - message
        role:
          type: string
          description: Role, fixed as `assistant`
          enum:
            - assistant
        model:
          type: string
          description: Model ID used for this request
        content:
          type: array
          description: List of response content blocks
          items:
            $ref: '#/components/schemas/ContentBlock'
        stop_reason:
          type: string
          description: |-
            Reason for stopping generation:
            - `end_turn`: Model ended naturally
            - `max_tokens`: Reached `max_tokens` limit
            - `stop_sequence`: Hit a stop sequence
          enum:
            - end_turn
            - max_tokens
            - stop_sequence
        usage:
          $ref: '#/components/schemas/Usage'
        base_resp:
          type: object
          description: Error status code and details
          properties:
            status_code:
              type: integer
              format: int64
              description: >-
                Status code


                - `1000`: Unknown error

                - `1001`: Request timeout

                - `1002`: Rate limit triggered

                - `1004`: Authentication failed

                - `1008`: Insufficient balance

                - `1013`: Internal server error

                - `1027`: Output content error

                - `1039`: Token limit exceeded

                - `2013`: Parameter error


                For more details, see [Error Code
                Reference](/api-reference/errorcode)
            status_msg:
              type: string
              description: Error details
    StreamEvent:
      type: object
      description: ''
      required:
        - type
      properties:
        type:
          type: string
          description: >-
            Event type:

            - `message_start`: Message start, contains complete message metadata

            - `ping`: Heartbeat event

            - `content_block_start`: Content block start

            - `content_block_delta`: Content block incremental update

            - `content_block_stop`: Content block end

            - `message_delta`: Message-level incremental update (e.g.,
            stop_reason)

            - `message_stop`: Message end
          enum:
            - message_start
            - ping
            - content_block_start
            - content_block_delta
            - content_block_stop
            - message_delta
            - message_stop
        message:
          type: object
          description: Message object (returned when `type` is `message_start`)
          properties:
            id:
              type: string
              description: Unique ID of the message
            type:
              type: string
              enum:
                - message
            role:
              type: string
              enum:
                - assistant
            content:
              type: array
              description: Content block list, initially an empty array
              items:
                $ref: '#/components/schemas/ContentBlock'
            model:
              type: string
              description: Model ID
            stop_reason:
              type: string
              nullable: true
              description: Stop reason, null at the start of streaming
            stop_sequence:
              type: string
              nullable: true
              description: Stop sequence, null at the start of streaming
            usage:
              $ref: '#/components/schemas/Usage'
            service_tier:
              type: string
              description: Service tier
        index:
          type: integer
          description: >-
            Index of the content block (returned for `content_block_start`,
            `content_block_delta`, `content_block_stop`)
        content_block:
          $ref: '#/components/schemas/ContentBlock'
          description: Content block object (returned when `type` is `content_block_start`)
        delta:
          type: object
          description: >-
            Incremental update content (returned for `content_block_delta` or
            `message_delta`)
          properties:
            type:
              type: string
              description: Delta type, e.g., `text_delta`
            text:
              type: string
              description: Incremental text content
            stop_reason:
              type: string
              description: Stop reason (returned for `message_delta`)
              enum:
                - end_turn
                - max_tokens
                - stop_sequence
        usage:
          $ref: '#/components/schemas/Usage'
          description: Token usage (returned for `message_delta`)
    Message:
      type: object
      required:
        - role
        - content
      properties:
        role:
          type: string
          enum:
            - user
            - assistant
            - user_system
            - group
            - sample_message_user
            - sample_message_ai
          description: |-
            Role of the message sender
            - `user`: User input
            - `assistant`: Model's historical reply
            - `user_system`: Set the user's role and persona
            - `group`: Name of the conversation
            - `sample_message_user`: Example user input
            - `sample_message_ai`: Example model output
        content:
          description: Message content, supports plain text string or content block array
          oneOf:
            - type: string
              description: Plain text message
            - type: array
              description: Content block array, supports text and thinking content blocks
              items:
                $ref: '#/components/schemas/ContentBlock'
    ContentBlock:
      type: object
      required:
        - type
      properties:
        type:
          type: string
          description: |-
            Content block type:
            - `text`: Text content
            - `thinking`: Model thinking process
          enum:
            - text
            - thinking
        text:
          type: string
          description: Text content (when `type` is `text`)
        thinking:
          type: string
          description: Model's thinking process content (when `type` is `thinking`)
        signature:
          type: string
          description: Signature of the thinking content (when `type` is `thinking`)
    Usage:
      type: object
      description: Token usage for this request
      properties:
        input_tokens:
          type: integer
          description: Number of tokens consumed by input
        output_tokens:
          type: integer
          description: Number of tokens consumed by output
  securitySchemes:
    apiKeyAuth:
      type: apiKey
      in: header
      name: X-Api-Key
      description: |-
        `API Key Auth`
         - Security Scheme Type: apiKey
         - API Key Header: X-Api-Key, used for account verification, can be viewed in [Account Management > API Keys](https://platform.minimax.io/user-center/basic-information/interface-key)

````

***************
From: https://platform.minimax.io/docs/api-reference/models/anthropic/list-models
***************

> ## Documentation Index
> Fetch the complete documentation index at: https://platform.minimax.io/docs/llms.txt
> Use this file to discover all available pages before exploring further.

<AgentInstructions>

## Submitting Feedback

If you encounter incorrect, outdated, or confusing documentation on this page, submit feedback:

POST https://platform.minimax.io/docs/feedback

```json
{
  "path": "/api-reference/models/anthropic/list-models",
  "feedback": "Description of the issue"
}
```

Only submit feedback when you have something specific and actionable to report.

</AgentInstructions>

# List Models

> Returns a list of all available models compatible with Anthropic API specification.



## OpenAPI

````yaml /api-reference/models/anthropic/api/list-models.json GET /anthropic/v1/models
openapi: 3.1.0
info:
  title: MiniMax Models API
  description: MiniMax models API compatible with Anthropic API specification.
  version: 1.0.0
servers:
  - url: https://api.minimax.io
security:
  - apiKeyAuth: []
paths:
  /anthropic/v1/models:
    get:
      tags:
        - Models
      summary: List Models
      description: >-
        Returns a list of all available models. This endpoint is compatible with
        Anthropic API specification.
      operationId: anthropicListModels
      parameters:
        - name: limit
          in: query
          required: false
          description: Number of items to return per page
          schema:
            type: integer
        - name: after_id
          in: query
          required: false
          description: Pagination cursor, returns models after this ID
          schema:
            type: string
        - name: before_id
          in: query
          required: false
          description: Pagination cursor, returns models before this ID
          schema:
            type: string
      responses:
        '200':
          description: A list of available models.
          content:
            application/json:
              schema:
                type: object
                properties:
                  data:
                    type: array
                    description: Array of model objects
                    items:
                      type: object
                      properties:
                        id:
                          type: string
                          description: Model identifier
                        created_at:
                          type: string
                          description: Model creation time (ISO 8601 format)
                        display_name:
                          type: string
                          description: Model display name
                        type:
                          type: string
                          description: Model type, always "model"
                  first_id:
                    type: string
                    description: First model ID in the returned data
                  has_more:
                    type: boolean
                    description: Whether there is more data
                  last_id:
                    type: string
                    description: Last model ID in the returned data
              examples:
                Default:
                  value:
                    data:
                      - id: MiniMax-M2.7
                        created_at: '2026-03-18T02:00:00Z'
                        display_name: MiniMax-M2.7
                        type: model
                      - id: MiniMax-M2.5
                        created_at: '2026-02-13T02:00:00Z'
                        display_name: MiniMax-M2.5
                        type: model
                    first_id: MiniMax-M2.7
                    has_more: false
                    last_id: MiniMax-M2.5
components:
  securitySchemes:
    apiKeyAuth:
      type: apiKey
      in: header
      name: X-Api-Key
      description: |-
        `API Key Auth`
         - Security Scheme Type: apiKey
         - API Key Header: X-Api-Key, used for account verification, can be viewed in [Account Management > API Keys](https://platform.minimax.io/user-center/basic-information/interface-key)

````

From: https://platform.minimax.io/docs/api-reference/models/anthropic/list-models

> ## Documentation Index
> Fetch the complete documentation index at: https://platform.minimax.io/docs/llms.txt
> Use this file to discover all available pages before exploring further.

<AgentInstructions>

## Submitting Feedback

If you encounter incorrect, outdated, or confusing documentation on this page, submit feedback:

POST https://platform.minimax.io/docs/feedback

```json
{
  "path": "/api-reference/models/anthropic/retrieve-model",
  "feedback": "Description of the issue"
}
```

Only submit feedback when you have something specific and actionable to report.

</AgentInstructions>

# Retrieve Model

> Retrieves details for a specific model, compatible with Anthropic API specification.



## OpenAPI

````yaml /api-reference/models/anthropic/api/retrieve-model.json GET /anthropic/v1/models/{model_id}
openapi: 3.1.0
info:
  title: MiniMax Models API
  description: MiniMax models API compatible with Anthropic API specification.
  version: 1.0.0
servers:
  - url: https://api.minimax.io
security:
  - apiKeyAuth: []
paths:
  /anthropic/v1/models/{model_id}:
    get:
      tags:
        - Models
      summary: Retrieve Model
      description: Retrieves details for a specific model.
      operationId: anthropicRetrieveModel
      parameters:
        - name: model_id
          in: path
          required: true
          description: Model identifier
          schema:
            type: string
      responses:
        '200':
          description: Details of the requested model.
          content:
            application/json:
              schema:
                type: object
                properties:
                  id:
                    type: string
                    description: Model identifier
                  created_at:
                    type: string
                    description: Model creation time (ISO 8601 format)
                  display_name:
                    type: string
                    description: Model display name
                  type:
                    type: string
                    description: Model type, always "model"
              examples:
                Default:
                  value:
                    id: MiniMax-M2.7
                    created_at: '2026-03-18T02:00:00Z'
                    display_name: MiniMax-M2.7
                    type: model
components:
  securitySchemes:
    apiKeyAuth:
      type: apiKey
      in: header
      name: X-Api-Key
      description: |-
        `API Key Auth`
         - Security Scheme Type: apiKey
         - API Key Header: X-Api-Key, used for account verification, can be viewed in [Account Management > API Keys](https://platform.minimax.io/user-center/basic-information/interface-key)

````
