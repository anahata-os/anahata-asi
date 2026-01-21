# Anahata AI V2 Metadata & Context Awareness

This document describes the "In-Band Metadata Injection" system, which provides the AI model with deep self-awareness of the conversation's structure, its own previous reasoning, and the state of the context window.

## 1. Core Philosophy

In the V2 architecture, the model is not just a passive recipient of text. It is an active participant that understands:
-   **Temporal Context**: When messages were sent and how much time has passed.
-   **Resource Management**: Which parts of the conversation have been pruned and why.
-   **Reasoning Continuity**: Its own previous "thoughts" (via thought signatures), even for pruned messages.
-   **Token Economy**: The cost and retention policy of every part of the context.

## 2. In-Band Metadata Injection

Metadata is injected directly into the conversation stream as text-based "headers" before each message and part. This ensures that the model's self-awareness is part of its primary input.

### 2.1. Message Headers
Every message is preceded by a header in the following format:
`--- Message ID: {ID} | Role: {ROLE} | From: {SENDER} | Time: {TIMESTAMP} | Tokens: {COUNT} [| PINNED/PRUNED] ---`

### 2.2. Part Headers
Every individual part (Text, Blob, Tool Call, Tool Response) has its own header:
`[Part ID: {ID} | Type: {TYPE} | Tokens: {COUNT} | Turns Left: {N} [| PINNED/PRUNED]]`

## 3. Pruning & Placeholders

When a part is "effectively pruned" (either explicitly or because its TTL expired), it is not simply deleted. Instead, it is replaced by its **Metadata Header**, which serves as a semantic placeholder.

-   **Pruned Reason**: If a reason was provided during pruning, it is included in the header.
-   **Semantic Hint**: The header includes a "Hint" (a truncated text representation of the original content) to help the model maintain the "gist" of the discussion.
-   **Thought Signatures**: For model-generated parts, the `thoughtSignature` is preserved even if the main content is pruned. This allows the model to maintain reasoning continuity across massive context windows.

## 4. Pruned Message Ranges (Deep Reasoning Recovery)

To further optimize the context, contiguous ranges of fully pruned messages are summarized in a single block at the end of the history:
`--- PRUNED MESSAGE RANGES ---`
`- Messages ID: 10 to 25 are PRUNED | Reasons: {REASON1, REASON2} | Thought Signatures Preserved: {COUNT}`

This aggregated summary ensures that even when large chunks of history are compressed, the model remains aware of the *intent* behind the pruning and the *existence* of previous reasoning steps.

## 5. Model Instructions (System Level)

The model is instructed to use this metadata to:
1.  **Reference by ID**: Use Message and Part IDs when discussing context management or pruning.
2.  **Monitor TTL**: Be aware of when critical information is about to expire and "pin" it if necessary.
3.  **Reasoning Recovery**: Use preserved thought signatures to reconstruct previous logic without re-reading the full text.
