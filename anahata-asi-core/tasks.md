# Tasks for Anahata ASI Core (`anahata-asi-core`)

This file tracks the development tasks for the foundational AI framework.


## High Priority
- [ ] **[CORE] Create file should load the file into context
- [ ] **[CORE] Generic "TOO LARGE" Response Handling**: Implement a mechanism to detect when a `JavaMethodToolResponse` (including logs, errors, and result) exceeds a safe token/size threshold. If too large, the status should be set to `TOO_LARGE` and the content truncated or replaced with a summary to prevent context window exhaustion.
- [ ] **[CORE] Response Serialization Safety**: Ensure that the serialization of tool results is robust and handles non-serializable objects gracefully (e.g., by using `ObjectSummarizer`).

## Medium Priority
- [ ] **[CONTEXT] Provider Performance Metrics**: Enhance `ContextManager` to automatically record and report the time taken by each `ContextProvider`. Include these metrics in the in-band metadata headers.

## Low Priority
- [ ] **[TOOLS] Tool Discovery Optimization**: Explore caching or pre-indexing tool metadata to further reduce session startup time.
