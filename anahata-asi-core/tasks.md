# Tasks for Anahata ASI Core (`anahata-asi-core`)

This file tracks the development tasks for the foundational AI framework.


## High Priority
- [ ] **[CORE] Generic "TOO LARGE" Response Handling**: Implement a mechanism to detect when a `JavaMethodToolResponse` (including logs, errors, and result) exceeds a safe token/size threshold. If too large, the status should be set to `TOO_LARGE` and the content truncated or replaced with a summary to prevent context window exhaustion.

## Medium Priority
- [ ] **[CONTEXT] Provider Performance Metrics**: Enhance `ContextManager` to automatically record and report the time taken by each `ContextProvider`. Include these metrics in the in-band metadata headers.

