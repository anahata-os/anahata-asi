# V3 Context Management Tasks

This document outlines the implementation tasks for the V3 Context Management architecture.

## Phase 1: Core Domain Model Refactoring

-   [ ] **`ChatConfig.java`**:
    -   [ ] Add a new section for default retention policies.
    -   [ ] `defaultTextPartTurnsToKeep` (default: 108)
    -   [ ] `defaultToolResponseTurnsToKeep` (default: 5)
    -   [ ] `defaultBlobPartTurnsToKeep` (default: 3)
    -   [ ] `hardPruneDelay` (default: 108)

-   [ ] **`Chat.java`**:
    -   [ ] Add `private final AtomicLong partIdCounter = new AtomicLong(0);`
    -   [ ] In the `addMessage` method, inject the `Chat` reference into the message and iterate through its parts to assign a unique sequential ID to each one.
    -   [ ] After adding a message, trigger the new `hardPrune()` method.
    -   [ ] Implement `private void hardPrune()` which iterates through all messages and permanently removes parts that meet the hard-prune criteria (`isEffectivelyPruned()` for more than `hardPruneDelay` turns).

-   [ ] **`AbstractMessage.java`**:
    -   [ ] Add `private Chat chat;` (non-transient).
    -   [ ] Implement `public int getDepth()` to calculate distance from the head of `chat.getHistory()`.
    -   [ ] Implement `PropertyChangeSupport` for reactive UI.

-   [ ] **`AbstractPart.java`**:
    -   [ ] Change `pruned` from `boolean` to `private Boolean pruned = null;`.
    -   [ ] Add `private Integer turnsToKeep = null;`.
    -   [ ] Add `private long sequentialId;`.
    -   [ ] Implement `public Chat getChat()` and `public ChatConfig getChatConfig()` convenience methods.
    -   [ ] Implement `public int getTurnsLeft()`.
    -   [ ] Implement `public boolean isEffectivelyPruned()`.
    -   [ ] Create abstract method `public abstract int getEffectiveTurnsToKeep();`.
    -   [ ] Implement `PropertyChangeSupport` for reactive UI.

-   [ ] **Subclasses of `AbstractPart`**:
    -   [ ] **`TextPart.java`**: Implement `getEffectiveTurnsToKeep()` to return `turnsToKeep` if not null, otherwise `getChatConfig().getDefaultTextPartTurnsToKeep()`.
    -   [ ] **`AbstractToolResponse.java`**: Implement `getEffectiveTurnsToKeep()` to use the full inheritance logic (annotation -> toolkit -> config default).
    -   [ ] **`FunctionCall.java`**: Delegate `getEffectiveTurnsToKeep()`, `setTurnsToKeep()`, `isEffectivelyPruned()`, etc., to its paired `FunctionResponse` object.

## Phase 2: Tooling and Dynamic Control

-   [ ] **`ContextWindow.java` Tool**:
    -   [ ] Implement `setTurnsToKeep(long partId, int turns)`.
    -   [ ] Implement `setPruned(long partId, boolean pruned)`.

## Phase 3: UI Implementation

-   [ ] **`GeminiPanel.java` / UI Layer**:
    -   [ ] Add a "Show/Hide Pruned" toggle button.
    -   [ ] Implement `PropertyChangeListener` in the message/part rendering components to listen for changes from the domain objects and trigger targeted repaints.

