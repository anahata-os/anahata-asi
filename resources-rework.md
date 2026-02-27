# Roadmap: The Universal Resource Rework

This document outlines the strategic transition of the Anahata ASI from a path-based file manager to a universal, URI-centric context orchestrator.

## Vision
To unify all stateful entities (Local Files, JAR Entries, Memory-backed sources, Multimodal assets, Remote Files) into a single, high-fidelity resource pipeline that supports native IDE navigation and intelligent lifecycle management.

## Phase 1: Breaking the Path Dependency (Current)
- **Objective**: Allow resources to be created from any `FileObject` or `URI`.
- **Implementation**: 
    - `NbFiles.loadFileObjectInternal()` for direct registration.
    - `CodeModel` integration: Type sources from JARs are added as `SNAPSHOT` resources.
    - `IDE.selectIn` support for `jar:file:` URLs and Resource IDs.

## Phase 2: The NetBeans Universal Bridge (FileSystems API)
- **Objective**: Leverage the NetBeans `FileSystems` API to treat all providers (Local, JAR, FTP, Memory) as native FileObjects.
- **Logic**: Consolidate `NbTextFileResource` and `NbBinaryFileResource` into a single `NbFileObjectResource`. Use NetBeans MIME lookup to determine handling.

## Phase 3: Core Multi-modal Integration (Standalone Support)
- **Objective**: Native support for `BlobPart` resources (Images, Audio, Video) in both Core and Swing UI.
- **Use Case**:
    - Standalone: ASI "sees" a UI screenshot or "hears" a user voice note via `AbstractResource<byte[], byte[]>`.
    - UI: `ResourcePanel` switches renderer based on `getContentType()`.

## Phase 4: Virtual & Remote Filesystems
- **Objective**: Support for resources that exist only in memory or on remote boxes (SCP/SFTP).
- **Goal**: Transition from `java.nio.file.Path` to `java.net.URI` as the primary handle in `AbstractResource`.
- **Impact**: The ASI can manage "Scratchpad" files or edit files on a remote server directly.

## Phase 5: Intelligent Lifecycle (GC 2.0)
- **Objective**: Automated policy switching based on usage and token pressure.
- **Logic**: A resource that hasn't been referenced in 50 turns might automatically switch from `LIVE` to `SNAPSHOT` or be suggested for unloading.

Força Barça!
