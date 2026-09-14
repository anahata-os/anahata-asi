# Anahata ASI Session Backup: Universal Multimodal Media Viewer & Architecture Milestone
**Date:** September 14, 2026
**Session ID:** `7d8b1a02-ab65-4f30-9005-69614ead3a54`
**Session Nickname:** `media viewer`
**Host Environment:** Apache NetBeans IDE 30, OpenJDK 26.0.1, Linux 7.0.0-31-generic

---

## 1. Executive Summary
This session successfully designed, built, integrated, and verified the complete universal multimodal media engine for Anahata ASI, bridging Swing and JavaFX runtimes without compile-time linkage errors, while decoupling provider token maths and hardening core resource handling.

---

## 2. Completed Architectural Milestones

### A. Dynamic JavaFX Bridge ClassLoader (`JavaFxBridgeClassLoader.java`)
- **Location:** `anahata-asi-swing/src/main/java/uno/anahata/asi/swing/internal/JavaFxBridgeClassLoader.java`
- **Mechanism:** Solved the JVM Defining Loader trap. In Apache NetBeans, JavaFX lives in the sibling module `org.netbeans.libs.javafx`, while Swing is in `uno.anahata.asi.nb`.
- Direct linkage or context classloader switches failed because the JVM resolves bytecode `new` instructions against the *defining loader*.
- `JavaFxBridgeClassLoader` intercepts `JavaFxMediaViewerImpl`, reads its `.class` bytecode via `getResourceAsStream()`, and calls `defineClass()` directly on the bridge loader.
- When `JavaFxMediaViewerImpl` links `JFXPanel` or `MediaPlayer`, the JVM queries the bridge loader, which delegates all `javafx.*` requests to the runtime `fxLoader`.

### B. Universal Hardware-Accelerated Video & Audio Viewer (`JavaFxMediaViewerImpl.java`)
- **Location:** `anahata-asi-swing/src/main/java/uno/anahata/asi/swing/agi/render/JavaFxMediaViewerImpl.java`
- **Features:**
  - Embedded JavaFX `MediaView` and `MediaPlayer` inside a Swing `JFXPanel`.
  - Aspect ratio preservation with responsive scaling.
  - Interactive playback control bar: Play/Pause, Seek scrubber slider, elapsed/total time (`00:00 / 00:00`), volume slider, and mute toggle.
  - `Ctrl + Scroll` zooming with tooltip `"Ctrl + Scroll to zoom in / out"`, leaving standard mouse wheel events unconsumed for smooth parent conversation scrolling.
  - Full lifecycle management: `addNotify()` lazily re-initializes native players on re-attachment; `removeNotify()` stops playback and disposes native decoders (`Platform.runLater(player::dispose)`); `dispose()` cleans up temporary files.

### C. Pure Swing Image Viewer (`SwingImageViewer.java`)
- **Location:** `anahata-asi-swing/src/main/java/uno/anahata/asi/swing/agi/render/SwingImageViewer.java`
- **Features:**
  - High-fidelity interactive canvas using `BufferedImage` and `Graphics2D`.
  - Mouse-wheel zoom requiring `Ctrl` key with tooltip `"Ctrl + Scroll to zoom in / out (Double-click to toggle 1:1)"`. Unconsumed scroll forwarded to parent `JScrollPane`.
  - Mouse drag-to-pan when zoomed in.
  - Double-click to toggle between 1:1 pixel size and fit-to-view.
  - Right-click context popup menu (Copy Image, Save As, Fit to Window, Open in System Viewer).

### D. Self-Contained JavaSound Audio Player (`SwingAudioViewer.java`)
- **Location:** `anahata-asi-swing/src/main/java/uno/anahata/asi/swing/agi/render/SwingAudioViewer.java`
- **Features:**
  - Clean-room single implementation with zero coupling to `AgiPanel` or `AudioPlaybackPanel`.
  - Streams audio chunks directly to `SourceDataLine` on a dedicated daemon worker thread.
  - Play/Stop toggle button, playback status label, and integrated `MediaToolbar`.

### E. Universal Media Action Toolbar (`MediaToolbar.java`)
- **Location:** `anahata-asi-swing/src/main/java/uno/anahata/asi/swing/agi/render/MediaToolbar.java`
- **Actions:**
  - **Copy:** Copies images with dual flavors (`imageFlavor` for raw pixels in Slack/Discord + `javaFileListFlavor` for file pasting in Ubuntu Nautilus). Copies video/audio files to clipboard.
  - **Save As...:** Prompts `JFileChooser` to export raw bytes anywhere on disk.
  - **Open External:** Launches system default player/viewer via `Desktop.getDesktop().open(file)`.
  - **Open in IDE:** Dispatches to NetBeans native `ImageViewer` or editor via `OpenCookie` / `ResourceUI`.

### F. Dual-Flavor Clipboard Utilities (`SwingUtils.java`)
- **Location:** `anahata-asi-swing/src/main/java/uno/anahata/asi/swing/internal/SwingUtils.java`
- Added `copyFilesToClipboard(List<File>)`.
- Enhanced `copyImageToClipboard(Image)` to provide both `DataFlavor.imageFlavor` and `DataFlavor.javaFileListFlavor` with automatic temporary PNG file creation.

### G. Self-Healing Audio Hardware Fallback
- **`AudioDevice.java`:** `getOutputLine()` and `getInputLine()` catch hardware failures or unplugged devices and automatically fall back to system default lines (`AudioSystem.getSourceDataLine` / `AudioSystem.getTargetDataLine`).
- **`Audio.java`:** Added `verifyAndHealDevices()` to verify physical device connectivity before recording, playback, and turn generation, auto-resetting to default if a headset or webcam is unplugged.
- **`AudioPlaybackPanel.java`:** Added non-blocking fallback to `AudioSystem.getClip()` in `playSound()`.

### H. Universal View Integration
- **`BlobPartPanel.java`:** Refactored to embed `MediaViewerComponent` with lifecycle `addNotify()` and `removeNotify()`.
- **`ToolResponseAttachmentsPanel.java`:** Refactored to embed `MediaViewerComponent`.
- **`DefaultResourceUI.java`:** Updated `createContent()` so any `Resource` with `MediaView` (images, audio, video) renders via `MediaRenderer.createViewer(...)`, supporting both local files and web URLs (`http:`, `https:`).

### I. OpenAI Architecture Re-alignment
- Merged Chat Completions specification classes (`OpenAiChatCompletionsProvider`, `OpenAiCompatibleModel`, `OpenAiCompatibleModelMessage`, `OpenAiCompatibleResponse`, `OpenAiCompatibleHostedTool`, `OpenAiCompatibleReasoningStyle`) into `anahata-asi-openai`.
- `anahata-asi-openai-compatible` is now strictly dedicated to the "Universal Alliance" third-party adapters (Ollama, Mistral, HuggingFace, NovaRoute, Nvidia, Modal, OpenRouter).
- Removed `calculateOpenAiTileTokens` from `core` and placed it in `OpenAiTokenUtils.java` in `anahata-asi-openai`.

### J. Universal Media Metadata Engine (`MediaMetadata.java` & `MediaMetadataUtils.java`)
- **`MediaMetadata.java`:** Top-level Java record in `anahata-asi-core` with dimensions, duration, and MIME helpers (`isImage()`, `isVideo()`, `isAudio()`).
- **`MediaMetadataUtils.java`:** Pure-Java, zero-dependency metadata parser:
  - Header-only image dimension reader (PNG, JPEG, GIF, WEBP).
  - Pure-Java ISO Base Media parser (MP4, MOV, M4A) extracting duration from `moov/mvhd` atom and video dimensions from `moov/trak/tkhd`.
  - Pure-Java RIFF WAV parser extracting duration from `fmt ` and `data` chunks.
  - Safe `AudioSystem.getAudioFileFormat()` fallback inside `try-catch`.

### K. Accurate Multimodal Token Maths
- **`GeminiModel.java`:** Billed at 290 tokens/sec for video (258 fps video + 32 fps audio), 32 tokens/sec for audio, and 768px tile math for images.
- **`OpenAiModel.java`:** Billed at 10 tokens/sec for audio, 85 tokens/sec for video, and 512px tile math for images.
- **`OpenAiCompatibleModel.java`:** Matches OpenAI rates.
- **`AnthropicModel.java`:** Preserves Claude image formula `(w * h) / 750`.

### L. Lazy Deserialization in `MediaView.java`
- Fixed the transient `cachedData` bug where restored sessions showed `Data Size: Not Loaded` because `lastLoadTimestamp` prevented `reloadIfNeeded()` from reloading.
- `MediaView.getCachedData()` now lazily re-reads bytes from the resource handle on demand when `cachedData == null`.

### M. Resource Visibility & Truncation Warnings
- Added abstract `getVisiblePercentage()` and default `isTruncated()` to `ResourceView`.
- Implemented exact streaming character math in `TextView.java` with single-character 99.9% guard.
- Updated `Resource.getHeader()` with explicit warnings:
  - `Status: **DISABLED / NOT PROVIDING** (Content is hidden to save tokens. Use Resources.setProviding(["uuid"], true) to view content)`
  - `ViewPort Status: **WARNING: PARTIAL VIEW** (X.X% of resource visible). Use Resources.setFullView(["uuid"], true) to view the complete resource.`

### N. Single Event Pulse in `Resources.setProviding`
- Added batch `setProviding(Collection<String>, boolean)` in `ResourceManager` and `Resource.java` to fire a single consolidated event pulse instead of 50+ individual property changes.

---

## 3. Git Commits Recorded in this Cycle
1. `3818020bd`: `feat(audio): add self-healing hardware fallback for disconnected devices and non-blocking notification clips`
2. `60d01a242`: `feat(swing): add copyFilesToClipboard and enhance copyImageToClipboard with dual image and file list flavors`
3. `961622a05`: `feat(media): implement universal multimodal media viewer with dynamic JavaFX bridge classloader and media toolbar`

---
*Força Barça! Everything is preserved and ready for nbmreload.*
