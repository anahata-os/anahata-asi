# Browser Toolkit Test Log

## 2026-02-08: The "Default Profile" Security Block
**Status**: ✅ RESOLVED (Verified on JVM)

### Problem Description
Chrome refuses to open port 9222 when `--user-data-dir` points to the default profile directory (`/home/pablo/.config/google-chrome`). This caused the 60-second timeouts.

### Logs Analysis (Test 1085-1087)
- `DevTools remote debugging requires a non-default data directory. Specify this using --user-data-dir.`
- Chrome's path normalization caught symlinks and relative path aliases.

### The Breakthrough (Test 1088 & 1095)
- **Hypothesis**: Chrome uses the `HOME` environment variable to determine what is "default".
- **Test**: Launched Chrome with `--user-data-dir=/home/pablo/.config/google-chrome` but set `HOME=/tmp/anahata-fake-home`.
- **Result**: `Handshake Result: SUCCESS (Port 9222 is OPEN!)`. Verified visually in Test 1095.

### Final Resolution
1.  **Home-Shift**: Use `ChromeDriverService.Builder().withEnvironment(Map.of("HOME", "/tmp/anahata-fake-home"))`.
2.  **Synchronous Launch**: Removed `runAsync` to ensure the tool waits for the handshake.
3.  **RAG Reporting**: Include connection status and full error traces in the prompt.
4.  **Mandatory Braces**: Applied `{}` to all control flow statements in `Browser.java`.

## 2026-02-08: Flicker-Free Listing & Sticky Connections
**Status**: ✅ RESOLVED

### Problem
`listTabs` used `driver.switchTo().window()`, which caused the browser to steal focus, flicker, and trigger OS notifications. Also, the connection was lost after session reloads.

### Resolution
1.  **CDP Target Info**: Updated `listTabs` to use the **Chrome DevTools Protocol (CDP)** (`Target.getTargetInfo`).
2.  **Conditional Switch-Back**: Removed the redundant `switchTo` at the end of `listTabs`. It now only switches back if the silent CDP method failed and it was forced to use the noisy fallback.
3.  **Sticky Connections**: Added `lastConnectedPort` field.
4.  **Auto-Reconnect**: Implemented `rebind()` to automatically attempt reconnection to the last known port when a session is restored.

## 2026-02-08: Windows Compatibility Refinement
**Status**: ✅ RESOLVED

### Problem
Windows uses a different lock file name (`lock`) and doesn't support the `ps` command for command-line retrieval.

### Resolution
1.  **Windows Lock Support**: Added `lock` to the `clearSingletonLock` and `hasLock` logic.
2.  **WMIC Fallback**: Implemented a Windows-specific fallback for `getCommandLine` using `wmic process`.
3.  **NOFOLLOW_LINKS**: Confirmed that `LinkOption.NOFOLLOW_LINKS` is safe and compatible with Windows.

## 2026-02-08: Session Restoration & API Cleanup
**Status**: ✅ RESOLVED

### Problem
Trying to reconnect asynchronously during `rebind()` caused Kryo serialization errors because the session wasn't fully initialized. The tool palette was also cluttered with troubleshooting tools.

### Resolution
1.  **Lazy Reconnection**: Moved reconnection logic to a private `getDriver()` method. Reconnection now happens on-demand during the first tool call after a session reload.
2.  **API Cleanup**: Commented out troubleshooting tools (`listProfiles`, `listProcesses`) to keep the agent's interface clean and token-efficient.
3.  **Internalization**: Converted surgical tools into private internal methods used by the high-level `connect()` tool.
4.  **Standardization**: Reordered methods in `Browser.java` to follow the new toolkit standard (rebind -> instructions -> rag -> tools -> public -> private).
5.  **Advanced Automation**: Made `getDriver()` public and added `@Getter` to the class to allow direct Selenium access via `Java` or `NbJava`.
6.  **Branding Removal**: Removed `brandPage` as the default automation message is no longer showing.
