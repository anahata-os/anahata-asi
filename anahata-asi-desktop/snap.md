# Canonical Snap Confinement & Sandboxing Architecture (`snap.md`)

This document records the exact root causes, exception analysis, and engineering solutions required to make **Anahata ASI Desktop** launch and operate cleanly under **Canonical Snap Strict Confinement** (`confinement: strict`).

---

## 1. Exception Log Analysis (from `NbTerminal`)

When launched inside a strictly confined Snap container (`snap run anahata-asi-desktop`), the application encountered four distinct sandbox collisions:

### 1.1. Session Preferences & Working Directory Access
```text
[main] ERROR uno.anahata.asi.AsiContainerPreferences - Error loading preferences from /home/pablo/.anahata/asi/AsiDesktop/preferences.kryo
java.nio.file.AccessDeniedException: /home/pablo/.anahata/asi/AsiDesktop/preferences.kryo
        at java.base/sun.nio.fs.UnixException.translateToIOException(Unknown Source)
        at java.base/sun.nio.fs.UnixFileSystemProvider.newFileChannel(Unknown Source)
        at uno.anahata.asi.AsiContainerPreferences.load(AsiContainerPreferences.java:249)
```
* **Root Cause**: In Snap strict confinement, the `home` interface grants access to standard user files (e.g. `~/Projects`, `~/Documents`), but **AppArmor strictly blocks access to top-level hidden dot-directories** directly under `$HOME` (`~/.anahata`).

---

### 1.2. JavaFX Native Pipeline Initialization Failure
```text
Loading library prism_es2 from resource failed: java.nio.file.AccessDeniedException: /home/pablo/.openjfx/cache/26+27/amd64/libprism_es2.so
Loading library prism_sw from resource failed: java.nio.file.AccessDeniedException: /home/pablo/.openjfx/cache/26+27/amd64/libprism_sw.so
Graphics Device initialization failed for : es2, sw
Error initializing QuantumRenderer: no suitable pipeline found
[main] ERROR uno.anahata.asi.javafx.util.JavaFxUtils - Failed to initialize JavaFX Platform: No toolkit found
```
* **Root Cause**: By default, OpenJFX attempts to unpack its native 2D/3D rendering libraries (`libprism_es2.so`, `libprism_sw.so`, `libglass.so`) into `~/.openjfx/cache/`. Because AppArmor denies creation of `~/.openjfx`, JavaFX fails to initialize `QuantumRenderer`.

---

### 1.3. Java User Preferences Lock Failure
```text
Aug 28, 2026 10:07:37 AM java.util.prefs.FileSystemPreferences loadCache
WARNING: Prefs file removed in background /home/pablo/.java/.userPrefs/uno/anahata/asi/desktop/prefs.xml
java.util.prefs.FileSystemPreferences: Could not lock User prefs. Lock file access denied.
```
* **Root Cause**: Standard `java.util.prefs.Preferences` writes to `~/.java/.userPrefs/`, which is blocked by the AppArmor sandbox.

---

### 1.4. Fontconfig & LookAndFeel Crash Cascade
```text
[main] ERROR uno.anahata.asi.desktop.Main - Failed to initialize Look and Feel
java.lang.RuntimeException: Fontconfig head is null, check your fonts or fonts configuration
        at java.desktop/sun.awt.FontConfiguration.getVersion(Unknown Source)
        at com.formdev.flatlaf.FlatLaf.initDefaultFont(FlatLaf.java:709)
        at java.desktop/javax.swing.UIManager.setLookAndFeel(Unknown Source)

java.lang.Error: no ComponentUI class for: javax.swing.JPanel
        at java.desktop/javax.swing.UIDefaults.getUIError(Unknown Source)
        at java.desktop/javax.swing.UIManager.getUI(Unknown Source)
        at java.desktop/javax.swing.JPanel.updateUI(Unknown Source)
```
* **Root Cause**: Java AWT/Swing failed to read system fontconfig caches in `/etc/fonts` or `/var/cache/fontconfig`. When `UIManager.setLookAndFeel` crashed, `UIDefaults` remained half-initialized, causing subsequent Swing component constructors (`JPanel`, `JButton`, `JLabel`) to throw `no ComponentUI class` errors.

---

## 2. Minimal Launch Solution (Making it "Just Launch" on Snap)

To allow Anahata ASI Desktop to launch cleanly under strict confinement:

### 2.1. Redirect Storage & Caches to `$SNAP_USER_DATA`
In `Main.java` (at the very top of `main()`):
```java
String snapUserData = System.getenv("SNAP_USER_DATA");
if (snapUserData != null && !snapUserData.isBlank()) {
    // 1. Redirect Java User Preferences
    System.setProperty("java.util.prefs.userRoot", snapUserData + "/.java");
    
    // 2. Redirect JavaFX Native Library Unpack Cache
    System.setProperty("javafx.cachedir", snapUserData + "/.openjfx/cache");
    
    // 3. Enable software rendering fallback for JavaFX if hardware graphics are sandboxed
    System.setProperty("prism.order", "es2,sw");
}
```

In `AbstractAsiContainer.java`:
```java
public static Path getWorkDir() {
    String snapUserData = System.getenv("SNAP_USER_DATA");
    if (snapUserData != null && !snapUserData.isBlank()) {
        return Paths.get(snapUserData, ".anahata", "asi");
    }
    return Paths.get(System.getProperty("user.home"), ".anahata", "asi");
}
```

### 2.2. Robust LookAndFeel Fallback
Wrap LookAndFeel initialization so `UIDefaults` are guaranteed to initialize even if fontconfig warnings occur:
```java
try {
    String lafClassName = Preferences.userNodeForPackage(Main.class).get("laf", "com.formdev.flatlaf.FlatDarkLaf");
    UIManager.setLookAndFeel(lafClassName);
} catch (Throwable t) {
    log.warn("Custom LAF initialization failed ({}), falling back to CrossPlatform LAF", t.getMessage());
    try {
        UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
    } catch (Throwable ignored) {}
}
```

### 2.3. Stage Packages in `snapcraft.yaml`
Ensure system fonts and fontconfig are bundled inside the snap:
```yaml
stage-packages:
  - fontconfig
  - libfontconfig1
  - fonts-dejavu-core
  - libx11-6
  - libxext6
  - libxrender1
  - libxtst6
  - libxi6
  - libasound2t64
  - libfreetype6
```

---

## 3. Toolkits to Disable / Constrain in a Strict Snap Environment

When `System.getenv("SNAP_USER_DATA") != null` is detected, the container should adjust the active toolkit registry:

| Toolkit | Status in Strict Snap | Technical Reason |
| :--- | :---: | :--- |
| **`Shell`** | ⚠️ Constrained / Disabled | Cannot execute host OS binaries (`/usr/bin/mvn`, `/usr/bin/git`, custom compilers) or user binaries in `$HOME` due to AppArmor `execve` blocks. |
| **`Host`** | ⚠️ Constrained | `listProcesses` and `killProcess` only see processes inside the snap's isolated PID namespace, not the host system. |
| **`Chrome` / `Firefox`** | ❌ Disabled | Cannot launch host browser binaries or download browser drivers to `~/.cache/selenium`. |
| **`Java` (SwingJava)** | ✅ Active (In-Process) | Can compile and run in-memory Java code, but cannot reference external host `.m2` dependencies unless bundled in `$SNAP_USER_DATA`. |
| **`Resources`** | ✅ Active | Can read/write user files in unhidden directories (`~/Projects`, `~/Documents`) and `$SNAP_USER_DATA`. |
| **`Audio` / `Radio` / `Speech`**| ✅ Active | Fully operational via `audio-playback` and `audio-record` plugs. |
| **`Screens`** | ✅ Active | Fully operational via `x11` and `wayland` plugs. |

---

## 4. `CoreContextProvider` Awareness Prompt Injection

When `SNAP_USER_DATA` is detected at runtime, `CoreContextProvider` should inject the following prompt augmentation:

```markdown
### ⚠️ Canonical Snap Strict Confinement Notice
The ASI Container is currently operating within a sandboxed **Ubuntu Snap container (`confinement: strict`)**:
- **Filesystem Access**: You have full read/write access to user-selected working directories in `$HOME` (e.g. `~/Projects`, `~/Documents`) and `$SNAP_USER_DATA`. Top-level hidden dotfiles (`~/.m2`, `~/.config`, `~/.ssh`) are restricted by AppArmor.
- **Process Spawning**: Subshell tools cannot spawn host-installed compilers or external CLI tools (`/usr/bin/mvn`, `/usr/bin/git`). Use in-process JVM capabilities for all code execution and analysis.
- **Persistence**: All session metadata, preferences, and generated media are persisted securely in `$SNAP_USER_DATA/.anahata/asi/`.
```
