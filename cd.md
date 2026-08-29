# Continuous Delivery & Multi-Platform Distribution Channels (`cd.md`)

This document provides the authoritative reference for all active, configured, and planned distribution channels for the **Anahata ASI Platform** across all target environments (NetBeans, IntelliJ, Desktop Standalone, and Linux/Windows/macOS native installers).

---

## 1. Distribution Architecture Matrix

```
┌────────────────────────────────────────────────────────────────────────┐
│                   ANAHATA ASI 3-TIER DISTRIBUTION ENGINE               │
├────────────────────────────────────────────────────────────────────────┤
│ TIER 1: Maven Central (repo1.maven.org)                                │
│ • Lightweight Core JARs (~4 MB total) & Standalone Update Center NBM.  │
│ • Gated strictly by `deploy_central: true` on production releases.     │
│                                                                        │
│ TIER 2: GitHub Releases CDN (github.com/anahata-os/anahata-asi)        │
│ • Heavy Studio NBMs (~140 MB each: NB 30 & NB 31).                     │
│ • IntelliJ IDEA Plugin Distribution (.zip).                            │
│ • Linux Native Packages: .AppImage, .deb, .tar.gz.                     │
│ • Windows Native Portable (.zip).                                      │
│ • macOS Native App Bundle (.zip).                                      │
│ • Active on BOTH `latest-snapshot` and official `v*` release tags.     │
│                                                                        │
│ TIER 3: User-Facing Public Channels & Update Centers                   │
│ • NetBeans Updates Catalogs (https://asi.anahata.uno/nb/{major}/).     │
│ • Canonical Ubuntu Snap Store (Ubuntu App Center).                     │
│ • Static GitHub Pages APT Repository (https://asi.anahata.uno/apt/).   │
│ • JetBrains Marketplace & Apache NetBeans Plugin Portal.               │
└────────────────────────────────────────────────────────────────────────┘
```

---

## 2. Active Distribution Channels

### 2.1. Apache NetBeans Ecosystem
1. **Universal Anahata ASI Update Center Plugin (`anahata-asi-nb-uc`)**:
   - **Target**: All Apache NetBeans versions (NB 28 &ndash; NB 35+).
   - **Size**: ~35 KB (Zero implementation dependencies).
   - **Distribution**: Maven Central & Apache NetBeans Plugin Portal.
   - **Catalog**: `https://asi.anahata.uno/nb/updates.xml`.
2. **NetBeans 30 Official Channel**:
   - **Stable GA**: `https://asi.anahata.uno/nb/30/updates.xml` &rarr; points to GitHub Releases CDN.
   - **Dev Snapshot**: `https://asi.anahata.uno/nb/30/dev-updates.xml` &rarr; points to `latest-snapshot`.
3. **NetBeans 31 Official Channel**:
   - **Stable GA**: `https://asi.anahata.uno/nb/31/updates.xml` &rarr; points to GitHub Releases CDN.
   - **Dev Snapshot**: `https://asi.anahata.uno/nb/31/dev-updates.xml` &rarr; points to `latest-snapshot`.

---

### 2.2. JetBrains IntelliJ IDEA Ecosystem
1. **IntelliJ Plugin Distribution ZIP**:
   - **Target**: IntelliJ IDEA 2024.x &ndash; 2026.x, Android Studio, CLion.
   - **Artifact**: `anahata-asi-intellij-${version}.zip`.
   - **Distribution**: Attached to GitHub Releases (`latest-snapshot` & `v*`) and available via direct download on `https://asi.anahata.uno/intellij.html`.

---

### 2.3. Standalone Desktop Suite (`anahata-asi-desktop`)

All standalone packages bundle a private, modular Java Runtime Environment (JRE) with **Generational ZGC** (`-XX:+UseZGC -XX:+ZGenerational`) and **dynamic memory scaling** (`-XX:MaxRAMPercentage=60.0`):

| Channel / Format | Target OS & Distros | Confinement / Permissions | Distribution Point |
| :--- | :--- | :--- | :--- |
| **Universal AppImage (`.AppImage`)** | All Linux (Ubuntu, Debian, Fedora, Arch, openSUSE, Mint, SteamOS) | **Unrestricted (Native User)** | Direct download on `asi.anahata.uno/desktop.html` & GitHub Releases. |
| **Debian Package (`.deb`)** | Ubuntu, Debian, Linux Mint, Pop!_OS | **System Package (`/opt`)** | Direct download & GitHub Pages APT Repo. |
| **Portable Tarball (`.tar.gz`)** | All Linux distributions (x86_64) | **Portable Folder** | Direct download on `asi.anahata.uno/desktop.html` & GitHub Releases. |
| **Canonical Snap Store (`.snap`)** | Ubuntu Desktop, Debian, Snap-enabled distros | **Strict / Classic** | Ubuntu App Center (`snapcraft.io/anahata-asi-desktop`). |
| **Windows Portable (`.zip`)** | Windows 10 & 11 (64-bit) | **Native User Application** | Direct download on `asi.anahata.uno/desktop.html` & GitHub Releases. |
| **macOS App Bundle (`.zip`)** | macOS Sonoma, Sequoia, Darwin (x86_64/ARM64) | **Native App Bundle** | Direct download on `asi.anahata.uno/desktop.html` & GitHub Releases. |

---

## 3. Planned Channels & Acquisition Roadmap

### 3.1. Linux Distribution Fronts
1. **FUSE-Independent AppImage Runtime**:
   - Deploy a modern Type-2 static runtime header or transparent `--appimage-extract-and-run` wrapper so AppImages launch with a single double-click on Ubuntu 24.04+ without needing `libfuse2t64`.
2. **Red Hat / Fedora / openSUSE RPM Package (`.rpm`)**:
   - Add `sudo apt install rpm` and `jpackage --type rpm` to `build3.yml` to produce native `.rpm` packages for Fedora and Enterprise Linux users.
3. **Static GitHub Pages APT Repository (`https://asi.anahata.uno/apt/`)**:
   - Automate `dpkg-scanpackages` during CI Step 6 so Debian/Ubuntu users can add our official repository:
     ```bash
     echo "deb [trusted=yes] https://asi.anahata.uno/apt/ stable main" | sudo tee /etc/apt/sources.list.d/anahata.list
     sudo apt update && sudo apt install anahata-asi-desktop
     ```
4. **Canonical Snap Store Classic Confinement**:
   - Complete forum review on `forum.snapcraft.io/c/store-requests/classic-confinement/26` to enable unrestricted JIT compilation and host developer toolchain execution for Ubuntu users.
5. **Flathub / Flatpak (`flathub.org`)**:
   - Submit `uno.anahata.asi.desktop` manifest to Flathub to reach Fedora, Steam Deck, Arch Linux, and openSUSE users through GNOME Software and KDE Discover.
6. **Arch User Repository (AUR)**:
   - Publish `PKGBUILD` for `anahata-asi-desktop` and `anahata-asi-desktop-bin` on `aur.archlinux.org` for 1-command installation via `yay -S anahata-asi-desktop`.

---

### 3.2. IDE Marketplaces
1. **Apache NetBeans Plugin Portal (`plugins.netbeans.apache.org`)**:
   - Maintain active catalogue listing for `anahata-asi-nb-uc` (ID: 135).
2. **JetBrains Marketplace**:
   - Configure JetBrains Marketplace automated upload action for official `v*` releases of `anahata-asi-intellij`.

---

### 3.3. macOS & Windows Package Managers
1. **Homebrew Cask / Tap (`brew install anahata-os/tap/anahata-asi-desktop`)**:
   - Provide official Homebrew formula for macOS and Linuxbrew users.
2. **Windows Package Manager (`winget`) & Chocolatey**:
   - Submit `Anahata.ASIDesktop` manifest to Microsoft `winget-pkgs` repository.
