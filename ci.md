# Continuous Integration & Deployment (CI/CD)

## 1. Architectural Strategy & Pipeline Overview
All project artifacts (NetBeans NBMs, IntelliJ IDEA plugin distributions, native Desktop installers across Linux/Windows/macOS, Core JARs, POMs), documentation vaults, and website deployments are compiled, validated, and published via a unified master GitHub Action: **`.github/workflows/build3.yml`**.

The pipeline is triggered automatically on pushes to the `main` branch (Dev Snapshot mode) or through the **1-Click Production Release Dispatcher** (`.github/workflows/deploy-to-prod.yml`) on official release tags (`v*`).

---

## 2. The 3-Tier Distribution Architecture

To prevent vendor lock-in, eliminate CDN sync race conditions, and strictly protect Sonatype Maven Central monthly quotas, distribution is split into 3 decoupled tiers:

| Tier | Target Repository | Scope & Artifacts | Automation / Gating |
| :--- | :--- | :--- | :--- |
| **Tier 1: Maven Central** | `repo1.maven.org` | Lightweight Core JARs (`core`, `swing`, `providers`, `desktop`, `parent` POM) & standalone Update Center NBM (~4 MB total). | **Gated by `deploy_central` checkbox** on official production releases only. Zero snapshot uploads. |
| **Tier 2: GitHub Releases** | GitHub Releases CDN (`v*` & `latest-snapshot`) | **ALL binary distributions**: NetBeans 30/31 NBMs (~140 MB), IntelliJ plugin `.zip`, Linux `.AppImage`, Linux `.deb`, Linux `.tar.gz`, Windows `.zip`, macOS `.zip`. | **Always active unconditionally** on both snapshot builds and official production releases. |
| **Tier 3: User-Facing Channels** | Update Centers & App Stores | NetBeans `updates.xml` catalogs, Canonical Snap Store (`stable`), JetBrains Marketplace. | **Granularly gated by target checkboxes** in `deploy-to-prod.yml` to prevent unwanted user update notifications. |

---

## 3. Master Pipeline Specifications (`build3.yml`)

### 3.1. Single-Runner Linux Master Pipeline (`build-and-deploy`)
Runs on `ubuntu-latest` and executes all core platform tasks in a single checkout:
1. **One-Pass Whole Reactor Build & Decoupled Javadocs**:
   - Compiles all 13 modules, runs test suites, and deploys to Sonatype Central if `deploy_central == true` (deploying Core, Swing, Desktop, Providers, and `nb-uc` atomically).
   - **Platform Core Javadoc Aggregate** (`apidocs/${version}/apidocs/`): Excludes heavy IDE modules (`-pl !:anahata-asi-nb,!:anahata-asi-nb-uc,!:anahata-asi-intellij`). Compiles in ~13s with zero NetBeans/JetBrains platform stubs. Parent POM uses `<inherited>false</inherited>` so CLI aggregate embeds `anahata-barca-theme.css` without breaking child artifact `javadoc.jar`s during release.
   - **NetBeans Studio Javadoc** (`apidocs/${version}/nb/`): Standalone build using `${project.parent.basedir}/src/main/javadoc` for single-source stylesheet and `<offlineLinks>` to link Core types (`Agi`, `ToolContext`, `AgiPanel`) as active Barça-gold hyperlinks without network calls.
   - **IntelliJ Studio Javadoc** (`apidocs/${version}/intellij/`): Standalone build with `<offlineLinks>` cross-linking to Core.
2. **Parameterized NetBeans NBM Stamping Loop & Isolated Catalogs (`-f`)**:
   - Compiles NetBeans 30 (`RELEASE300`) and NetBeans 31 (`RELEASE310`) NBMs.
   - **Target Directory Cleanup**: Runs `rm -rf anahata-asi-nb/target/*.nbm anahata-asi-nb/target/netbeans_site` before building each target to eliminate cross-version NBM pollution.
   - **Strict 1-Module per Catalog Isolation**: Executes `mvn -f anahata-asi-nb/pom.xml nbm:autoupdate`. Using `-f` isolates the reactor to 1 project, preventing `nbm:autoupdate` from bundling `nb-uc` into versioned catalogs. `nb/30` contains strictly `1.x.300`, and `nb/31` contains strictly `1.x.310`.
   - Configures `distBase` to point to **GitHub Releases CDN**, eliminating Central 404 sync race conditions.
3. **Standalone Update Center Plugin (`anahata-asi-nb-uc`)**:
   - Ultra-lightweight (~35 KB) module pinned to `RELEASE300` for universal backwards compatibility (NB 30+).
   - **Unconditional Binary Staging**: Compiled and staged into `staging-binaries/` on **every build** (including snapshots) so `anahata-asi-nb-uc-*.nbm` is always available in `latest-snapshot` on GitHub Releases (preventing 404s).
   - **No Duplicate Central Deployment**: Deployed to Maven Central exclusively via the Step 1 reactor bundle when `deploy_central: true`. Step 3 never runs `mvn deploy` to avoid Sonatype GAV collision errors.
   - **Isolated Universal Catalog**: Executes `mvn -f anahata-asi-nb-uc/pom.xml nbm:autoupdate` to guarantee `nb/updates.xml` contains **strictly and only `uno.anahata.asi.nb.uc`** (zero Studio NBMs).
4. **IntelliJ IDEA Plugin Packaging**:
   - Packages `anahata-asi-intellij-*.zip` directly from compiled classes.
5. **Linux Native Desktop Suite**:
   - **`jpackage` App-Image**: Bundles private JRE with Generational ZGC (`-XX:+UseZGC -XX:+ZGenerational`) and adaptive memory scaling (`-XX:MaxRAMPercentage=60.0`).
   - **Universal `.AppImage`**: Standalone cross-distro executable for all Linux distributions.
   - **Debian Package (`.deb`)**: Native installer with system desktop integration.
   - **Portable `.tar.gz`**: Standalone binary directory.
   - **Canonical Snap Store**: Builds and publishes `.snap` container (`channel: edge` on dev snapshots, `channel: stable` on release when `release_snap == true`).
6. **Website, Javadoc Vault & GitHub Pages**:
   - Compiles static web portal from `anahata-asi-web`.
   - **Unified Split-View Javadoc Portal**: Deploys `apidocs/index.html` featuring interactive tabs (`⚡ Core Platform`, `☕ NetBeans Studio`, `💡 IntelliJ Studio`) embedding the selected Javadoc in an `<iframe>` alongside a scrollable SemVer release sidebar.
   - Merges versioned Javadocs into the persistent `apidocs/` vault on `gh-pages` and prunes obsolete snapshots.
   - Deploys live to **`https://asi.anahata.uno`**.

### 3.2. Parallel Multi-OS Matrix Builders
- **Windows Builder (`build-desktop-windows`)**: Runs on `windows-latest` to build native Windows portable `.zip`.
- **macOS Builder (`build-desktop-macos`)**: Runs on `macos-latest` to build native macOS App Bundle `.zip`.

### 3.3. Unified Release Publisher (`publish-release`)
- Collects all staged binaries from all three VM runners.
- Purges stale snapshot assets and publishes fresh packages to GitHub Releases in a single transaction.

---

## 4. 1-Click Production Release Dispatcher (`deploy-to-prod.yml`)

To cut an official release (e.g. `v1.1.14`):
1. Navigate to **Actions** &rarr; **🚀 1-Click Production Release Dispatcher (V3)**.
2. Select target options (defaults to `false` for safety):
   - `release_version` & `next_snapshot` (leave empty for automatic SemVer calculation).
   - ☐ `deploy_central` &mdash; Deploy Core Platform JARs to Maven Central.
   - ☐ `release_nb_300` &mdash; Release NetBeans 30 ASI Studio (Update `nb/30/updates.xml`).
   - ☐ `release_nb_310` &mdash; Release NetBeans 31 ASI Studio (Update `nb/31/updates.xml`).
   - ☐ `release_nb_uc` &mdash; Release NetBeans Update Center Plugin.
   - ☐ `release_intellij` &mdash; Release IntelliJ IDEA Plugin (.zip).
   - ☐ `release_snap` &mdash; Release ASI Desktop Snap Package (to Canonical Snap Store `stable`).
   - ☐ `release_desktop` &mdash; Release ASI Desktop Native Installers (Windows & Mac).
3. Click **Run workflow** &mdash; the dispatcher tags the release commit, advances POMs to the next snapshot development cycle, and triggers `build3.yml` in Release Mode.

---

## 5. Current Status & Pending Roadmap

### Completed:
- ✅ Full CI unification into `build3.yml` with parallel Windows/macOS runners.
- ✅ Sonatype Central quota preservation (Studio NBMs served from GitHub Releases).
- ✅ Decoupled 3-tier Javadoc pipeline (`Core`, `NetBeans`, `IntelliJ`) with cross-linking (`offlineLinks`).
- ✅ Strict "1 Module per Catalog" isolation using `-f <module>/pom.xml` on `nbm:autoupdate`.
- ✅ Standalone Update Center NBM staging on snapshot builds (eliminating 404 errors).
- ✅ Native NetBeans 31 (`RELEASE310`) platform migration for `anahata-asi-nb`.
- ✅ JavaFX implementation runtime installer with interactive `[ Restart Now ]` / `[ Restart Later ]` prompts.
- ✅ Multi-format Linux packaging (`.AppImage`, `.deb`, `.tar.gz`, `.snap`).
- ✅ Tabbed responsive download portals on `nb.html` and `desktop.html`.

### Pending / Next Steps:
- ⏳ **1-Click Release Announcement**: Wire `release_announcement` input in `deploy-to-prod.yml` to automatically inject `<notification>` into NetBeans `updates.xml`, `<change-notes>` into IntelliJ `plugin.xml`, and the GitHub Release header.
- ⏳ **Canonical Snap Classic Confinement**: Awaiting forum review on `forum.snapcraft.io`.
- ⏳ **GitHub Pages APT Repository**: Set up automated `dpkg-scanpackages` indexing at `https://asi.anahata.uno/apt/`.
- ⏳ **FUSE-Independent AppImage Runtime**: Ensure 1-click double-click launch on modern Ubuntu without manual `libfuse2` installation.
- ⏳ **Flathub (Flatpak) Manifest**: Submit `uno.anahata.asi.desktop` to Flathub.
- ⏳ **JetBrains Marketplace**: Connect automated upload token for IntelliJ plugin releases.
