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
1. **One-Pass Whole Reactor Build**:
   - Compiles all 13 modules, runs test suites, and aggregates Javadocs.
   - If `IS_RELEASE == true` and `inputs.deploy_central == true`, deploys lightweight core JARs to Sonatype Central. Otherwise, builds strictly locally.
2. **Parameterized NetBeans NBM Stamping Loop**:
   - Compiles NetBeans 30 (`RELEASE300`) and NetBeans 31 (`RELEASE310`) NBMs.
   - Stamps versions (`1.1.x.300` / `1.1.x.310`) and generates `updates.xml` (release) or `dev-updates.xml` (dev).
   - Configures `distBase` to point to **GitHub Releases CDN**, eliminating Central 404 sync race conditions.
3. **Standalone Update Center Plugin (`anahata-asi-nb-uc`)**:
   - Ultra-lightweight (~35 KB) module with zero implementation locks.
   - Deploys to Maven Central for Apache NetBeans Plugin Portal verification when `deploy_central` and `release_nb_uc` are selected.
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
   - Merges versioned Javadocs into the persistent `apidocs/` vault on `gh-pages`.
   - Prunes obsolete development snapshot docs and auto-indexes `apidocs/index.html` with smart version badges.
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
- ✅ Dynamic versioning for Canonical Snap Store (`snapcraft.yaml`).
- ✅ Multi-format Linux packaging (`.AppImage`, `.deb`, `.tar.gz`, `.snap`).
- ✅ Tabbed responsive download portals on `nb.html` and `desktop.html`.

### Pending / Next Steps:
- ⏳ **Canonical Snap Classic Confinement**: Awaiting forum review on `forum.snapcraft.io`.
- ⏳ **GitHub Pages APT Repository**: Set up automated `dpkg-scanpackages` indexing at `https://asi.anahata.uno/apt/`.
- ⏳ **FUSE-Independent AppImage Runtime**: Ensure 1-click double-click launch on modern Ubuntu without manual `libfuse2` installation.
- ⏳ **Flathub (Flatpak) Manifest**: Submit `uno.anahata.asi.desktop` to Flathub.
- ⏳ **JetBrains Marketplace**: Connect automated upload token for IntelliJ plugin releases.
