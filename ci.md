# Continuous Integration & Deployment (CI/CD)

## Artifact Publishing
All project artifacts (NBMs, IntelliJ IDEA plugin distributions, native Desktop installers, JARs, POMs) and website/Javadoc deployments are compiled, validated, and published via a unified, multi-job GitHub Action (`build.yml`), triggered on pushes to the `main` branch or release tags (`v*`).

### Publishing Pipelines
1.  **Core Platform & Application JARs**:
    - Deploys `parent`, `core`, `swing`, `javafx`, `desktop`, `intellij`, `yam`, `web`, and all AI providers in a single atomic bundle to **Maven Central** (`central-publishing-maven-plugin`).
    - Excludes NetBeans NBM modules (`anahata-asi-nb`, `anahata-asi-nb-uc`) from the core JAR upload to prevent staging coordinate collisions.
    - Packages the standalone IntelliJ IDEA plugin distribution ZIP (`anahata-asi-intellij-${version}.zip`) directly during `build-core`, guaranteeing byte-for-byte binary identity between deployed JARs and distribution archives.
2.  **Platform NBMs & NetBeans Generation Suffixes**:
    - Multi-target matrix build for NetBeans releases (e.g. `300` for `RELEASE300`, `310` for `RELEASE310`).
    - Deterministic version stamping: `1.1.0-SNAPSHOT` -> `1.1.0.300-SNAPSHOT` (Dev) / `1.1.0` -> `1.1.0.300` (Release).
    - Uses `flatten-maven-plugin` (OSS mode) to generate standalone, self-contained POMs with hard-inlined metadata and dependency versions, eliminating parent POM resolution errors on Sonatype Central.
    - Automated catalog generation: `mvn nbm:autoupdate` produces `updates.xml` (for stable releases) and `dev-updates.xml` (for dev snapshots), deploying catalogs per NetBeans generation (`/nb/30/`, `/nb/31/`) with fail-fast validation in CI.
3.  **Standalone Update Center Plugin (`anahata-asi-nb-uc`)**:
    - Dedicated standalone NetBeans module with zero implementation dependencies.
    - Deploys to Maven Central and produces the universal Update Center catalog at `https://asi.anahata.uno/nb/updates.xml`.
    - Only rebuilt and deployed when explicitly enabled in release options to avoid end-user update notification fatigue.
4.  **Native Desktop Installers**:
    - Compiled on a cross-platform matrix (Linux, Windows, macOS) and packaged into portable native standalone app-bundles (`.zip` and `.tar.gz`) using `jpackage`.
    - Ready for Canonical Ubuntu Snap Store deployment via `canonical/action-publish`.
5.  **Atomic GitHub Releases**:
    - The synchronized release job (`if: ${{ !cancelled() && !failed() }}`) purges old snapshots and uploads all binaries (NBMs, IntelliJ plugin ZIP, and the 3 native desktop installers) together in a single, atomic, collision-free transaction to the `latest-snapshot` release (or versioned release on `v*` tags).

### Credentials & Repositories
-   Both snapshot and release paths use the `sonatype-central` server ID for credential management in GitHub Actions.
-   **Maven Snapshot Isolation**: The `sonatype-snapshots` repository in the parent `pom.xml` is encapsulated inside a Maven profile activated **strictly on GitHub Actions runners** (`env.GITHUB_ACTIONS=true`). Local IDE and CLI builds never poll remote snapshot repositories over the network.
-   **Verification**: The build uses `central-publishing-maven-plugin:0.11.0` to handle deferred deployment and portal staging validation.

## Website & Javadoc Deployment
The project website, update catalogs, and aggregated Javadocs are deployed to **GitHub Pages** using the modern Actions-based deployment method.

-   **Workflow**: `.github/workflows/build.yml`
-   **Custom Domain**: [https://asi.anahata.uno](https://asi.anahata.uno)
-   **Deployment Method**: Hybrid Cloud Deployment. The runner compiles the new version's Javadocs, pulls the historical `apidocs/` vault and existing `nb/` catalogs from the persistent `gh-pages` branch, merges them, auto-indexes the landing page with smart version badges, prunes obsolete development snapshots, and commits the updated vault back to `gh-pages` automatically.

### Update Center Strategy
- **Universal Channel**: `https://asi.anahata.uno/nb/updates.xml` (Universal Update Center plugin).
- **NetBeans 30 Stable Channel**: `https://asi.anahata.uno/nb/30/updates.xml`.
- **NetBeans 30 Dev Channel**: `https://asi.anahata.uno/nb/30/dev-updates.xml`.
- **NetBeans 31 Stable Channel**: `https://asi.anahata.uno/nb/31/updates.xml`.
- **NetBeans 31 Dev Channel**: `https://asi.anahata.uno/nb/31/dev-updates.xml`.
- **Persistent Storage**: All catalogs are preserved on `gh-pages` by default. Releasing one target surgically updates only that target's catalog without overwriting or blanking other channels.

## Triggering Releases on GitHub

### 1. Rolling Snapshots (Automatic on `main`)
Every push to `main` automatically:
- Builds target-specific NBMs (`1.1.0.300-SNAPSHOT`, `1.1.0.310-SNAPSHOT`) and deploys them to the **Sonatype Central Snapshot repository**.
- Generates snapshot update catalogs (`/nb/30/dev-updates.xml`, `/nb/31/dev-updates.xml`).
- Compiles native Desktop binaries (Linux, Windows, macOS) and the IntelliJ plugin ZIP.
- Atomically refreshes the `latest-snapshot` release tag on GitHub.
- Updates the live website, prunes old snapshot Javadocs, and publishes to `asi.anahata.uno`.

### 2. Official Stable GA Releases
To cut an official release (e.g. `v1.1.8`), use the 1-Click Production Release Dispatcher:

#### 1-Click GitHub Actions Web UI (`manual-release.yml`)
1. Navigate to **Actions** &rarr; **🚀 1-Click Production Release Dispatcher**.
2. Click **Run workflow**.
3. Select granular release options:
   - Target Release & Next Snapshot versions (leave blank for automatic SemVer calculation).
   - ☑️ **`release_nb_300`** *(Default: YES)* &mdash; NetBeans 30 ASI Studio NBM
   - ☑️ **`release_nb_310`** *(Default: YES)* &mdash; NetBeans 31 ASI Studio NBM
   - ☐ **`release_nb_uc`** *(Default: NO)* &mdash; NetBeans Update Center Plugin
   - ☑️ **`release_desktop`** *(Default: YES)* &mdash; ASI Desktop Native Installers
   - ☑️ **`release_intellij`** *(Default: YES)* &mdash; IntelliJ IDEA Plugin (.zip)
4. Click **Run workflow** &mdash; the cloud runner handles version bumping, commits, tag creation, triggers the master production release build, and automatically launches the continuous snapshot build for the next development cycle!

### Javadoc Strategy
We maintain a stateful, multi-version Javadoc repository in the cloud without local git bloat.
-   **Storage Path**: `apidocs/${project.version}/`
-   **Aggregation**: Javadocs are aggregated at the parent level using `javadoc:aggregate`.
-   **Persistence**: The deployment workflow automatically preserves all historical stable release folders on the `gh-pages` branch, while maintaining a rolling, live-updated `Latest` directory for SNAPSHOT builds.
-   **Access**: The dynamic directory entry point is [https://asi.anahata.uno/apidocs/index.html](https://asi.anahata.uno/apidocs/index.html).

## Current Status & Transition Plan
-   **V1**: The `anahata.uno` domain is currently pointed to the V1 website (hosted in the `anahata-netbeans-ai` project).
-   **V2 (ASI)**: The V2 portal is live at `asi.anahata.uno`.
