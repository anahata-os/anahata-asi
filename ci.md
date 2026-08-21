# Continuous Integration & Deployment (CI/CD)

## Artifact Publishing
All project artifacts (NBMs, IntelliJ IDEA plugin distributions, native Desktop installers, JARs, POMs) and website/Javadoc deployments are compiled, validated, and published via a unified, multi-job GitHub Action (`build.yml`), triggered on pushes to the `main` branch or release tags (`v*`).

### Publishing Pipelines
1.  **Platform NBMs & NetBeans Generation Suffixes**:
    - Multi-target matrix build for NetBeans releases (e.g. `300` for `RELEASE300`, `310` for `RELEASE310`).
    - Deterministic version stamping: `1.1.0-SNAPSHOT` -> `1.1.0.300-SNAPSHOT` (Dev) / `1.1.0` -> `1.1.0.300` (Release).
    - Published to **Sonatype Central Snapshot repository** on pushes to `main`, and **Sonatype Central Release portal** on release tags.
    - **Website & Direct Downloads**: Direct NBM download links on `asi.anahata.uno` resolve from **Maven Central** (for Stable releases) and **Sonatype Snapshots** (for Dev builds).
    - Automated catalog generation: `mvn nbm:autoupdate` produces `updates.xml` (for stable releases) and `dev-updates.xml` (for dev snapshots), deploying both uncompressed `.xml` and compressed `.xml.gz` catalogs per NetBeans generation (`/nb/30/`, `/nb/31/`) with fail-fast validation in CI.
2.  **IntelliJ IDEA Plugin Distribution**:
    - Packaged as a standalone distribution ZIP (`anahata-asi-intellij-${version}.zip`) via `maven-assembly-plugin`.
    - Bundles all core and swing dependencies alongside PSI-based IDE tools.
3.  **Native Desktop Installers**:
    - Compiled on a cross-platform matrix (Linux, Windows, macOS) and packaged into portable native standalone app-bundles (`.zip` and `.tar.gz`) using `jpackage`.
4.  **Atomic GitHub Releases**:
    - The synchronized release job purges old snapshots and uploads all binaries (NBMs, IntelliJ plugin ZIP, and the 3 native desktop installers) together in a single, atomic, collision-free transaction to the `latest-snapshot` release (or versioned release on `v*` tags).

### Credentials
-   Both paths use the `sonatype-central` server ID for credential management in GitHub Actions.
-   **Verification**: The build uses the `central-publishing-maven-plugin` to handle the deferred deployment and portal integration.

## Website & Javadoc Deployment
The project website, update catalogs, and aggregated Javadocs are deployed to **GitHub Pages** using the modern Actions-based deployment method.

-   **Workflow**: `.github/workflows/build.yml`
-   **Custom Domain**: [https://asi.anahata.uno](https://asi.anahata.uno)
-   **Deployment Method**: Hybrid Cloud Deployment. The runner compiles the new version's Javadocs, pulls the historical `apidocs/` vault from the persistent `gh-pages` branch, merges them, auto-indexes the landing page via an inline Python script, deploys NetBeans update center catalogs, and commits the updated vault back to `gh-pages` automatically.

### Update Center Strategy
- **Stable Channel**: `https://asi.anahata.uno/nb/30/updates.xml` (NetBeans 30) / `https://asi.anahata.uno/nb/31/updates.xml` (NetBeans 31).
- **Development Channel**: `https://asi.anahata.uno/nb/30/dev-updates.xml` / `https://asi.anahata.uno/nb/31/dev-updates.xml`.
- **Hosting & Fail-Fast Delivery**: Catalogs are published in both uncompressed (`.xml`) and gzip-compressed (`.xml.gz`) formats. The CI build fails immediately if `target/netbeans_site/updates.xml` is missing.

## Triggering Releases on GitHub

### 1. Rolling Snapshots (Automatic on `main`)
Every push to `main` automatically:
- Builds target-specific NBMs (`1.1.0.300-SNAPSHOT`, `1.1.0.310-SNAPSHOT`) and deploys them to the **Sonatype Central Snapshot repository**.
- Generates snapshot update catalogs (`/nb/30/dev-updates.xml.gz`, `/nb/31/dev-updates.xml.gz`).
- Compiles native Desktop binaries (Linux, Windows, macOS) and the IntelliJ plugin ZIP.
- Atomically refreshes the `latest-snapshot` release tag on GitHub.
- Updates the live website and latest Javadoc vault on `asi.anahata.uno`.

### 2. Official Stable GA Releases
To cut an official release (e.g. `v1.1.0`), choose any of the three synchronized release methods:

#### Method A: 1-Click GitHub Actions Web UI (Recommended for Team)
Anyone with maintainer permissions can trigger a release from their browser (even on a smartphone):
1. Navigate to **Actions** &rarr; **🚀 1-Click Production Release Dispatcher** (`manual-release.yml`).
2. Click **Run workflow**.
3. *(Optional)* Leave inputs blank to automatically strip `-SNAPSHOT` from POMs and auto-increment the next patch cycle, or specify custom versions (e.g. Release: `1.1.0`, Next: `1.2.0-SNAPSHOT`).
4. Click **Run workflow** &mdash; the cloud runner handles version bumping, commits, tag creation, and rollover automatically!

#### Method B: Transactional Cross-Platform Release Scripts
Run the automated pre-flight release coordinator locally:
- **macOS / Linux / Git Bash**:
  ```bash
  ./release.sh 1.1.0 1.2.0-SNAPSHOT
  git push origin main --tags
  ```
- **Windows (CMD / PowerShell)**:
  ```cmd
  release.bat 1.1.0 1.2.0-SNAPSHOT
  git push origin main --tags
  ```

#### Method C: Direct CLI Git Tagging
1. **Set Release Version**:
   ```bash
   mvn versions:set -DnewVersion=1.1.0 -DgenerateBackupPoms=false
   git commit -am "chore(release): prepare v1.1.0"
   git push origin main
   ```
2. **Push Git Tag**:
   ```bash
   git tag v1.1.0
   git push origin v1.1.0
   ```
3. **Prepare Next Development Cycle**:
   ```bash
   mvn versions:set -DnewVersion=1.2.0-SNAPSHOT -DgenerateBackupPoms=false
   git commit -am "chore: open 1.2.0-SNAPSHOT development cycle"
   git push origin main
   ```

### 3. Automated Release Cloud Execution
When a `v*` tag is pushed (via Web UI, script, or CLI):
- **`build.yml` (Artifacts Pipeline)**:
  - Stamps NetBeans generation suffixes (`1.1.0.300`, `1.1.0.310`).
  - Activates `-P release`, signs all artifacts with GPG, and deploys to the **Sonatype Central Release Portal** (`central-publishing-maven-plugin`).
  - Packages standalone IntelliJ `.zip` distribution and native Desktop app-images (Linux, Windows, macOS).
  - Publishes the official GitHub Release for `v1.1.0` marked as `Latest` with all binaries attached.
- **`build.yml` (Website & Javadoc Pipeline)**:
  - Archives versioned Javadocs under `apidocs/1.1.0/` and persists to `gh-pages`.
  - Deploys official `updates.xml` catalogs to `/nb/30/` and `/nb/31/` on `asi.anahata.uno`.

### Javadoc Strategy
We maintain a stateful, multi-version Javadoc repository in the cloud without local git bloat.
-   **Storage Path**: `apidocs/${project.version}/`
-   **Aggregation**: Javadocs are aggregated at the parent level using `javadoc:aggregate`.
-   **Persistence**: The deployment workflow automatically preserves all historical stable release folders on the `gh-pages` branch, while maintaining a rolling, live-updated `Latest` directory for SNAPSHOT builds.
-   **Access**: The dynamic directory entry point is [https://asi.anahata.uno/apidocs/index.html](https://asi.anahata.uno/apidocs/index.html).

## Current Status & Transition Plan
-   **V1**: The `anahata.uno` domain is currently pointed to the V1 website (hosted in the `anahata-netbeans-ai` project).
-   **V2 (ASI)**: The V2 portal is live at `asi.anahata.uno`.
