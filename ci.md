# Continuous Integration & Deployment (CI/CD)

## Artifact Publishing
All project artifacts (NBMs, IntelliJ IDEA plugin distributions, native Desktop installers, JARs, POMs) are compiled, validated, and published via a unified, multi-job GitHub Action (`deploy-artifacts.yml`), triggered on pushes to the `main` branch or release tags (`v*`).

### Publishing Pipelines
1.  **Platform NBMs & NetBeans Generation Suffixes**:
    - Multi-target matrix build for NetBeans releases (e.g. `300` for `RELEASE300`, `310` for `RELEASE310`).
    - Deterministic version stamping: `1.1.0-SNAPSHOT` -> `1.1.0.300-SNAPSHOT` (Dev) / `1.1.0` -> `1.1.0.300` (Release).
    - Published to **Sonatype Central Snapshot repository** on pushes to `main`, and **Sonatype Central Release portal** on release tags.
    - Automated catalog generation: `mvn nbm:autoupdate` produces `updates.xml` and `updates.xml.gz` catalogs deployed per NetBeans generation (`/netbeans/30/`, `/netbeans/31/`).
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

-   **Workflow**: `.github/workflows/deploy-website.yml`
-   **Custom Domain**: [https://asi.anahata.uno](https://asi.anahata.uno)
-   **Deployment Method**: Hybrid Cloud Deployment. The runner compiles the new version's Javadocs, pulls the historical `apidocs/` vault from the persistent `gh-pages` branch, merges them, auto-indexes the landing page via an inline Python script, deploys NetBeans update center catalogs, and commits the updated vault back to `gh-pages` automatically.

### Update Center Strategy
- **Stable Channel**: `https://asi.anahata.uno/netbeans/30/updates.xml` (NetBeans 30) / `https://asi.anahata.uno/netbeans/31/updates.xml` (NetBeans 31).
- **Development Channel**: `https://asi.anahata.uno/netbeans/30/dev-updates.xml` / `https://asi.anahata.uno/netbeans/31/dev-updates.xml`.

### Javadoc Strategy
We maintain a stateful, multi-version Javadoc repository in the cloud without local git bloat.
-   **Storage Path**: `apidocs/${project.version}/`
-   **Aggregation**: Javadocs are aggregated at the parent level using `javadoc:aggregate`.
-   **Persistence**: The deployment workflow automatically preserves all historical stable release folders on the `gh-pages` branch, while maintaining a rolling, live-updated `Latest` directory for SNAPSHOT builds.
-   **Access**: The dynamic directory entry point is [https://asi.anahata.uno/apidocs/index.html](https://asi.anahata.uno/apidocs/index.html).

## Current Status & Transition Plan
-   **V1**: The `anahata.uno` domain is currently pointed to the V1 website (hosted in the `anahata-netbeans-ai` project).
-   **V2 (JASI)**: The V2 portal is live at `asi.anahata.uno`.
-   **Active Modules**: All modules, including `anahata-asi-intellij` and `anahata-asi-yam`, are part of the automated CI/CD pipeline.
