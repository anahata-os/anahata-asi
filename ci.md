# Continuous Integration & Deployment (CI/CD)

## Artifact Publishing
All project artifacts (JARs, POMs) are published to **Maven Central** via a GitHub Action (`deploy-artifacts.yml`), triggered on push to master or release tags.

### Deployment Paths
-   **Releases & Snapshots**: Published to the **Sonatype Central Portal** ecosystem.
-   **Credentials**: Both paths use the `sonatype-central` server ID for credential management in GitHub Actions.
-   **Verification**: The build uses the `central-publishing-maven-plugin` to handle the deferred deployment and portal integration.

## Website & Javadoc Deployment
The project website and aggregated Javadocs are deployed to **GitHub Pages** using the modern Actions-based deployment method.

-   **Workflow**: `.github/workflows/deploy-website.yml`
-   **Custom Domain**: [https://asi.anahata.uno](https://asi.anahata.uno)
-   **Deployment Method**: Hybrid Cloud Deployment. The runner compiles the new version's Javadocs, pulls the historical `apidocs/` vault from the persistent `gh-pages` branch, merges them, auto-indexes the landing page via an inline Python script, and commits the updated vault back to `gh-pages` automatically.

### Javadoc Strategy
We maintain a stateful, multi-version Javadoc repository in the cloud without local git bloat.
-   **Storage Path**: `apidocs/${project.version}/`
-   **Aggregation**: Javadocs are aggregated at the parent level using `javadoc:aggregate`.
-   **Persistence**: The deployment workflow automatically preserves all historical stable release folders on the `gh-pages` branch, while maintaining a rolling, live-updated `Latest` directory for SNAPSHOT builds.
-   **Access**: The dynamic directory entry point is [https://asi.anahata.uno/apidocs/index.html](https://asi.anahata.uno/apidocs/index.html).

## Current Status & Transition Plan
-   **V1**: The `anahata.uno` domain is currently pointed to the V1 website (hosted in the `anahata-netbeans-ai` project).
-   **V2 (JASI)**: The V2 portal is live at `asi.anahata.uno`.
-   **Active Modules**: All modules, including `anahata-asi-yam`, are part of the automated CI/CD pipeline.
