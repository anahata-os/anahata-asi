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
-   **Deployment Method**: Direct deployment from the build runner (no `gh-pages` branch).

### Javadoc Strategy
We maintain a versioned Javadoc repository to allow users to access documentation for specific releases.
-   **Storage Path**: `apidocs/${project.version}/`
-   **Aggregation**: Javadocs are aggregated at the parent level using `javadoc:aggregate`.
-   **Persistence**: The deployment action is configured to preserve existing versioned Javadocs.
-   **Access**: The primary entry point is `apidocs/${project.version}/apidocs/index.html`.

## Current Status & Transition Plan
-   **V1**: The `anahata.uno` domain is currently pointed to the V1 website (hosted in the `anahata-netbeans-ai` project).
-   **V2 (JASI)**: The V2 portal is live at `asi.anahata.uno`.
-   **Active Modules**: All modules, including `anahata-asi-yam`, are part of the automated CI/CD pipeline.
