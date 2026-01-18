# Continuous Integration & Deployment (CI/CD)

## Artifact Publishing
All project artifacts (JARs, POMs) are published to **Maven Central** via a GitHub Action (e.g., `publish-to-maven.yml`), triggered on release tags. This process leverages the `central-publishing-maven-plugin` configured in the parent POM's `release` profile.

## Website & Javadoc Deployment
The project website and aggregated Javadocs are deployed to **GitHub Pages**.

- **Workflow**: `.github/workflows/deploy-website.yml`
- **Target Branch**: `gh-pages`
- **Current Deployment URL**: [https://anahata-asi.github.io](https://anahata-asi.github.io) (V2 Beta)
- **Production URL**: [https://anahata.uno](https://anahata.uno) (Currently hosts V1)

### Javadoc Strategy
We maintain a versioned Javadoc repository on the `gh-pages` branch to allow users to access documentation for specific releases.
- **Storage Path**: `apidocs/${project.version}/`
- **Aggregation**: Javadocs are aggregated at the parent level using `javadoc:aggregate`.
- **Persistence**: The deployment action is configured with `clean: false`. This is critical; it ensures that Javadocs for older versions already on the `gh-pages` branch are **not** deleted when a new version is pushed.

#### Javadoc Configuration Notes
- **`outputDirectory`**: The target directory for generated HTML files.
- **`javadocDirectory`**: The source directory for custom Javadoc resources (CSS, images).
- **Note**: Do **not** use `destDir`. The Maven Javadoc plugin (v3.11.2) reports this parameter as "unknown" or "does not exist". Prefer setting the full versioned path directly in the output configuration.

## Current Status & Transition Plan
- **V1**: The `anahata.uno` domain is currently pointed to the V1 website (hosted in the `anahata-netbeans-ai` project).
- **V2**: The V2 website (this project) is being published to the GitHub Pages default URL for testing and preview.
- **Transition**: Once V2 reaches feature parity and stability, the `anahata.uno` domain will be updated to point to the V2 deployment.
