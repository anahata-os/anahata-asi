# Anahata ASI Release Coordination Protocol

This document outlines the standard operating procedure for cutting and deploying a new release (or Release Candidate) of the Anahata ASI platform.

The release process is a hybrid "Tiki-Taka" flow: preparation is executed locally within the IDE or container, while the heavy lifting of compilation, signing, and distribution is handled autonomously by GitHub Actions.

## Phase 1: Local Preparation (The Setup)
Executed directly within the Anahata ASI Container or the host IDE terminal.

1. **Version Bump**: Surgically update the `<version>` tags in the parent `pom.xml` and all sub-modules (e.g., from `1.0.0-SNAPSHOT` to `1.0.0-RC1`).
2. **Commit the State**: Commit the version changes to the Git repository.
   ```bash
   git commit -am "chore: cut release v1.0.0-RC1"
   ```
3. **Tag the Release**: Create a Git tag to mark the exact commit for the CI/CD pipeline.
   ```bash
   git tag v1.0.0-RC1
   ```
4. **Push the Payload**: Push the commit and the tags to the remote repository.
   ```bash
   git push origin main --tags
   ```

## Phase 2: Remote Execution (The Cloud Pipeline)
The moment the tag hits GitHub, the CI/CD pipeline takes over completely hands-free.

1. **Artifact Deployment (`deploy-artifacts.yml`)**:
   - Triggered automatically by the `v*` tag.
   - Provisions a cloud runner with JDK 25.
   - Compiles the multi-module project and runs all tests.
   - Signs the JARs, POMs, and `.nbm` files using configured GPG keys.
   - Deploys the artifacts directly to the **Sonatype Central Portal** using the `central-publishing-maven-plugin`.

2. **Website & Javadoc Deployment (`deploy-website.yml`)**:
   - Aggregates Javadocs across all modules using `maven-javadoc-plugin`.
   - Deploys the HTML documentation, SEO metadata, and the website assets directly to GitHub Pages (`https://asi.anahata.uno`).

## Phase 3: Verification (The Ledger Protocol)
1. **Wait**: Allow 5-10 minutes for Sonatype to sync the artifacts to the public Maven Central index.
2. **Verify Central**: Use the `Maven.searchMavenIndex` tool (or search manually) to confirm the new artifacts are live and resolvable globally.
3. **Verify Documentation**: Check `https://asi.anahata.uno/apidocs/index.html` to ensure the new version's Javadocs have been successfully aggregated, and the `sitemap.xml` is actively routing crawlers to the new docs.