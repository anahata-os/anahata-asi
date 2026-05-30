# Anahata ASI Release Coordination Protocol

This document outlines the standard operating procedure for cutting and deploying a new release (or Release Candidate) of the Anahata ASI platform.

The release process is a hybrid "Tiki-Taka" flow: preparation is executed locally within the IDE or container, while the heavy lifting of compilation, signing, and distribution is handled autonomously by GitHub Actions.

---

## Strategic Branching and Support Model

To maintain a fast development cadence on cutting-edge features while providing absolute stability for enterprise users, Anahata uses a parallel branch layout:

1. **`main` (Bleeding Edge)**:
   * This is our primary integration branch.
   * All forward-looking features and capability expansions are developed and merged here.
   * The moment an official release or Release Candidate is cut, `main` POM versions must be immediately bumped to the next minor snapshot (e.g., `<version>1.1.0-SNAPSHOT</version>`).

2. **`support-X.Y` (Long-Term Support Branches)**:
   * When an official release is tagged (e.g., `v1.0.0-rc1` or `v1.0.0`), a dedicated, permanent support branch is spawned from that tag:
     ```bash
     git checkout -b support-1.0 v1.0.0
     git push origin support-1.0
     ```
   * **Hotfix protocol**: Any critical bug fixes, security patches, or platform-compatibility adjustments (e.g., producing `1.0.1`) are committed directly to `support-1.0`.
   * **Regression Prevention**: Once a hotfix is deployed, it must be surgically ported back to `main` using `git cherry-pick <commit-hash>` to ensure the cutting-edge version inherits the stability patch.

---

## Release Naming and Versioning Standards

Anahata strictly follows **Semantic Versioning 2.0.0 (SemVer)** and the custom sorting rules of Maven's `ComparableVersion` analyzer:

1. **The Gold Standard Qualifier**:
   * Pre-release versions must use lowercase qualifiers and a single-hyphen separator: **`1.0.0-rc1`** (Release Candidate 1).
   * Do not use uppercase `RC-1` or dotless shorthand `1.0.0rc1`. Standardizing on `1.0.0-rc1` guarantees that Maven, Sonatype, and external package managers parse, sort, and compare versions with 100% mathematical precision.

2. **Snapshot vs. Stable Isolation**:
   * Local development must always use snapshots (e.g., `1.0.0-SNAPSHOT`).
   * Never deploy raw SNAPSHOT versions to Sonatype Central. Keep snapshot builds strictly confined to local Maven repositories (`~/.m2/repository`) during hot-testing.

---

## Phase 1: Local Automation & Symmetrical Release Prep

All local release preparation—pre-flight compilation checks, symmetrical SemVer bumping, tagging, and post-release snapshot transitions—is fully automated via the root **`release.sh`** script.

### Option A: Standard Interactive Execution
For normal releases, execute the script from the root folder without arguments:
```bash
./release.sh
```
*   **Step-by-Step Flow**:
    1.  **Prudence Check**: Verifies that your git status is 100% clean.
    2.  **Version Capture**: Prompts you interactively for the `TARGET RELEASE` version (e.g., `1.0.0`) and the `NEXT DEVELOPMENT` snapshot version (e.g., `1.1.0-SNAPSHOT`).
    3.  **Local Pre-flight**: Runs `mvn clean install` locally to guarantee zero compiler alerts before pushing.
    4.  **Symmetrical Promotion**: Uses the versions plugin to set all 13 modules to the release version.
    5.  **Git Tagging**: Automatically commits the release and cuts the annotated tag (e.g., `v1.0.0`).
    6.  **Post-Release Transition**: Bumps the parent and submodules to the development snapshot, commits, and exits.

### Option B: Programmatic Execution (LTS & Support Branches)
If you are on a support branch (e.g., `support-1.0`) and need to cut a hotfix (e.g., `1.0.1`) and advance the branch's development cycle to the next maintenance snapshot (e.g., `1.0.2-SNAPSHOT`), the script supports **direct command-line arguments**, completely bypassing the interactive prompts:
```bash
./release.sh 1.0.1 1.0.2-SNAPSHOT
```
This enables headless, programmatic releases across any support branch or automated runner!

### Step 2: Push the Payload
To complete the transaction and unleash the automated cloud pipelines, execute:
```bash
git push origin main --tags
```
*(On support branches, push your support branch instead, e.g., `git push origin support-1.0 --tags`)*

---

## Phase 2: Remote Execution & Cloud Pipeline

The moment your push hits GitHub, the automation runner triggers hands-free orchestration:

1. **Artifact Deployment (`deploy-artifacts.yml`)**:
   * Kicks off automatically on the tag pattern `v*`.
   * Provisions an Ubuntu cloud runner with **JDK 25** and dependencies.
   * Compiles the multi-module project and runs all test suites.
   * Digitally signs the compiled JARs, POMs, and NetBeans `.nbm` files using your configured GPG keys.
   * Deploys the signed artifacts directly to **Sonatype Central Portal** using the `central-publishing-maven-plugin`.

2. **Standalone App Bundles (`standalone-release.yml`)**:
   * Triggered in parallel by the tag push.
   * Deploys on multi-platform runners (Ubuntu, macOS, Windows) and utilizes `jpackage` to package the standalone ASI Desktop application into native installers (`.zip` and `.tar.gz`).
   * Uploads the native binaries and the NetBeans `.nbm` file directly to a beautifully formatted, centralized release page corresponding to your tag.

3. **Dynamic Portal Documentation (`deploy-website.yml`)**:
   * Aggregates Javadocs across all active modules.
   * Syncs documentation to `https://asi.anahata.uno/apidocs/\${project.version}/apidocs/index.html`.
   * Dynamically triggers search engine crawl updates by updating the `sitemap.xml`.

---

## Phase 3: The NetBeans Plugin Portal Protocol (Volunteers Protection)

The Apache NetBeans Plugin Portal administers updates via a developer-triggered synchronization process. It does **not** blindly auto-publish versions from Maven Central.

### The Manual Synchronization Flow
1. **Maven Central Publishing**: Compile and deploy the version to Maven Central first.
2. **Sync Manifest**: Log into the NetBeans Plugin Portal, navigate to **My Plugins -> Anahata ASI**, and click the 🔄 **"Sync with source manifest"** button. This programmatically pulls the latest coordinate metadata (e.g., `1.0.0-rc1` or `1.0.0`) from the Central repository.

> [!WARNING]
> **PRESERVE VOLUNTEER SANITY**
> Because NetBeans requires manual human verification for each validated release, we gate our verification requests strictly by milestone:
> 1. **For Release Candidates (`-rc*`)**: We manually trigger "Sync with source manifest" so the RC is registered and accessible for power-users, but **we do NOT request verification**. This keeps the RC visible but leaves the volunteers' queue untouched.
> 2. **For Stable GA (`1.0.0`)**: Once the stable version is synced, we click **"Request Verification"** to get the green **"NB 30 - Verified"** badge.
> 3. **Prune Minor Iterations**: Accumulate minor bug fixes locally; never request verification for transient micro-versions.

### The V1-to-V2 User Migration Bridge
To cleanly migrate the existing 3,200 active V1 users (`anahata-netbeans-ai`) to the V2 platform:
1. Ensure the V2 (`anahata-asi-nb`) framework is fully published, synced, and approved in the NetBeans Plugin Portal.
2. Build a final, minor update for the V1 plugin and mark it as compatible with **NetBeans 30**.
3. This V1 patch will serve a prominent, high-salience notice inside the IDE components, highlighting V2's incredible capabilities (the CwGC, AST refiners, native side-by-side Diffs, and stand-alone desktop runtime) and direct them to download the V2 client at **`https://asi.anahata.uno`**.

---

## Phase 4: Verification and Cleanups

1. **Prune Stale Snapshots (via GitHub CLI)**:
   Ensure your release pages remain uncluttered. Use the official GitHub CLI (`gh`) over SSH to remove obsolete artifacts:
   ```bash
   gh release delete-asset -R anahata-os/anahata-asi latest-snapshot <stale-filename> -y
   ```

2. **Wait for Sonatype Indexing**:
   Allow 5 to 10 minutes for Sonatype Central to index and distribute the artifacts.

3. **Verify the Website**:
   Check `https://asi.anahata.uno/desktop.html` to confirm that the navigation and dynamic javascript is successfully fetching and rewriting the direct download URLs to point to your new release candidate!