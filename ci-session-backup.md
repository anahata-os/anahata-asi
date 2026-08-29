# Flight Recorder Session Backup (`ci-session-backup.md`)
**Session ID**: `3fde32fd-2eb2-4089-905a-7597569dc221`  
**Nickname**: `CI/CD`  
**Timestamp**: 2026-08-28 19:15 CEST  
**Host Environment**: Linux 7.0.0-30-generic | Apache NetBeans 30 | JDK 26.0.1  

---

## 1. Executive Summary & Session Mission
This session completed the comprehensive multi-platform release engineering, distribution overhaul, and CI/CD architecture for the **Anahata ASI Platform**, covering:
1. **Unification into `build3.yml`**: Master multi-platform pipeline for Linux (`ubuntu-latest`), Windows (`windows-latest`), and macOS (`macos-latest`).
2. **Sonatype Central Quota Protection**: Eliminated all snapshot uploads to Central, gated core JAR releases via `deploy_central`, and routed heavy NetBeans Studio NBMs (~140 MB) through GitHub Releases CDN.
3. **Multi-Format Linux Desktop Distribution**: Added **Universal AppImage (`.AppImage`)**, **Debian (`.deb`)**, **Portable Tarball (`.tar.gz`)**, and **Ubuntu Snap Store (`.snap`)** to `build3.yml`.
4. **Canonical Snap Store Integration**: Generated dynamic version stamping, high-res lotus icon, and web store listing on `snapcraft.io`. Documented strict sandboxing collisions and classic confinement requirements in `snap.md`.
5. **Distribution Reference & Strategic Roadmap**: Authored `cd.md` and updated `ci.md`.
6. **Web Portal Synchronization**: Updated `desktop.html`, `nb.html`, and `intellij.html` with responsive tabbed download layouts.

---

## 2. In-Context Managed Resources (All 43 Active Files)
1. `anahata.md` (Parent Root) &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata.md`
2. `anahata-asi-web/anahata.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-web/anahata.md`
3. `anahata-asi-desktop/anahata.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-desktop/anahata.md`
4. `anahata-asi-swing/anahata.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-swing/anahata.md`
5. `anahata-asi-core/anahata.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-core/anahata.md`
6. `anahata-asi-gemini/anahata.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-gemini/anahata.md`
7. `anahata-asi-openai-compatible/anahata.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-openai-compatible/anahata.md`
8. `anahata-asi-javafx/anahata.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-javafx/anahata.md`
9. `anahata-asi-openai/anahata.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-openai/anahata.md`
10. `anahata-asi-anthropic/anahata.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-anthropic/anahata.md`
11. `pom.xml` (Parent Root) &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/pom.xml`
12. `anahata-asi-nb/pom.xml` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-nb/pom.xml`
13. `anahata-asi-nb-uc/pom.xml` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-nb-uc/pom.xml`
14. `anahata-asi-desktop/pom.xml` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-desktop/pom.xml`
15. `anahata-asi-intellij/pom.xml` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-intellij/pom.xml`
16. `.github/workflows/build3.yml` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/.github/workflows/build3.yml`
17. `.github/workflows/deploy-to-prod.yml` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/.github/workflows/deploy-to-prod.yml`
18. `.github/workflows/build.yml` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/.github/workflows/build.yml`
19. `.github/workflows/manual-release.yml` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/.github/workflows/manual-release.yml`
20. `ci.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/ci.md`
21. `cd.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/cd.md`
22. `snap/snapcraft.yaml` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/snap/snapcraft.yaml`
23. `anahata-asi-desktop/snap.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-desktop/snap.md`
24. `anahata-asi-web/src/main/resources/web/desktop.html` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-web/src/main/resources/web/desktop.html`
25. `anahata-asi-web/src/main/resources/web/nb.html` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-web/src/main/resources/web/nb.html`
26. `anahata-asi-web/src/main/resources/web/intellij.html` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-web/src/main/resources/web/intellij.html`
27. `anahata-asi-web/src/main/resources/web/index.html` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-web/src/main/resources/web/index.html`
28. `anahata-asi-web/src/main/resources/web/compatible.html` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-web/src/main/resources/web/compatible.html`
29. `anahata-asi-web/src/main/resources/web/nav.js` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-web/src/main/resources/web/nav.js`
30. `anahata-asi-web/src/main/resources/web/apidocs/index.html` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-web/src/main/resources/web/apidocs/index.html`
31. `README.md` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/README.md`
32. `.gitignore` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/.gitignore`
33. `anahata-asi-desktop/src/main/java/uno/anahata/asi/desktop/Main.java` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-desktop/src/main/java/uno/anahata/asi/desktop/Main.java`
34. `anahata-asi-core/src/main/java/uno/anahata/asi/AbstractAsiContainer.java` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-core/src/main/java/uno/anahata/asi/AbstractAsiContainer.java`
35. `anahata-asi-core/src/main/java/uno/anahata/asi/AsiContainerPreferences.java` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-core/src/main/java/uno/anahata/asi/AsiContainerPreferences.java`
36. `anahata-asi-core/src/main/java/uno/anahata/asi/agi/AgiConfig.java` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-core/src/main/java/uno/anahata/asi/agi/AgiConfig.java`
37. `anahata-asi-core/src/main/java/uno/anahata/asi/toolkit/AsiContainer.java` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-core/src/main/java/uno/anahata/asi/toolkit/AsiContainer.java`
38. `anahata-asi-swing/src/main/java/uno/anahata/asi/swing/AgiCard.java` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-swing/src/main/java/uno/anahata/asi/swing/AgiCard.java`
39. `anahata-asi-nb-uc/src/main/java/uno/anahata/asi/nb/uc/AnahataUcUtils.java` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-nb-uc/src/main/java/uno/anahata/asi/nb/uc/AnahataUcUtils.java`
40. `anahata-asi-nb-uc/src/main/java/uno/anahata/asi/nb/uc/AnahataUpdateCenterPanel.java` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-nb-uc/src/main/java/uno/anahata/asi/nb/uc/AnahataUpdateCenterPanel.java`
41. `anahata-asi-nb/src/main/java/uno/anahata/asi/nb/util/AnahataUpdateCenterUtils.java` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-nb/src/main/java/uno/anahata/asi/nb/util/AnahataUpdateCenterUtils.java`
42. `anahata-asi-nb-uc/long-description.html` &mdash; `file:///home/pablo/NetBeansProjects/anahata-asi-parent/anahata-asi-nb-uc/long-description.html`
43. `Pictures/anahata.png` &mdash; `file:///home/pablo/Pictures/anahata.png`

---

## 3. Key Technical Inventions & Solutions Established in this Session
* **Universal AppImage Self-Extracting Wrapper**: Eliminates `libfuse.so.2` dependency on modern Linux distributions (Ubuntu 24.04+, Fedora, Arch).
* **Direct Debian `.deb` Generator**: Integrates native system installation to `/opt/` via `jpackage --type deb`.
* **Zero-Quota NBM Delivery**: Studio NBMs delivered directly via GitHub Releases CDN, avoiding Sonatype Central 80MB/month limits.
* **1-Click SemVer Release Engine**: `deploy-to-prod.yml` tags releases, increments patch versions, and dispatches multi-platform builds.
