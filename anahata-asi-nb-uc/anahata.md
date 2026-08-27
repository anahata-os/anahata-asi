# Project Instructions: Anahata ASI Update Center - NetBeans

This file contains project-specific system instructions for the **Anahata ASI Update Center - NetBeans** project.

**Note**: This is a **Sub-module** of **anahata-asi-parent**. These instructions are intended to extend the shared context provided by the parent project's `anahata.md`.

## 2. Plugin About / Long Description HTML 3.2 Standard

> [!IMPORTANT]
> The NetBeans Plugins Manager / About panel (`DetailsPanel.java` / `UnitDetails.java`) utilizes Java Swing's built-in `HTMLEditorKit`, which strictly supports **HTML 3.2 only**.
>
> - **Image Attributes Mandatory:** Swing completely ignores inline CSS on images (e.g. `style="height: 24px;"`). All `<img>` tags **must** declare explicit HTML attributes: `width="32" height="32" align="middle" border="0"`.
> - **Screenshots:** Standard capability screenshots must use explicit pixel widths (e.g., `width="650" border="0"`), while native-size screenshots (`uc-1.png`, `uc-2.png`) omit artificial width constraints.
> - **No CSS3 / Flexbox:** `display: flex`, `gap`, `!important`, and `border-radius` are ignored or cause parsing distortions in Swing. Use standard `<table>`, `&nbsp;&nbsp;`, and `<br/>` tags.
> - **The About panel is mission-critical** for high-impact presentation on both the Apache NetBeans Plugin Portal and inside the IDE.
