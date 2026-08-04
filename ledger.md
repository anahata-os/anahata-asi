# Anahata High-ROI Ledger: Operation Deep Strike

## 📈 Distribution Funnel
| Channel | Version | Status | Downloads (Est) | Strategy |
| :--- | :--- | :--- | :--- | :--- |
| **NB Plugin Portal (V1)** | 30.0.1 | Stable | 4,414 | Brand Awareness / Discovery |
| **NB Plugin Portal (V2)** | 1.0.0 | Stable | 859 | The Singularity / JASI Container |
| **Maven Central** | 30.0.1 | Stable | Unknown | Developer SDK Adoption |
| **GitHub Releases** | 1.1.0-SN | Beta | Unknown | Direct Binary Distribution |

 > [!TIP]
 > **One-Shot Portal Scraper & Velocity Guide:**
 > Run this snippet via `RunningJVM.compileAndExecuteJava` (or Anahata's JIT Compiler) to fetch both V1 and V2 counts in a single pass:
 > ```java
 > import org.jsoup.Jsoup;
 > import org.jsoup.nodes.Document;
 > import org.jsoup.nodes.Element;
 > import java.util.concurrent.Callable;
 > import java.util.regex.Matcher;
 > import java.util.regex.Pattern;
 > 
 > public class Anahata implements Callable<String> {
 >     @Override
 >     public String call() throws Exception {
 >         StringBuilder sb = new StringBuilder();
 >         sb.append(scrape("V1", "125")).append("\n");
 >         sb.append(scrape("V2", "135"));
 >         return sb.toString();
 >     }
 > 
 >     private String scrape(String version, String id) { 
 >         try { 
 >             String url = "https://plugins.netbeans.apache.org/catalogue/?id=" + id; 
 >             Document doc = Jsoup.connect(url).get(); 
 >             Element downloadIcon = doc.selectFirst("i.fa-download"); 
 >             if (downloadIcon != null) { 
 >                 String text = downloadIcon.parent().text().trim(); 
 >                 Matcher m = Pattern.compile("([\\d,]+)$").matcher(text); 
 >                 if (m.find()) return version + " Portal Downloads: " + m.group(1); 
 >                 return version + " Found icon but could not parse count from: " + text; 
 >             } 
 >             return version + " Could not find download count on the page."; 
 >         } catch (Exception e) { 
 >             return "Error fetching " + version + ": " + e.getMessage(); \n" +
"        } \n" +
"    } \n" +
"} \n" +
"```\n" +
" \n" +
"### 📊 How to Calculate & Update the Downloads-Per-Hour Velocity:\n" +
"1. **Capture the current count** ($C_{curr}$) and system time ($T_{curr}$).\n" +
"2. **Retrieve the baseline count** ($C_{base}$) and timestamp ($T_{base}$) from the latest log entry in `ledger.md`.\n" +
"3. **Calculate elapsed hours** ($H$):\n" +
"   $$H = \\frac{T_{curr} - T_{base}}{\\text{3600 seconds}}$$\n" +
"4. **Calculate Velocity** ($V$):\n" +
"   $$V = \\frac{C_{curr} - C_{base}}{H} \\text{ downloads per hour}$$\n" +
"5. **Update `ledger.md`**: Record the new counts in `## 📈 Distribution Funnel` and append a fresh log row in `## 🛠️ Milestone Log`.

## 📊 Scarf Analytics (Deep Strike Intelligence)
> [!IMPORTANT]
> **Scarf API Token:** `GPf7FDFry9n4l3JHUNnrXaU9cWhLVV8KOGtF5YHKJOmDZXTPwi`

### V1 Ecosystem
- **Plugin Package ID:** `f315fd98-2ea2-42de-ad8e-f3702396d3ac`
- **SDK Package ID:** `ac594bd8-fd22-49b3-9626-88a888502a3c`
- **Status:** Sonatype integration enabled. Scarf is receiving logs from Maven Central.

### V2 Ecosystem (Singularity)
- **Parent Package ID:** `[Pending]`
- **Status:** Snapshots enabled. Releases will be tracked via Scarf templates.

## 🛠️ Milestone Log
| Date | Milestone | Token ROI | Impact |
| :--- | :--- | :--- | :--- |
| 2026-08-04 09:03 | Portal Scrape: V1=4,414, V2=859 | High | V1: +13 (0.57 DLs/hr), V2: +8 (0.35 DLs/hr) in 22.7 hours (Combined: 0.93 DLs/hr) |
| 2026-08-03 10:21 | Portal Scrape: V1=4,401, V2=851 | High | V1: +5 (0.41 DLs/hr), V2: +3 (0.25 DLs/hr) in 12.2 hours (Combined: 0.66 DLs/hr) |
| 2026-08-02 22:10 | Portal Scrape: V1=4,396, V2=848 | High | V1: +10 (0.40 DLs/hr), V2: +12 (0.47 DLs/hr) in 25.3 hours (Combined: 0.87 DLs/hr) |
| 2026-08-01 20:52 | Portal Scrape: V1=4,386, V2=836 | High | V1: +44 (0.73 DLs/hr), V2: +31 (0.52 DLs/hr) in 60.1 hours (Combined: 1.25 DLs/hr) |
| 2026-07-30 08:44 | Portal Scrape: V1=4,342, V2=805 | High | V1: +4 (0.40 DLs/hr), V2: +9 (0.90 DLs/hr) in 10.0 hours (Combined: 1.31 DLs/hr) |
| 2026-07-29 22:47 | Portal Scrape: V1=4,338, V2=796 | High | V1: +10 (0.72 DLs/hr), V2: +9 (0.65 DLs/hr) in 13.9 hours (Combined: 1.36 DLs/hr) |
| 2026-07-29 08:52 | Portal Scrape: V1=4,328, V2=787 | High | V1: +10 (0.41 DLs/hr), V2: +10 (0.41 DLs/hr) in 24.2 hours (Combined: 0.83 DLs/hr) |
| 2026-07-28 08:41 | Portal Scrape: V1=4,318, V2=777 | High | V1: +23 (2.25 DLs/hr), V2: +5 (0.49 DLs/hr) in 10.2 hours (Combined: 2.73 DLs/hr!) |
| 2026-07-27 22:27 | Portal Scrape: V1=4,295, V2=772 | High | V1: +0 (0.00 DLs/hr), V2: +0 (0.00 DLs/hr) in 0.1 hours (Combined: 0.00 DLs/hr!) |
| 2026-07-27 22:23 | Portal Scrape: V1=4,295, V2=772 | High | V1: +7 (0.60 DLs/hr), V2: +8 (0.69 DLs/hr) in 11.7 hours (Combined: 1.29 DLs/hr!) |
| 2026-07-27 10:44 | Portal Scrape: V1=4,288, V2=764 | High | V1: +8 (0.35 DLs/hr), V2: +9 (0.39 DLs/hr) in 23.0 hours (Combined: 0.74 DLs/hr!) |
| 2026-07-26 11:42 | Portal Scrape: V1=4,280, V2=755 | High | V1: +11 (0.25 DLs/hr), V2: +11 (0.25 DLs/hr) in 43.5 hours (Combined: 0.51 DLs/hr!) |
| 2026-07-24 16:15 | Portal Scrape: V1=4,269, V2=744 | High | V1: +28 (1.13 DLs/hr), V2: +19 (0.77 DLs/hr) in 24.8 hours (Combined: 1.90 DLs/hr!) |
| 2026-07-23 15:30 | Portal Scrape: V1=4,241, V2=725 | High | V1: +15 (0.66 DLs/hr), V2: +9 (0.40 DLs/hr) in 22.7 hours (Combined: 1.06 DLs/hr!) |
| 2026-07-22 16:51 | Portal Scrape: V1=4,226, V2=716 | High | V1: +19 (0.70 DLs/hr), V2: +22 (0.81 DLs/hr) in 27.2 hours (Combined: 1.51 DLs/hr!) |
| 2026-07-21 13:38 | Portal Scrape: V1=4,207, V2=694 | High | V1: +63 (0.67 DLs/hr), V2: +52 (0.55 DLs/hr) in 94.4 hours (Combined: 1.22 DLs/hr!) |
| 2026-07-17 15:16 | Portal Scrape: V1=4,144, V2=642 | High | V1: +13 (0.62 DLs/hr), V2: +8 (0.38 DLs/hr) in 21.1 hours (Combined: 1.00 DLs/hr!) |
| 2026-07-16 18:12 | Portal Scrape: V1=4,131, V2=634 | High | V1: +62 (0.69 DLs/hr), V2: +58 (0.64 DLs/hr) in 90.3 hours (Combined: 1.33 DLs/hr!) |
| 2026-07-12 23:57 | Portal Scrape: V1=4,069, V2=576 | High | V1: +2 (0.26 DLs/hr), V2: +3 (0.40 DLs/hr) in 7.6 hours (Combined: 0.66 DLs/hr!) |
| 2026-07-12 16:24 | Portal Scrape: V1=4,067, V2=573 | High | V1: +16 (0.52 DLs/hr), V2: +15 (0.49 DLs/hr) in 30.6 hours (Combined: 1.01 DLs/hr!) |
| 2026-07-11 09:51 | Portal Scrape: V1=4,051, V2=558 | High | V1: +124 (0.77 DLs/hr), V2: +84 (0.52 DLs/hr) in 161.4 hours (Combined: 1.29 DLs/hr!) |
| 2026-07-04 16:26 | Portal Scrape: V1=3,927, V2=474 | High | V1: +28 (0.59 DLs/hr), V2: +22 (0.46 DLs/hr) in 47.6 hours (Combined: 1.05 DLs/hr!) |
| 2026-07-02 16:49 | Portal Scrape: V1=3,899, V2=452 | High | V1: +11 (0.55 DLs/hr), V2: +13 (0.65 DLs/hr) in 20.1 hours (Combined: 1.20 DLs/hr!) |
| 2026-07-01 20:44 | Portal Scrape: V1=3,888, V2=439 | High | V1: +28 (0.95 DLs/hr), V2: +28 (0.95 DLs/hr) in 29.5 hours (Combined: 1.90 DLs/hr!) |
| 2026-06-30 15:17 | Portal Scrape: V1=3,860, V2=411 | High | V1: +35 (0.72 DLs/hr), V2: +51 (1.05 DLs/hr) in 48.8 hours (Combined: 1.76 DLs/hr!) |
| 2026-06-28 14:32 | Portal Scrape: V1=3,825, V2=360 | High | V1: +69 (2.44 DLs/hr), V2: +14 (0.49 DLs/hr) in 28.3 hours (Combined: 2.93 DLs/hr!) |
| 2026-06-27 10:14 | Portal Scrape: V1=3,756, V2=346 | High | V1: +42 (3.77 DLs/hr), V2: +10 (0.90 DLs/hr) in 11.1 hours (Combined: 4.67 DLs/hr!) |
| 2026-06-26 23:06 | Portal Scrape: V1=3,714, V2=336 | High | V1: +23 (0.67 DLs/hr), V2: +23 (0.67 DLs/hr) in 34.3 hours (Combined: 1.34 DLs/hr!) |
| 2026-06-25 12:51 | Portal Scrape: V1=3,691, V2=313 | High | V1: +20 (0.73 DLs/hr), V2: +16 (0.59 DLs/hr) in 27.2 hours (Combined: 1.32 DLs/hr!) |
| 2026-06-24 09:37 | Portal Scrape: V1=3,671, V2=297 | High | V1: +13 (0.95 DLs/hr), V2: +9 (0.66 DLs/hr) in 13.7 hours (Combined: 1.61 DLs/hr!) |
| 2026-06-23 19:57 | Portal Scrape: V1=3,658, V2=288 | High | V1: +32 (1.19 DLs/hr), V2: +32 (1.19 DLs/hr) in 27.0 hours (Combined: 2.37 DLs/hr!) |
| 2026-06-22 16:58 | Portal Scrape: V1=3,626, V2=256 | High | V1: +3 (0.54 DLs/hr), V2: +15 (2.68 DLs/hr) in 5.6 hours (Combined: 3.21 DLs/hr!) |
| 2026-06-22 11:22 | Portal Scrape: V1=3,623, V2=241 | High | V1: +23 (0.52 DLs/hr), V2: +28 (0.63 DLs/hr) in 44.5 hours (Combined: 1.15 DLs/hr!) |
| 2026-06-20 14:52 | Portal Scrape: V1=3,600, V2=213 | High | V1: +111 (0.98 DLs/hr), V2: +71 (0.62 DLs/hr) in 113.8 hours (Combined: 1.60 DLs/hr!) |
| 2026-06-15 21:05 | Portal Scrape: V1=3,489, V2=142 | High | V1: +10 (0.99 DLs/hr), V2: +9 (0.89 DLs/hr) in 10.1 hours (Combined: 1.87 DLs/hr!) |
| 2026-06-15 10:56 | Portal Scrape: V1=3,479, V2=133 | High | V1: +30 (0.63 DLs/hr), V2: +28 (0.58 DLs/hr) in 48 hours |
| 2026-06-13 | Portal Scrape: V1=3,449, V2=105 | High | Daily ledger tracking initiated |
| 2026-02-07 | Portal Scrape: 603 DLs | High | Verified growth on Plugin Portal |
| 2026-02-06 | **Stable Release: v28.1.0** | **MAX** | First stable version of the ecosystem. |
| 2026-02-06 | CI/CD Optimization | High | Resolved duplicate deployment IDs. |
| 2026-02-06 | Scarf Integration | High | Enabled organization-level download tracking. |
| 2026-02-06 | Portal Scrape: 588 DLs | High | Verified growth on Plugin Portal |
| 2026-02-05 | V1 Release: 28.0.18 | High | UI/UX Stability & Theme Overhaul |

## ⏳ Pending Actions
- [x] **Enable Scarf in Sonatype:** Done.
- [ ] **Claim V2 Packages in Scarf:** Add `uno.anahata:anahata-asi-parent` to Scarf.
- [ ] **Automated Stats:** Implement a script to pull Scarf data into this ledger.

## 🛡️ Release Coordination Protocol
1. **Library First:** Release `gemini-java-client`.
2. **Wait for Central:** Wait 5-10 minutes for the artifact to appear in Maven Central.
3. **Verify:** Use `searchMavenIndex` to confirm availability.
4. **Plugin Second:** Trigger the `anahata-netbeans-ai` release.
5. **V2 Sync:** Ensure V2 snapshots are rolling out to Central.
