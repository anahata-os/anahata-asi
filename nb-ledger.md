# Anahata High-ROI Ledger: Operation Deep Strike

## 📈 Distribution Funnel
| Channel | Version | Status | Downloads (Est) | Strategy |
| :--- | :--- | :--- | :--- | :--- |
| **NB Plugin Portal (V1)** | 30.0.1 | Stable | 4,968 | Brand Awareness / Discovery (id=125) |
| **NB Plugin Portal (V2)** | 1.0.0 | Stable | 1,162 | The Singularity / ASI Container (id=135) |
| **NB Plugin Portal (Update Center)** | 1.0.0 | Active | 233 | Autonomous Update Channel (id=141) |

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
 >         sb.append(scrape("V1", "125")).append("\n"); //v1
 >         sb.append(scrape("V2", "135")).append("\n"); //v2
 >         sb.append(scrape("UC", "141")); //update center
 >         return sb.toString();
 >     }
 > 
 >     private String scrape(String version, String id) { 
 >         try { 
 >             String url = "https://plugins.netbeans.apache.org/catalogue/?id=" + id; 
 >             Document doc = Jsoup.connect(url).get(); 
 >             Element downloadIcon = doc.selectFirst("p i.fa-download, i.fa-download"); 
 >             if (downloadIcon != null) { 
 >                 if (downloadIcon.nextSibling() != null) { 
 >                     String val = downloadIcon.nextSibling().toString().replaceAll("[^\\d,]", "").trim(); 
 >                     if (!val.isEmpty()) return version + " Portal Downloads: " + val; 
 >                 } 
 >                 String text = downloadIcon.parent().text().trim(); 
 >                 Matcher m = Pattern.compile("([\\d,]+)\\s*$").matcher(text); 
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
"5. **Update `ledger.md`**: Record the new counts in `## 📈 Distribution Funnel` and append a fresh log row in `## 🛠️ Milestone Log
| Date | V1 (125) | V2 (135) | UC (141) | Total | Δ Total | Velocity (DL/h) | Notes |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| 2026-10-01 16:52 | 4,968 | 1,162 | 233 | 6,363 | +9 | 1.35 | V1: +1 (0.15 DLs/hr), V2: +2 (0.30 DLs/hr), UC: +6 (0.90 DLs/hr) in 6.7 hours (Total Velocity: 1.35 DLs/hr) |
| 2026-10-01 10:13 | 4,967 | 1,160 | 227 | 6,354 | +13 | 0.66 | V1: +1 (0.05 DLs/hr), V2: +3 (0.15 DLs/hr), UC: +9 (0.46 DLs/hr) in 19.8 hours (Total Velocity: 0.66 DLs/hr) |
| 2026-09-30 14:27 | 4,966 | 1,157 | 218 | 6,341 | +31 | 1.04 | V1: +2 (0.07 DLs/hr), V2: +0 (0.00 DLs/hr), UC: +29 (0.97 DLs/hr) in 29.8 hours (Total Velocity: 1.04 DLs/hr) |
| 2026-09-29 08:38 | 4,964 | 1,157 | 189 | 6,310 | +42 | 1.73 | V1: +26 (1.07 DLs/hr), V2: +5 (0.21 DLs/hr), UC: +11 (0.45 DLs/hr) in 24.2 hours (Total Velocity: 1.73 DLs/hr) |
| 2026-09-28 08:25 | 4,938 | 1,152 | 178 | 6,268 | +22 | 0.95 | V1: +3 (0.13 DLs/hr), V2: +8 (0.35 DLs/hr), UC: +11 (0.47 DLs/hr) in 23.2 hours (Total Velocity: 0.95 DLs/hr) |
| 2026-09-27 09:15 | 4,935 | 1,144 | 167 | 6,246 | +16 | 1.05 | V1: +0 (0.00 DLs/hr), V2: +0 (0.00 DLs/hr), UC: +16 (1.05 DLs/hr) in 15.2 hours (Total Velocity: 1.05 DLs/hr) |
| 2026-09-26 18:03 | 4,935 | 1,144 | 151 | 6,230 | +152 | 2.77 | V1: +38 (0.69 DLs/hr), V2: +7 (0.13 DLs/hr), UC: +107 (1.95 DLs/hr) in 55.0 hours (Total Velocity: 2.77 DLs/hr) |
| 2026-09-24 11:06 | 4,897 | 1,137 | 44 | 6,078 | +393 |  | V1: +237 (0.31 DLs/hr), V2: +112 (0.15 DLs/hr), UC: 44 DLs (id=141) in 753.7 hours (Total Velocity: 0.52 DLs/hr) |
| 2026-08-24 01:23 | 4,660 | 1,025 | 0 | 5,685 | +14 | 0.26 | V1: +10 (0.19 DLs/hr), V2: +4 (0.07 DLs/hr) in 53.8 hours (Combined: 0.26 DLs/hr) |
| 2026-08-21 19:37 | 4,650 | 1,021 | 0 | 5,671 | +17 | 0.52 | V1: +9 (0.28 DLs/hr), V2: +8 (0.25 DLs/hr) in 32.6 hours (Combined: 0.52 DLs/hr) |
| 2026-08-20 11:01 | 4,641 | 1,013 | 0 | 5,654 | +16 | 0.98 | V1: +7 (0.43 DLs/hr), V2: +9 (0.55 DLs/hr) in 16.3 hours (Combined: 0.98 DLs/hr) |
| 2026-08-19 18:43 | 4,634 | 1,004 | 0 | 5,638 | +194 | 1.10 | V1: +145 (0.83 DLs/hr), V2: +49 (0.28 DLs/hr) in 175.7 hours (Combined: 1.10 DLs/hr) - V2 crossed 1,000! |
| 2026-08-12 11:01 | 4,489 | 955 | 0 | 5,444 | +112 | 1.18 | V1: +48 (0.51 DLs/hr), V2: +64 (0.68 DLs/hr) in 94.5 hours (Combined: 1.18 DLs/hr) |
| 2026-08-08 12:30 | 4,441 | 891 | 0 | 5,332 | +9 | 0.58 | V1: +3 (0.19 DLs/hr), V2: +6 (0.39 DLs/hr) in 15.5 hours (Combined: 0.58 DLs/hr) |
| 2026-08-07 20:59 | 4,438 | 885 | 0 | 5,323 | +24 | 0.68 | V1: +9 (0.25 DLs/hr), V2: +15 (0.42 DLs/hr) in 35.4 hours (Combined: 0.68 DLs/hr) |
| 2026-08-06 09:38 | 4,429 | 870 | 0 | 5,299 | +8 | 0.33 | V1: +7 (0.29 DLs/hr), V2: +1 (0.04 DLs/hr) in 23.9 hours (Combined: 0.33 DLs/hr) |
| 2026-08-05 09:43 | 4,422 | 869 | 0 | 5,291 | +2 | 0.17 | V1: +1 (0.09 DLs/hr), V2: +1 (0.09 DLs/hr) in 11.7 hours (Combined: 0.17 DLs/hr) |
| 2026-08-04 22:01 | 4,421 | 868 | 0 | 5,289 | +16 | 1.23 | V1: +7 (0.54 DLs/hr), V2: +9 (0.69 DLs/hr) in 13.0 hours (Combined: 1.23 DLs/hr) |
| 2026-08-04 09:03 | 4,414 | 859 | 0 | 5,273 | +21 | 0.93 | V1: +13 (0.57 DLs/hr), V2: +8 (0.35 DLs/hr) in 22.7 hours (Combined: 0.93 DLs/hr) |
| 2026-08-03 10:21 | 4,401 | 851 | 0 | 5,252 | +8 | 0.66 | V1: +5 (0.41 DLs/hr), V2: +3 (0.25 DLs/hr) in 12.2 hours (Combined: 0.66 DLs/hr) |
| 2026-08-02 22:10 | 4,396 | 848 | 0 | 5,244 | +22 | 0.87 | V1: +10 (0.40 DLs/hr), V2: +12 (0.47 DLs/hr) in 25.3 hours (Combined: 0.87 DLs/hr) |
| 2026-08-01 20:52 | 4,386 | 836 | 0 | 5,222 | +75 | 1.25 | V1: +44 (0.73 DLs/hr), V2: +31 (0.52 DLs/hr) in 60.1 hours (Combined: 1.25 DLs/hr) |
| 2026-07-30 08:44 | 4,342 | 805 | 0 | 5,147 | +13 | 1.31 | V1: +4 (0.40 DLs/hr), V2: +9 (0.90 DLs/hr) in 10.0 hours (Combined: 1.31 DLs/hr) |
| 2026-07-29 22:47 | 4,338 | 796 | 0 | 5,134 | +19 | 1.36 | V1: +10 (0.72 DLs/hr), V2: +9 (0.65 DLs/hr) in 13.9 hours (Combined: 1.36 DLs/hr) |
| 2026-07-29 08:52 | 4,328 | 787 | 0 | 5,115 | +20 | 0.83 | V1: +10 (0.41 DLs/hr), V2: +10 (0.41 DLs/hr) in 24.2 hours (Combined: 0.83 DLs/hr) |
| 2026-07-28 08:41 | 4,318 | 777 | 0 | 5,095 | +28 | 2.73 | V1: +23 (2.25 DLs/hr), V2: +5 (0.49 DLs/hr) in 10.2 hours (Combined: 2.73 DLs/hr!) |
| 2026-07-27 22:27 | 4,295 | 772 | 0 | 5,067 | +0 | 0.00 | V1: +0 (0.00 DLs/hr), V2: +0 (0.00 DLs/hr) in 0.1 hours (Combined: 0.00 DLs/hr!) |
| 2026-07-27 22:23 | 4,295 | 772 | 0 | 5,067 | +15 | 1.29 | V1: +7 (0.60 DLs/hr), V2: +8 (0.69 DLs/hr) in 11.7 hours (Combined: 1.29 DLs/hr!) |
| 2026-07-27 10:44 | 4,288 | 764 | 0 | 5,052 | +17 | 0.74 | V1: +8 (0.35 DLs/hr), V2: +9 (0.39 DLs/hr) in 23.0 hours (Combined: 0.74 DLs/hr!) |
| 2026-07-26 11:42 | 4,280 | 755 | 0 | 5,035 | +22 | 0.51 | V1: +11 (0.25 DLs/hr), V2: +11 (0.25 DLs/hr) in 43.5 hours (Combined: 0.51 DLs/hr!) |
| 2026-07-24 16:15 | 4,269 | 744 | 0 | 5,013 | +47 | 1.90 | V1: +28 (1.13 DLs/hr), V2: +19 (0.77 DLs/hr) in 24.8 hours (Combined: 1.90 DLs/hr!) |
| 2026-07-23 15:30 | 4,241 | 725 | 0 | 4,966 | +24 | 1.06 | V1: +15 (0.66 DLs/hr), V2: +9 (0.40 DLs/hr) in 22.7 hours (Combined: 1.06 DLs/hr!) |
| 2026-07-22 16:51 | 4,226 | 716 | 0 | 4,942 | +41 | 1.51 | V1: +19 (0.70 DLs/hr), V2: +22 (0.81 DLs/hr) in 27.2 hours (Combined: 1.51 DLs/hr!) |
| 2026-07-21 13:38 | 4,207 | 694 | 0 | 4,901 | +115 | 1.22 | V1: +63 (0.67 DLs/hr), V2: +52 (0.55 DLs/hr) in 94.4 hours (Combined: 1.22 DLs/hr!) |
| 2026-07-17 15:16 | 4,144 | 642 | 0 | 4,786 | +21 | 1.00 | V1: +13 (0.62 DLs/hr), V2: +8 (0.38 DLs/hr) in 21.1 hours (Combined: 1.00 DLs/hr!) |
| 2026-07-16 18:12 | 4,131 | 634 | 0 | 4,765 | +120 | 1.33 | V1: +62 (0.69 DLs/hr), V2: +58 (0.64 DLs/hr) in 90.3 hours (Combined: 1.33 DLs/hr!) |
| 2026-07-12 23:57 | 4,069 | 576 | 0 | 4,645 | +5 | 0.66 | V1: +2 (0.26 DLs/hr), V2: +3 (0.40 DLs/hr) in 7.6 hours (Combined: 0.66 DLs/hr!) |
| 2026-07-12 16:24 | 4,067 | 573 | 0 | 4,640 | +31 | 1.01 | V1: +16 (0.52 DLs/hr), V2: +15 (0.49 DLs/hr) in 30.6 hours (Combined: 1.01 DLs/hr!) |
| 2026-07-11 09:51 | 4,051 | 558 | 0 | 4,609 | +208 | 1.29 | V1: +124 (0.77 DLs/hr), V2: +84 (0.52 DLs/hr) in 161.4 hours (Combined: 1.29 DLs/hr!) |
| 2026-07-04 16:26 | 3,927 | 474 | 0 | 4,401 | +50 | 1.05 | V1: +28 (0.59 DLs/hr), V2: +22 (0.46 DLs/hr) in 47.6 hours (Combined: 1.05 DLs/hr!) |
| 2026-07-02 16:49 | 3,899 | 452 | 0 | 4,351 | +24 | 1.20 | V1: +11 (0.55 DLs/hr), V2: +13 (0.65 DLs/hr) in 20.1 hours (Combined: 1.20 DLs/hr!) |
| 2026-07-01 20:44 | 3,888 | 439 | 0 | 4,327 | +56 | 1.90 | V1: +28 (0.95 DLs/hr), V2: +28 (0.95 DLs/hr) in 29.5 hours (Combined: 1.90 DLs/hr!) |
| 2026-06-30 15:17 | 3,860 | 411 | 0 | 4,271 | +86 | 1.76 | V1: +35 (0.72 DLs/hr), V2: +51 (1.05 DLs/hr) in 48.8 hours (Combined: 1.76 DLs/hr!) |
| 2026-06-28 14:32 | 3,825 | 360 | 0 | 4,185 | +83 | 2.93 | V1: +69 (2.44 DLs/hr), V2: +14 (0.49 DLs/hr) in 28.3 hours (Combined: 2.93 DLs/hr!) |
| 2026-06-27 10:14 | 3,756 | 346 | 0 | 4,102 | +52 | 4.67 | V1: +42 (3.77 DLs/hr), V2: +10 (0.90 DLs/hr) in 11.1 hours (Combined: 4.67 DLs/hr!) |
| 2026-06-26 23:06 | 3,714 | 336 | 0 | 4,050 | +46 | 1.34 | V1: +23 (0.67 DLs/hr), V2: +23 (0.67 DLs/hr) in 34.3 hours (Combined: 1.34 DLs/hr!) |
| 2026-06-25 12:51 | 3,691 | 313 | 0 | 4,004 | +36 | 1.32 | V1: +20 (0.73 DLs/hr), V2: +16 (0.59 DLs/hr) in 27.2 hours (Combined: 1.32 DLs/hr!) |
| 2026-06-24 09:37 | 3,671 | 297 | 0 | 3,968 | +22 | 1.61 | V1: +13 (0.95 DLs/hr), V2: +9 (0.66 DLs/hr) in 13.7 hours (Combined: 1.61 DLs/hr!) |
| 2026-06-23 19:57 | 3,658 | 288 | 0 | 3,946 | +64 | 2.37 | V1: +32 (1.19 DLs/hr), V2: +32 (1.19 DLs/hr) in 27.0 hours (Combined: 2.37 DLs/hr!) |
| 2026-06-22 16:58 | 3,626 | 256 | 0 | 3,882 | +18 | 3.21 | V1: +3 (0.54 DLs/hr), V2: +15 (2.68 DLs/hr) in 5.6 hours (Combined: 3.21 DLs/hr!) |
| 2026-06-22 11:22 | 3,623 | 241 | 0 | 3,864 | +51 | 1.15 | V1: +23 (0.52 DLs/hr), V2: +28 (0.63 DLs/hr) in 44.5 hours (Combined: 1.15 DLs/hr!) |
| 2026-06-20 14:52 | 3,600 | 213 | 0 | 3,813 | +182 | 1.60 | V1: +111 (0.98 DLs/hr), V2: +71 (0.62 DLs/hr) in 113.8 hours (Combined: 1.60 DLs/hr!) |
| 2026-06-15 21:05 | 3,489 | 142 | 0 | 3,631 | +19 | 1.87 | V1: +10 (0.99 DLs/hr), V2: +9 (0.89 DLs/hr) in 10.1 hours (Combined: 1.87 DLs/hr!) |
| 2026-06-15 10:56 | 3,479 | 133 | 0 | 3,612 | +58 |  | V1: +30 (0.63 DLs/hr), V2: +28 (0.58 DLs/hr) in 48 hours |
| 2026-06-13 | 3,449 | 105 | 0 | 3,554 | - |  | Daily ledger tracking initiated |
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
