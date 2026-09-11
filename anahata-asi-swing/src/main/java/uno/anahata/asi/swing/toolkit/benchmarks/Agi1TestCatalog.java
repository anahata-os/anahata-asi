/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.toolkit.benchmarks;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import lombok.SneakyThrows;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.swing.toolkit.Screens;
import uno.anahata.asi.toolkit.java.Java;

/**
 * The official Anahata-AGI-1 benchmark suite catalog.
 * <p>
 * Standardizes prompts, isolated tool environments, and permissions for the
 * official pure-Java certification benchmark.
 * </p>
 *
 * @author anahata
 */
public class Agi1TestCatalog extends TestCatalog {

    /**
     * Standard header template for Anahata-AGI-1 tests.
     */
    public static final String STANDARD_HEADER =
            "You are participating in the official Anahata-AGI-1 Benchmark (%s: %s).\n"
            + "Your task must be executed autonomously with zero defects.";

    /**
     * Standard footer template for Anahata-AGI-1 tests.
     */
    public static final String STANDARD_FOOTER =
            "Note: This is an official Anahata-AGI-1 benchmark challenge being recorded live on Screen $target.screen$ for community review and crowd voting on YouTube:\n"
            + "- Position and launch your application window on Screen $target.screen$ within the recorded display bounds.\n"
            + "- Aim to complete and launch your implementation in your very first turn.\n"
            + "- Only take additional turns if:\n"
            + "  a) you encounter a compilation or runtime error that requires self-alignment and correction.\n"
            + "  b) you cannot fit the entire deliverable within your $effective.user.max.out.tokens$ max output tokens in a single output.\n"
            + "  c) you need an extra turn for verification,\n"
            + "Once your task is running with zero defects, provide a concise final summary and conclude immediately.\n\n"
            + "Some of the general benchmarked criterias are \n "
            + " a) total tokens taken (both input and output)\n"
            + " b) total turns taken\n"
            + " c) youtube likes\n"
            + " d) subjective score by human judges (anahata community members)\n";

    /**
     * Test #1: OS Hardware &amp; System Values Dashboard (JNA Native C-Library Binding).
     */
    public static final TestDefinition JAVA_JNA_1 = TestDefinition.builder()
            .testCode("JAVA-JNA-1")
            .title("OS Hardware & System Values Dashboard")
            .rawPrompt("Build a real-time, interactive native system telemetry dashboard using JNA (com.sun.jna.Library) to monitor live host system and hardware values.\n\n"
                    + "Your implementation will be evaluated on:\n"
                    + "1. Native JNA Depth: Richness of native C-library integration (e.g. binding native functions or C structs for sysinfo, memory usage via getrusage, load averages, kernel/OS specs, or available hardware sensors).\n"
                    + "2. Real-Time Interactivity & UI Thread Safety: Continuous live updates (e.g. 1-second refresh), strictly respecting the threading model of your chosen UI framework (e.g. Swing EDT via runInEdtAndWait/invokeLater or JavaFX Application Thread via Platform.runLater).\n"
                    + "3. Dashboard Polish & Presentation: Clean layout, metric cards, visual indicators, or live charts.\n"
                    + "4. Zero-Defect Resilience: Robust handling of missing hardware sensors or platform variations with zero crashes.\n\n"
                    + "Window title MUST contain your Model ID. You have complete creative freedom to choose your UI framework (Swing, JavaFX, etc.), styling, and interface design.")
            .toolkits(List.of(
                    ToolkitSettings.of(Java.class, "compileAndExecute", ToolPermission.APPROVE_ALWAYS)
            ))
            .build();

    /**
     * Test #2: Retro Arcade Game Execution (Swing EDT Loop &amp; Physics).
     */
    public static final TestDefinition JAVA_ARKANOID_1 = TestDefinition.builder()
            .testCode("JAVA-ARKANOID-1")
            .title("Retro Arcade Game Execution")
            .rawPrompt("Build a fully playable, retro Arkanoid brick-breaker game in Swing with smooth 60 FPS animation loop. Window title MUST contain your Model ID.")
            .toolkits(List.of(
                    ToolkitSettings.of(Java.class, "compileAndExecute", ToolPermission.APPROVE_ALWAYS)
            ))
            .build();

    /**
     * Test #3: Classic Snake Game.
     */
    public static final TestDefinition JAVA_SNAKE_GAME_1 = TestDefinition.builder()
            .testCode("JAVA-SNAKE-GAME-1")
            .title("Snake Game")
            .rawPrompt("Make a snake game using the java tool. Window title MUST contain your Model ID.")
            .toolkits(List.of(
                    ToolkitSettings.of(Java.class, "compileAndExecute", ToolPermission.APPROVE_ALWAYS)
                    
            ))
            .build();
    
    /**
     * Test #4: Nou Camp Nou 3D Stadium Model.
     */
    public static final TestDefinition JAVA_3D_NOU_CAMP_NOU_1 = TestDefinition.builder()
            .testCode("JAVA-NOU-CAMP-NOU-1")
            .title("Nou Camp Nou 3D Model")
            .rawPrompt("Make a 3D model of what the Nou Camp Nou will look like when completed")
            .toolkits(List.of(
                    ToolkitSettings.of(Java.class, "compileAndExecute", ToolPermission.APPROVE_ALWAYS)
            ))
            .build();

    /**
     * Test #5: 3D Planetary Satellite Tracker & Air Defense Command Center.
     */
    public static final TestDefinition JAVA_ORBITAL_C4ISR_1 = TestDefinition.builder()
            .testCode("JAVA-ORBITAL-C4ISR-1")
            .title("3D Planetary Satellite Tracker & Air Defense Command Center")
            .rawPrompt("Build a high-performance, real-time 3D Planetary Satellite Tracker & Air Defense Flight Radar Command Center in Java (resembling NASA WorldWind / Palantir C4ISR).\n\n"
                    + "Your implementation will be evaluated on:\n"
                    + "1. 3D Globe & Surface Texture Mapping: Hardware-accelerated 3D spherical Earth globe with mouse drag rotation, pitch tilt, and mouse-wheel zoom. "
                    + "Render photorealistic satellite imagery or dark tactical terrain (e.g. using a global spherical texture or slippy map tile grid). "
                    + "The following endpoints have been tested and verified to work without API keys: "
                    + "Global NASA Blue Marble texture: https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/textures/planets/earth_atmos_2048.jpg ; "
                    + "Night city lights: https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/textures/planets/earth_lights_2048.png ; "
                    + "ArcGIS World Satellite tiles: https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg ; "
                    + "CartoDB Dark Matter tiles: https://basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png . "
                    + "You are completely free to use these verified links or source your own data/imagery.\n"
                    + "2. 3D Elevated Airspace & Defense Domes: Render aircraft physically elevated in 3D above the planetary surface proportional to flight level. "
                    + "Render semi-transparent glowing 3D SAM engagement domes over strategic defense airbases (Ramstein, Torrejón, Sigonella, Nevatim) and tactical interceptor vectors.\n"
                    + "3. Satellite Mechanics & Telemetry: Live or simulated satellite orbital propagation (e.g. ISS live coordinates via http://api.open-notify.org/iss-now.json, Starlink shell, GPS/Galileo ring) with glowing orbital paths and sub-satellite ground footprints.\n"
                    + "4. Tactical C4ISR HUD: Dark military cyberpunk HUD overlay (FlatLaf Dark), target lock telemetry card (callsign, altitude, speed, lat/lon), Zulu UTC clock, and simulation controls.\n"
                    + "5. Performance & Thread Safety: Background threads for network/telemetry updates and smooth 60 FPS animation loop with zero UI freezing.\n\n"
                    + "Window title MUST contain your Model ID. You have complete creative freedom over visual styling, architecture, and technology choice.")
            .toolkits(List.of(
                    ToolkitSettings.of(Java.class, "compile", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Java.class, "compileAndExecute", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Java.class, "getAgiClassSources", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Java.class, "removeAgiClasses", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Java.class, "clearAllAgiClasses", ToolPermission.APPROVE_ALWAYS)
            ))
            .build();

    /**
     * Test #6: Interactive 3D Multi-Layer Satellite Earth Globe (Google Earth / NASA WorldWind).
     */
    public static final TestDefinition JAVA_EARTH_GLOBE_1 = TestDefinition.builder()
            .testCode("JAVA-EARTH-GLOBE-1")
            .title("Interactive 3D Multi-Layer Satellite Earth Globe")
            .rawPrompt("Build a high-performance, interactive 3D Satellite Earth Globe Viewer in Java (resembling Google Earth / NASA WorldWind).\n\n"
                    + "Your implementation will be evaluated on:\n\n"
                    + "1. Interactive 3D Sphere & Natural Camera Controls:\n"
                    + "   - Hardware-accelerated 3D spherical Earth.\n"
                    + "   - Natural Dragging: Mouse drag must grab the planetary surface naturally (dragging right pulls the surface right, dragging left pulls the surface left, dragging up tilts North, dragging down tilts South).\n"
                    + "   - Global Interaction: Ensure mouse drag and scroll interactions seamlessly rotate and zoom the globe across the entire window area.\n"
                    + "   - Continuous Smooth Zoom & Altitude Range: Continuous logarithmic zoom from global orbit down to street/ground level (~200 meters altitude at zoom 19), with altitude clamping to keep the camera gracefully outside the globe surface.\n"
                    + "   - Fly-To Presets: Preset locations with smooth camera animation (e.g. Barcelona Camp Nou, Giza Pyramids, Messi's home town, Angkor Wat, Mount Kailash, CIA headquarters, and anything else entertaining).\n\n"
                    + "2. Live Multi-Layer Slippy Map Imagery (No API Keys Required):\n"
                    + "   - Provide a top toolbar or combo box to switch live between 2 free, high-resolution slippy map tile pyramids:\n"
                    + "     * Esri ArcGIS World Satellite: https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg\n"
                    + "     * OpenStreetMap (OSM): https://tile.openstreetmap.org/{z}/{x}/{y}.png\n"
                    + "   - Dynamic Level of Detail (LOD): Both tile providers support continuous zoom levels from global orbit (z=1) down to street/building detail (up to zoom 19), returning valid HTTP 200 imagery across land and oceans.\n\n"
                    + "3. High-Performance Concurrency, Frustum Culling & Cache Sizing:\n"
                    + "   - Thread Pool Sizing: Use a background daemon worker thread pool sized to Runtime.getRuntime().availableProcessors() (one thread per available CPU core).\n"
                    + "   - LIFO (Last-In, First-Out) Priority: Prioritize newly requested tiles currently visible in the camera frustum over older, stale off-screen requests.\n"
                    + "   - Frustum Tile Budget: Cull off-screen and back-facing tiles to keep active visible geometry tightly bounded to the immediate camera viewpoint.\n"
                    + "   - Memory Cache Sizing: Ensure your in-memory RAM cache capacity comfortably exceeds this visible tile demand to prevent LRU cache eviction thrashing.\n"
                    + "   - Visual Continuity: Maintain low-resolution base or parent tiles visible on the globe until higher-detail child tiles have finished downloading and decoding, ensuring the planetary surface never disappears or displays empty voids while streaming.\n\n"
                    + "4. Persistent Local Disk Caching:\n"
                    + "   - Check if the tile file already exists on disk before initiating any HTTP request. If present, load it immediately from disk; otherwise download, persist to disk, and decode.\n"
                    + "   - Cache all downloaded tiles locally under ${user.home}/.anahata/asi/cache/<host>/<path> (e.g. ${user.home}/.anahata/asi/cache/server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg and ${user.home}/.anahata/asi/cache/tile.openstreetmap.org/{z}/{x}/{y}.png).\n\n"
                    + "5. Atmospheric Rendering & Surface Clarity:\n"
                    + "   - If rendering an atmospheric haze or rim glow, ensure terrain and ground-level map imagery remain bright, clear, and visible from ground level up to orbit without obscuring the surface.\n\n"
                    + "6. HUD, Telemetry & UI Styling:\n"
                    + "   - Top Toolbar Layout: Anchor the controls neatly to the top edge of the window so they do not obstruct or float across the 3D globe viewport.\n"
                    + "   - Telemetry Overlay: Sleek overlay showing live Camera Lat / Lon, Altitude, active Zoom LOD level, memory/disk cache stats, FPS, basic JVM runtime stats, and a 'Reset View' button.\n"
                    + "   - Apply styling locally to your window and components, preserving host environment defaults (don't alter the host system's look and feel).\n\n"
                    + "7. Execution & Self-Verification:\n"
                    + "   - Smooth 60 FPS animation loop with strict UI thread safety (tile I/O on worker threads).\n"
                    + "   - Window title MUST contain your Model ID.\n"
                    + "   - Once launched, allow a brief delay (e.g. 2-3 seconds) for the window to open, render its initial 3D scene, and load base tiles, then take a screenshot of using the Screens toolkit to visually verify that your application has opened and rendered properly.\n\n"
                    + "\"**Environment Note**: All libraries available on the default classpath are pre-configured and manage any native library extraction and loading.\n" 
                    + "")
            .toolkits(List.of(
                    ToolkitSettings.of(Java.class, "compile", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Java.class, "compileAndExecute", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Java.class, "getAgiClassSources", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Java.class, "removeAgiClasses", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Java.class, "clearAllAgiClasses", ToolPermission.APPROVE_ALWAYS),
                    ToolkitSettings.of(Screens.class, "takeScreenshot", ToolPermission.APPROVE_ALWAYS)
            ))
            .build();

    /**
     * Resolves the official results directory in the website source tree or fallback.
     *
     * @return The path to the Anahata-AGI-1 results directory.
     */
    @SneakyThrows
    public static Path resolveOfficialResultsDirectory() {
        Path devWebPath = Paths.get(System.getProperty("user.home"), "NetBeansProjects", "anahata-asi-parent",
                "anahata-asi-web", "src", "main", "resources", "web", "benchmarks", "anahata-agi-1");
        if (Files.exists(devWebPath)) {
            return devWebPath;
        }

        return AbstractAsiContainer.getWorkDirSubDir("benchmarks").resolve("anahata-agi-1");
    }

    /**
     * Constructs the official Anahata-AGI-1 test catalog with the default web results directory.
     */
    public Agi1TestCatalog() {
        this(resolveOfficialResultsDirectory());
    }

    /**
     * Constructs the official Anahata-AGI-1 test catalog with a custom results directory.
     *
     * @param resultsDirectory The filesystem directory where JSON results files are persisted.
     */
    public Agi1TestCatalog(Path resultsDirectory) {
        super(
                "ANAHATA-AGI-1",
                "Anahata-AGI-1",
                "Flagship pure-Java performance, resilience, and multi-modal autonomy suite.",
                STANDARD_HEADER,
                STANDARD_FOOTER,
                resultsDirectory
        );
        addTest(JAVA_JNA_1);
        addTest(JAVA_ARKANOID_1);
        addTest(JAVA_SNAKE_GAME_1);
        addTest(JAVA_3D_NOU_CAMP_NOU_1);
        addTest(JAVA_ORBITAL_C4ISR_1);
        addTest(JAVA_EARTH_GLOBE_1);
    }
}
