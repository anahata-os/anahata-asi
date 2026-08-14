/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.games.asikart;

import com.jme3.app.SimpleApplication;
import com.jme3.asset.AssetLoader;
import com.jme3.asset.plugins.FileLocator;
import com.jme3.effect.ParticleEmitter;
import com.jme3.effect.ParticleMesh;
import com.jme3.font.BitmapText;
import com.jme3.input.KeyInput;
import com.jme3.input.controls.ActionListener;
import com.jme3.input.controls.KeyTrigger;
import com.jme3.light.AmbientLight;
import com.jme3.light.DirectionalLight;
import com.jme3.light.SpotLight;
import com.jme3.material.Material;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.post.FilterPostProcessor;
import com.jme3.renderer.queue.RenderQueue;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.scene.shape.Box;
import com.jme3.scene.shape.Cylinder;
import com.jme3.scene.shape.Dome;
import com.jme3.scene.shape.Sphere;

import com.jme3.shadow.DirectionalLightShadowRenderer;
import com.jme3.system.AppSettings;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * AsiKart 3D v4.2 — Console-Grade Arcade Kart Racer.
 * <p>
 * Features a clean, modular multi-class architecture:
 * </p>
 * <ul>
 * <li>{@link TrackBuilder} — Multi-circuit 3D world generator (Rainbow Circuit,
 * Desert Speedway, Cyber Circuit).</li>
 * <li>{@link ItemManager} — Stationary 3D Item Mystery Boxes, 3D Upright Gold
 * Coins, Green/Red Shells, and Bananas.</li>
 * <li>{@link AudioManager} — Spatial audio and sound feedback chimes.</li>
 * </ul>
 * <p>
 * Includes 3-Tier Mini-Turbo Drift Sparks, Interactive Radar Minimap,
 * Directional Corner Arrow Warning Signs, real-time directional shadow maps,
 * PBR asphalt normal/bump surface textures, and 3D Sky Domes.
 * </p>
 *
 * @author anahata
 * @since 1.1.0-SNAPSHOT
 */
public class AsiKart extends SimpleApplication implements ActionListener, ItemManager.ItemPickupListener {

    // --- Singleton Window Lifecycle Tracker ---
    public static AsiKart activeInstance = null;

    // --- Modular Subsystems ---
    private TrackBuilder trackBuilder;
    private ItemManager itemManager;
    private AudioManager audioManager;
    private TrackBuilder.TrackResult currentTrack;

    // --- Player Kart State ---
    private Node playerKartNode;
    private Node playerChassisNode;
    private ParticleEmitter exhaustFlame;
    private ParticleEmitter driftSparks;

    private float heading = 0f;
    private float speed = 0f;
    private final float maxSpeed = 44f;
    private final float acceleration = 28f;
    private final float deceleration = 16f;
    private final float steeringSensitivity = 2.6f;

    private boolean isDrifting = false;
    private float driftAngle = 0f;
    private float driftTime = 0f;
    private float turboBoostTimer = 0f;
    private float starInvincibleTimer = 0f;

    // --- Controls ---
    private boolean accel = false, brake = false, steerLeft = false, steerRight = false;

    // --- AI Competitors & Waypoints ---
    private final List<Node> aiKarts = new ArrayList<>();
    private final List<Integer> aiWaypoints = new ArrayList<>();

    private String currentItem = "NONE";
    private int playerLap = 1;
    private int nextCheckpointIdx = 1;

    // --- Lap Timer & Best Record ---
    private float lapStartTime = 0f;
    private float currentLapTime = 0f;
    private float bestLapTime = Float.MAX_VALUE;

    // --- Lighting & HUD ---
    private SpotLight headlight;
    private BitmapText positionHud;
    private BitmapText speedometerHud;
    private BitmapText itemHud;
    private BitmapText lapHud;
    private BitmapText timerHud;

    // --- Radar Minimap ---
    private Node radarNode;
    private Geometry playerRadarDot;
    private final List<Geometry> aiRadarDots = new ArrayList<>();
    private final float radarSize = 130f;
    private final float radarCenterX = 150f;
    private final float radarCenterY = 130f;

    private final Random random = new Random();

    /**
     * Main entry point to launch AsiKart 3D.
     *
     * @param args Command line arguments.
     */
    public static void main(String[] args) {
        if (activeInstance != null) {
            try {
                System.out.println("Closing previous AsiKart window instance...");
                activeInstance.stop(true);
                Thread.sleep(350);
            } catch (Exception e) {
                System.out.println("Error closing previous instance: " + e.getMessage());
            }
        }

        AsiKart app = new AsiKart();
        activeInstance = app;
        AppSettings settings = new AppSettings(true);
        settings.setTitle("AsiKart 3D v3.5 [Turn 291] - Modular Multi-Circuit Architecture");
        settings.setResolution(1280, 720);
        settings.setVSync(true);
        settings.setSamples(4);
        settings.setRenderer(AppSettings.LWJGL_OPENGL33);

        app.setSettings(settings);
        app.setShowSettings(false);
        app.start();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void simpleInitApp() {
        viewPort.setBackgroundColor(new ColorRGBA(0.08f, 0.08f, 0.18f, 1.0f));
        flyCam.setEnabled(false);

        // Register glTF loader for 3D .glb / .gltf models
        try {
            @SuppressWarnings("unchecked")
            Class<? extends AssetLoader> gltfClass = (Class<? extends AssetLoader>) Class.forName("com.jme3.scene.plugins.gltf.GltfLoader");
            assetManager.registerLoader(gltfClass, "glb", "gltf");
        } catch (Throwable ignored) {
        }

        // Register local asset locator for downloading 3D glTF models
        File assetFolder = new File("/tmp/asikart_assets");
        if (assetFolder.exists()) {
            assetManager.registerLocator("/tmp/asikart_assets/", FileLocator.class);
        }

        // Initialize Modular Subsystems
        audioManager = new AudioManager(rootNode, assetManager);
        itemManager = new ItemManager(rootNode, assetManager, audioManager);
        trackBuilder = new TrackBuilder();

        setupLightingAndShadows();
        setupPostProcessingFilters();

        // Build 3D Track via TrackBuilder
        currentTrack = trackBuilder.buildTrack(rootNode, assetManager, TrackBuilder.CircuitType.RAINBOW_CIRCUIT);

        // Spawn Items and Coins
        itemManager.spawnItemBoxes(currentTrack.getWaypoints());
        itemManager.spawnCoins(currentTrack.getWaypoints());

        createPlayerKart();
        createAiKarts();

        initControls();
        initHud();
    }

    private void setupLightingAndShadows() {
        DirectionalLight sun = new DirectionalLight();
        sun.setDirection(new Vector3f(-0.5f, -0.8f, -0.4f).normalizeLocal());
        sun.setColor(new ColorRGBA(1.0f, 0.95f, 0.85f, 1.0f).mult(1.4f));
        rootNode.addLight(sun);

        AmbientLight ambient = new AmbientLight();
        ambient.setColor(new ColorRGBA(0.35f, 0.4f, 0.5f, 1.0f));
        rootNode.addLight(ambient);

        DirectionalLightShadowRenderer shadowRenderer = new DirectionalLightShadowRenderer(assetManager, 1024, 2);
        shadowRenderer.setLight(sun);
        viewPort.addProcessor(shadowRenderer);
    }

    private void setupPostProcessingFilters() {
        FilterPostProcessor fpp = new FilterPostProcessor(assetManager);
        viewPort.addProcessor(fpp);
    }

    private void createPlayerKart() {
        playerKartNode = new Node("PlayerKartNode");
        playerChassisNode = buildShinyKartGeometry(new ColorRGBA(0.1f, 0.4f, 0.95f, 1.0f), "Anahata-ASI");

        try {
            Spatial gltfModel = assetManager.loadModel("kart.glb");
            gltfModel.setLocalScale(0.9f);
            playerChassisNode.attachChild(gltfModel);
        } catch (Exception ignored) {
        }

        playerKartNode.attachChild(playerChassisNode);

        // Exhaust Particles
        exhaustFlame = new ParticleEmitter("ExhaustFlame", ParticleMesh.Type.Triangle, 25);
        Material pMat = new Material(assetManager, "Common/MatDefs/Misc/Particle.j3md");
        exhaustFlame.setMaterial(pMat);
        exhaustFlame.setStartColor(ColorRGBA.Cyan);
        exhaustFlame.setEndColor(ColorRGBA.Blue);
        exhaustFlame.getParticleInfluencer().setInitialVelocity(new Vector3f(0, 0, -5f));
        exhaustFlame.setStartSize(0.35f);
        exhaustFlame.setEndSize(0.05f);
        playerChassisNode.attachChild(exhaustFlame);
        exhaustFlame.setLocalTranslation(0, 0.35f, -1.5f);

        // Drift Spark Particles
        driftSparks = new ParticleEmitter("DriftSparks", ParticleMesh.Type.Triangle, 40);
        Material sparkMat = new Material(assetManager, "Common/MatDefs/Misc/Particle.j3md");
        driftSparks.setMaterial(sparkMat);
        driftSparks.setStartColor(ColorRGBA.Cyan);
        driftSparks.setEndColor(ColorRGBA.Blue);
        driftSparks.getParticleInfluencer().setInitialVelocity(new Vector3f(0, 3f, -4f));
        driftSparks.setStartSize(0.4f);
        driftSparks.setEndSize(0.08f);
        driftSparks.setParticlesPerSec(0);
        playerChassisNode.attachChild(driftSparks);
        driftSparks.setLocalTranslation(0, 0.2f, -1.0f);

        playerKartNode.setLocalTranslation(0, 0.2f, 0);
        playerKartNode.setShadowMode(RenderQueue.ShadowMode.Cast);
        rootNode.attachChild(playerKartNode);

        // 3D Headlight
        headlight = new SpotLight();
        headlight.setSpotRange(80f);
        headlight.setSpotInnerAngle(12f * FastMath.DEG_TO_RAD);
        headlight.setSpotOuterAngle(28f * FastMath.DEG_TO_RAD);
        headlight.setColor(new ColorRGBA(1.0f, 0.98f, 0.85f, 1.0f).mult(3.5f));
        rootNode.addLight(headlight);
    }

    private void createAiKarts() {
        ColorRGBA[] colors = {new ColorRGBA(0.9f, 0.15f, 0.1f, 1f), new ColorRGBA(0.95f, 0.75f, 0.1f, 1f), new ColorRGBA(0.1f, 0.8f, 0.2f, 1f)};
        String[] names = {"RedRacer", "GoldKart", "GreenPhantom"};

        for (int i = 0; i < 3; i++) {
            Node aiKart = new Node("AiKart_" + i);
            Node chassis = buildShinyKartGeometry(colors[i], names[i]);
            aiKart.attachChild(chassis);

            float offset = (i + 1) * 3.8f;
            aiKart.setLocalTranslation(-offset, 0.2f, -6f);
            aiKart.setShadowMode(RenderQueue.ShadowMode.Cast);
            rootNode.attachChild(aiKart);

            aiKarts.add(aiKart);
            aiWaypoints.add(1);
        }
    }

    private Node buildShinyKartGeometry(ColorRGBA primaryColor, String kartName) {
        Node kart = new Node(kartName);

        Material bodyMat = createLightedMaterial(primaryColor, ColorRGBA.White, 96f);
        Material tireMat = createLightedMaterial(new ColorRGBA(0.08f, 0.08f, 0.08f, 1f), ColorRGBA.DarkGray, 8f);
        Material rimMat = createLightedMaterial(ColorRGBA.White, ColorRGBA.White, 128f);
        Material chromeMat = createLightedMaterial(ColorRGBA.LightGray, ColorRGBA.White, 128f);
        Material visorMat = createLightedMaterial(new ColorRGBA(0.1f, 0.9f, 0.95f, 1f), ColorRGBA.White, 128f);
        Material engineMat = createLightedMaterial(new ColorRGBA(0.2f, 0.2f, 0.22f, 1f), ColorRGBA.White, 64f);

        // 1. Lower Main Chassis Frame
        Box mainChassis = new Box(0.75f, 0.18f, 1.4f);
        Geometry chassisGeo = new Geometry("ChassisFrame", mainChassis);
        chassisGeo.setMaterial(bodyMat);
        chassisGeo.setLocalTranslation(0, 0.25f, 0);
        kart.attachChild(chassisGeo);

        // 2. Aerodynamic Slanted Nose Cone
        Dome noseCone = new Dome(Vector3f.ZERO, 2, 16, 0.74f, false);
        Geometry noseGeo = new Geometry("NoseCone", noseCone);
        noseGeo.setMaterial(bodyMat);
        noseGeo.setLocalRotation(new Quaternion().fromAngleAxis(FastMath.PI, Vector3f.UNIT_X));
        noseGeo.setLocalScale(1.0f, 0.4f, 1.4f);
        noseGeo.setLocalTranslation(0, 0.26f, 1.4f);
        kart.attachChild(noseGeo);

        // 3. Front Bumper Spoiler Guard
        Box frontBumper = new Box(0.92f, 0.06f, 0.25f);
        Geometry bumperGeo = new Geometry("FrontBumper", frontBumper);
        bumperGeo.setMaterial(chromeMat);
        bumperGeo.setLocalTranslation(0, 0.18f, 1.85f);
        kart.attachChild(bumperGeo);

        // 4. Cockpit Driver Seat & Steering Wheel
        Box seat = new Box(0.42f, 0.3f, 0.42f);
        Geometry seatGeo = new Geometry("DriverSeat", seat);
        seatGeo.setMaterial(engineMat);
        seatGeo.setLocalTranslation(0, 0.4f, -0.2f);
        kart.attachChild(seatGeo);

        // 3D Driver Helmet
        Sphere helmet = new Sphere(16, 16, 0.32f);
        Geometry helmetGeo = new Geometry("DriverHelmet", helmet);
        helmetGeo.setMaterial(bodyMat);
        helmetGeo.setLocalTranslation(0, 0.82f, -0.2f);
        kart.attachChild(helmetGeo);

        // Helmet Visor
        Box visor = new Box(0.22f, 0.08f, 0.12f);
        Geometry visorGeo = new Geometry("HelmetVisor", visor);
        visorGeo.setMaterial(visorMat);
        visorGeo.setLocalTranslation(0, 0.85f, 0.05f);
        kart.attachChild(visorGeo);

        // Steering Wheel Column
        Cylinder steeringColumn = new Cylinder(12, 12, 0.04f, 0.5f, true);
        Geometry colGeo = new Geometry("SteeringColumn", steeringColumn);
        colGeo.setMaterial(chromeMat);
        colGeo.setLocalTranslation(0, 0.52f, 0.35f);
        colGeo.setLocalRotation(new Quaternion().fromAngleAxis(FastMath.QUARTER_PI, Vector3f.UNIT_X));
        kart.attachChild(colGeo);

        Cylinder steeringWheel = new Cylinder(16, 16, 0.2f, 0.04f, true);
        Geometry wheelGeo = new Geometry("SteeringWheel", steeringWheel);
        wheelGeo.setMaterial(engineMat);
        wheelGeo.setLocalTranslation(0, 0.68f, 0.48f);
        wheelGeo.setLocalRotation(new Quaternion().fromAngleAxis(FastMath.QUARTER_PI, Vector3f.UNIT_X));
        kart.attachChild(wheelGeo);

        // 5. Quad Rubber Tires with Metallic Alloy Rims
        Cylinder tireShape = new Cylinder(16, 16, 0.38f, 0.32f, true);
        Cylinder rimShape = new Cylinder(16, 16, 0.22f, 0.33f, true);

        float[][] wheelPositions = {
            {-0.92f, 0.38f, 1.05f}, // Front Left
            {0.92f, 0.38f, 1.05f}, // Front Right
            {-0.95f, 0.38f, -0.95f}, // Rear Left
            {0.95f, 0.38f, -0.95f} // Rear Right
        };

        for (int i = 0; i < 4; i++) {
            Node wheelNode = new Node("WheelAssembly_" + i);

            // Outer Rubber Tire
            Geometry tGeo = new Geometry("TireRubber", tireShape);
            tGeo.setMaterial(tireMat);
            tGeo.setLocalRotation(new Quaternion().fromAngleAxis(FastMath.HALF_PI, Vector3f.UNIT_Y));
            wheelNode.attachChild(tGeo);

            // Inner Metallic Alloy Rim
            Geometry rGeo = new Geometry("AlloyRim", rimShape);
            rGeo.setMaterial(rimMat);
            rGeo.setLocalRotation(new Quaternion().fromAngleAxis(FastMath.HALF_PI, Vector3f.UNIT_Y));
            wheelNode.attachChild(rGeo);

            wheelNode.setLocalTranslation(wheelPositions[i][0], wheelPositions[i][1], wheelPositions[i][2]);
            kart.attachChild(wheelNode);
        }

        // 6. Rear Engine Block & Dual Chrome Exhaust Pipes
        Box engineBlock = new Box(0.55f, 0.32f, 0.45f);
        Geometry engineGeo = new Geometry("EngineBlock", engineBlock);
        engineGeo.setMaterial(engineMat);
        engineGeo.setLocalTranslation(0, 0.48f, -1.05f);
        kart.attachChild(engineGeo);

        Cylinder pipeShape = new Cylinder(12, 12, 0.1f, 0.55f, true);
        Geometry pipeL = new Geometry("ExhaustPipeL", pipeShape);
        pipeL.setMaterial(chromeMat);
        pipeL.setLocalTranslation(-0.3f, 0.45f, -1.55f);
        kart.attachChild(pipeL);

        Geometry pipeR = new Geometry("ExhaustPipeR", pipeShape);
        pipeR.setMaterial(chromeMat);
        pipeR.setLocalTranslation(0.3f, 0.45f, -1.55f);
        kart.attachChild(pipeR);

        // 7. High Rear Spoiler Wing
        Box wingPillar = new Box(0.06f, 0.4f, 0.08f);
        Geometry pillarL = new Geometry("PillarL", wingPillar);
        pillarL.setMaterial(chromeMat);
        pillarL.setLocalTranslation(-0.55f, 0.85f, -1.4f);
        kart.attachChild(pillarL);

        Geometry pillarR = new Geometry("PillarR", wingPillar);
        pillarR.setMaterial(chromeMat);
        pillarR.setLocalTranslation(0.55f, 0.85f, -1.4f);
        kart.attachChild(pillarR);

        Box mainWing = new Box(1.15f, 0.06f, 0.38f);
        Geometry wingGeo = new Geometry("SpoilerWing", mainWing);
        wingGeo.setMaterial(bodyMat);
        wingGeo.setLocalTranslation(0, 1.25f, -1.4f);
        kart.attachChild(wingGeo);

        return kart;
    }

    private Material createLightedMaterial(ColorRGBA diffuse, ColorRGBA specular, float shininess) {
        Material mat = new Material(assetManager, "Common/MatDefs/Light/Lighting.j3md");
        mat.setBoolean("UseMaterialColors", true);
        mat.setColor("Diffuse", diffuse);
        mat.setColor("Ambient", diffuse.mult(0.6f));
        mat.setColor("Specular", specular);
        mat.setFloat("Shininess", shininess);
        return mat;
    }

    private void initControls() {
        inputManager.addMapping("Accel", new KeyTrigger(KeyInput.KEY_W), new KeyTrigger(KeyInput.KEY_UP));
        inputManager.addMapping("Brake", new KeyTrigger(KeyInput.KEY_S), new KeyTrigger(KeyInput.KEY_DOWN));
        inputManager.addMapping("Left", new KeyTrigger(KeyInput.KEY_A), new KeyTrigger(KeyInput.KEY_LEFT));
        inputManager.addMapping("Right", new KeyTrigger(KeyInput.KEY_D), new KeyTrigger(KeyInput.KEY_RIGHT));
        inputManager.addMapping("Drift", new KeyTrigger(KeyInput.KEY_SPACE));
        inputManager.addMapping("UseItem", new KeyTrigger(KeyInput.KEY_E), new KeyTrigger(KeyInput.KEY_LSHIFT));
        inputManager.addMapping("Circuit1", new KeyTrigger(KeyInput.KEY_1));
        inputManager.addMapping("Circuit2", new KeyTrigger(KeyInput.KEY_2));
        inputManager.addMapping("Circuit3", new KeyTrigger(KeyInput.KEY_3));

        inputManager.addListener(this, "Accel", "Brake", "Left", "Right", "Drift", "UseItem", "Circuit1", "Circuit2", "Circuit3");
    }

    @Override
    public void onAction(String name, boolean isPressed, float tpf) {
        if (name.equals("Accel")) {
            accel = isPressed;
        }
        if (name.equals("Brake")) {
            brake = isPressed;
        }
        if (name.equals("Left")) {
            steerLeft = isPressed;
        }
        if (name.equals("Right")) {
            steerRight = isPressed;
        }
        if (name.equals("Drift")) {
            if (isPressed && Math.abs(speed) > 12f) {
                isDrifting = true;
                driftAngle = steerLeft ? -0.5f : (steerRight ? 0.5f : 0f);
            } else if (!isPressed) {
                if (isDrifting) {
                    if (driftTime > 2.8f) {
                        turboBoostTimer = 4.0f; // Ultra Mini Turbo
                    } else if (driftTime > 1.8f) {
                        turboBoostTimer = 2.8f; // Super Mini Turbo
                    } else if (driftTime > 0.8f) {
                        turboBoostTimer = 1.6f; // Mini Turbo
                    }
                    if (audioManager != null) {
                        audioManager.playTurboSound();
                    }
                }
                isDrifting = false;
                driftTime = 0f;
                driftAngle = 0f;
                driftSparks.setParticlesPerSec(0);
            }
        }
        if (name.equals("Circuit1") && isPressed) {
            switchCircuit(TrackBuilder.CircuitType.RAINBOW_CIRCUIT);
        }
        if (name.equals("Circuit2") && isPressed) {
            switchCircuit(TrackBuilder.CircuitType.DESERT_SPEEDWAY);
        }
        if (name.equals("Circuit3") && isPressed) {
            switchCircuit(TrackBuilder.CircuitType.CYBER_CIRCUIT);
        }
    }

    public void switchCircuit(TrackBuilder.CircuitType circuitType) {
        if (currentTrack != null && currentTrack.getTrackNode() != null) {
            rootNode.detachChild(currentTrack.getTrackNode());
        }

        currentTrack = trackBuilder.buildTrack(rootNode, assetManager, circuitType);
        itemManager.spawnItemBoxes(currentTrack.getWaypoints());
        itemManager.spawnCoins(currentTrack.getWaypoints());

        playerKartNode.setLocalTranslation(0, 0.2f, 0);
        heading = 0f;
        speed = 0f;
        playerLap = 1;
        nextCheckpointIdx = 1;
        lapHud.setText("LAP: 1 / 3");

        for (int i = 0; i < aiKarts.size(); i++) {
            aiKarts.get(i).setLocalTranslation(-(i + 1) * 3.8f, 0.2f, -6f);
            aiWaypoints.set(i, 1);
        }
    }

    private void initRadarMinimap() {
        radarNode = new Node("RadarMinimap");
        float posX = settings.getWidth() - radarCenterX;
        float posY = radarCenterY;
        radarNode.setLocalTranslation(posX, posY, 0);

        // Radar Background Disc
        Sphere bgDisc = new Sphere(24, 24, radarSize * 0.5f);
        Geometry bgGeo = new Geometry("RadarBg", bgDisc);
        Material bgMat = new Material(assetManager, "Common/MatDefs/Misc/Unshaded.j3md");
        bgMat.setColor("Color", new ColorRGBA(0.05f, 0.05f, 0.12f, 0.75f));
        bgGeo.setMaterial(bgMat);
        bgGeo.setLocalScale(1.0f, 1.0f, 0.01f);
        radarNode.attachChild(bgGeo);

        // Player Dot (Cyan)
        Sphere dotShape = new Sphere(12, 12, 5.0f);
        playerRadarDot = new Geometry("PlayerDot", dotShape);
        Material playerDotMat = new Material(assetManager, "Common/MatDefs/Misc/Unshaded.j3md");
        playerDotMat.setColor("Color", ColorRGBA.Cyan);
        playerRadarDot.setMaterial(playerDotMat);
        radarNode.attachChild(playerRadarDot);

        // AI Dots (Red, Yellow, Green)
        ColorRGBA[] aiColors = {ColorRGBA.Red, ColorRGBA.Yellow, ColorRGBA.Green};
        aiRadarDots.clear();
        for (int i = 0; i < 3; i++) {
            Geometry aiDot = new Geometry("AiDot_" + i, dotShape);
            Material aiDotMat = new Material(assetManager, "Common/MatDefs/Misc/Unshaded.j3md");
            aiDotMat.setColor("Color", aiColors[i]);
            aiDot.setMaterial(aiDotMat);
            radarNode.attachChild(aiDot);
            aiRadarDots.add(aiDot);
        }

        guiNode.attachChild(radarNode);
    }

    private void updateRadarMinimap() {
        if (playerKartNode == null || currentTrack == null) {
            return;
        }

        Vector3f playerPos = playerKartNode.getLocalTranslation();
        float scale = 0.45f; // Map world units to radar GUI pixels

        // Center player dot on radar
        playerRadarDot.setLocalTranslation(0, 0, 1);

        // Position AI Dots relative to Player position
        for (int i = 0; i < aiKarts.size() && i < aiRadarDots.size(); i++) {
            Vector3f aiPos = aiKarts.get(i).getLocalTranslation();
            Vector3f relPos = aiPos.subtract(playerPos);

            float mapX = relPos.x * scale;
            float mapY = relPos.z * scale;

            // Clamp to radar radius
            float dist = FastMath.sqrt(mapX * mapX + mapY * mapY);
            float maxRadius = radarSize * 0.45f;
            if (dist > maxRadius) {
                mapX = (mapX / dist) * maxRadius;
                mapY = (mapY / dist) * maxRadius;
            }

            aiRadarDots.get(i).setLocalTranslation(mapX, mapY, 1);
        }
    }

    private void useCurrentItem() {
        Vector3f spawnPos = playerKartNode.getLocalTranslation().add(new Vector3f(FastMath.sin(heading), 0, FastMath.cos(heading)).mult(2.5f));

        if ("MUSHROOM".equals(currentItem)) {
            turboBoostTimer = 3.0f;
            if (audioManager != null) {
                audioManager.playTurboSound();
            }
            currentItem = "NONE";
        } else if ("GREEN_SHELL".equals(currentItem)) {
            itemManager.launchGreenShell(spawnPos, heading);
            currentItem = "NONE";
        } else if ("RED_SHELL".equals(currentItem)) {
            itemManager.launchRedShell(spawnPos, heading);
            currentItem = "NONE";
        } else if ("BANANA".equals(currentItem)) {
            Vector3f dropPos = playerKartNode.getLocalTranslation().subtract(new Vector3f(FastMath.sin(heading), 0, FastMath.cos(heading)).mult(2.5f));
            itemManager.dropBanana(dropPos);
            currentItem = "NONE";
        } else if ("STAR".equals(currentItem)) {
            starInvincibleTimer = 6.0f;
            currentItem = "NONE";
        }
    }

    public void onItemPickedUp(String itemType) {
        // Trigger Rolling Item Roulette on HUD
        new Thread(() -> {
            String[] items = {"MUSHROOM", "GREEN_SHELL", "RED_SHELL", "BANANA", "STAR"};
            for (int i = 0; i < 8; i++) {
                try {
                    String cycleItem = items[i % items.length];
                    itemHud.setText("ITEM: [" + cycleItem + "] | COINS: " + itemManager.getCoinsCollected());
                    if (audioManager != null) {
                        audioManager.playItemBoxSound();
                    }
                    Thread.sleep(120);
                } catch (InterruptedException ignored) {
                }
            }
            this.currentItem = itemType;
            itemHud.setText("ITEM: [" + currentItem + "] | COINS: " + itemManager.getCoinsCollected());
        }).start();
    }

    private void initHud() {
        guiNode.detachAllChildren();

        positionHud = new BitmapText(guiFont, false);
        positionHud.setSize(guiFont.getCharSet().getRenderedSize() * 2.4f);
        positionHud.setColor(ColorRGBA.Yellow);
        positionHud.setText("1st / 4");
        positionHud.setLocalTranslation(settings.getWidth() - 190, settings.getHeight() - 30, 0);
        guiNode.attachChild(positionHud);

        speedometerHud = new BitmapText(guiFont, false);
        speedometerHud.setSize(guiFont.getCharSet().getRenderedSize() * 1.7f);
        speedometerHud.setColor(ColorRGBA.Cyan);
        speedometerHud.setText("0 KM/H");
        speedometerHud.setLocalTranslation(settings.getWidth() - 190, 85, 0);
        guiNode.attachChild(speedometerHud);

        itemHud = new BitmapText(guiFont, false);
        itemHud.setSize(guiFont.getCharSet().getRenderedSize() * 1.5f);
        itemHud.setColor(ColorRGBA.Orange);
        itemHud.setText("ITEM: [ ITEM BOX ]");
        itemHud.setLocalTranslation(30, settings.getHeight() - 30, 0);
        guiNode.attachChild(itemHud);

        lapHud = new BitmapText(guiFont, false);
        lapHud.setSize(guiFont.getCharSet().getRenderedSize() * 1.4f);
        lapHud.setColor(ColorRGBA.White);
        lapHud.setText("LAP: 1 / 3");
        lapHud.setLocalTranslation(30, settings.getHeight() - 75, 0);
        guiNode.attachChild(lapHud);

        timerHud = new BitmapText(guiFont, false);
        timerHud.setSize(guiFont.getCharSet().getRenderedSize() * 1.3f);
        timerHud.setColor(ColorRGBA.Green);
        timerHud.setText("TIME: 00:00.00 | BEST: --:--.--");
        timerHud.setLocalTranslation(30, settings.getHeight() - 115, 0);
        guiNode.attachChild(timerHud);

        initRadarMinimap();
    }

    private float findTrackSurfaceHeight(float x, float z, List<Vector3f> waypoints) {
        // Updated via BCR: Calculate exact interpolated track surface elevation
        float minDistanceSq = Float.MAX_VALUE;
        float bestGroundY = 0.2f;
        int numWaypoints = waypoints.size();
        for (int i = 0; i < numWaypoints; i++) {
            Vector3f p1 = waypoints.get(i);
            Vector3f p2 = waypoints.get((i + 1) % numWaypoints);
            float dx = p2.x - p1.x;
            float dz = p2.z - p1.z;
            float lenSq = dx * dx + dz * dz;
            float t = 0f;
            if (lenSq > 1e-6f) {
                t = FastMath.clamp(((x - p1.x) * dx + (z - p1.z) * dz) / lenSq, 0f, 1f);
            }
            float projX = p1.x + t * dx;
            float projZ = p1.z + t * dz;
            float distSq = (x - projX) * (x - projX) + (z - projZ) * (z - projZ);
            if (distSq < minDistanceSq) {
                minDistanceSq = distSq;
                bestGroundY = FastMath.interpolateLinear(t, p1.y, p2.y) + 0.2f;
            }
        }
        return bestGroundY;
    }

    public void simpleUpdate(float tpf) {
        // Updated via BCR: Simple update loop with tpf delta processing
        itemManager.update(tpf, playerKartNode, aiKarts, currentItem, this);

        // Steering & Acceleration Physics
        float effectiveMaxSpeed = maxSpeed;
        if (turboBoostTimer > 0f) {
            effectiveMaxSpeed *= 1.45f;
            speed = effectiveMaxSpeed;
            turboBoostTimer -= tpf;
        }

        if (accel) {
            speed = Math.min(effectiveMaxSpeed, speed + acceleration * tpf);
        } else if (brake) {
            speed = Math.max(-10f, speed - deceleration * 1.5f * tpf);
        } else {
            if (speed > 0) {
                speed = Math.max(0, speed - deceleration * tpf);
            }
            if (speed < 0) {
                speed = Math.min(0, speed + deceleration * tpf);
            }
        }

        if (audioManager != null) {
            audioManager.updateEnginePitch(speed, maxSpeed);
        }

        if (Math.abs(speed) > 0.5f) {
            float dirMultiplier = speed > 0 ? 1f : -1f;
            if (steerLeft) {
                heading += steeringSensitivity * dirMultiplier * tpf;
            }
            if (steerRight) {
                heading -= steeringSensitivity * dirMultiplier * tpf;
            }
        }

        if (isDrifting) {
            driftTime += tpf;
            driftSparks.setParticlesPerSec(35);
            if (driftTime > 2.8f) {
                driftSparks.setStartColor(ColorRGBA.Magenta);
                driftSparks.setEndColor(ColorRGBA.Pink);
            } else if (driftTime > 1.8f) {
                driftSparks.setStartColor(ColorRGBA.Orange);
                driftSparks.setEndColor(ColorRGBA.Yellow);
            } else {
                driftSparks.setStartColor(ColorRGBA.Cyan);
                driftSparks.setEndColor(ColorRGBA.Blue);
            }
        } else {
            driftSparks.setParticlesPerSec(0);
        }

        Vector3f moveDir = new Vector3f(FastMath.sin(heading), 0, FastMath.cos(heading));
        Vector3f currentPos = playerKartNode.getLocalTranslation();
        Vector3f nextPos = currentPos.add(moveDir.mult(speed * tpf));

        // Bulletproof Closest Segment Track Surface Elevation Finder
        List<Vector3f> waypoints = currentTrack.getWaypoints();
        float exactGroundY = findTrackSurfaceHeight(nextPos.x, nextPos.z, waypoints);
        nextPos.y = FastMath.interpolateLinear(tpf * 10.0f, currentPos.y, exactGroundY);

        playerKartNode.setLocalTranslation(nextPos);

        Quaternion kartRot = new Quaternion().fromAngleAxis(heading, Vector3f.UNIT_Y);
        playerKartNode.setLocalRotation(kartRot);

        // Update 3D Headlight
        Vector3f headlightPos = playerKartNode.getLocalTranslation().add(0, 0.6f, 0);
        headlight.setPosition(headlightPos);
        headlight.setDirection(moveDir);

        Quaternion chassisRot = new Quaternion().fromAngles(0, driftAngle, steerLeft ? 0.12f : (steerRight ? -0.12f : 0f));
        playerChassisNode.setLocalRotation(chassisRot);

        // Dynamic Chase Camera
        Vector3f camTargetPos = playerKartNode.getLocalTranslation().add(moveDir.mult(turboBoostTimer > 0 ? -15f : -12.5f)).add(0, 4.8f, 0);
        cam.setLocation(cam.getLocation().interpolateLocal(camTargetPos, tpf * 9.0f));
        cam.lookAt(playerKartNode.getLocalTranslation().add(0, 1.3f, 0), Vector3f.UNIT_Y);

        // Boost Pad Checks
        for (Geometry boost : currentTrack.getBoostPads()) {
            if (playerKartNode.getLocalTranslation().distance(boost.getLocalTranslation()) < 3.8f) {
                turboBoostTimer = 2.0f;
                if (audioManager != null) {
                    audioManager.playTurboSound();
                }
            }
        }

        // Checkpoint & Lap Calculation
        if (playerLap <= 3) {
            currentLapTime += tpf;
        }

        int curMin = (int) (currentLapTime / 60);
        int curSec = (int) (currentLapTime % 60);
        int curMs = (int) ((currentLapTime * 100) % 100);
        String timeStr = String.format("TIME: %02d:%02d.%02d", curMin, curSec, curMs);

        String bestStr = (bestLapTime == Float.MAX_VALUE) ? "--:--.--" : String.format("%02d:%02d.%02d", (int) (bestLapTime / 60), (int) (bestLapTime % 60), (int) ((bestLapTime * 100) % 100));
        timerHud.setText(timeStr + " | BEST: " + bestStr);

        Vector3f targetCheckpoint = waypoints.get(nextCheckpointIdx);
        if (playerKartNode.getLocalTranslation().distance(targetCheckpoint) < 18f) {
            nextCheckpointIdx = (nextCheckpointIdx + 1) % waypoints.size();
            if (nextCheckpointIdx == 1) {
                if (currentLapTime < bestLapTime) {
                    bestLapTime = currentLapTime;
                }
                currentLapTime = 0f;
                playerLap++;

                if (playerLap > 3) {
                    lapHud.setText("RACE FINISHED! 1ST PLACE - GOLD TROPHY!");
                    lapHud.setColor(ColorRGBA.Yellow);
                    if (exhaustFlame != null) {
                        exhaustFlame.setParticlesPerSec(200);
                    }
                } else {
                    lapHud.setText("LAP: " + playerLap + " / 3");
                    if (exhaustFlame != null) {
                        exhaustFlame.setParticlesPerSec(120);
                    }
                }
            }
        }

        updateAiCompetitors(tpf, waypoints);
        updateRadarMinimap();

        int currentKmh = Math.round(Math.abs(speed) * 3.6f);
        speedometerHud.setText(currentKmh + " KM/H" + (turboBoostTimer > 0 ? " [TURBO BOOST!]" : ""));
        itemHud.setText("ITEM: " + ("NONE".equals(currentItem) ? "[ ITEM BOX ]" : "[" + currentItem + "]") + " | COINS: " + itemManager.getCoinsCollected());
    }

    private void updateAiCompetitors(float tpf, List<Vector3f> waypoints) {
        for (int i = 0; i < aiKarts.size(); i++) {
            Node ai = aiKarts.get(i);
            int targetIdx = aiWaypoints.get(i);
            int prevIdx = (targetIdx - 1 + waypoints.size()) % waypoints.size();

            Vector3f p1 = waypoints.get(prevIdx);
            Vector3f p2 = waypoints.get(targetIdx);

            Vector3f dir = p2.subtract(ai.getLocalTranslation());

            if (dir.length() < 12f) {
                targetIdx = (targetIdx + 1) % waypoints.size();
                aiWaypoints.set(i, targetIdx);
            }

            dir.y = 0;
            dir.normalizeLocal();
            float aiSpeed = 24f + i * 2.5f;

            Vector3f nextAiPos = ai.getLocalTranslation().add(dir.mult(aiSpeed * tpf));
            float exactGroundY = findTrackSurfaceHeight(nextAiPos.x, nextAiPos.z, waypoints);
            nextAiPos.y = FastMath.interpolateLinear(tpf * 10.0f, ai.getLocalTranslation().y, exactGroundY);

            ai.setLocalTranslation(nextAiPos);

            Quaternion rot = new Quaternion();
            rot.lookAt(dir, Vector3f.UNIT_Y);
            ai.setLocalRotation(rot);
        }
    }

    /**
     * Checks if an active AsiKart 3D game window instance is currently running
     * in this JVM process.
     *
     * @return true if an active game application instance exists, false
     * otherwise.
     */
    public boolean isGameRunning() {
        return activeInstance != null;
    }
}
