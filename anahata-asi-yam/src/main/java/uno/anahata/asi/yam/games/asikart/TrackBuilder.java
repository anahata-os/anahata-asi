/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.games.asikart;

import com.jme3.asset.AssetManager;
import com.jme3.asset.plugins.FileLocator;
import com.jme3.material.Material;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.renderer.queue.RenderQueue;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.shape.Box;
import com.jme3.scene.shape.Cylinder;
import com.jme3.scene.shape.Dome;
import com.jme3.texture.Texture;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * TrackBuilder — Specialized 3D Circuit Generator for AsiKart 3D.
 * <p>
 * Supports multiple 3D circuits:
 * </p>
 * <ul>
 * <li>{@link CircuitType#RAINBOW_CIRCUIT} — High-altitude cosmic track
 * surrounded by starfield and neon light-rails.</li>
 * <li>{@link CircuitType#DESERT_SPEEDWAY} — Sun-scorched desert raceway with
 * dunes, palm trees, and ancient stone arches.</li>
 * <li>{@link CircuitType#CYBER_CIRCUIT} — Futuristic neon-grid circuit set in a
 * synthwave cyber metropolis.</li>
 * </ul>
 * <p>
 * Features 3D Sky Dome, PBR Asphalt/Grass normal bump textures, 3D Finish
 * Arches, and thematic scenery trees.
 * </p>
 *
 * @author anahata
 */
public class TrackBuilder {

    /**
     * Enumeration of available 3D circuit themes.
     */
    public enum CircuitType {
        RAINBOW_CIRCUIT("Rainbow Circuit", "High-altitude cosmic track surrounded by starfield and neon light-rails."),
        DESERT_SPEEDWAY("Desert Speedway", "Sun-scorched desert raceway with dunes, palm trees, and ancient stone arches."),
        CYBER_CIRCUIT("Cyber Circuit", "Futuristic neon-grid circuit set in a synthwave cyber metropolis.");

        private final String displayName;
        private final String description;

        CircuitType(String displayName, String description) {
            this.displayName = displayName;
            this.description = description;
        }

        public String getDisplayName() {
            return displayName;
        }

        public String getDescription() {
            return description;
        }
    }

    /**
     * Container holding generated track objects and geometry references.
     */
    public static class TrackResult {

        private final Node trackNode;
        private final Geometry skyDome;
        private final List<Vector3f> waypoints;
        private final List<Geometry> trackCurbs;
        private final List<Geometry> boostPads;
        private final List<Node> itemBoxGroups;
        private final List<Geometry> trackCoins;

        public TrackResult(Node trackNode, Geometry skyDome, List<Vector3f> waypoints,
                List<Geometry> trackCurbs, List<Geometry> boostPads,
                List<Node> itemBoxGroups, List<Geometry> trackCoins) {
            this.trackNode = trackNode;
            this.skyDome = skyDome;
            this.waypoints = waypoints;
            this.trackCurbs = trackCurbs;
            this.boostPads = boostPads;
            this.itemBoxGroups = itemBoxGroups;
            this.trackCoins = trackCoins;
        }

        public Node getTrackNode() {
            return trackNode;
        }

        public Geometry getSkyDome() {
            return skyDome;
        }

        public List<Vector3f> getWaypoints() {
            return waypoints;
        }

        public List<Geometry> getTrackCurbs() {
            return trackCurbs;
        }

        public List<Geometry> getBoostPads() {
            return boostPads;
        }

        public List<Node> getItemBoxGroups() {
            return itemBoxGroups;
        }

        public List<Geometry> getTrackCoins() {
            return trackCoins;
        }
    }

    private final Random random = new Random(42);

    /**
     * Builds a complete 3D circuit environment for AsiKart.
     *
     * @param rootNode The scene root node.
     * @param assetManager The jME3 asset manager.
     * @param type The circuit type to construct.
     * @return TrackResult containing created elements and waypoints.
     */
    public TrackResult buildTrack(Node rootNode, AssetManager assetManager, CircuitType type) {
        Node trackNode = new Node("TrackEnvironment_" + type.name());
        rootNode.attachChild(trackNode);

        List<Vector3f> waypoints = generateWaypoints(type);
        List<Geometry> trackCurbs = new ArrayList<>();
        List<Geometry> boostPads = new ArrayList<>();
        List<Node> itemBoxGroups = new ArrayList<>();
        List<Geometry> trackCoins = new ArrayList<>();

        Geometry skyDome = create3DSkyDome(trackNode, assetManager, type);
        buildGroundAndAsphalt(trackNode, assetManager, type, waypoints, trackCurbs);
        buildFinishArch(trackNode, assetManager, type, waypoints.get(0));
        buildDirectionalArrowSigns(trackNode, assetManager, type, waypoints);
        buildSceneryAndTrees(trackNode, assetManager, type, waypoints);
        spawnItemBoxesAndBoostPads(trackNode, assetManager, type, waypoints, itemBoxGroups, boostPads);
        spawnTrackCoins(trackNode, assetManager, type, waypoints, trackCoins);

        return new TrackResult(trackNode, skyDome, waypoints, trackCurbs, boostPads, itemBoxGroups, trackCoins);
    }

    private List<Vector3f> generateWaypoints(CircuitType type) {
        List<Vector3f> waypoints = new ArrayList<>();
        switch (type) {
            case DESERT_SPEEDWAY -> {
                waypoints.add(new Vector3f(0, 0.2f, 0));
                waypoints.add(new Vector3f(0, 2f, 150));
                waypoints.add(new Vector3f(60, 6f, 220));
                waypoints.add(new Vector3f(160, 10f, 240));
                waypoints.add(new Vector3f(240, 5f, 170));
                waypoints.add(new Vector3f(260, 2.5f, 50));
                waypoints.add(new Vector3f(220, 1.2f, -80));
                waypoints.add(new Vector3f(130, 0.8f, -150));
                waypoints.add(new Vector3f(30, 0.5f, -120));
                waypoints.add(new Vector3f(-40, 0.2f, -60));
            }
            case CYBER_CIRCUIT -> {
                waypoints.add(new Vector3f(0, 0.2f, 0));
                waypoints.add(new Vector3f(0, 3f, 110));
                waypoints.add(new Vector3f(50, 8f, 160));
                waypoints.add(new Vector3f(130, 12f, 160));
                waypoints.add(new Vector3f(180, 6f, 110));
                waypoints.add(new Vector3f(180, 2f, -30));
                waypoints.add(new Vector3f(120, 1.2f, -100));
                waypoints.add(new Vector3f(40, 0.8f, -100));
                waypoints.add(new Vector3f(-30, 0.2f, -50));
            }
            case RAINBOW_CIRCUIT -> {
                waypoints.add(new Vector3f(0, 0.2f, 0));
                waypoints.add(new Vector3f(0, 4f, 120));
                waypoints.add(new Vector3f(40, 12f, 180));
                waypoints.add(new Vector3f(120, 18f, 200));
                waypoints.add(new Vector3f(190, 12f, 150));
                waypoints.add(new Vector3f(210, 5f, 60));
                waypoints.add(new Vector3f(200, 2f, -50));
                waypoints.add(new Vector3f(150, 1.2f, -120));
                waypoints.add(new Vector3f(60, 0.8f, -140));
                waypoints.add(new Vector3f(-30, 0.2f, -90));
            }
        }
        return waypoints;
    }

    private Geometry create3DSkyDome(Node parentNode, AssetManager assetManager, CircuitType type) {
        Dome skyDome = new Dome(Vector3f.ZERO, 16, 32, 450f, false);
        Geometry skyGeo = new Geometry("3DSkyDome_" + type.name(), skyDome);
        Material skyMat = new Material(assetManager, "Common/MatDefs/Misc/Unshaded.j3md");

        ColorRGBA skyColor = switch (type) {
            case RAINBOW_CIRCUIT ->
                new ColorRGBA(0.08f, 0.05f, 0.18f, 1.0f);
            case DESERT_SPEEDWAY ->
                new ColorRGBA(0.85f, 0.62f, 0.35f, 1.0f);
            case CYBER_CIRCUIT ->
                new ColorRGBA(0.04f, 0.02f, 0.12f, 1.0f);
        };

        skyMat.setColor("Color", skyColor);
        skyGeo.setMaterial(skyMat);
        skyGeo.setLocalRotation(new Quaternion().fromAngleAxis(FastMath.PI, Vector3f.UNIT_X));
        parentNode.attachChild(skyGeo);
        return skyGeo;
    }

    public List<Vector3f> generateSmoothSplineWaypoints(List<Vector3f> waypoints, int samplesPerSegment) {
        List<Vector3f> smoothPoints = new ArrayList<>();
        int numWaypoints = waypoints.size();
        for (int i = 0; i < numWaypoints; i++) {
            Vector3f p0 = waypoints.get((i - 1 + numWaypoints) % numWaypoints);
            Vector3f p1 = waypoints.get(i);
            Vector3f p2 = waypoints.get((i + 1) % numWaypoints);
            Vector3f p3 = waypoints.get((i + 2) % numWaypoints);

            for (int step = 0; step < samplesPerSegment; step++) {
                float t = step / (float) samplesPerSegment;
                float t2 = t * t;
                float t3 = t2 * t;

                float f0 = -0.5f * t3 + t2 - 0.5f * t;
                float f1 = 1.5f * t3 - 2.5f * t2 + 1.0f;
                float f2 = -1.5f * t3 + 2.0f * t2 + 0.5f * t;
                float f3 = 0.5f * t3 - 0.5f * t2;

                Vector3f pt = p0.mult(f0).add(p1.mult(f1)).add(p2.mult(f2)).add(p3.mult(f3));
                smoothPoints.add(pt);
            }
        }
        return smoothPoints;
    }

    private void buildGroundAndAsphalt(Node parentNode, AssetManager assetManager, CircuitType type,
            List<Vector3f> waypoints, List<Geometry> trackCurbs) {
        try {
            assetManager.registerLocator("/tmp/asikart_assets/", FileLocator.class);
        } catch (Exception ignored) {
        }

        ColorRGBA groundColor = switch (type) {
            case RAINBOW_CIRCUIT ->
                new ColorRGBA(0.05f, 0.03f, 0.12f, 1.0f);
            case DESERT_SPEEDWAY ->
                new ColorRGBA(0.78f, 0.65f, 0.42f, 1.0f);
            case CYBER_CIRCUIT ->
                new ColorRGBA(0.08f, 0.08f, 0.15f, 1.0f);
        };

        Material groundMat = createLightedMaterial(assetManager, groundColor, ColorRGBA.Gray, 12f);
        try {
            Texture grassTex = assetManager.loadTexture("grass_diffuse.jpg");
            grassTex.setWrap(Texture.WrapMode.Repeat);
            groundMat.setTexture("DiffuseMap", grassTex);
        } catch (Exception ignored) {
        }

        Box ground = new Box(500, 0.1f, 500);
        Geometry groundGeo = new Geometry("Ground_" + type.name(), ground);
        groundGeo.setMaterial(groundMat);
        groundGeo.setLocalTranslation(100, -0.5f, 30);
        groundGeo.setShadowMode(RenderQueue.ShadowMode.Receive);
        parentNode.attachChild(groundGeo);

        ColorRGBA asphaltColor = switch (type) {
            case RAINBOW_CIRCUIT ->
                new ColorRGBA(0.3f, 0.3f, 0.45f, 1.0f);
            case DESERT_SPEEDWAY ->
                new ColorRGBA(0.35f, 0.32f, 0.28f, 1.0f);
            case CYBER_CIRCUIT ->
                new ColorRGBA(0.12f, 0.12f, 0.22f, 1.0f);
        };

        Material asphaltMat = createLightedMaterial(assetManager, asphaltColor, ColorRGBA.White, 32f);
        try {
            Texture asphaltDiffuse = assetManager.loadTexture("asphalt_diffuse.jpg");
            asphaltDiffuse.setWrap(Texture.WrapMode.Repeat);
            asphaltMat.setTexture("DiffuseMap", asphaltDiffuse);
        } catch (Exception ignored) {
        }

        try {
            Texture asphaltNormal = assetManager.loadTexture("asphalt_normal.jpg");
            asphaltNormal.setWrap(Texture.WrapMode.Repeat);
            asphaltMat.setTexture("NormalMap", asphaltNormal);
        } catch (Exception ignored) {
        }

        ColorRGBA curbColor = switch (type) {
            case RAINBOW_CIRCUIT ->
                new ColorRGBA(0.95f, 0.2f, 0.8f, 1.0f);
            case DESERT_SPEEDWAY ->
                new ColorRGBA(0.85f, 0.2f, 0.1f, 1.0f);
            case CYBER_CIRCUIT ->
                new ColorRGBA(0.1f, 0.9f, 0.95f, 1.0f);
        };

        Material curbMat = createLightedMaterial(assetManager, curbColor, ColorRGBA.White, 64f);
        Material whiteLineMat = createLightedMaterial(assetManager, ColorRGBA.White, ColorRGBA.White, 128f);
        Material yellowLineMat = createLightedMaterial(assetManager, new ColorRGBA(1.0f, 0.85f, 0.1f, 1.0f), ColorRGBA.White, 128f);

        // Generate 120 Catmull-Rom Smooth Sub-Waypoints for Buttery Smooth Track Curvature
        List<Vector3f> smoothWaypoints = generateSmoothSplineWaypoints(waypoints, 12);

        for (int i = 0; i < smoothWaypoints.size(); i++) {
            Vector3f p1 = smoothWaypoints.get(i);
            Vector3f p2 = smoothWaypoints.get((i + 1) % smoothWaypoints.size());

            Vector3f dir = p2.subtract(p1);
            float len = dir.length();
            if (len < 0.01f) {
                continue;
            }
            Vector3f mid = p1.add(p2).mult(0.5f);

            Node segmentNode = new Node("TrackSegment_" + i);
            Box trackBox = new Box(11f, 0.05f, len * 0.52f);
            Geometry trackGeo = new Geometry("TrackGeo_" + i, trackBox);
            trackGeo.setMaterial(asphaltMat);
            trackGeo.setShadowMode(RenderQueue.ShadowMode.Receive);
            segmentNode.attachChild(trackGeo);

            // Corner Joint Cylinder disc at sub-waypoint joints to eliminate seams
            Cylinder jointCap = new Cylinder(16, 16, 11f, 0.1f, true);
            Geometry jointGeo = new Geometry("JointCap_" + i, jointCap);
            jointGeo.setMaterial(asphaltMat);
            jointGeo.setLocalTranslation(0, 0.001f, -len * 0.5f);
            jointGeo.setLocalRotation(new Quaternion().fromAngleAxis(FastMath.HALF_PI, Vector3f.UNIT_X));
            segmentNode.attachChild(jointGeo);

            Box edgeBox = new Box(0.25f, 0.052f, len * 0.52f);
            Geometry edgeL = new Geometry("EdgeL_" + i, edgeBox);
            edgeL.setMaterial(whiteLineMat);
            edgeL.setLocalTranslation(-10.5f, 0f, 0);
            segmentNode.attachChild(edgeL);

            Geometry edgeR = new Geometry("EdgeR_" + i, edgeBox);
            edgeR.setMaterial(whiteLineMat);
            edgeR.setLocalTranslation(10.5f, 0f, 0);
            segmentNode.attachChild(edgeR);

            if (i % 2 == 0) {
                Box dashBox = new Box(0.18f, 0.052f, len * 0.25f);
                Geometry dashGeo = new Geometry("CenterDash_" + i, dashBox);
                dashGeo.setMaterial(yellowLineMat);
                dashGeo.setLocalTranslation(0f, 0f, 0f);
                segmentNode.attachChild(dashGeo);
            }

            Box curbL = new Box(0.5f, 0.25f, len * 0.52f);
            Geometry curbLGeo = new Geometry("CurbL_" + i, curbL);
            curbLGeo.setMaterial(curbMat);
            curbLGeo.setLocalTranslation(-11.5f, 0.1f, 0);
            curbLGeo.setShadowMode(RenderQueue.ShadowMode.CastAndReceive);
            segmentNode.attachChild(curbLGeo);
            trackCurbs.add(curbLGeo);

            Box curbR = new Box(0.5f, 0.25f, len * 0.52f);
            Geometry curbRGeo = new Geometry("CurbR_" + i, curbR);
            curbRGeo.setMaterial(curbMat);
            curbRGeo.setLocalTranslation(11.5f, 0.1f, 0);
            curbRGeo.setShadowMode(RenderQueue.ShadowMode.CastAndReceive);
            segmentNode.attachChild(curbRGeo);
            trackCurbs.add(curbRGeo);

            segmentNode.setLocalTranslation(mid);
            Quaternion rot = new Quaternion();
            rot.lookAt(dir.normalizeLocal(), Vector3f.UNIT_Y);
            segmentNode.setLocalRotation(rot);

            parentNode.attachChild(segmentNode);
        }
    }

    private void buildFinishArch(Node parentNode, AssetManager assetManager, CircuitType type, Vector3f startPos) {
        Node arch = new Node("FinishArch_" + type.name());

        ColorRGBA archColor = switch (type) {
            case RAINBOW_CIRCUIT ->
                new ColorRGBA(0.95f, 0.85f, 0.1f, 1.0f);
            case DESERT_SPEEDWAY ->
                new ColorRGBA(0.75f, 0.55f, 0.3f, 1.0f);
            case CYBER_CIRCUIT ->
                new ColorRGBA(0.95f, 0.1f, 0.85f, 1.0f);
        };

        Material archMat = createLightedMaterial(assetManager, archColor, ColorRGBA.White, 128f);
        Box pillar = new Box(0.7f, 6.5f, 0.7f);
        Box header = new Box(13.5f, 0.9f, 0.7f);

        Geometry p1 = new Geometry("ArchP1", pillar);
        p1.setMaterial(archMat);
        p1.setLocalTranslation(-12.5f, 6.5f, 0);
        p1.setShadowMode(RenderQueue.ShadowMode.Cast);
        arch.attachChild(p1);

        Geometry p2 = new Geometry("ArchP2", pillar);
        p2.setMaterial(archMat);
        p2.setLocalTranslation(12.5f, 6.5f, 0);
        p2.setShadowMode(RenderQueue.ShadowMode.Cast);
        arch.attachChild(p2);

        Geometry top = new Geometry("ArchTop", header);
        top.setMaterial(archMat);
        top.setLocalTranslation(0, 13f, 0);
        top.setShadowMode(RenderQueue.ShadowMode.Cast);
        arch.attachChild(top);

        arch.setLocalTranslation(startPos.x, startPos.y, startPos.z + 15f);
        parentNode.attachChild(arch);
    }

    private void buildDirectionalArrowSigns(Node parentNode, AssetManager assetManager, CircuitType type, List<Vector3f> waypoints) {
        ColorRGBA arrowColor = switch (type) {
            case RAINBOW_CIRCUIT ->
                new ColorRGBA(0.1f, 0.95f, 0.95f, 1f);
            case DESERT_SPEEDWAY ->
                new ColorRGBA(0.98f, 0.85f, 0.1f, 1f);
            case CYBER_CIRCUIT ->
                new ColorRGBA(0.95f, 0.1f, 0.85f, 1f);
        };

        Material arrowMat = createLightedMaterial(assetManager, arrowColor, ColorRGBA.White, 128f);
        Material postMat = createLightedMaterial(assetManager, new ColorRGBA(0.2f, 0.2f, 0.22f, 1f), ColorRGBA.White, 32f);

        Box signBoard = new Box(2.2f, 1.4f, 0.12f);
        Cylinder post = new Cylinder(12, 12, 0.15f, 3.8f, true);

        for (int i = 2; i < waypoints.size(); i += 2) {
            Vector3f p1 = waypoints.get(i - 1);
            Vector3f p2 = waypoints.get(i);
            Vector3f p3 = waypoints.get((i + 1) % waypoints.size());

            Vector3f dirIn = p2.subtract(p1).normalizeLocal();
            Vector3f dirOut = p3.subtract(p2).normalizeLocal();

            // Detect if this waypoint is a curve
            if (dirIn.dot(dirOut) < 0.92f) {
                Node arrowSign = new Node("ArrowSign_" + i);

                Geometry boardGeo = new Geometry("SignBoard", signBoard);
                boardGeo.setMaterial(arrowMat);
                boardGeo.setLocalTranslation(0, 3.2f, 0);
                boardGeo.setShadowMode(RenderQueue.ShadowMode.Cast);
                arrowSign.attachChild(boardGeo);

                Geometry postGeo = new Geometry("SignPost", post);
                postGeo.setMaterial(postMat);
                postGeo.setLocalRotation(new Quaternion().fromAngleAxis(FastMath.HALF_PI, Vector3f.UNIT_X));
                postGeo.setLocalTranslation(0, 1.9f, 0);
                postGeo.setShadowMode(RenderQueue.ShadowMode.Cast);
                arrowSign.attachChild(postGeo);

                // Position sign on the outside of the curve
                Vector3f sideOffset = dirIn.cross(Vector3f.UNIT_Y).normalizeLocal().mult(13.8f);
                arrowSign.setLocalTranslation(p2.add(sideOffset));

                Quaternion rot = new Quaternion();
                rot.lookAt(dirIn, Vector3f.UNIT_Y);
                arrowSign.setLocalRotation(rot);

                parentNode.attachChild(arrowSign);
            }
        }
    }

    private void build3DTunnelAndGrandstands(Node parentNode, AssetManager assetManager, CircuitType type, List<Vector3f> waypoints) {
        if (waypoints.size() < 6) {
            return;
        }

        // 1. Build 3D Tunnel Arch over Waypoint 4
        Vector3f tunnelPos = waypoints.get(4);
        Node tunnel = new Node("3DTunnel_" + type.name());

        ColorRGBA tunnelColor = switch (type) {
            case RAINBOW_CIRCUIT ->
                new ColorRGBA(0.12f, 0.08f, 0.25f, 1f);
            case DESERT_SPEEDWAY ->
                new ColorRGBA(0.45f, 0.35f, 0.25f, 1f);
            case CYBER_CIRCUIT ->
                new ColorRGBA(0.05f, 0.12f, 0.22f, 1f);
        };

        Material tunnelMat = createLightedMaterial(assetManager, tunnelColor, ColorRGBA.White, 64f);
        Cylinder archOuter = new Cylinder(16, 16, 14.5f, 32f, false);
        Geometry archGeo = new Geometry("TunnelArch", archOuter);
        archGeo.setMaterial(tunnelMat);
        archGeo.setShadowMode(RenderQueue.ShadowMode.CastAndReceive);
        tunnel.attachChild(archGeo);

        tunnel.setLocalTranslation(tunnelPos);
        parentNode.attachChild(tunnel);

        // 2. Build 3D Grandstands along Start Straight
        Vector3f startPos = waypoints.get(0);
        Node grandstand = new Node("3DGrandstand");
        Material standMat = createLightedMaterial(assetManager, new ColorRGBA(0.25f, 0.25f, 0.28f, 1f), ColorRGBA.White, 32f);

        for (int row = 0; row < 4; row++) {
            Box step = new Box(1.2f, 0.4f, 24f);
            Geometry stepGeo = new Geometry("StandStep_" + row, step);
            stepGeo.setMaterial(standMat);
            stepGeo.setLocalTranslation(-18.5f - row * 1.8f, row * 0.8f, 0);
            stepGeo.setShadowMode(RenderQueue.ShadowMode.CastAndReceive);
            grandstand.attachChild(stepGeo);
        }

        grandstand.setLocalTranslation(startPos.x, startPos.y, startPos.z + 40f);
        parentNode.attachChild(grandstand);
    }

    private float getMinDistanceToTrack(float x, float z, List<Vector3f> waypoints) {
        float minDistSq = Float.MAX_VALUE;
        int numWaypoints = waypoints.size();
        for (int i = 0; i < numWaypoints; i++) {
            Vector3f p1 = waypoints.get(i);
            Vector3f p2 = waypoints.get((i + 1) % numWaypoints);
            float dx = p2.x - p1.x;
            float dz = p2.z - p1.z;
            float lenSq = dx * dx + dz * dz;
            float t = 0f;
            if (lenSq > 1e-6f) {
                t = ((x - p1.x) * dx + (z - p1.z) * dz) / lenSq;
                t = FastMath.clamp(t, 0f, 1f);
            }
            float projX = p1.x + t * dx;
            float projZ = p1.z + t * dz;
            float distSq = (x - projX) * (x - projX) + (z - projZ) * (z - projZ);
            if (distSq < minDistSq) {
                minDistSq = distSq;
            }
        }
        return FastMath.sqrt(minDistSq);
    }

    private void buildSceneryAndTrees(Node parentNode, AssetManager assetManager, CircuitType type, List<Vector3f> waypoints) {
        int count = switch (type) {
            case RAINBOW_CIRCUIT ->
                45;
            case DESERT_SPEEDWAY ->
                60;
            case CYBER_CIRCUIT ->
                50;
        };

        Material trunkMat = createLightedMaterial(assetManager, new ColorRGBA(0.32f, 0.18f, 0.08f, 1.0f), ColorRGBA.Black, 1f);

        ColorRGBA leafColor1 = switch (type) {
            case RAINBOW_CIRCUIT ->
                new ColorRGBA(0.85f, 0.2f, 0.95f, 1.0f);
            case DESERT_SPEEDWAY ->
                new ColorRGBA(0.2f, 0.65f, 0.15f, 1.0f);
            case CYBER_CIRCUIT ->
                new ColorRGBA(0.1f, 0.9f, 0.98f, 1.0f);
        };

        ColorRGBA leafColor2 = switch (type) {
            case RAINBOW_CIRCUIT ->
                new ColorRGBA(0.45f, 0.1f, 0.75f, 1.0f);
            case DESERT_SPEEDWAY ->
                new ColorRGBA(0.12f, 0.45f, 0.1f, 1.0f);
            case CYBER_CIRCUIT ->
                new ColorRGBA(0.05f, 0.6f, 0.85f, 1.0f);
        };

        Material leafMat1 = createLightedMaterial(assetManager, leafColor1, ColorRGBA.White, 32f);
        Material leafMat2 = createLightedMaterial(assetManager, leafColor2, ColorRGBA.White, 32f);

        Cylinder trunkShape = new Cylinder(16, 16, 0.45f, 4.5f, true);
        Dome leafLayer1 = new Dome(Vector3f.ZERO, 2, 16, 2.8f, false);
        Dome leafLayer2 = new Dome(Vector3f.ZERO, 2, 16, 2.2f, false);
        Dome leafLayer3 = new Dome(Vector3f.ZERO, 2, 16, 1.5f, false);

        for (int i = 0; i < count; i++) {
            Node tree = new Node("Tree_AAA_" + type.name() + "_" + i);

            Geometry trunk = new Geometry("Trunk", trunkShape);
            trunk.setMaterial(trunkMat);
            trunk.setLocalRotation(new Quaternion().fromAngleAxis(FastMath.HALF_PI, Vector3f.UNIT_X));
            trunk.setLocalTranslation(0, 2.25f, 0);
            trunk.setShadowMode(RenderQueue.ShadowMode.Cast);
            tree.attachChild(trunk);

            // Tier 1 Leaf Layer
            Geometry l1 = new Geometry("LeafTier1", leafLayer1);
            l1.setMaterial(leafMat1);
            l1.setLocalTranslation(0, 3.2f, 0);
            l1.setShadowMode(RenderQueue.ShadowMode.Cast);
            tree.attachChild(l1);

            // Tier 2 Leaf Layer
            Geometry l2 = new Geometry("LeafTier2", leafLayer2);
            l2.setMaterial(leafMat2);
            l2.setLocalTranslation(0, 4.6f, 0);
            l2.setShadowMode(RenderQueue.ShadowMode.Cast);
            tree.attachChild(l2);

            // Tier 3 Top Crown
            Geometry l3 = new Geometry("LeafTier3", leafLayer3);
            l3.setMaterial(leafMat1);
            l3.setLocalTranslation(0, 5.8f, 0);
            l3.setShadowMode(RenderQueue.ShadowMode.Cast);
            tree.attachChild(l3);

            float rx = 0f;
            float rz = 0f;
            boolean valid = false;
            int attempts = 0;
            while (!valid && attempts < 500) {
                rx = (random.nextFloat() - 0.5f) * 420f + 100f;
                rz = (random.nextFloat() - 0.5f) * 420f + 30f;
                if (getMinDistanceToTrack(rx, rz, waypoints) >= 24.0f) {
                    valid = true;
                }
                attempts++;
            }
            if (valid) {
                tree.setLocalTranslation(rx, 0, rz);
                parentNode.attachChild(tree);
            }
        }
    }

    private void spawnItemBoxesAndBoostPads(Node parentNode, AssetManager assetManager, CircuitType type,
            List<Vector3f> waypoints, List<Node> itemBoxGroups, List<Geometry> boostPads) {
        ColorRGBA boxColor = switch (type) {
            case RAINBOW_CIRCUIT ->
                new ColorRGBA(0.98f, 0.75f, 0.05f, 1f);
            case DESERT_SPEEDWAY ->
                new ColorRGBA(0.95f, 0.45f, 0.05f, 1f);
            case CYBER_CIRCUIT ->
                new ColorRGBA(0.05f, 0.95f, 0.85f, 1f);
        };

        Material boxMat = createLightedMaterial(assetManager, boxColor, ColorRGBA.White, 128f);
        Material boostMat = createLightedMaterial(assetManager, new ColorRGBA(0.1f, 0.95f, 0.95f, 1f), ColorRGBA.White, 128f);

        Box itemShape = new Box(0.85f, 0.85f, 0.85f);
        Box boostShape = new Box(3.5f, 0.08f, 2.2f);

        for (int i = 1; i < waypoints.size(); i += 2) {
            Vector3f pos = waypoints.get(i);

            Node boxGroup = new Node("BoxGroup_" + i);
            for (int offset = -4; offset <= 4; offset += 4) {
                Geometry box = new Geometry("ItemBox", itemShape);
                box.setMaterial(boxMat);
                box.setLocalTranslation(pos.x + offset, 0.65f, pos.z);
                box.setShadowMode(RenderQueue.ShadowMode.Cast);
                boxGroup.attachChild(box);
            }
            parentNode.attachChild(boxGroup);
            itemBoxGroups.add(boxGroup);

            Geometry boost = new Geometry("BoostPad", boostShape);
            boost.setMaterial(boostMat);
            boost.setLocalTranslation(pos.x, 0.08f, pos.z + 12f);
            parentNode.attachChild(boost);
            boostPads.add(boost);
        }
    }

    private void spawnTrackCoins(Node parentNode, AssetManager assetManager, CircuitType type,
            List<Vector3f> waypoints, List<Geometry> trackCoins) {
        Cylinder coinShape = new Cylinder(16, 16, 0.45f, 0.12f, true);
        Material coinMat = createLightedMaterial(assetManager, new ColorRGBA(1.0f, 0.85f, 0.1f, 1.0f), ColorRGBA.White, 128f);

        for (int i = 0; i < waypoints.size(); i++) {
            Vector3f pos = waypoints.get(i);
            for (int offset = -3; offset <= 3; offset += 3) {
                Geometry coin = new Geometry("TrackCoin", coinShape);
                coin.setMaterial(coinMat);
                coin.setLocalTranslation(pos.x + offset, 0.6f, pos.z + 5f);
                coin.setLocalRotation(new Quaternion().fromAngleAxis(FastMath.HALF_PI, Vector3f.UNIT_X));
                coin.setShadowMode(RenderQueue.ShadowMode.Cast);
                parentNode.attachChild(coin);
                trackCoins.add(coin);
            }
        }
    }

    private Material createLightedMaterial(AssetManager assetManager, ColorRGBA diffuse, ColorRGBA specular, float shininess) {
        Material mat = new Material(assetManager, "Common/MatDefs/Light/Lighting.j3md");
        mat.setBoolean("UseMaterialColors", true);
        mat.setColor("Diffuse", diffuse);
        mat.setColor("Ambient", diffuse.mult(0.6f));
        mat.setColor("Specular", specular);
        mat.setFloat("Shininess", shininess);
        return mat;
    }

    /**
     * Calculates the total number of primary waypoints defining the specified
     * circuit layout.
     *
     * @param type The circuit type to evaluate.
     * @return The number of waypoints in the track spline.
     */
    public int getWaypointCount(CircuitType type) {
        return generateWaypoints(type).size();
    }
}
