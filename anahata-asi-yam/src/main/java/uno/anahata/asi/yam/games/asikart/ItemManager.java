/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.games.asikart;

import com.jme3.asset.AssetManager;
import com.jme3.material.Material;
import com.jme3.material.RenderState.BlendMode;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.renderer.queue.RenderQueue;
import com.jme3.scene.Geometry;
import com.jme3.scene.Node;
import com.jme3.scene.shape.Box;
import com.jme3.scene.shape.Cylinder;
import com.jme3.scene.shape.Sphere;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * ItemManager handles 3D game items in AsiKart 3D.
 * <p>
 * Manages stationary rotating Item Mystery Boxes (centered at height Y=0.65f),
 * 3D Gold Coins, Green/Red Shell ballistics with collision logic, and Bananas.
 * </p>
 *
 * @author anahata
 */
public class ItemManager {

    /**
     * Listener interface for item box pickups.
     */
    public interface ItemPickupListener {
        void onItemPickedUp(String itemType);
    }

    private final Node rootNode;
    private final AssetManager assetManager;
    private final AudioManager audioManager;
    private final Random random = new Random();

    private final List<Geometry> itemBoxGeometries = new ArrayList<>();
    private final List<Geometry> trackCoins = new ArrayList<>();
    private final List<Node> activeShells = new ArrayList<>();
    private final List<Vector3f> shellVelocities = new ArrayList<>();
    private final List<Boolean> shellIsRed = new ArrayList<>();
    private final List<Node> activeBananas = new ArrayList<>();

    private int coinsCollected = 0;

    public ItemManager(Node rootNode, AssetManager assetManager, AudioManager audioManager) {
        this.rootNode = rootNode;
        this.assetManager = assetManager;
        this.audioManager = audioManager;
    }

    /**
     * Spawns stationary 3D rotating Item Mystery Boxes at track waypoints.
     * Mystery boxes are placed at height Y=0.65f and rotated individually
     * around their local center so they do NOT swing across the track.
     *
     * @param trackWaypoints Waypoints defining the track path.
     */
    public void spawnItemBoxes(List<Vector3f> trackWaypoints) {
        Material boxMat = createLightedMaterial(new ColorRGBA(0.98f, 0.75f, 0.05f, 1.0f), ColorRGBA.White, 128f);
        Box itemShape = new Box(0.85f, 0.85f, 0.85f);

        for (int i = 1; i < trackWaypoints.size(); i += 2) {
            Vector3f pos = trackWaypoints.get(i);
            for (int offset = -4; offset <= 4; offset += 4) {
                Geometry boxGeo = new Geometry("ItemBox_" + i + "_" + offset, itemShape);
                boxGeo.setMaterial(boxMat);
                // Placed at Y=0.65f
                boxGeo.setLocalTranslation(pos.x + offset, 0.65f, pos.z);
                boxGeo.setShadowMode(RenderQueue.ShadowMode.Cast);
                rootNode.attachChild(boxGeo);
                itemBoxGeometries.add(boxGeo);
            }
        }
    }

    /**
     * Spawns 3D Gold Coins along track waypoints.
     *
     * @param trackWaypoints Waypoints defining the track path.
     */
    public void spawnCoins(List<Vector3f> trackWaypoints) {
        Cylinder coinShape = new Cylinder(16, 16, 0.55f, 0.12f, true);
        Material coinMat = createLightedMaterial(new ColorRGBA(1.0f, 0.85f, 0.1f, 1.0f), ColorRGBA.White, 128f);

        for (int i = 0; i < trackWaypoints.size(); i++) {
            Vector3f pos = trackWaypoints.get(i);
            for (int offset = -3; offset <= 3; offset += 3) {
                Geometry coin = new Geometry("TrackCoin_" + i + "_" + offset, coinShape);
                coin.setMaterial(coinMat);
                // Placed upright vertically at Y=1.1f above ground
                coin.setLocalTranslation(pos.x + offset, 1.1f, pos.z + 5f);
                coin.setLocalRotation(new Quaternion().fromAngleAxis(FastMath.HALF_PI, Vector3f.UNIT_Y));
                coin.setShadowMode(RenderQueue.ShadowMode.Cast);
                rootNode.attachChild(coin);
                trackCoins.add(coin);
            }
        }
    }

    /**
     * Main update loop for items, coins, shells, and bananas.
     *
     * @param tpf Time per frame.
     * @param playerKartNode Node of the player's kart.
     * @param aiKarts List of AI competitor kart nodes.
     * @param currentItem Current item held by player ("NONE" if empty).
     * @param listener Callback when an item box is picked up.
     */
    public void update(float tpf, Node playerKartNode, List<Node> aiKarts, String currentItem, ItemPickupListener listener) {
        // 1. In-place rotation of Item Mystery Boxes around local Y axis (no swinging!)
        for (Geometry boxGeo : itemBoxGeometries) {
            boxGeo.rotate(0, tpf * 2.5f, 0);

            // Item Box Collision with Player
            if ("NONE".equals(currentItem) && playerKartNode != null) {
                if (playerKartNode.getLocalTranslation().distance(boxGeo.getWorldTranslation()) < 2.4f) {
                    if (audioManager != null) {
                        audioManager.playItemBoxSound();
                    }
                    String[] items = {"MUSHROOM", "GREEN_SHELL", "RED_SHELL", "BANANA", "STAR"};
                    String awarded = items[random.nextInt(items.length)];
                    if (listener != null) {
                        listener.onItemPickedUp(awarded);
                    }
                }
            }

            // Item Box Collision with AI Competitors
            for (Node ai : aiKarts) {
                if (ai.getLocalTranslation().distance(boxGeo.getWorldTranslation()) < 2.4f) {
                    if (random.nextFloat() < 0.05f) {
                        Vector3f aiPos = ai.getLocalTranslation();
                        float aiHeading = ai.getLocalRotation().toAngles(null)[1];
                        if (random.nextBoolean()) {
                            launchGreenShell(aiPos.add(0, 0.4f, 0), aiHeading);
                        } else {
                            dropBanana(aiPos.subtract(new Vector3f(FastMath.sin(aiHeading), 0, FastMath.cos(aiHeading)).mult(2f)));
                        }
                    }
                }
            }
        }

        // 2. Vertical Floating Sine Wave & Clean Pickup Collisions for 3D Gold Coins
        float coinTimer = System.currentTimeMillis() * 0.003f;
        for (int i = trackCoins.size() - 1; i >= 0; i--) {
            Geometry coin = trackCoins.get(i);
            coin.rotate(0, tpf * 4.5f, 0);

            // Smooth floating sine wave height
            Vector3f pos = coin.getLocalTranslation();
            coin.setLocalTranslation(pos.x, 1.1f + FastMath.sin(coinTimer + i) * 0.22f, pos.z);

            if (playerKartNode != null && playerKartNode.getLocalTranslation().distance(coin.getLocalTranslation()) < 2.8f) {
                coinsCollected++;
                if (audioManager != null) {
                    audioManager.playCoinSound();
                }
                coin.setShadowMode(RenderQueue.ShadowMode.Off);
                coin.removeFromParent();
                rootNode.detachChild(coin);
                trackCoins.remove(i);
            }
        }

        // 3. Green / Red Shell Ballistics & Collision Logic
        for (int i = activeShells.size() - 1; i >= 0; i--) {
            Node shell = activeShells.get(i);
            Vector3f vel = shellVelocities.get(i);
            boolean isRed = shellIsRed.get(i);

            if (isRed && !aiKarts.isEmpty()) {
                Node target = findNearestAiKart(shell.getLocalTranslation(), aiKarts);
                if (target != null) {
                    Vector3f targetDir = target.getLocalTranslation().subtract(shell.getLocalTranslation()).normalizeLocal();
                    vel.interpolateLocal(targetDir.mult(85f), tpf * 6.5f);
                }
            }

            shell.move(vel.mult(tpf));

            boolean hit = false;
            for (Node ai : aiKarts) {
                if (shell.getLocalTranslation().distance(ai.getLocalTranslation()) < 2.5f) {
                    ai.rotate(0, FastMath.PI, 0);
                    if (audioManager != null) {
                        audioManager.playShellImpactSound();
                    }
                    hit = true;
                    break;
                }
            }

            if (!hit && playerKartNode != null && shell.getLocalTranslation().distance(playerKartNode.getLocalTranslation()) < 2.5f) {
                playerKartNode.rotate(0, FastMath.PI, 0);
                if (audioManager != null) {
                    audioManager.playShellImpactSound();
                }
                hit = true;
            }

            if (hit || shell.getLocalTranslation().length() > 650f) {
                shell.removeFromParent();
                activeShells.remove(i);
                shellVelocities.remove(i);
                shellIsRed.remove(i);
            }
        }

        // 4. Bananas Collision Logic
        for (int i = activeBananas.size() - 1; i >= 0; i--) {
            Node banana = activeBananas.get(i);

            if (playerKartNode != null && playerKartNode.getLocalTranslation().distance(banana.getLocalTranslation()) < 2.2f) {
                playerKartNode.rotate(0, FastMath.TWO_PI, 0);
                if (audioManager != null) {
                    audioManager.playBananaSlipSound();
                }
                banana.removeFromParent();
                activeBananas.remove(i);
                continue;
            }

            for (Node ai : aiKarts) {
                if (ai.getLocalTranslation().distance(banana.getLocalTranslation()) < 2.2f) {
                    ai.rotate(0, FastMath.TWO_PI, 0);
                    if (audioManager != null) {
                        audioManager.playBananaSlipSound();
                    }
                    banana.removeFromParent();
                    activeBananas.remove(i);
                    break;
                }
            }
        }
    }

    /**
     * Launches a Green Shell linearly in the heading direction.
     *
     * @param spawnPos Position to spawn shell.
     * @param heading Direction angle in radians.
     */
    public void launchGreenShell(Vector3f spawnPos, float heading) {
        Node shellNode = createShellNode(ColorRGBA.Green);
        shellNode.setLocalTranslation(spawnPos.x, 0.5f, spawnPos.z);
        rootNode.attachChild(shellNode);

        activeShells.add(shellNode);
        shellVelocities.add(new Vector3f(FastMath.sin(heading), 0, FastMath.cos(heading)).mult(75f));
        shellIsRed.add(false);

        if (audioManager != null) {
            audioManager.playShellLaunchSound();
        }
    }

    /**
     * Launches a Red Homing Shell in the heading direction.
     *
     * @param spawnPos Position to spawn shell.
     * @param heading Direction angle in radians.
     */
    public void launchRedShell(Vector3f spawnPos, float heading) {
        Node shellNode = createShellNode(ColorRGBA.Red);
        shellNode.setLocalTranslation(spawnPos.x, 0.5f, spawnPos.z);
        rootNode.attachChild(shellNode);

        activeShells.add(shellNode);
        shellVelocities.add(new Vector3f(FastMath.sin(heading), 0, FastMath.cos(heading)).mult(85f));
        shellIsRed.add(true);

        if (audioManager != null) {
            audioManager.playShellLaunchSound();
        }
    }

    /**
     * Drops a Banana peel behind the kart.
     *
     * @param spawnPos Position to spawn banana.
     */
    public void dropBanana(Vector3f spawnPos) {
        Node bananaNode = new Node("Banana");
        Cylinder b = new Cylinder(12, 12, 0.25f, 0.9f, true);
        Geometry bGeo = new Geometry("BananaGeo", b);
        Material bMat = createLightedMaterial(ColorRGBA.Yellow, ColorRGBA.White, 64f);
        bGeo.setMaterial(bMat);
        bananaNode.attachChild(bGeo);

        bananaNode.setLocalTranslation(spawnPos.x, 0.3f, spawnPos.z);
        rootNode.attachChild(bananaNode);

        activeBananas.add(bananaNode);
    }

    private Node createShellNode(ColorRGBA color) {
        Node shellNode = new Node("Shell");
        Sphere s = new Sphere(16, 16, 0.55f);
        Geometry sGeo = new Geometry("ShellGeo", s);
        Material sMat = createLightedMaterial(color, ColorRGBA.White, 128f);
        sGeo.setMaterial(sMat);
        shellNode.attachChild(sGeo);
        return shellNode;
    }

    private Node findNearestAiKart(Vector3f position, List<Node> aiKarts) {
        Node nearest = null;
        float minDist = Float.MAX_VALUE;
        for (Node ai : aiKarts) {
            float dist = position.distance(ai.getLocalTranslation());
            if (dist < minDist) {
                minDist = dist;
                nearest = ai;
            }
        }
        return nearest;
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

    public int getCoinsCollected() {
        return coinsCollected;
    }

    public void setCoinsCollected(int coins) {
        this.coinsCollected = coins;
    }
}
