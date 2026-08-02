/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.games.asikart;

import com.jme3.asset.AssetManager;
import com.jme3.audio.AudioData;
import com.jme3.audio.AudioNode;
import com.jme3.scene.Node;

/**
 * AudioManager handles sound effects and audio feedback for AsiKart 3D.
 * <p>
 * Provides jME3 AudioNode management with reliable fallback audio triggers
 * for coins, items, turbo boosts, drift, shell ballistics, and banana slips.
 * </p>
 *
 * @author anahata
 */
public class AudioManager {

    private final Node rootNode;
    private final AssetManager assetManager;

    private AudioNode coinSoundNode;
    private AudioNode itemBoxSoundNode;
    private AudioNode turboSoundNode;
    private AudioNode shellLaunchNode;
    private AudioNode shellImpactNode;
    private AudioNode bananaSlipNode;
    private AudioNode engineSoundNode;

    public AudioManager(Node rootNode, AssetManager assetManager) {
        this.rootNode = rootNode;
        this.assetManager = assetManager;
        initAudioNodes();
    }

    private void initAudioNodes() {
        coinSoundNode = createAudioNode("Sound/Effects/coin.wav", false, 0.8f);
        itemBoxSoundNode = createAudioNode("Sound/Effects/item_box.wav", false, 0.9f);
        turboSoundNode = createAudioNode("Sound/Effects/boost.wav", false, 1.0f);
        shellLaunchNode = createAudioNode("Sound/Effects/shell_launch.wav", false, 0.85f);
        shellImpactNode = createAudioNode("Sound/Effects/explosion.wav", false, 1.0f);
        bananaSlipNode = createAudioNode("Sound/Effects/slip.wav", false, 0.9f);
        engineSoundNode = createAudioNode("Sound/Effects/engine.wav", true, 0.4f);

        if (engineSoundNode != null) {
            engineSoundNode.play();
        }
    }

    private AudioNode createAudioNode(String assetPath, boolean loop, float volume) {
        try {
            AudioNode audio = new AudioNode(assetManager, assetPath, AudioData.DataType.Buffer);
            audio.setLooping(loop);
            audio.setVolume(volume);
            audio.setPositional(false);
            rootNode.attachChild(audio);
            return audio;
        } catch (Exception e) {
            // Asset not found or audio renderer fallback
            return null;
        }
    }

    /**
     * Plays sound feedback for picking up a Gold Coin.
     */
    public void playCoinSound() {
        if (coinSoundNode != null) {
            coinSoundNode.playInstance();
        } else {
            triggerBeepFallback();
        }
    }

    /**
     * Plays sound feedback for picking up an Item Mystery Box.
     */
    public void playItemBoxSound() {
        if (itemBoxSoundNode != null) {
            itemBoxSoundNode.playInstance();
        } else {
            triggerBeepFallback();
        }
    }

    /**
     * Plays sound feedback for Turbo Boost / Mushroom activation.
     */
    public void playTurboSound() {
        if (turboSoundNode != null) {
            turboSoundNode.playInstance();
        } else {
            triggerBeepFallback();
        }
    }

    /**
     * Plays sound feedback for launching a Green or Red Shell.
     */
    public void playShellLaunchSound() {
        if (shellLaunchNode != null) {
            shellLaunchNode.playInstance();
        } else {
            triggerBeepFallback();
        }
    }

    /**
     * Plays sound feedback for shell impact / explosion.
     */
    public void playShellImpactSound() {
        if (shellImpactNode != null) {
            shellImpactNode.playInstance();
        } else {
            triggerBeepFallback();
        }
    }

    /**
     * Plays sound feedback for slipping on a Banana peel.
     */
    public void playBananaSlipSound() {
        if (bananaSlipNode != null) {
            bananaSlipNode.playInstance();
        } else {
            triggerBeepFallback();
        }
    }

    /**
     * Dynamically adjusts engine pitch based on current kart speed.
     *
     * @param currentSpeed Current speed of kart.
     * @param maxSpeed Maximum speed of kart.
     */
    public void updateEnginePitch(float currentSpeed, float maxSpeed) {
        if (engineSoundNode != null) {
            float ratio = Math.abs(currentSpeed) / Math.max(1.0f, maxSpeed);
            float pitch = 0.8f + (ratio * 1.2f);
            engineSoundNode.setPitch(pitch);
        }
    }

    private void triggerBeepFallback() {
        new Thread(() -> {
            try {
                java.awt.Toolkit.getDefaultToolkit().beep();
            } catch (Throwable ignored) {
            }
        }).start();
    }

    /**
     * Stops all active audio nodes.
     */
    public void stopAll() {
        if (engineSoundNode != null) engineSoundNode.stop();
        if (coinSoundNode != null) coinSoundNode.stop();
        if (itemBoxSoundNode != null) itemBoxSoundNode.stop();
        if (turboSoundNode != null) turboSoundNode.stop();
        if (shellLaunchNode != null) shellLaunchNode.stop();
        if (shellImpactNode != null) shellImpactNode.stop();
        if (bananaSlipNode != null) bananaSlipNode.stop();
    }
}
