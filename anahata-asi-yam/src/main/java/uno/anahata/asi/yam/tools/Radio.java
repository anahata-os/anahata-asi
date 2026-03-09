/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools;

import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.Future;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.SourceDataLine;
import javazoom.jl.decoder.Decoder;
import javazoom.jl.decoder.JavaLayerException;
import javazoom.jl.player.AudioDeviceBase;
import javazoom.jl.player.Player;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.toolkit.audio.AudioDevice.Type;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AiTool;
import uno.anahata.asi.agi.tool.AiToolException;
import uno.anahata.asi.agi.tool.AiToolParam;
import uno.anahata.asi.agi.tool.AiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * A stateful toolkit for playing internet radio streams.
 * <p>
 * This toolkit maintains its own playback state and hardware routing, 
 * allowing for independent audio management within an AGI session.
 * It features a custom JLayer bridge to route MP3 streams to selected hardware.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for playing internet radio streams.")
public class Radio extends AnahataToolkit {

    /** The curated list of radio stations. */
    public static final Map<String, String> STATIONS;

    static {
        Map<String, String> stations = new LinkedHashMap<>();
        stations.put("SomaFM Groove Salad", "http://ice1.somafm.com/groovesalad-128-mp3");
        stations.put("SomaFM DEF CON Radio", "http://ice1.somafm.com/defcon-128-mp3");
        stations.put("SomaFM Secret Agent", "http://ice1.somafm.com/secretagent-128-mp3");
        stations.put("SomaFM Indie Pop Rocks!", "http://ice1.somafm.com/indiepop-128-mp3");
        stations.put("SomaFM Drone Zone", "http://ice1.somafm.com/dronezone-128-mp3");
        stations.put("SomaFM Cliqhop IDM", "http://ice1.somafm.com/cliqhop-128-mp3");
        stations.put("SomaFM Beat Blender", "http://ice1.somafm.com/beatblender-128-mp3");
        stations.put("SomaFM Fluid", "http://ice1.somafm.com/fluid-128-mp3");
        stations.put("SomaFM Lush", "http://ice1.somafm.com/lush-128-mp3");
        stations.put("SomaFM Space Station Soma", "http://ice1.somafm.com/spacestation-128-mp3");
        stations.put("KEXP Seattle", "https://kexp-mp3-128.streamguys1.com/kexp128.mp3");
        stations.put("WQXR New York", "https://stream.wqxr.org/wqxr");
        stations.put("FIP Paris", "http://icecast.radiofrance.fr/fip-midfi.mp3");
        stations.put("Radio Paradise", "http://stream.radioparadise.com/mp3-128");
        stations.put("Nightride FM (Synthwave)", "https://stream.nightride.fm/nightride.m4a");
        stations.put("NTS Radio 1", "https://stream-relay-geo.ntslive.net/stream");
        STATIONS = Collections.unmodifiableMap(stations);
    }

    /** The URL of the currently playing station. */
    @Getter
    private String currentStationUrl;

    /** Whether the radio is currently playing. */
    @Getter
    private boolean playing;

    /** The selected output device for radio playback. */
    @Getter
    private uno.anahata.asi.toolkit.audio.AudioDevice selectedOutputDevice;

    /** JLayer player instance. */
    private transient Player player;
    /** Current playback task future. */
    private transient Future<?> playbackTask;

    /** {@inheritDoc} */
    @Override
    public void initialize() {
        super.initialize();
        initOutputLineFromDefault();
        
        // Select a random station by default as requested
        if (this.currentStationUrl == null) {
            List<String> urls = new ArrayList<>(STATIONS.values());
            if (!urls.isEmpty()) {
                this.currentStationUrl = urls.get(new Random().nextInt(urls.size()));
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void rebind() {
        super.rebind();
        if (selectedOutputDevice != null) {
            uno.anahata.asi.toolkit.audio.AudioDevice live = uno.anahata.asi.toolkit.audio.AudioDevice.findDevice(Type.OUTPUT, selectedOutputDevice.getId());
            if (live == null) {
                initOutputLineFromDefault();
            } else {
                this.selectedOutputDevice = live;
            }
        } else {
            initOutputLineFromDefault();
        }

        if (playing && currentStationUrl != null) {
            log.info("Attempting to resume radio stream: {}", currentStationUrl);
            start(currentStationUrl);
        }
    }

    private void initOutputLineFromDefault() {
        uno.anahata.asi.toolkit.audio.AudioDevice.listAvailableDevices(Type.OUTPUT).stream()
                .filter(uno.anahata.asi.toolkit.audio.AudioDevice::isDefaultLine)
                .findFirst()
                .ifPresent(this::setSelectedOutputDevice);
    }

    /**
     * Sets the selected output device and fires a property change event.
     * @param device The device to select.
     */
    public void setSelectedOutputDevice(uno.anahata.asi.toolkit.audio.AudioDevice device) {
        uno.anahata.asi.toolkit.audio.AudioDevice old = this.selectedOutputDevice;
        if (Objects.equals(old, device)) {
            return;
        }
        this.selectedOutputDevice = device;
        propertyChangeSupport.firePropertyChange("selectedOutputDevice", old, device);
        
        // Restart playback if changed while playing to route to new hardware
        if (playing && currentStationUrl != null) {
            start(currentStationUrl);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        StringBuilder sb = new StringBuilder("\n## Radio Status\n");
        sb.append("- **Playing**: ").append(playing).append("\n");
        if (currentStationUrl != null) {
            String name = STATIONS.entrySet().stream()
                    .filter(e -> e.getValue().equals(currentStationUrl))
                    .map(Map.Entry::getKey)
                    .findFirst()
                    .orElse("Unknown Station");
            sb.append("- **Current Station**: ").append(name).append(" (").append(currentStationUrl).append(")\n");
        }
        sb.append("- **Output Device**: ").append(selectedOutputDevice != null ? selectedOutputDevice.getName() : "Default").append("\n");
        
        sb.append("\n### Available Radio Stations\n");
        STATIONS.forEach((name, url) -> {
            sb.append("- ").append(name).append(": `").append(url).append("`").append("\n");
        });
        
        ragMessage.addTextPart(sb.toString());
    }

    /**
     * Starts playing a radio station.
     * @param url The station URL.
     * @return Status message.
     */
    @AiTool("Starts playing a specific internet radio station by its URL.")
    public String start(@AiToolParam("The URL of the radio station to play.") String url) {
        stop();

        this.currentStationUrl = url;
        this.playing = true;
        propertyChangeSupport.firePropertyChange("playing", false, true);
        propertyChangeSupport.firePropertyChange("currentStationUrl", null, url);

        final uno.anahata.asi.toolkit.audio.AudioDevice device = selectedOutputDevice;

        playbackTask = getExecutorService().submit(() -> {
            try {
                URLConnection conn = URI.create(url).toURL().openConnection();
                conn.setConnectTimeout(5000);
                conn.setReadTimeout(5000);

                try (InputStream is = conn.getInputStream()) {
                    // BRIDGE: Use our custom reactive bridge to bypass rigid JLayer defaults
                    javazoom.jl.player.AudioDevice jlayerDevice = (device != null) 
                            ? new BridgedJavaSoundAudioDevice(device) 
                            : new javazoom.jl.player.JavaSoundAudioDevice();

                    player = new Player(is, jlayerDevice);
                    player.play();
                }
            } catch (Exception e) {
                log.error("Radio playback failed for: " + url, e);
                playing = false;
                propertyChangeSupport.firePropertyChange("playing", true, false);
            }
        });

        return "Radio playback started.";
    }

    /**
     * Stops the radio.
     * @return Status message.
     */
    @AiTool("Stops the currently playing radio stream.")
    public String stop() {
        if (player != null) {
            player.close();
            player = null;
        }
        if (playbackTask != null) {
            playbackTask.cancel(true);
            playbackTask = null;
        }
        boolean old = playing;
        this.playing = false;
        propertyChangeSupport.firePropertyChange("playing", old, false);
        return "Radio stopped.";
    }

    /**
     * Selects the output device for the radio.
     * @param deviceId The unique ID of the output device.
     * @return Status message.
     */
    @AiTool("Selects a specific device for radio playback.")
    public String selectOutputDevice(@AiToolParam("The unique ID of the output device.") String deviceId) {
        uno.anahata.asi.toolkit.audio.AudioDevice device = uno.anahata.asi.toolkit.audio.AudioDevice.findDevice(Type.OUTPUT, deviceId);
        if (device == null) {
            throw new AiToolException("Device not found: " + deviceId);
        }
        setSelectedOutputDevice(device);
        return "Radio output device selected: " + device.getName();
    }

    /**
     * A specialized JLayer-to-JavaSound bridge that routes streams to a specific {@link uno.anahata.asi.toolkit.audio.AudioDevice}.
     * Ported from JavaSoundAudioDevice (LGPL) to bypass private field restrictions and rigid hardware discovery.
     */
    private static class BridgedJavaSoundAudioDevice extends AudioDeviceBase {
        private final uno.anahata.asi.toolkit.audio.AudioDevice hardwareDevice;
        private SourceDataLine source = null;
        private AudioFormat fmt = null;
        private byte[] byteBuf = new byte[4096];

        public BridgedJavaSoundAudioDevice(uno.anahata.asi.toolkit.audio.AudioDevice hardwareDevice) {
            this.hardwareDevice = hardwareDevice;
        }

        @Override
        protected void openImpl() throws JavaLayerException {
            // Lazy initialization handled in createSource()
        }

        protected void createSource() throws JavaLayerException {
            try {
                Decoder decoder = getDecoder();
                fmt = new AudioFormat(decoder.getOutputFrequency(), 16, decoder.getOutputChannels(), true, false);
                source = hardwareDevice.getOutputLine(fmt);
                source.open(fmt);
                source.start();
            } catch (Exception ex) {
                throw new JavaLayerException("Cannot obtain source audio line for " + hardwareDevice.getName(), ex);
            }
        }

        @Override
        protected void writeImpl(short[] samples, int offs, int len) throws JavaLayerException {
            if (source == null) {
                createSource();
            }
            byte[] b = toByteArray(samples, offs, len);
            source.write(b, 0, len * 2);
        }

        protected byte[] toByteArray(short[] samples, int offs, int len) {
            int byteLen = len * 2;
            if (byteBuf.length < byteLen) {
                byteBuf = new byte[byteLen + 1024];
            }
            int idx = 0;
            while (len-- > 0) {
                short s = samples[offs++];
                byteBuf[idx++] = (byte) s;
                byteBuf[idx++] = (byte) (s >>> 8);
            }
            return byteBuf;
        }

        @Override
        protected void closeImpl() {
            if (source != null) {
                source.stop();
                source.close();
            }
        }

        @Override
        protected void flushImpl() {
            if (source != null) {
                source.drain();
            }
        }

        @Override
        public int getPosition() {
            return (source != null) ? (int) (source.getMicrosecondPosition() / 1000) : 0;
        }
    }
}
