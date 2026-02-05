/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.ide;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AiExecutors;

/**
 * A global, singleton monitor that periodically polls the NetBeans IDE state
 * (open windows, output tabs) on a background thread. This prevents context 
 * providers from hijacking the EDT during prompt assembly.
 * 
 * @author anahata
 */
@Slf4j
public class NetBeansIdeMonitor {

    private static NetBeansIdeMonitor instance;

    /**
     * Gets the singleton monitor instance.
     * @return The monitor instance.
     */
    public static synchronized NetBeansIdeMonitor getInstance() {
        if (instance == null) {
            instance = new NetBeansIdeMonitor();
        }
        return instance;
    }

    private final ScheduledExecutorService scheduler = AiExecutors.newScheduledThreadPool("ide-monitor", 1);
    
    @Getter
    private String openTopComponentsMarkdown = "Initializing...";
    
    @Getter
    private String outputTabsMarkdown = "Initializing...";

    private NetBeansIdeMonitor() {
        // Poll every 2 seconds for better responsiveness
        scheduler.scheduleAtFixedRate(this::refresh, 0, 2, TimeUnit.SECONDS);
    }

    /**
     * Refreshes the cached IDE state.
     */
    public void refresh() {
        try {
            long start = System.currentTimeMillis();
            this.openTopComponentsMarkdown = NetBeansTopComponents.getMarkdownReport();
            this.outputTabsMarkdown = NetBeansOutput.getMarkdownReport();
            log.debug("IDE state refreshed in {}ms", System.currentTimeMillis() - start);
        } catch (Exception e) {
            log.error("Failed to refresh NetBeans IDE state", e);
        }
    }

    /**
     * Shuts down the monitor and its background thread.
     */
    public void shutdown() {
        scheduler.shutdownNow();
    }
}
