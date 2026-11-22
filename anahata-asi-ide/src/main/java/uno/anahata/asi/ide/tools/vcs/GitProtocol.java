/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.tools.vcs;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Supported network transport protocols for remote Git repository connections.
 * <p>
 * Standardizes protocol selection between SSH (recommended for pre-configured key agents
 * and passwordless authentication) and HTTPS across IDE environments.
 * </p>
 *
 * @author Anahata
 */
@Schema(description = "Git network transport protocol: SSH (default, key-based) or HTTPS.")
public enum GitProtocol {

    /**
     * Secure Shell protocol (e.g. {@code git@github.com:org/repo.git}).
     * <p>
     * Recommended default for seamless push and fetch operations using local SSH keys
     * and credentials managed by the host environment or system agent.
     * </p>
     */
    @Schema(description = "Secure Shell protocol (e.g. git@github.com:org/repo.git). Recommended default.")
    SSH,

    /**
     * Hypertext Transfer Protocol Secure (e.g. {@code https://github.com/org/repo.git}).
     * <p>
     * Standard web protocol, suitable for public anonymous checkouts or token-based authentication.
     * </p>
     */
    @Schema(description = "Hypertext Transfer Protocol Secure (e.g. https://github.com/org/repo.git).")
    HTTPS
}
