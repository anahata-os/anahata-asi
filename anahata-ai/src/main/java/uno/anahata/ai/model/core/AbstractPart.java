/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/pablo-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Força Barça!
 */
package uno.anahata.ai.model.core;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.ai.Chat;
import uno.anahata.ai.config.ChatConfig;

/**
 * The abstract base class for all components of a {@link AbstractMessage}.
 * <p>
 * This rich, hierarchical model allows for type-safe handling of different
 * content types (text, tool calls, etc.) and provides common functionality
 * like a back-reference to the parent message and a pruning flag.
 */
@Getter
@Setter
public abstract class AbstractPart {
    /**
     * A unique, sequential identifier assigned to this part when it is added to a chat.
     */
    private long sequentialId;

    /**
     * A backward reference to the Message that contains this part.
     * This is for runtime convenience and is ignored during schema generation
     * to keep the public contract clean.
     */
    @JsonIgnore
    private AbstractMessage message;

    /**
     * A three-state flag for explicit pruning control.
     * - {@code true}: This part is explicitly pruned and will be hidden.
     * - {@code false}: This part is "pinned" and will never be auto-pruned.
     * - {@code null}: (Default) Auto-pruning is active based on {@code turnsToKeep}.
     */
    private Boolean pruned = null;

    /**
     * An explicit, instance-level override for the number of user turns this
     * part should remain in the active context. If {@code null}, the effective
     * value is determined by the part type's default, resolved via the
     * {@link #getDefaultTurnsToKeep()} template method.
     */
    private Integer turnsToKeep = null;

    /**
     * Calculates the EFFECTIVE pruned state of this part.
     * A part is effectively pruned if it was explicitly pruned, if its PARENT MESSAGE
     * was explicitly pruned, or if its time-to-live has expired.
     * @return {@code true} if the part is effectively pruned, {@code false} otherwise.
     */
    public boolean isEffectivelyPruned() {
        // 1. Check the explicit flag on the part itself.
        if (Boolean.TRUE.equals(this.pruned)) {
            return true;
        }

        // 2. BREAK THE RECURSION: Check the PARENT'S EXPLICIT state, not its effective state.
        if (Boolean.TRUE.equals(getMessage().isPruned())) {
            return true;
        }

        // 3. Finally, check the time-based auto-pruning logic.
        if (getTurnsLeft() <= 0) {
            return true;
        }

        return false;
    }

    /**
     * Calculates the remaining turns before this part is auto-pruned.
     * @return The number of turns left, or a large positive number for indefinite retention.
     */
    public int getTurnsLeft() {
        int effectiveTurns = getEffectiveTurnsToKeep();
        if (effectiveTurns < 0) {
            return Integer.MAX_VALUE; // Indefinite
        }
        return effectiveTurns - getMessage().getDepth();
    }

    /**
     * The definitive method for resolving the retention policy for this part.
     * It follows the Template Method pattern, first checking for an explicit
     * instance-level override before falling back to the subclass-specific
     * default.
     * @return The effective number of turns to keep this part.
     */
    public final int getEffectiveTurnsToKeep() {
        if (turnsToKeep != null) {
            return turnsToKeep;
        }
        return getDefaultTurnsToKeep();
    }

    /**
     * Template method hook for subclasses to provide their specific default
     * retention policy. This is the fallback value used when no explicit
     * {@code turnsToKeep} is set on the instance.
     * @return The default number of turns for this part type.
     */
    protected abstract int getDefaultTurnsToKeep();

    //<editor-fold defaultstate="collapsed" desc="Convenience Methods">
    @JsonIgnore
    public Chat getChat() {
        return getMessage().getChat();
    }

    @JsonIgnore
    public ChatConfig getChatConfig() {
        return getChat().getConfig();
    }
    //</editor-fold>

    /**
     * Returns the content of the part as a simple string.
     * This is implemented by subclasses.
     * @return The text representation of the part.
     */
    public abstract String asText();
}
