/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

import java.util.List;

/**
 * A simple interface for observing a stream of values, typically used for
 * asynchronous token streaming from an AI model.
 *
 * @author anahata-gemini-pro-2.5
 * @param <R> The type of Response in the stream.
 */
public interface StreamObserver<R extends Response<?>> {

    /**
     * Called when the stream starts, providing the initial candidate messages.
     * 
     * @param candidates The list of candidate messages that will be updated.
     */
    void onStart(List<? extends AbstractModelMessage> candidates);

    /**
     * Called when a new value is available in the stream.
     *
     * @param value The next value.
     */
    void onNext(R value);

    /**
     * Called when the stream has completed successfully.
     */
    void onComplete();

    /**
     * Called when an error occurs in the stream.
     *
     * @param t The error that occurred.
     */
    void onError(Throwable t);
}
