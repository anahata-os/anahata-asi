/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.util;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import lombok.extern.slf4j.Slf4j;
import org.openide.windows.InputOutput;
import org.openide.windows.OutputListener;
import org.openide.windows.OutputWriter;

/**
 * A decorator for the NetBeans {@link InputOutput} system that captures 
 * output and error streams into internal buffers while delegating to 
 * the original implementation.
 * 
 * @author anahata
 */
@Slf4j
public class TeeInputOutput implements InputOutput {

    private final InputOutput delegate;
    private final StringWriter capturedOutWriter = new StringWriter();
    private final StringWriter capturedErrWriter = new StringWriter();
    private final OutputWriter teeOut;
    private final OutputWriter teeErr;

    /**
     * Constructs a new TeeInputOutput by wrapping an existing delegate.
     * 
     * @param delegate The NetBeans InputOutput instance to wrap and mirror.
     */
    public TeeInputOutput(InputOutput delegate) {
        log.info("TeeInputOutput: Constructor called");
        this.delegate = delegate;
        this.teeOut = new TeeOutputWriter(delegate.getOut(), capturedOutWriter);
        this.teeErr = new TeeOutputWriter(delegate.getErr(), capturedErrWriter);
    }

    /**
     * Gets the full content captured from the standard output stream since 
     * this instance was created.
     * 
     * @return The captured standard output as a String.
     */
    public String getCapturedOutput() {
        log.info("TeeInputOutput: getCapturedOutput called");
        return capturedOutWriter.toString();
    }

    /**
     * Gets the full content captured from the error output stream since 
     * this instance was created.
     * 
     * @return The captured error output as a String.
     */
    public String getCapturedError() {
        log.info("TeeInputOutput: getCapturedError called");
        return capturedErrWriter.toString();
    }

    /**
     * {@inheritDoc}
     * <p>Returns the specialized tee output writer that mirrors content to both the 
     * NetBeans console and the internal buffer.</p>
     */
    @Override
    public OutputWriter getOut() {
        log.info("TeeInputOutput: getOut called");
        return teeOut;
    }

    /**
     * {@inheritDoc}
     * <p>Returns the specialized tee error writer that mirrors content to both the 
     * NetBeans console and the internal error buffer.</p>
     */
    @Override
    public OutputWriter getErr() {
        log.info("TeeInputOutput: getErr called");
        return teeErr;
    }

    /**
     * {@inheritDoc}
     * <p>Returns a reader for the captured output buffer.</p>
     */
    @Override
    public Reader getIn() {
        log.info("TeeInputOutput: getIn called");
        return new StringReader(getCapturedOutput());
    }

    /**
     * {@inheritDoc}
     * <p>Closes the input/output session, ensuring all tee writers are flushed and closed.</p>
     */
    @Override
    public void closeInputOutput() {
        log.info("TeeInputOutput: closeInputOutput called");
        teeOut.close();
        teeErr.close();
        delegate.closeInputOutput();
    }

    /**
     * {@inheritDoc}
     * <p>Delegates the closed state check to the original implementation.</p>
     */
    @Override
    public boolean isClosed() {
        log.info("TeeInputOutput: isClosed called");
        return delegate.isClosed();
    }

    /**
     * {@inheritDoc}
     * <p>Delegates the reader flushing to the original implementation.</p>
     */
    @Override
    public Reader flushReader() {
        log.info("TeeInputOutput: flushReader called");
        return delegate.flushReader();
    }

    /**
     * {@inheritDoc}
     * <p>Requests focus for the delegated InputOutput window.</p>
     */
    @Override
    public void select() {
        log.info("TeeInputOutput: select() called");
        delegate.select();
    }

    /**
     * {@inheritDoc}
     * <p>Checks if error output is separated in the delegate.</p>
     */
    @Override
    public boolean isErrSeparated() {
        log.info("TeeInputOutput: isErrSeparated called");
        return delegate.isErrSeparated();
    }

    /**
     * {@inheritDoc}
     * <p>Sets whether error output should be separated in the delegate.</p>
     */
    @Override
    public void setErrSeparated(boolean value) {
        log.info("TeeInputOutput: setErrSeparated called with value: {}", value);
        delegate.setErrSeparated(value);
    }

    /**
     * {@inheritDoc}
     * <p>Checks if the focus is taken by the delegate window.</p>
     */
    @Override
    public boolean isFocusTaken() {
        log.info("TeeInputOutput: isFocusTaken called");
        return delegate.isFocusTaken();
    }

    /**
     * {@inheritDoc}
     * <p>Sets whether focus should be taken by the delegate window.</p>
     */
    @Override
    public void setFocusTaken(boolean value) {
        log.info("TeeInputOutput: setFocusTaken called with value: {}", value);
        delegate.setFocusTaken(value);
    }

    /**
     * {@inheritDoc}
     * <p>Sets the visibility of the output window in the delegate.</p>
     */
    @Override
    public void setOutputVisible(boolean value) {
        log.info("TeeInputOutput: setOutputVisible called with value: {}", value);
        delegate.setOutputVisible(value);
    }

    /**
     * {@inheritDoc}
     * <p>Sets the visibility of the error window in the delegate.</p>
     */
    @Override
    public void setErrVisible(boolean value) {
        log.info("TeeInputOutput: setErrVisible called with value: {}", value);
        delegate.setErrVisible(value);
    }

    /**
     * {@inheritDoc}
     * <p>Sets the visibility of the input window in the delegate.</p>
     */
    @Override
    public void setInputVisible(boolean value) {
        log.info("TeeInputOutput: setInputVisible called with value: {}", value);
        delegate.setInputVisible(value);
    }

    /**
     * A specialized {@link OutputWriter} that writes to two different 
     * {@link Writer} instances simultaneously.
     */
    @Slf4j
    private static class TeeOutputWriter extends OutputWriter {
        private final Writer w1;
        private final Writer w2;
        private volatile boolean errorOccurred = false;

        /**
         * Constructs a new TeeOutputWriter.
         * 
         * @param w1 The primary writer (usually the NetBeans output writer).
         * @param w2 The secondary writer (the internal buffer).
         */
        public TeeOutputWriter(Writer w1, Writer w2) {
            super(w1);
            this.w1 = w1;
            this.w2 = w2;
            log.info("TeeOutputWriter: Constructor called");
        }

        /**
         * Handles IOExceptions by logging them once to prevent log flooding.
         * 
         * @param e The exception to handle.
         */
        private void handleException(IOException e) {
            if (!errorOccurred) {
                errorOccurred = true;
                log.warn("IOException in TeeOutputWriter. Further exceptions will be suppressed.", e);
            }
        }

        /**
         * {@inheritDoc}
         * <p>Writes a character to both underlying writers.</p>
         */
        @Override
        public void write(int c) {
            try { w1.write(c); w2.write(c); } catch (IOException e) { handleException(e); }
        }

        /**
         * {@inheritDoc}
         * <p>Writes a portion of an array of characters to both underlying writers.</p>
         */
        @Override
        public void write(char[] cbuf, int off, int len) {
            try { w1.write(cbuf, off, len); w2.write(cbuf, off, len); } catch (IOException e) { handleException(e); }
        }

        /**
         * {@inheritDoc}
         * <p>Writes a portion of a string to both underlying writers.</p>
         */
        @Override
        public void write(String s, int off, int len) {
            try { w1.write(s, off, len); w2.write(s, off, len); } catch (IOException e) { handleException(e); }
        }

        /**
         * {@inheritDoc}
         * <p>Flushes both underlying writers.</p>
         */
        @Override
        public void flush() {
            log.info("TeeOutputWriter: flush called");
            try { w1.flush(); w2.flush(); } catch (IOException e) { handleException(e); }
        }

        /**
         * {@inheritDoc}
         * <p>Closes both underlying writers.</p>
         */
        @Override
        public void close() {
            log.info("TeeOutputWriter: close called");
            try { w1.close(); w2.close(); } catch (IOException e) { handleException(e); }
        }

        /**
         * {@inheritDoc}
         * <p>Resets the primary writer if it supports resetting.</p>
         */
        @Override
        public void reset() throws IOException {
            log.info("TeeOutputWriter: reset called");
            if (w1 instanceof OutputWriter) {
                ((OutputWriter) w1).reset();
            }
        }

        /**
         * {@inheritDoc}
         * <p>Prints a line of text to both writers, handling the optional 
         * listener for the primary writer.</p>
         */
        @Override
        public void println(String s, OutputListener l) {
            try {
                if (w1 instanceof OutputWriter) {
                    ((OutputWriter) w1).println(s, l);
                } else {
                    w1.write(s);
                    w1.write(System.lineSeparator());
                }
                w2.write(s);
                w2.write(System.lineSeparator());
            } catch (IOException e) {
                handleException(e);
            }
        }
    }
}