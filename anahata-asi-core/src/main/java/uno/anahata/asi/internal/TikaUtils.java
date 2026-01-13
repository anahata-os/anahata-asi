/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.internal;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import org.apache.tika.Tika;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.parser.AutoDetectParser;
import org.apache.tika.parser.ParseContext;
import org.apache.tika.sax.BodyContentHandler;

public final class TikaUtils {

    private static final Tika TIKA = new Tika();

    /**
     * Detects the MIME type of a given file.
     *
     * @param file The file to inspect.
     * @return The detected MIME type (e.g., "image/png", "application/pdf").
     * @throws Exception if an error occurs during detection.
     */
    public static String detectMimeType(File file) throws Exception {
        return TIKA.detect(file);
    }

    /**
     * Detects the file type and parses the text content from a given file.
     *
     * @param file The file to parse.
     * @return The extracted text content.
     * @throws Exception if an error occurs during parsing.
     */
    public static String detectAndParse(File file) throws Exception {
        AutoDetectParser parser = new AutoDetectParser();
        BodyContentHandler handler = new BodyContentHandler(-1); // -1 for no write limit
        Metadata metadata = new Metadata();
        ParseContext context = new ParseContext();

        try (InputStream stream = new FileInputStream(file)) {
            parser.parse(stream, handler, metadata, context);
            return handler.toString();
        }
    }

    /**
     * Infers a file extension from a MIME type.
     * 
     * @param mimeType The MIME type (e.g., "image/png").
     * @return The extension including the dot (e.g., ".png"), or ".bin" if unknown.
     */
    public static String getExtension(String mimeType) {
        if (mimeType == null) return ".bin";
        return switch (mimeType.toLowerCase()) {
            case "image/png" -> ".png";
            case "image/jpeg", "image/jpg" -> ".jpg";
            case "image/gif" -> ".gif";
            case "audio/wav", "audio/x-wav" -> ".wav";
            case "audio/mpeg", "audio/mp3" -> ".mp3";
            case "application/pdf" -> ".pdf";
            case "text/plain" -> ".txt";
            case "text/html" -> ".html";
            case "application/json" -> ".json";
            default -> ".bin";
        };
    }
}
