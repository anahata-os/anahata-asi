/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * Robust utility for parsing primitive and wrapper numbers from heterogeneous input objects.
 * <p>
 * Handles numeric types, localized formatting with commas/underscores/spaces,
 * blank strings, and textual placeholders like "N/A" or "null".
 * </p>
 * 
 * @author anahata
 */
public final class TypeParsers {

    /**
     * Private constructor to prevent instantiation of utility class.
     */
    private TypeParsers() {
    }

    /**
     * Parses a heterogeneous collection or object into a typed {@link List} of enum constants.
     *
     * @param <E> The enum type.
     * @param val The source value (Collection, Enum, String, or null).
     * @param enumClass The target enum class.
     * @return A list of matching enum constants, never null.
     */
    public static <E extends Enum<E>> List<E> parseEnumList(Object val, Class<E> enumClass) {
        if (val == null) {
            return Collections.emptyList();
        }
        if (val instanceof Collection<?> col) {
            List<E> list = new ArrayList<>();
            for (Object item : col) {
                if (enumClass.isInstance(item)) {
                    list.add(enumClass.cast(item));
                } else if (item instanceof String s) {
                    for (E constant : enumClass.getEnumConstants()) {
                        if (constant.name().equalsIgnoreCase(s.trim())) {
                            list.add(constant);
                            break;
                        }
                    }
                }
            }
            return list;
        }
        if (enumClass.isInstance(val)) {
            return List.of(enumClass.cast(val));
        }
        return Collections.emptyList();
    }

    /**
     * Parses an object value into an {@link Integer}.
     *
     * @param val The raw value (Number, String, or null).
     * @return The parsed Integer, or {@code null} if blank, invalid, or null.
     */
    public static Integer parseInteger(Object val) {
        if (val == null) {
            return null;
        }
        if (val instanceof Number num) {
            return num.intValue();
        }
        String s = val.toString().trim();
        if (s.isEmpty() || s.equalsIgnoreCase("N/A") || s.equalsIgnoreCase("null")) {
            return null;
        }
        try {
            return Integer.parseInt(s.replaceAll("[,_ ]", ""));
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /**
     * Parses an object value into a {@link Float}.
     *
     * @param val The raw value (Number, String, or null).
     * @return The parsed Float, or {@code null} if blank, invalid, or null.
     */
    public static Float parseFloat(Object val) {
        if (val == null) {
            return null;
        }
        if (val instanceof Number num) {
            return num.floatValue();
        }
        String s = val.toString().trim();
        if (s.isEmpty() || s.equalsIgnoreCase("N/A") || s.equalsIgnoreCase("null")) {
            return null;
        }
        try {
            return Float.parseFloat(s.replaceAll("[,_ ]", ""));
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /**
     * Parses an object value into a {@link Double}.
     *
     * @param val The raw value (Number, String, or null).
     * @return The parsed Double, or {@code null} if blank, invalid, or null.
     */
    public static Double parseDouble(Object val) {
        if (val == null) {
            return null;
        }
        if (val instanceof Number num) {
            return num.doubleValue();
        }
        String s = val.toString().trim();
        if (s.isEmpty() || s.equalsIgnoreCase("N/A") || s.equalsIgnoreCase("null")) {
            return null;
        }
        try {
            return Double.parseDouble(s.replaceAll("[,_ ]", ""));
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /**
     * Parses an object value into a {@link Long}.
     *
     * @param val The raw value (Number, String, or null).
     * @return The parsed Long, or {@code null} if blank, invalid, or null.
     */
    public static Long parseLong(Object val) {
        if (val == null) {
            return null;
        }
        if (val instanceof Number num) {
            return num.longValue();
        }
        String s = val.toString().trim();
        if (s.isEmpty() || s.equalsIgnoreCase("N/A") || s.equalsIgnoreCase("null")) {
            return null;
        }
        try {
            return Long.parseLong(s.replaceAll("[,_ ]", ""));
        } catch (NumberFormatException e) {
            return null;
        }
    }
}
