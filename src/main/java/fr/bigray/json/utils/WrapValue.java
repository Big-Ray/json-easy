package fr.bigray.json.utils;

import fr.bigray.json.*;

import java.math.BigDecimal;
import java.math.BigInteger;

public class WrapValue {
    public static JsonValue wrap(Object value) {

        if (value instanceof JsonValue) {
            return (JsonValue) value;
        } else if (value instanceof String) {
            return new JsonString((String) value);
        } else if (value instanceof Integer) {
            return new JsonNumber((Integer) value);
        } else if (value instanceof BigDecimal) {
            return new JsonNumber((BigDecimal) value);
        } else if (value instanceof BigInteger) {
            return new JsonNumber((BigInteger) value);
        } else if (value instanceof Double) {
            return new JsonNumber((Double) value);
        } else if (value instanceof Long) {
            return new JsonNumber((Long) value);
        }  else if (value instanceof Boolean) {
            return new JsonBoolean((Boolean) value);
        } else if (null == value) {
            return JsonNull.NULL;
        } else {
            throw new RuntimeException("Unknown json type.");
        }

    }
}
