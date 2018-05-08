package fr.bigray.json;

import java.util.Objects;

public class JsonNull implements JsonValue {

    public static final JsonNull NULL = new JsonNull();

    @Override
    public String toJson() {
        return "null";
    }

    public <T> T getValue() {
        return null;
    }

    @Override
    public String toString() {
        return "JsonNull{}";
    }

//    @Override
//    public int hashCode() {
//        return Objects.hash(null);
//    }

    @Override
    public boolean equals(Object obj) {
        if (null == obj) return false;
        if (!(obj instanceof JsonNull)) return true;

        JsonNull jsonNull = (JsonNull) obj;

        return Objects.equals(null, jsonNull.getValue());
    }
}
