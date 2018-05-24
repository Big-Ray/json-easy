package fr.bigray.json;

import java.util.Objects;
import java.util.Optional;

public class JsonString implements JsonValue {

    private String value;

    public JsonString(String value) {
        this.value = Optional.ofNullable(value).orElse("null");
    }

    public String getValue() {
        return value;
    }

    @Override
    public String toJson() {
        return String.format("\"%s\"", this.value.trim());
    }


    @Override
    public String toString() {
        return "JsonString{" +
                "value='" + value.trim() + '\'' +
                '}';
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public boolean equals(Object obj) {
        if (null == obj) return false;
        if (!(obj instanceof JsonString)) return true;

        JsonString jsonString = (JsonString) obj;

        return Objects.equals(value, jsonString.getValue());
    }
}
