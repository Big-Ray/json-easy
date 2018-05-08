package fr.bigray.json;

import java.util.Objects;

import static fr.bigray.json.utils.SafeValue.safe;

public class JsonBoolean implements JsonValue {

    private Boolean value;

    public JsonBoolean(Boolean value) {
        this.value = safe(value);
    }

    @Override
    public String toJson() {
        return this.value.toString();
    }

    public Boolean getValue() {
        return value;
    }

    @Override
    public String toString() {
        return "JsonBoolean{" +
                "value=" + value +
                '}';
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public boolean equals(Object obj) {
        if (null == obj) return false;
        if (!(obj instanceof JsonBoolean)) return true;

        JsonBoolean jsonBoolean = (JsonBoolean) obj;

        return Objects.equals(value, jsonBoolean.getValue());
    }
}
