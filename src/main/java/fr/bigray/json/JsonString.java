package fr.bigray.json;

import java.util.Optional;

public class JsonString implements JsonValue {

    private String value;

    public JsonString(String value) {
        this.value = Optional.ofNullable(value).orElse("null");
    }

    @Override
    public String toJson() {
        return String.format("\"%s\"", this.value);
    }

}
