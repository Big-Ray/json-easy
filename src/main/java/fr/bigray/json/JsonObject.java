package fr.bigray.json;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

import static fr.bigray.json.utils.WrapValue.wrap;

public class JsonObject extends LinkedHashMap<String, JsonValue> implements JsonValue {

    private JsonObject() {
        super(Collections.emptyMap());
    }

    public static JsonObject createObject() {
        return new JsonObject();
    }

    public JsonObject $(String key, Object value) {
        this.put(key, wrap(value));
        return this;
    }

    public JsonObject $$(Map<String, JsonValue> values) {
        this.putAll(values);
        return this;
    }

    @Override
    public String toJson() {
        return "{" + transformToString() + "}";
    }

    private String transformToString() {
        return this.entrySet().stream()
                .map(entry -> String.format("\"%s\":%s", entry.getKey(), entry.getValue().toJson()))
                .collect(Collectors.joining(","));
    }

}
