package fr.bigray.json;

import fr.bigray.json.parser.JsonParser;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import static fr.bigray.json.utils.WrapValue.wrap;

public class JsonArray extends LinkedList<JsonValue> implements JsonValue {

    private JsonArray() {
        super(Collections.emptyList());
    }

    public static JsonArray createArray() {
        return new JsonArray();
    }

    public JsonArray $(Object value) {
        this.add(wrap(value));
        return this;
    }

    public JsonArray $$(List<JsonValue> values) {
        this.addAll(values);
        return this;
    }

    public static JsonArray fromJson(String json) {
        return JsonParser.parse(json).asJsArray();
    }

    @Override
    public String toJson() {
        return "[" + transformToString() + "]";
    }


    private String transformToString() {
        return this.stream().map(JsonValue::toJson).collect(Collectors.joining(","));
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);

    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
