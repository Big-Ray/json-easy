package fr.bigray.json;

public class JsonNull implements JsonValue {

    public static final JsonNull NULL = new JsonNull();

    @Override
    public String toJson() {
        return "null";
    }
}
