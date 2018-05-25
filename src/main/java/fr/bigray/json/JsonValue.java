package fr.bigray.json;

import java.io.Serializable;

public interface JsonValue extends Serializable {

    String toJson();


    default JsonObject asJsObject() {
        return as(JsonObject.class);
    }

    default JsonArray asJsArray() {
        return as(JsonArray.class);
    }

    default JsonString asJsString() {
        return as(JsonString.class);
    }

    default JsonNumber asJsNumber() {
        return as(JsonNumber.class);
    }

    default JsonBoolean asJsBoolean() {
        return as(JsonBoolean.class);
    }

    default JsonNull asJsNull() {
        return as(JsonNull.class);
    }

    default JsonValue asJsValue() {
        return as(JsonValue.class);
    }

    default <T> T as(Class<T> clazz) {
        return clazz.cast(this);
    }

}
