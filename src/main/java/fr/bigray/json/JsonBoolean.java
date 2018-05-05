package fr.bigray.json;

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
}
