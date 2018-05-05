package fr.bigray.json;

import java.math.BigDecimal;
import java.math.BigInteger;

import static fr.bigray.json.utils.SafeValue.safe;

public class JsonNumber implements JsonValue {

    private BigDecimal value;

    public JsonNumber(BigDecimal value) {
        this.value = safe(value);
    }

    public JsonNumber(BigInteger value) {
        this.value = safe(new BigDecimal(value));
    }

    public JsonNumber(Integer value) {
        this.value = safe(new BigDecimal(value));
    }

    public JsonNumber(Long value) {
        this.value = safe(BigDecimal.valueOf(value));
    }

    public JsonNumber(Double value) {
        this.value = safe(BigDecimal.valueOf(value));
    }

    @Override
    public String toJson() {
        return this.value.toPlainString();
    }
}
