package fr.bigray.json.utils;

import fr.bigray.json.*;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.junit.jupiter.api.Assertions.*;

class WrapValueTest {

    @Test
    void wrap() {

        JsonObject jsonObject = JsonObject.createObject();
        JsonArray jsonArray = JsonArray.createArray();
        String aString = "str value";
        Integer integer = 1234;
        BigDecimal bigDecimal = new BigDecimal(1234);
        BigInteger bigInteger = new BigInteger("1", 3);
        Double aDouble = 12.34d;
        Long aLong = 1L;
        Boolean aBoolean = true;

        assertAll("wrap tests",
                () -> assertTrue(WrapValue.wrap(jsonObject) instanceof JsonObject),
                () -> assertTrue(WrapValue.wrap(jsonArray) instanceof JsonArray),
                () -> assertTrue(WrapValue.wrap(aString) instanceof JsonString),
                () -> assertTrue(WrapValue.wrap(integer) instanceof JsonNumber),
                () -> assertTrue(WrapValue.wrap(bigDecimal) instanceof JsonNumber),
                () -> assertTrue(WrapValue.wrap(bigInteger) instanceof JsonNumber),
                () -> assertTrue(WrapValue.wrap(aDouble) instanceof JsonNumber),
                () -> assertTrue(WrapValue.wrap(aLong) instanceof JsonNumber),
                () -> assertTrue(WrapValue.wrap(aBoolean) instanceof JsonBoolean),
                () -> assertTrue(WrapValue.wrap(null) instanceof JsonNull),
                () -> assertThrows(RuntimeException.class, () -> WrapValue.wrap(12.3f))
        );

    }
}