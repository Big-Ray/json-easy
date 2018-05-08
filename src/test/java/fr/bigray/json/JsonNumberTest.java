package fr.bigray.json;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class JsonNumberTest {

    private static JsonNumber jsonNumber;

    @BeforeAll
    static void initAll() {
        jsonNumber = new JsonNumber(1234);
    }

    @Test
    void toJson() {
        assertEquals("1234", jsonNumber.toJson());
    }

    @Test
    void getValue() {
        assertEquals(1234, jsonNumber.getValue().intValue());
    }
}