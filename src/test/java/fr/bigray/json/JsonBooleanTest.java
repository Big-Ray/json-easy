package fr.bigray.json;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class JsonBooleanTest {

    private static JsonBoolean jsonBooleanTrue;
    private static JsonBoolean jsonBooleanFalse;

    @BeforeAll
    static void initAll() {
        jsonBooleanTrue = new JsonBoolean(true);
        jsonBooleanFalse = new JsonBoolean(false);
    }

    @Test
    void toJson() {
        assertEquals("false", jsonBooleanFalse.toJson());
        assertEquals("true", jsonBooleanTrue.toJson());
    }

    @Test
    void getValue() {
        assertTrue(jsonBooleanTrue.getValue());
        assertFalse(jsonBooleanFalse.getValue());
    }
}