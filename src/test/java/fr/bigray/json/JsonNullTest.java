package fr.bigray.json;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class JsonNullTest {

    private static JsonNull jsonNull;

    @BeforeAll
    static void initAll() {
        jsonNull = JsonNull.NULL;
    }

    @Test
    void toJson() {
        assertEquals("null", jsonNull.toJson());
    }

    @Test
    void getValue() {
        assertNull(jsonNull.getValue());
    }


}