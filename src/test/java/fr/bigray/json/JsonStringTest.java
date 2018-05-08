package fr.bigray.json;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class JsonStringTest {

    private static JsonString jsonString;

    @BeforeAll
    static void initAll() {
        jsonString = new JsonString("A string value");
    }

    @Test
    void getValue() {
        assertEquals("A string value", jsonString.getValue());

    }

    @Test
    void toJson() {
        assertEquals("\"A string value\"", jsonString.toJson());
    }

}