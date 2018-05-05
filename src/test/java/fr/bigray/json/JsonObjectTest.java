package fr.bigray.json;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class JsonObjectTest {

    @Test
    void createObject() {
        JsonObject address = JsonObject.createObject()
                .$("number", 4)
                .$("zipCode", new JsonNumber(17540))
                .$("street", "Chemin de la gare")
                .$("city", new JsonString("Le Gué d'Alleré"))
                .$("digiCode", null);

        JsonObject actual = JsonObject.createObject()
                .$("firstName", new JsonString("John"))
                .$("lastName", new JsonString("Doe"))
                .$("age", new JsonNumber(40))
                .$("isStrong", new JsonBoolean(true))
                .$("address", JsonObject.createObject()
                        .$$(address))
                .$("hobbies", JsonArray.createArray()
                        .$("F1")
                        .$("Rally")
                        .$("Music"));

        String expectedJson = "{\"firstName\":\"John\",\"lastName\":\"Doe\",\"age\":40,\"isStrong\":true,\"address\":{\"number\":4,\"zipCode\":17540,\"street\":\"Chemin de la gare\",\"city\":\"Le Gué d'Alleré\",\"digiCode\":null},\"hobbies\":[\"F1\",\"Rally\",\"Music\"]}";

        assertEquals(expectedJson, actual.toJson());
    }

}