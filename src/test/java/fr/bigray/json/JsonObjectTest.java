package fr.bigray.json;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class JsonObjectTest {

    @Test
    void toJson() {
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

    @Test
    void fromJson() {
        String json = "{\"firstName\":\"John\",\"lastName\":\"Doe\",\"age\":40,\"isStrong\":true,\"address\":{\"number\":4,\"zipCode\":17540,\"street\":\"Chemin de la gare\",\"city\":\"Le Gué d'Alleré\",\"digiCode\":null},\"hobbies\":[\"F1\",\"Rally\",\"Music\"]}";

//        String json = "{\"name\":\"buquet\",\"prenom\":\"raynald\"}";

        JsonObject actual = JsonObject.fromJson(json);

        JsonObject expected = JsonObject.createObject()
                .$("firstName", new JsonString("John"))
                .$("lastName", new JsonString("Doe"))
                .$("age", new JsonNumber(40))
                .$("isStrong", new JsonBoolean(true))
                .$("address", JsonObject.createObject()
                        .$("number", 4)
                        .$("zipCode", new JsonNumber(17540))
                        .$("street", "Chemin de la gare")
                        .$("city", new JsonString("Le Gué d'Alleré"))
                        .$("digiCode", null))
                .$("hobbies", JsonArray.createArray()
                        .$("F1")
                        .$("Rally")
                        .$("Music"));

        assertEquals(actual, expected);

        assertTrue(actual.get("firstName") instanceof JsonString);
        assertEquals("John", actual.get("firstName").asJsString().getValue());

        assertTrue(actual.get("lastName") instanceof JsonString);
        assertEquals("Doe", actual.get("lastName").asJsString().getValue());

        assertTrue(actual.get("age") instanceof JsonNumber);
        assertEquals(40, actual.get("age").asJsNumber().getValue().intValue());

        assertTrue(actual.get("isStrong") instanceof JsonBoolean);
        assertTrue(actual.get("isStrong").asJsBoolean().getValue());

        assertTrue(actual.get("address") instanceof JsonObject);
        assertEquals(5, actual.get("address").asJsObject().size());

        assertTrue(actual.get("hobbies") instanceof JsonArray);
        assertEquals(3, actual.get("hobbies").asJsArray().size());

    }

    @Test
    void fromJson_nominal_case() {
        String json = "{\"firstName\":\"John\", \"lastName\" : \"Doe\"}";

        JsonObject actual = JsonObject.fromJson(json);

        JsonObject expected = JsonObject.createObject()
                .$("firstName", "John")
                .$("lastName", "Doe");

        assertEquals(actual, expected);


        assertTrue(actual.get("firstName") instanceof JsonString);
        assertEquals("John", actual.get("firstName").asJsString().getValue());

        assertTrue(actual.get("lastName") instanceof JsonString);
        assertEquals("Doe", actual.get("lastName").asJsString().getValue());

    }

}