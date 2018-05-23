package fr.bigray.json;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.junit.jupiter.api.Assertions.*;


class JsonArrayTest {

    @Test
    void toJson() {

        JsonArray jsonArray = JsonArray.createArray()
                .£("arr1")
                .£(12);

        JsonArray actual = JsonArray.createArray()
                .£(JsonObject.createObject()
                        .£("firstName", "Mick")
                        .£("lastName", "Tyson")
                        .£("age", 55)
                        .£("hobbies", JsonArray.createArray()
                                .£("Boxe")
                                .£("Catch")
                                .£("Movies")))
                .£(JsonObject.createObject()
                        .£("firstName", "Alain")
                        .£("lastName", "Prost")
                        .£("age", 65)
                        .£("hobbies", JsonArray.createArray()
                                .£("F1")
                                .£("Rally")
                                .£("Music")))
                .£(JsonObject.createObject()
                        .£("firstName", "Big")
                        .£("lastName", "Ray")
                        .£("age", 40)
                        .£("hobbies", JsonArray.createArray()
                                .£("guitar")
                                .£("basket ball")))
                .£("string value")
                .£(new BigDecimal(3456))
                .£(new BigInteger("1", 3))
                .£(123)
                .£(123.56)
                .£(1234567L)
                .£(null)
                .££(jsonArray);

        String expected = "[{\"firstName\":\"Mick\",\"lastName\":\"Tyson\",\"age\":55,\"hobbies\":[\"Boxe\",\"Catch\",\"Movies\"]},{\"firstName\":\"Alain\",\"lastName\":\"Prost\",\"age\":65,\"hobbies\":[\"F1\",\"Rally\",\"Music\"]},{\"firstName\":\"Big\",\"lastName\":\"Ray\",\"age\":40,\"hobbies\":[\"guitar\",\"basket ball\"]},\"string value\",3456,1,123,123.56,1234567,null,\"arr1\",12]";

        assertEquals(expected, actual.toJson());

    }

    @Test
    void fromJson() {
        String json = "[{\"firstName\":\"Mick\",\"lastName\":\"Tyson\",\"age\":55,\"hobbies\":[\"Boxe\",\"Catch\",\"Movies\"]},\"string value\",3456,null,[\"arr1\",12]]";
        JsonArray actual = JsonArray.fromJson(json);

        JsonArray expected = JsonArray.createArray()
                .£(JsonObject.createObject()
                        .£("firstName", "Mick")
                        .£("lastName", "Tyson")
                        .£("age", 55)
                        .£("hobbies", JsonArray.createArray()
                                .£("Boxe")
                                .£("Catch")
                                .£("Movies")))
                .£("string value")
                .£(new BigDecimal(3456))
                .£(null)
                .£(JsonArray.createArray()
                        .£("arr1")
                        .£(12));


        assertTrue(actual.containsAll(expected));

        assertEquals(5 ,actual.size());
        assertTrue(actual.get(0) instanceof JsonObject);
        assertEquals(4 , actual.get(0).asJsObject().size());
        assertEquals("Mick", actual.get(0).asJsObject().get("firstName").asJsString().getValue());
        assertEquals("Tyson", actual.get(0).asJsObject().get("lastName").asJsString().getValue());
        assertEquals(55, actual.get(0).asJsObject().get("age").asJsNumber().getValue().intValue());
        assertTrue(actual.get(0).asJsObject().get("hobbies") instanceof JsonArray);
        assertEquals(3, actual.get(0).asJsObject().get("hobbies").asJsArray().size());

        assertEquals("string value", actual.get(1).asJsString().getValue());

        assertEquals(3456, actual.get(2).asJsNumber().getValue().intValue());

        assertNull(actual.get(3).asJsNull().getValue());

        assertTrue(actual.get(4) instanceof JsonArray);
        assertEquals(2, actual.get(4).asJsArray().size());
    }
}