package fr.bigray.json;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.junit.jupiter.api.Assertions.*;


class JsonArrayTest {

    @Test
    void createArray() {

        JsonArray jsonArray = JsonArray.createArray()
                .$("arr1")
                .$(12);

        JsonArray actual = JsonArray.createArray()
                .$(JsonObject.createObject()
                        .$("firstName", "Mick")
                        .$("lastName", "Tyson")
                        .$("age", 55)
                        .$("hobbies", JsonArray.createArray()
                                .$("Boxe")
                                .$("Catch")
                                .$("Movies")))
                .$(JsonObject.createObject()
                        .$("firstName", "Alain")
                        .$("lastName", "Prost")
                        .$("age", 65)
                        .$("hobbies", JsonArray.createArray()
                                .$("F1")
                                .$("Rally")
                                .$("Music")))
                .$(JsonObject.createObject()
                        .$("firstName", "Big")
                        .$("lastName", "Ray")
                        .$("age", 40)
                        .$("hobbies", JsonArray.createArray()
                                .$("guitar")
                                .$("basket ball")))
                .$("toto")
                .$(new BigDecimal(3456))
                .$(new BigInteger("1", 3))
                .$(123)
                .$(123.56)
                .$(1234567L)
                .$(null)
                .$$(jsonArray);

        String expected = "[{\"firstName\":\"Mick\",\"lastName\":\"Tyson\",\"age\":55,\"hobbies\":[\"Boxe\",\"Catch\",\"Movies\"]},{\"firstName\":\"Alain\",\"lastName\":\"Prost\",\"age\":65,\"hobbies\":[\"F1\",\"Rally\",\"Music\"]},{\"firstName\":\"Big\",\"lastName\":\"Ray\",\"age\":40,\"hobbies\":[\"guitar\",\"basket ball\"]},\"toto\",3456,1,123,123.56,1234567,null,\"arr1\",12]";

        assertEquals(expected, actual.toJson());

    }
}