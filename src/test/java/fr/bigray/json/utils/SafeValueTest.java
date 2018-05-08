package fr.bigray.json.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SafeValueTest {

    @Test
    void safe() {

        assertThrows(IllegalArgumentException.class, () -> SafeValue.safe(null), "Value can't be null.");
        assertTrue(!SafeValue.safe("not null value").isEmpty());
        assertEquals(123, SafeValue.safe(123).intValue());
        assertEquals(1L, SafeValue.safe(1L).longValue());
        assertEquals(1.45f, SafeValue.safe(1.45f).floatValue());
        assertEquals(1.45d, SafeValue.safe(1.45d).doubleValue());

    }
}