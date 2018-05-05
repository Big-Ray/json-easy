package fr.bigray.json.utils;

import java.util.Optional;

public class SafeValue {
    public static <T> T safe(T value) {
        return Optional.ofNullable(value)
                .orElseThrow(() -> new IllegalArgumentException("Value can't be null."));
    }
}
