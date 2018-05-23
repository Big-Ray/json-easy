package fr.bigray.json.parser;

import fr.bigray.json.*;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiPredicate;
import java.util.stream.Collectors;

public class JsonParser {

    private static final int OPEN_BRACE = 123;
    private static final int CLOSE_BRACE = 125;
    private static final int OPEN_BRACKET = 91;
    private static final int CLOSE_BRACKET = 93;
    private static final int DBL_QUOTE = 34;
    private static final int COMMA = 44;

    public static JsonValue parse(String json) {

        var firstCharacter = json.charAt(0);
        var lastCharacter = json.charAt(json.length() - 1);

        if (isJson(OPEN_BRACE, CLOSE_BRACE).test(firstCharacter, lastCharacter)) {
            var jsonValue = JsonObject.createObject();

            split(json).stream()
                    .map(entry -> entry.trim().split(":", 2))
                    .forEach(keysValues -> {
                        var key = keysValues[0];
                        var value = keysValues[1];
                        var firstChar = value.charAt(0);
                        var lastChar = value.charAt(value.length() - 1);

                        if (isJson(OPEN_BRACE, CLOSE_BRACE).or(isJson(OPEN_BRACKET, CLOSE_BRACKET)).test(firstChar, lastChar)) {
                            jsonValue.£(key, parse(value));
                        } else {
                            jsonValue.£(key, wrap(value));
                        }
                    });

            return jsonValue;
        } else if (isJson(OPEN_BRACKET, CLOSE_BRACKET).test(firstCharacter, lastCharacter)) {
            var jsonValue = JsonArray.createArray();

            split(json).forEach(value -> {
                var firstChar = value.charAt(0);
                var lastChar = value.charAt(value.length() - 1);
                if (isJson(OPEN_BRACE, CLOSE_BRACE).or(isJson(OPEN_BRACKET, CLOSE_BRACKET)).test(firstChar, lastChar)) {
                    jsonValue.£(parse(value));
                } else {
                    jsonValue.£(wrap(value));
                }
            });

            return jsonValue;
        } else {
            throw new RuntimeException("Is not a valid json!");
        }

    }

    private static BiPredicate<Character, Character> isJson(int open, int close) {
        return (firstChar, lastChar) -> firstChar == open && lastChar == close;
    }

    private static List<Character> splitToCharList(String jsonToSplit) {
        return jsonToSplit.trim().chars().mapToObj(item -> (char) item).collect(Collectors.toList());
    }

    private static boolean stringIsNumber(String value) {
        try {
            new BigDecimal(value);
        } catch (NumberFormatException e) {
            return false;
        }

        return true;
    }

    private static boolean stringIsBoolean(String value) {
        return Boolean.TRUE.toString().equals(value) || Boolean.FALSE.toString().equals(value);
    }

    private static boolean stringIsNull(String value) {
        return value == null || value.isEmpty() || value.equals("null");
    }

    private static JsonValue wrap(String value) {
        if (stringIsBoolean(value)) {
            return new JsonBoolean(Boolean.valueOf(value));
        } else if (stringIsNull(value)) {
            return new JsonNull();
        } else if (stringIsNumber(value)) {
            return new JsonNumber(new BigDecimal(value));
        } else {
            return new JsonString(value);
        }
    }

    private static List<String> split(String json) {

        var jsonEntryList = new ArrayList<String>();
        var jsonChar = splitToCharList(json);

        jsonChar.remove(0);
        jsonChar.remove(jsonChar.size() - 1);

        var lastCharacterIdx = jsonChar.size() - 1;

        var stringBuilder = new StringBuilder();
        var jsValueBuilder = new StringBuilder();

        var inJsValue = false;

        for (var i = 0; i < jsonChar.size(); i++) {
            switch (jsonChar.get(i)) {
                case OPEN_BRACE:
                    jsValueBuilder.append(jsonChar.get(i));
                    inJsValue = true;
                    break;
                case CLOSE_BRACE:
                    jsValueBuilder.append(jsonChar.get(i));
                    stringBuilder.append(jsValueBuilder.toString());

                    if (lastCharacterIdx == i) {
                        jsonEntryList.add(stringBuilder.toString());
                    }

                    jsValueBuilder = new StringBuilder();
                    inJsValue = false;
                    break;
                case OPEN_BRACKET:
                    jsValueBuilder.append(jsonChar.get(i));
                    inJsValue = true;
                    break;
                case CLOSE_BRACKET:
                    jsValueBuilder.append(jsonChar.get(i));
                    stringBuilder.append(jsValueBuilder.toString());

                    if (lastCharacterIdx == i) {
                        jsonEntryList.add(stringBuilder.toString());
                    }

                    jsValueBuilder = new StringBuilder();
                    inJsValue = false;
                    break;
                case COMMA:
                    if (inJsValue) {
                        jsValueBuilder.append(jsonChar.get(i));
                        break;
                    }
                    jsonEntryList.add(stringBuilder.toString());
                    stringBuilder = new StringBuilder();
                    break;
                case DBL_QUOTE:
                    break;
                default:
                    if (inJsValue) {
                        jsValueBuilder.append(jsonChar.get(i));
                    } else if (lastCharacterIdx == i) {
                        stringBuilder.append(jsonChar.get(i));
                        jsonEntryList.add(stringBuilder.toString());
                    } else {
                        stringBuilder.append(jsonChar.get(i));
                    }

            }
        }

        return jsonEntryList;
    }

}
