# Json-Easy

Json-Easy has for vocation to be most simple to create, parse and manipulate Json with Java.

Json-Easy is written in pure Java without any others dependencies to be lightweight as possible.

# Getting Started
<!--
### Import library

#### Maven

```xml
<dependency>
  <groupId>fr.bigray</groupId>
  <artifactId>json-easy</artifactId>
  <version>1.1.0</version>
</dependency>
``` 

#### Gradle

```groovy
compile 'fr.bigray:json-easy:v1.1.0'
```
-->
## Create a JsonObject

```java
JsonObject jsObjectChild = JsonObject.createObject()
	.$( "key1", "value1" )
	.$( "key2", "value2" );

JsonObject jsObject = JsonObject.createObject()
    .$( "key1", "value1" )
    .$( "key2", "value2" )
    .$( "key3", 1 )
    .$( "key4", 123.45 )
    .$$( jsObjectChild )
    .$( "key5", JsonObject.createObject()
        .$( "key1", "value1" )
        .$( "key2", "value2" )
        .$( "key3", JsonArray.createArray()
            .$( "value1" )
            .$( "value2" )
            .$( 3 )
            .$( JsonObject.createObject()
                .$( "key1", "value1" )
            )
        ))
    ))
);
```

## Create a JsonArray

```java
JsonArray jsArrayChild = JsonArray.createArray()
	.$( "value1" )
	.$( 12345 )
	.$( 1L );

JsonArray jsArray = JsonArray.createArray()
    .$( "value1" )
    .$( "value2" )
    .$( 1 )
    .$( 123.45 )
    .$$( jsArrayChild )
    .$( JsonObject.createObject()
        .$( "key1", "value1" )
        .$( "key2", "value2" )
        .$( "key3", JsonArray.createArray()
            .$( "value1" )
            .$( "value2" )
            .$( 3 )
            .$( JsonObject.createObject()
                .$( "key1", "value1" )
            )
        ))
    ))
);
```

## JsonValue to json string

```java
JsonObject jsObject = JsonObject.createObject()
    .$( "key1", "value1" )
    .$( "key2", "value2" )
);
String json = JsObject.toJson();

// Ouput {"key1":"value1", "key2":"value2"}
```

## JsonValue from json string

```java
JsonValue jsValue = JsonParser.parse( "{\"key1\":\"value1\"}" );
JsonObject jsObject = jsonValue.as(JsonObject.class);

// Other way
JsonArray jsArray = JsonArray.fromJson( "[\"value1\", 1234]" );

```

## JsonObject and JsonArray manipulation

JsonObject and JsonArray classes extends respectively LinkedHashMap\<String, JsonValue\> and LinkedList\<JsonValue\>, so, if you known how to manipule these Collections, you known to manipulate JsonObject and JsonArray.

## License

This project is licensed under the Apache-2.0 License - see the [LICENSE](LICENSE) file for details.

### Built With

* [Maven](https://maven.apache.org/) - Dependency Management


### Author

* **Raynald BUQUET** - *Big-Ray*

