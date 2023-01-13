# Iridium CBOR-LD
An implementation of the [CBOR-LD 1.0 Draft](https://digitalbazaar.github.io/cbor-ld-spec/) in Java.

[![Java 17 CI](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java17-build.yml/badge.svg)](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java17-build.yml)
[![Android (Java 8) CI](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java8-build.yml/badge.svg)](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java8-build.yml)
[![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/iridium-cbor-ld.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22com.apicatalog%22%20AND%20a:%22iridium-cbor-ld%22)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Installation

### Maven

```xml
<!-- Java 17 -->
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>iridium-cbor-ld</artifactId>
    <version>0.1.1</version>
</dependency>

```

or

```xml
<!-- Android 12 or higher (API Level >=31) -->
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>iridium-cbor-ld-jre8</artifactId>
    <version>0.1.1</version>
</dependency>
```

#### JSON-P Provider

Add JSON-P provider, if it is not on the classpath already.


```xml
<dependency>
    <groupId>org.glassfish</groupId>
    <artifactId>jakarta.json</artifactId>
    <version>2.0.1</version>
</dependency>
```


## Usage

### Encoding

```java
  byte[] encoded = CborLd.encoder(document).encode();
```

### Decoding

```java
  document = CborLd.decoder(encoded).decode();
```

### @digitalbazaar/cborld compatibility

Set `DbConfig` as a configuration option to an encoder or decoder API.

e.g.

```java
  CborLd.encoder(document)
        .config(DbConfig.INSTANCE)
        .encode();
        
  CborLd.decoder(document)
        .config(DbConfig.INSTANCE)
        .decode();
        
```

## Documentation

[![javadoc](https://javadoc.io/badge2/com.apicatalog/iridium-cbor-ld/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/iridium-cbor-ld)


## Contributing

All PR's welcome!


### Building

Fork and clone the project repository.

#### Java 17
```bash
> cd iridium-cbor-ld
> mvn clean package
```

#### Java 8
```bash
> cd iridium-cbor-ld
> mvn -f pom_jre8.xml clean package
```


## Resources

* [CBOR-LD 1.0 Draft](https://digitalbazaar.github.io/cbor-ld-spec/)
* [Concise Binary Object Representation (CBOR)](https://datatracker.ietf.org/doc/html/rfc8949)
* [Hello CBOR-LD Presentation](https://docs.google.com/presentation/d/1ksh-gUdjJJwDpdleasvs9aRXEmeRvqhkVWqeitx5ZAE/edit?usp=sharing)
* [cbor.io](https://cbor.io/)
* [JavaScript CBOR-LD Processor](https://github.com/digitalbazaar/cborld)
* [CBOR Playground](https://cbor.me/)

## Sponsors

<a href="https://github.com/digitalbazaar">
  <img src="https://avatars.githubusercontent.com/u/167436?s=200&v=4" width="40" />
</a> 

## Commercial Support
Commercial support is available at filip26@gmail.com
