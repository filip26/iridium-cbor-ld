# Iridium CBOR-LD
An implementation of the [CBOR-LD 1.0 Draft](https://digitalbazaar.github.io/cbor-ld-spec/) in Java.

[![Java 17 CI](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java17-build.yml/badge.svg)](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java17-build.yml)
[![Java 8 CI](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java8-build.yml/badge.svg)](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java8-build.yml)
[![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/iridium-cbor-ld.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:iridium-cbor-ld)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## CLI
[LD-CLI](https://github.com/filip26/ld-cli) ia a command line utility for Ubuntu, Mac and Windows.


## Installation

### Maven

Java 17+

```xml
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>iridium-cbor-ld</artifactId>
    <version>0.1.2</version>
</dependency>

```

### Gradle
Java 8+, Android API Level >=31

```gradle
implementation("com.apicatalog:iridium-cbor-ld-jre8:0.1.2")
```

#### JSON-P Provider

Add JSON-P provider, if it is not on the classpath already.

##### Maven

```xml
<dependency>
    <groupId>org.glassfish</groupId>
    <artifactId>jakarta.json</artifactId>
    <version>2.0.1</version>
</dependency>
```

##### Gradle

```gradle
implementation("org.glassfish:jakarta.json:2.0.1")
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
