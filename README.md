# Iridium CBOR-LD
An implementation of the [CBOR-LD 1.0](https://json-ld.github.io/cbor-ld-spec/) in Java.

### Features

* Semantic compression / decompression
* Configurable API
* Custom document dictionaries and builders
* [Verifiable Credential Barcodes](https://w3c-ccg.github.io/vc-barcodes/)

### Status

[![Java 17 CI](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java17-push.yml/badge.svg)](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java17-push.yml)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/07fd47ee8fa64d68a47cc83365fa07d6)](https://app.codacy.com/gh/filip26/iridium-cbor-ld/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)
[![Codacy Badge](https://app.codacy.com/project/badge/Coverage/07fd47ee8fa64d68a47cc83365fa07d6)](https://app.codacy.com/gh/filip26/iridium-cbor-ld/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_coverage)
[![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/iridium-cbor-ld.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:iridium-cbor-ld)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


## CLI
[LD-CLI](https://github.com/filip26/ld-cli) is a command line utility for Ubuntu, Mac and Windows.

## Usage

### Encoding

```javascript
// create an encoder builder initialized with default values
var encoder = CborLd.createEncoder()
               // use bundled static contexts (true by default)
               .useBundledContexts(true)
               // loader (optional)
               .loader(...)
               // custom terms dictionary (optional)
               .dictionary(customDictionary)
               // create a new encoder instance
               .build(); 
                   
// create barcodes encoder builder 
var encoder = CborLd.createEncoder(BarcodesConfig.INSTANCE)
               // ... customize
               .build()

// encode a document
byte[] encoded = encoder.encode(document);
```

### Decoding

```javascript
// create a decoder builder initialized with default values
var decoder = CborLd.createDecoder()
               // use bundled static contexts (true by default)
               .useBundledContexts(true)
               // loader (optional)
               .loader(...)
               // add custom terms dictionary (optional)
               .dictionary(customDictionary);
               // create a new decoder instance
               .build(); 
                   
// create barcodes decoder builder
var decoder = CborLd.createDecoder(BarcodesConfig.INSTANCE)
               // ... customize
               .build()
  
// decode
document = decoder.decode(encoded);
```

### Backward Compatibility

```javascript
// Iridium < v0.2.0
CborLd.create[Encoder|Decoder](V05Config.INSTANCE)
      // ... customize      
      .build();
      
// Iridium < v0.2.0, @digitalbazaar/cborld compatibility
CborLd.create[Encoder|Decoder](V05Config.INSTANCE)
      .compactArrays(false)
      // ... customize      
      .build();
```


## Installation

Java 17+

```xml
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>iridium-cbor-ld</artifactId>
    <version>0.2.2</version>
</dependency>

```

Iridium CBOR-LD for Android is distributed under a commercial license. [Contact](mailto:filip26@gmail.com)

#### JSON-P Provider

Add JSON-P provider, if it is not on the classpath already.

```xml
<dependency>
    <groupId>org.glassfish</groupId>
    <artifactId>jakarta.json</artifactId>
    <version>2.0.1</version>
</dependency>
```

## Documentation

[![javadoc](https://javadoc.io/badge2/com.apicatalog/iridium-cbor-ld/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/iridium-cbor-ld)

## Contributing

All PR's welcome!


### Building

Fork and clone the project repository.

```bash
> cd iridium-cbor-ld
> mvn clean package
```

## Resources

* [CBOR-LD 1.0](https://json-ld.github.io/cbor-ld-spec/)
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

