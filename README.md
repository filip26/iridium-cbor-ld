
> [!IMPORTANT]
> Your feedback is essential to the improvement of this library. Please share any concerns, primary use cases, areas for enhancement, or challenges you have encountered. Your insights help refine and optimize the library to better meet user needs. Thank you for your time and contributions.

# Iridium CBOR-LD

Iridium CBOR-LD is a Java implementation of [CBOR-LD 1.0](https://json-ld.github.io/cbor-ld-spec/), a compact binary format for Linked Data.

## ✨ Features

- 🔄 Semantic compression and decompression
- ⚙️ Configurable API for flexible integration
- 🧱 Support for custom document dictionaries and builders
- 📦 Integration with [Verifiable Credential Barcodes](https://w3c-ccg.github.io/vc-barcodes/)


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
               // terms dictionary (optional)
               .dictionary(dictionary)
               // use bundled static contexts (true by default)
               .useBundledContexts(true)
               // loader (optional)
               .loader(...)
               // create a new encoder instance
               .build(); 

// encode a document
byte[] encoded = encoder.encode(document);
```

### Decoding

```javascript
// create a decoder builder initialized with default values
var decoder = CborLd.createDecoder()
               // terms dictionaries (optional)
               .dictionary(dictionary1);
               .dictionary(dictionary2);               
               // ...
               // use bundled static contexts (true by default)
               .useBundledContexts(true)
               // loader (optional)
               .loader(...)
               // create a new decoder instance
               .build(); 

// decode
document = decoder.decode(encoded);
```

### Dictionary

It’s highly recommended to use dictionaries to maximize the compression ratio. A dictionary consists of terms and the codes they are encoded to. The dictionary code is preserved in the encoded CBOR-LD, but the decoder must have the same dictionary enabled. See the [W3C CBOR-LD Registry for examples](https://json-ld.github.io/cbor-ld-spec/#registry).

A dictionary should contain terms that are not present in contexts, such as the context URIs themselves, custom common values bound to types, and custom common URIs.

```javascript
// build a new dictionary
var dictionary = DocumentDictionaryBuilder
            .create(DICTIONARY_CODE)
            .context("https://www.w3.org/ns/credentials/v2", 1)
            .context("https://w3id.org/vc-barcodes/v1", 2)
            .type("https://w3id.org/security#cryptosuiteString",
                    DictionaryBuilder.create()
                            .set("ecdsa-rdfc-2019", 1)
                            .set("ecdsa-sd-2023", 2)
                            .set("eddsa-rdfc-2022", 3)
                            .set("ecdsa-xi-2023", 4))
            .uri("did:key:zDnaeWjKfs1ob9QcgasjYSPEMkwq31hmvSAWPVAgnrt1e9GKj", 1)
            .uri("did:key:zDnaeWjKfs1ob9QcgasjYSPEMkwq31hmvSAWPVAgnrt1e9GKj#zDnaeWjKfs1ob9QcgasjYSPEMkwq31hmvSAWPVAgnrt1e9GKj", 2)            
            .uri("https://sandbox.platform.veres.dev/statuses/z19rJ4oGrbFCqf3cNTVDHSbNd/status-lists", 3)
            .uri("did:key:zDnaeZSD9XcuULaS8qmgDUa6TMg2QjF9xABnZK42awDH3BEzj", 4)
            .build();

// use with encoder
var encoder = CborLd.createEncoder()
                .dictionary(dictionary)
                .loader(...)
                // ... customize
                .build();

// use with decoder, please note you can register multiple dictionaries
var decoder = CborLd.createDecoder()
                .dictionary(dictionary)
                .loader(...)
                // ... customize
                .build();
               
```

### Diagnose / Debug

```javascript
var debug = CborLd.create[Decoder|Encoder]()
                // ... customize
                .debug();

debug.[encode|decode](...);

debug.isCborLd();      // true if the encoded document is in CBOR-LD format
debug.version();       // CBOR-LD encoding version
debug.dictionary();    // static terms
debug.terms();         // dynamic term map
debug.decoded();       // decoded JSON-LD document

debug.isError();       // true if an exception has been thrown
debug.error();         // an exception or null

debug.dump();          // dump as JSON

```

### Backward Compatibility

```javascript
// CBOR-LD v0.5
CborLd.createEncoder(CborLdVersion.V05)
      // ... customize      
      .build();

// CBOR-LD v0.6
CborLd.createEncoder(CborLdVersion.V06)
      // ... customize      
      .build();

// Multi-Decoder 
CborLd.createDecoder(CborLdVersion.V1, CborLdVersion.V06, CborLdVersion.V05)
      // sets dictionary for v1
      .dictionary(dictionary)
      // sets dictionary for v0.6
      .dictionary(CborLdVersion.V06, dictionary)
      // ... customize
      .build();      
```


## Installation

Java 17+

```xml
<dependency>
    <groupId>com.apicatalog</groupId>
    <artifactId>iridium-cbor-ld</artifactId>
    <version>0.3.0</version>
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

