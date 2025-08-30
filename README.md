# Iridium CBOR-LD

CBOR-LD is a **self-describing** binary format that encodes Linked Data expressed in JSON-LD into a compact form. Its primary purpose is to compress the verbose textual representation of JSON-LD into just a few bytes; small enough to embed in a QR code, barcode, or any other constrained medium.

Compression is achieved through **semantic compaction** by relying on shared semantics. The result can always be expanded back into full JSON-LD without loss of meaning.  

The CBOR-LD format makes Linked Data efficient to store, transmit, and embed in environments where space and bandwidth are limited, while retaining semantic interoperability with the wider Linked Data ecosystem.  

Iridium CBOR-LD provides a full implementation of the [CBOR-LD 1.0 specification](https://json-ld.github.io/cbor-ld-spec/), including support for custom dictionaries and integration with Verifiable Credential Barcodes.

[![Java 17 CI](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java17-push.yml/badge.svg)](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java17-push.yml)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/07fd47ee8fa64d68a47cc83365fa07d6)](https://app.codacy.com/gh/filip26/iridium-cbor-ld/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)
[![Codacy Badge](https://app.codacy.com/project/badge/Coverage/07fd47ee8fa64d68a47cc83365fa07d6)](https://app.codacy.com/gh/filip26/iridium-cbor-ld/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_coverage)
[![javadoc](https://javadoc.io/badge2/com.apicatalog/iridium-cbor-ld/javadoc.svg)](https://javadoc.io/doc/com.apicatalog/iridium-cbor-ld)
[![Maven Central](https://img.shields.io/maven-central/v/com.apicatalog/iridium-cbor-ld.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:com.apicatalog%20AND%20a:iridium-cbor-ld)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## ✨ Features

- 🔄 Semantic compression and decompression
- ⚙️ Configurable API for flexible integration
- 🧱 Support for custom document dictionaries and builders
- 📦 Integration with [Verifiable Credential Barcodes](https://w3c-ccg.github.io/vc-barcodes/)

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

> Please note: Only JSON-LD documents containing referenced contexts can be compressed. This is because referenced contexts act as shared dictionaries that enable term mapping across documents. Compression is not possible with inline contexts, as their definitions are embedded directly in the document and cannot be reused or referenced externally.


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

Use dictionaries to maximize the compression ratio. A dictionary consists of terms and the codes they are encoded to. The dictionary code is preserved in the encoded CBOR-LD, but the decoder must have the same dictionary enabled. See the [W3C CBOR-LD Registry for examples](https://json-ld.github.io/cbor-ld-spec/#registry).

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
            .uri("did:key:zD...", 1)
            .uri("did:key:zD...", 2)            
            .uri("https://example.../status-lists", 3)
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
    <version>0.7.3</version>
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

## 🛠️ LD-CLI
[LD-CLI](https://github.com/filip26/ld-cli) is a command-line utility for
working with CBOR-LD, JSON-LD, multiformats, and related specifications.

It provides encoding, decoding, detection, analysis, and format conversion
features, making it useful for inspecting identifiers, testing content
addressing, and integrating multiformats into development workflows.

### Example

Decompress CBOR-LD into JSON-LD
```bash
ld-cli decompress --pretty --hex --dictionary ./utopia-barcodes-dictionary-example.json <<< 'd90664a60183198000198001198002189d82187618a418b8a3189c18a618ce18b218d01ae592208118baa2189c18a018a8447582002018be18aa18c0a5189c186c18d60418e018e618e258417ab7c2e56b49e2cce62184ce26818e15a8b173164401b5d3bb93ffd6d2b5eb8f6ac0971502ae3dd49d17ec66528164034c912685b8111bc04cdc9ec13dbadd91cc18e418ac'
```

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

Commercial support and consulting are available.  
For inquiries, please contact: filip26@gmail.com
