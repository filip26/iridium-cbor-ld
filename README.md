# Iridium CBOR-LD
An implementation of the [CBOR-LD 1.0 Draft](https://digitalbazaar.github.io/cbor-ld-spec/) in Java.

[![Android (Java 8) CI](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java8-build.yml/badge.svg)](https://github.com/filip26/iridium-cbor-ld/actions/workflows/java8-build.yml)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Usage

### Encoding

```java
  byte[] encoded = CborLd.encoder(document).encode();
```

### Decoding

```java
  document = CborLd.decoder(encoded).decode();
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

* [CBOR-LD 1.0 Draft](https://digitalbazaar.github.io/cbor-ld-spec/)
* [Concise Binary Object Representation (CBOR)](https://datatracker.ietf.org/doc/html/rfc8949)
* [Hello CBOR-LD Presentation](https://docs.google.com/presentation/d/1ksh-gUdjJJwDpdleasvs9aRXEmeRvqhkVWqeitx5ZAE/edit?usp=sharing)
* [cbor.io](https://cbor.io/)
* [JavaScript CBOR-LD Processor](https://github.com/digitalbazaar/cborld)
* [CBOR Playground](https://cbor.me/)
