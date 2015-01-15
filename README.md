# SOTP

SOTP (Sam's Obvious TOML Parser) is a Scala parser for the [TOML](https://github.com/toml-lang/toml)
file format.
Its implementation is based on
[parboiled2](https://github.com/sirthias/parboiled2)'s
parser combinators.
It currently only does de-serialization (parsing), but serialization is
planned for the future.

SOTP was written prioritizing straight-forwardness and fidelity to the spec over
speed.
I've done no benchmarking, and I'm sure there is tons of room for improvement.
(Almost) compatible with
TOML [v0.3.1](https://github.com/toml-lang/toml/tree/v0.3.1) (see TODO list
for incompatibilities).
The test suite includes all fixtures from
[toml-test](https://github.com/BurntSushi/toml-test).

Both the TOML spec and the SOTP API should be considered experimental
and subject to change until v1.0.

SOTP is open source, under the MIT license (see LICENSE.txt).

## TODO
- Incompatibilities:
    * [x] 8 digit unicode format
    * [x] Check for duplicate keys
    * [x] Check that types of all elements of array match (up to erasure)
    * [ ] Disallow control characters
- [ ] Check that valid toml-test fixtures match expected JSON
- [ ] Write serializer
- [ ] De-serialize into objects with more precise types (shapeless records?)
