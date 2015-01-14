# Tombinator

Tombinator is a Scala parser for the [TOML](https://github.com/toml-lang/toml)
file format.
Its implementation is based on
[parboiled2](https://github.com/sirthias/parboiled2)'s
parser combinators.

(Almost) compatible with
TOML [v0.3.1](https://github.com/toml-lang/toml/tree/v0.3.1).
The test suite includes all fixtures from
[toml-test](https://github.com/BurntSushi/toml-test).
Tombinator was written prioritizing correctness and fidelity to the spec over
speed.
I've done no benchmarking, and I'm sure there is tons of room for improvement.

Tombinator is open source, under the MIT license (see LICENSE.txt).

## TODO
- [x] 8 digit unicode format
- [ ] Disallow control characters
- [ ] Check that types of all elements of array match (up to erasure)
- [ ] Check that BurntSushi tests match expected JSON
- [ ] Write serializer
- [ ] De-serialize into objects with more precise types (shapeless records?)
