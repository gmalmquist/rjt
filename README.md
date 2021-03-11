# Rust Java Tool
`rjt` is a CLI utility for analyzing `.jar` files.

## Features
### Print Public API
Run `rjt dump <foo.jar>` on a jar to output the defined classes, methods,
and service interfaces.

Adding the `-r` or `--references` option will additionally print all
*references* made to other classes, methods, etc.

### Check Reference Validity
Run `rjt check <foo.jar>` to check all references (class references,
method calls, references to fields) to ensure that the relevant symbols
are actually defined inside the fatjar (or are part of the Java SDK).

Note that even fully operational fatjars are likely to contain a
number of invalid references, because it is common for thirdparty
java libraries to conditionally reference other libraries if it
detects a particular platform (e.g., windows only libraries, or
logging libraries that may be used optionally).

### Check Invalid Reference Diffs
Run `rjt compare-missing <one.jar> <two.jar>` to print all invalid references
made by `two.jar` which are _not_ made by `one.jar`. This is useful
for identifying cases where changing dependencies or build tooling
is likely to cause new runtime breakages.

## Options
 * `--parallelism` Set the size of the threadpool used to parse fat jars.
   Defaults to 8.
 * `--full` When printing invalid references, list out all classes which
   make those references, rather than just counting up how many there are.

## Building and Running
Run `cargo build --release` to create an optimized binary at
`target/release/rjt`.

If you don't have `cargo` installed, you can get it by installing the rust
toolchain [here](https://rustup.rs/).

For development, I recommend running with `cargo run --release -- <args>`.
Running without the `--release` flag will build slightly faster,
but because of incremental compilation the difference is not super
noticable. Furthermore, the optimized binary runs *several* times
faster than the unoptimized binary, so running the optimized one
actually yields much faster iteration when testing changes.

## Implementation Notes
To identify classes provided by the standard library, the tool actually
downloads the list of classes from
[Oracle's website](https://docs.oracle.com/en/java/javase/11/docs/api/allclasses.html),
parses it, and then spins up a java sidecar to dump the list of methods and fields
for each class listed.

This feels gross and inefficient, though in practice it actually executes
quite quickly (just a few seconds), and is negligible compared to time
spent parsing the actual fatjar's bytecode.

Currently, it is also hardcoded to pull this class list for java 11,
which may result in slightly inaccurate analysis for fatjars
targetting different runtimes.

There are a few ways this might be improved:
 * Make the page it pulls the class list for configurable, so it works with different
   versions of Java.
 * Make the version of java we run more easily configurable (currently it just checks
   the `JAVA_HOME` environment variable).
 * Check in a binary blob storing all the metadata for JVM-provided classes, so we
   don't have to calculate it each time at runtime.

