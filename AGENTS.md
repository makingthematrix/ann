# AGENTS.md

## Project Overview
This project is in Scala 2.13. It is an attempt to use an artificial neural network as a data flow transformer. Having an input stream of symbols which can be decoded into an input vector, the network will be able to generate a stream of more abstract symbols, using as additional information both the context (ie. data which was received before) and time gaps between consecutive chunks of data. The network is reactive - its computations are triggered by the input stream itself, not by a third agent watching the stream and sending requests to the network - and it can simultaneously receive and generate data.

## Repository Structure
- `src/main/scala/anna/`: Core implementation
- `src/test/scala/anna/`: Unit tests

## Development Workflow
1. Always make sure that the previous changes are commited before you add new ones. 
2. If they are not, make a simple `git commit -am "[MESSAGE]"` where MESSAGE is a short description of the change. 
3. After new changes, always run tests with `sbt test`.
4. If any test fail, try to figure out why and make another update.
5. If the test still fail after 2-3 updates, stop your work.
6. Don't push the commits and don't create new branches. Leave it to me.

## Coding Standards
- Use Scala 2.13 and follow idiomatic Scala syntax conventions.
- Prefer using Scala standard library over both imperative loops and more advanced FP techniques that would require adding a dependency.
- Don't throw exceptions - use `Either` and `Try` instead
- Don't use `null` - use `Option` instead
- Read Scaladoc comments attached to every class and method.
- Do not modify files in `target/`.
- Prefer functional programming patterns.
- Avoid modifying the `build.sbt` file unless necessary.
- Use `sbt` for all build tasks.
- Some unit tests deal with concurrency and can be flaky. If a unit test that is not directly related to the feature you work on fails, rerun it (but only once).

## Contacts
- Maintainer: [@makingthematrix](https://github.com/makingthematrix)