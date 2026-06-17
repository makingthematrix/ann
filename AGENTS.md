# AGENTS.md

## Project Overview
This is a Scala project implementing a lightweight event streaming library.

## Repository Structure
- `src/main/scala/io/github/makingthematrix/signals3/`: Core implementation
- `src/test/scala/io/github/makingthematrix/signals3/`: Unit tests

## Development Workflow
1. Clone the repository: `git clone https://github.com/makingthematrix/signals3.git`.
2. Run tests: `sbt test`.
3. Run the project: `sbt run`.

## Coding Standards
- Use Scala 3 with braces syntax.

## GitHub Workflow
- Create a new branch for each feature or bug fix.
- Use `feature/xxx` for new features.
- Use `bugfix/xxx` for bug fixes.
- Commit changes with `git commit -am "[MESSAGE]"`
- Push changes to the branch with `git push`
- Don't merge the branch to the main branch directly.

## AI-Specific Guidelines
- Read Scaladoc comments attached to every class and method.
- Always run `sbt test` before committing.
- Do not modify files in `target/`.
- Prefer functional programming patterns.
- Avoid modifying the `build.sbt` file unless necessary.
- Use `sbt` for all build tasks.
- Some unit tests deal with concurrency and can be flaky. If a unit test that is not directly related to the feature you work on fails, rerun it (but only once).

## Contacts
- Maintainer: [@makingthematrix](https://github.com/makingthematrix)