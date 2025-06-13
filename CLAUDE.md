# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is `me.lomin/ex`, a Clojure library that provides enhanced exception handling and flow control mechanisms. It extends Clojure's try/catch with keyword-based exits, ExceptionInfo navigation via Specter, tracing capabilities, and example-driven development features.

## Development Commands

**Testing:**
```bash
clojure -M:test -m cognitect.test-runner    # Run all tests
clojure -T:build test                       # Run tests via build tool
```

**Linting:**
```bash
clojure -M:lint --lint src test    # Run clj-kondo linter
```

**Building:**
```bash
clojure -T:build ci          # Run CI pipeline (test + build JAR)
clojure -T:build install     # Install JAR locally
clojure -T:build deploy      # Deploy to Clojars
```

## Architecture

The library is built around four core concepts:

1. **Enhanced Exception Handling (`try+`)**: Extends try/catch with keyword-based exits and Specter navigation for ExceptionInfo data
2. **Tracing and Context (`with-ex`)**: Provides execution context threading and hierarchical trace logging
3. **Code Exchange (`exchange`)**: Enables runtime code replacement for testing with lazy evaluation
4. **Example-Driven Development**: Macros like `with-examples` and `example` support replaceable code blocks

## Key Dependencies

- `metosin/malli "0.18.0"` for schema validation (recent breaking change update)
- `com.rpl/specter "1.1.4"` for data navigation in exception handling
- `org.clojure/tools.analyzer.jvm` for AST manipulation in macros

## Testing Setup

Uses cognitect.test-runner with property-based testing via test.check. Test resources include XML currency data and JSON payloads for comprehensive scenario testing. The library includes custom clj-kondo exports for static analysis of its macros.

## Important Patterns

- Extensive Malli schema validation throughout
- AST manipulation for macro transformations
- Specter for complex data navigation in exceptions
- Multimethod dispatch for extensible behavior
- Custom proxy classes for exception types