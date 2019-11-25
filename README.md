# breadcrumbs

### Introduction

Very simple stack traces.
Exposes
  - `TraceT e s m a`, which is `ExceptT e m a` augmented with stack frames of type `s`.
  - `MonadTrace`, a subclass of `MonadError`.
  - The same convenience functions for `ExceptT` adapted to `TraceT`
  - `Trace e s a` as a synonym for `TraceT e s Identity a`.

Errors can be thrown with `throwError` from `MonadError`.
To annotate a stack frame, use `traceError` from `MonadTrace`.

Has some, but not all instances for interop with `mtl`.
Please add as needed
