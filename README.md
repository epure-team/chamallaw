# Chamallaw

Standalone law-domain package scaffold for Epic 89.

## Scope of this first iteration

This package establishes the boundary and composition contract required before
the full contextual-law resolver lands.

It intentionally does **not** implement the full vocabulary catalog, predicate
engine, profile resolver, assignment workflows, or `Context.assemble` cutover.

## Package-owned responsibilities

- package-owned schema fragments
- package-owned store APIs
- typed public package API
- typed host adapter interfaces
- package-owned init protocol with an opaque `ctx`
- package smoke verification

## Host-owned responsibilities

- authoritative SQLite file lifecycle and the host `schema_version` history
- authorized scope and actor context
- normalized work context
- configured Cabal runner injection
- CLI/web/remote adapters
- the single prompt injection point through `Context.assemble`

## Binding execution contract for this iteration

1. The existing `laws` table remains canonical initially.
2. The package must not depend directly on Épure host libraries such as DB,
   agents, web server, TUI, or forge modules.
3. Package-owned schema is initialized through `Chamallaw.init`, which owns a
   package-local `law_package_schema_version` ledger. This ledger coexists with
   the host's `schema_version` table; the host no longer assigns package
   migration slots.
4. Candidate suggestions remain non-authoritative until explicitly accepted.
5. Governance flows fail closed on authoritative law-resolution failure unless
   the host explicitly selects a degraded mode.

## Initialization protocol

Call `Chamallaw.init conn` before using package stores.

- `Ready ctx` means the package schema is current and public package APIs can
  be called with `ctx`.
- `Requires_migration { from_version; to_version; apply }` means the host must
  explicitly approve and run `apply ()`; the returned `ctx` is the only handle
  that enables public store access.

The `ctx` is opaque and holds the checked connection internally. This prevents
callers from accidentally using package-owned stores against an unchecked or
stale schema.

## Chosen package identity

- directory: `libs/chamallaw/`
- opam package: `chamallaw`
- findlib public library: `chamallaw`
- top-level OCaml module: `Chamallaw`
