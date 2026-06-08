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

## Runnable Claude workflow example

`examples/chamallaw_claude_workflow.ml` demonstrates a local Chamallaw
law+concept workflow against a file-backed SQLite database and the real Cabal
Claude Code backend.

Run it from the repository root with an authenticated `claude` CLI on `PATH`:

```bash
EPURE_NO_COMMIT_CHECK=1 dune exec libs/chamallaw/examples/chamallaw_claude_workflow.exe -- \
  --model haiku
```

Options:

- `--project-dir DIR` (default: a unique `0700` temp directory named like
  `$TMPDIR/chamallaw-claude-workflow-<suffix>`) is the working directory used by
  Cabal/Claude and stores generated example files.
- `--db PATH` overrides the SQLite database path (default:
  `$project_dir/chamallaw-demo.db`).
- `--log PATH` overrides the curator log path (default:
  `$project_dir/curator-output.log`).
- `--model MODEL` passes a Claude model override; omit it to use the CLI default.
  `CHAMALLAW_CLAUDE_MODEL` is also honored.
- `--curator-output-json PATH` skips the live Claude call and applies a local
  curator JSON document. This is intended for deterministic local smoke tests of
  DB creation, vocabulary seeding, parsing, application, queries, and log
  generation; it does not introduce a mock or fake Cabal backend.

The example:

1. initializes Chamallaw and applies package migrations when needed;
2. seeds the built-in vocabulary;
3. creates project-local concept schemes/concepts and law↔concept links;
4. invokes `Cabal.Claude_code` through `Cabal.Agentic_backend.run_task`
   with a JSON Schema-constrained `curate_ontology` request;
5. logs raw curator output, stderr/agent text on error paths, and parsed
   structured curator output to stdout and the visible log file path;
6. applies curator concepts/laws/links back into Chamallaw stores;
7. prints final concepts, final laws, and concept queries with associated laws.

The live curator call is intentionally manual-only: tests/builds compile the
example but do not call Claude. If Claude is missing or unauthenticated, the
example exits non-zero with the backend error and keeps the log file for review.
