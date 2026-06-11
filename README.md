# Chamallaw

Chamallaw is an OCaml law-domain package extracted from Épure. It owns
package-local law/concept schema fragments, store APIs, typed host adapter
interfaces, and a checked initialization protocol. Host applications own the
SQLite database lifecycle, authorization model, prompt/context policy, UI/API
adapters, and product workflow orchestration around it.

The public standalone repository target is
[`epure-team/chamallaw`](https://github.com/epure-team/chamallaw). The source
of truth remains `epure-team/epure:libs/chamallaw`.

## Package status

Chamallaw currently provides the boundary and composition contract needed before
the full contextual-law resolver lands. Candidate suggestions are still
non-authoritative until a host explicitly accepts them, and governance flows
should fail closed on authoritative law-resolution failures unless the host
selects a documented degraded mode.

## Responsibilities

Chamallaw owns:

- package-owned schema fragments and migration ledger;
- package-owned store APIs;
- typed public package API;
- typed host adapter interfaces;
- package-owned init protocol with an opaque `ctx`;
- package smoke verification.

Host applications own:

- authoritative SQLite file lifecycle and any host `schema_version` history;
- authorized scope and actor context;
- normalized work context;
- configured Cabal runner injection;
- CLI/web/remote adapters;
- prompt-context assembly and injection policy.

Chamallaw must not depend directly on Épure host libraries such as `epure_lib`,
`epure_db`, agents, web server, TUI, forge modules, or architecture-indexing
tools.

## Build and test

From the standalone Chamallaw repository:

```bash
opam pin add cabal git+https://github.com/epure-team/cabal.git#main -n -y
opam install . --deps-only --with-test --with-doc -y
opam exec -- dune build @install
opam exec -- dune runtest
opam exec -- dune build @doc
opam lint chamallaw.opam
```

The explicit Cabal pin is temporary: Chamallaw examples/tests use Cabal for real
backend invocation, while Cabal is not yet assumed to be available from the
default opam repositories. The standalone CI follows the same strategy by
pinning Cabal from `epure-team/cabal` for now.

When Chamallaw is vendored inside Épure, Dune sees `libs/chamallaw` and
`libs/cabal` in the same workspace, so no opam pin is required for Épure-side
builds.

## Initialization protocol

Call `Chamallaw.init conn` before using package stores.

- `Ready ctx` means the package schema is current and public package APIs can be
  called with `ctx`.
- `Requires_migration { from_version; to_version; apply }` means the host must
  explicitly approve and run `apply ()`; the returned `ctx` is the only handle
  that enables public store access.

The `ctx` is opaque and holds the checked connection internally. This prevents
callers from accidentally using package-owned stores against an unchecked or
stale schema.

## Package identity

- standalone repository: `epure-team/chamallaw`
- source-of-truth directory in Épure: `libs/chamallaw/`
- opam package: `chamallaw`
- findlib public library: `chamallaw`
- top-level OCaml module: `Chamallaw`
- license: MIT

## Runnable Cabal backend workflow example

`examples/chamallaw_claude_workflow.ml` demonstrates a local Chamallaw
law+concept workflow against a file-backed SQLite database and a real backend
selected through Cabal at runtime. It defaults to `claude-code` and also
supports `codex`.

Run it from the standalone repository root with an authenticated backend CLI on
`PATH`:

```bash
opam exec -- dune exec examples/chamallaw_claude_workflow.exe -- \
  --backend claude-code \
  --model haiku
```

To use Codex instead:

```bash
opam exec -- dune exec examples/chamallaw_claude_workflow.exe -- \
  --backend codex
```

Add `--debug-cabal` when debugging the Cabal/backend invocation; it prints Cabal
diagnostics to stderr, including the backend command line.

Options:

- `--project-dir DIR` (default: a unique `0700` temp directory named like
  `$TMPDIR/chamallaw-cabal-workflow-<suffix>`) is the working directory used by
  Cabal and stores generated example files.
- `--db PATH` overrides the SQLite database path (default:
  `$project_dir/chamallaw-demo.db`).
- `--log PATH` overrides the curator log path (default:
  `$project_dir/curator-output.log`).
- `--backend BACKEND` selects the real Cabal backend. Supported values are
  `claude-code` (default) and `codex`.
- `--model MODEL` passes a backend-specific model override; omit it to use the
  selected CLI default. `CHAMALLAW_MODEL` is also honored, with
  `CHAMALLAW_CLAUDE_MODEL` and `CHAMALLAW_CODEX_MODEL` as backend-specific
  fallbacks.
- `--curator-output-json PATH` skips the live backend call and applies a local
  curator JSON document. This is intended for deterministic local smoke tests of
  DB creation, vocabulary seeding, parsing, application, queries, and log
  generation; it does not introduce a mock or fake Cabal backend.
- `--debug-cabal` installs a Cabal diagnostics handler that forwards debug/info/
  warn/error messages to stderr so `backend command: ...` is visible.

The example:

1. initializes Chamallaw and applies package migrations when needed;
2. seeds the built-in vocabulary;
3. creates project-local concept schemes/concepts and law↔concept links;
4. invokes the selected real Cabal backend through
   `Cabal.Agentic_backend.run_task` with a JSON Schema-constrained
   `curate_ontology` request;
5. logs raw curator output, stderr/agent text on error paths, and parsed
   structured curator output to stdout and the visible log file path;
6. applies curator concepts/laws/links back into Chamallaw stores;
7. prints final concepts, final laws, and concept queries with associated laws.

The live curator call is intentionally manual-only: tests/builds compile the
example but do not call a live backend. If the selected backend is missing or
unauthenticated, the example exits non-zero with the backend error and keeps the
log file for review.

## Sync model

The current source of truth is `libs/chamallaw` in the Épure monorepo. Épure CI
dispatches Chamallaw's mirror-sync workflow on pushes to Épure `main`. The
Chamallaw standalone repository receives a `repository_dispatch` event, validates
that the payload came from `epure-team/epure@refs/heads/main`, checks out the
exact payload SHA, verifies that SHA is still the latest Épure `main`, splits
the `libs/chamallaw` subtree, and pushes the split commit to
`epure-team/chamallaw:main` with a short-lived GitHub App installation token.

The mirror app installed on `epure-team/chamallaw` needs Contents write and
Workflows write permissions because mirrored subtree updates may change
`.github/workflows/*` files. Épure only dispatches the sync event; it does not
hold branch-write or branch-protection-bypass credentials for Chamallaw.

Only one-way sync from Épure is enabled for now. There is intentionally no
`pull_request_target`, `sync-pr-to-epure`, or other PR mirror-back workflow in
Chamallaw yet.

Normal contribution flow for now:

1. change Chamallaw under `libs/chamallaw` in Épure;
2. merge through Épure's normal review path;
3. let the mirror-sync workflow update `epure-team/chamallaw` automatically.

Do not merge direct Chamallaw PRs independently except for an emergency fix that
cannot wait for the Épure path. If that escape hatch is used, reconcile the
change in `libs/chamallaw` immediately so Épure remains the source of truth.
