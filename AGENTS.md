# AGENTS.md for Chamallaw

Chamallaw is the law-domain OCaml package extracted from Épure. Keep it usable
both as a standalone library and as the vendored subtree under
`epure/libs/chamallaw`.

## Chamallaw-specific rules

- Source of truth remains `libs/chamallaw` in the Épure monorepo until a later
  governance change says otherwise.
- The standalone repository target is `epure-team/chamallaw`; it is updated by a
  one-way subtree mirror from Épure. Do not add `sync-pr-to-epure`,
  `pull_request_target`, or any PR mirror-back workflow yet.
- Keep Chamallaw host-neutral. It may define law/concept stores, schema
  fragments, typed host adapters, and checked initialization, but it must not
  depend on `epure_lib`, `epure_db`, `epure_agents`, Épure web/TUI modules,
  forge modules, or architecture-indexing tools.
- Host applications own database file lifecycle, authorization, prompt-context
  policy, UI/API adapters, and product workflow orchestration.
- Tests for Chamallaw live under `libs/chamallaw/test` in Épure and under
  `test` in the standalone mirror.
- Public APIs must be documented in `.mli` files. Keep private helpers in
  implementation files unless they are intentionally part of the library API.
- Maintain `dune-project`, `chamallaw.opam`, README, changelog, and standalone
  CI workflow metadata when changing dependencies, supported OCaml versions, or
  test/build requirements.
- Cabal is currently consumed through an explicit opam pin to
  `epure-team/cabal` in standalone CI because it is not assumed to be published
  in the default opam repositories yet. In the Épure monorepo, Dune sees
  `libs/cabal` directly and no pin is needed.
- The Chamallaw mirror-sync workflow must remain repository-dispatch-driven from
  Épure `main` only. It must validate payload repository/ref/SHA, check out the
  exact Épure SHA, verify the SHA is still latest Épure `main`, subtree-split
  `libs/chamallaw`, and push with the Chamallaw mirror GitHub App token carrying
  Contents and Workflows write permissions.
- Épure only dispatches the sync event. Do not reintroduce deploy keys or direct
  branch-write pushes from Épure to Chamallaw.
- Direct changes in `epure-team/chamallaw` should not be merged independently
  except for emergency fixes that cannot wait for the Épure path. Reconcile any
  emergency direct fix back into `libs/chamallaw` immediately.
