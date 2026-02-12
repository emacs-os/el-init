# PLAN: Service Units and Command-Surface Normalization

Date: 2026-02-12
Status: Approved planning baseline
Source of truth: `FINDINGS.md` TODO items 1-6 and 9

## Scope

This plan covers:

- unit-files-only runtime model,
- removal of user-facing `reconcile`,
- restart policy model replacement,
- oneshot keyword rename and consistency pass,
- oneshot/systemd comparison documentation,
- CLI rename `validate` -> `verify` (with `blame` unchanged).

This plan explicitly excludes logging architecture work (covered by `PLAN-logging.md`).

## Locked Decisions (Do Not Reopen)

1. Unit-files-only mode:
- remove legacy `supervisor-programs` runtime path.

2. Remove user-facing `reconcile`:
- no CLI command, no help/docs/dashboard references.

3. Restart policy model:
- replace binary `on|off` with `no|on-success|on-failure|always`.

4. Oneshot option rename:
- `:async` -> `:oneshot-async`
- `:oneshot-wait` -> `:oneshot-blocking`
- semantics remain equivalent.

5. Consistency pass is mandatory:
- parser/defaults/validation/messages/docs/tests must all align.

6. Add systemd comparison note for oneshot behavior.

7. CLI terminology normalization:
- rename `validate` to `verify` (hard break, no alias),
- keep `blame` unchanged,
- do not add an `analyze` namespace.

8. ABI policy for this plan:
- hard break is intentional; no compatibility aliases/shims for removed names.

## Canonical Contracts

### A) Service source model

- Runtime service definitions come from unit files only.
- `supervisor-unit-directory` remains the canonical source path.
- No merge path from `supervisor-programs`.
- Dashboard/CLI/core/timer must all consume the same effective unit-file set.

### B) `reconcile` policy

- User-facing `reconcile` is removed.
- Internal convergence machinery may remain temporarily for config-watch internals,
  but is not user-invocable and not user-documented.

### C) Restart policy semantics

Policies:

- `no`: never auto-restart.
- `on-success`: restart only for clean exits.
- `on-failure`: restart for non-clean exits.
- `always`: restart regardless of exit type.

Clean exit definition (for `on-success`):

- process status `exit` with code `0`,
- or process status `signal` with one of: `SIGHUP`, `SIGINT`, `SIGTERM`, `SIGPIPE`.

Restart suppression gates (still apply to all non-`no` policies):

- shutdown in progress,
- unit manually stopped,
- unit disabled/masked by effective policy,
- crash-loop threshold exceeded.

### D) Oneshot keyword semantics

- Canonical keys:
  - `:oneshot-blocking`
  - `:oneshot-async`
- Mutually exclusive when both explicitly set with conflicting intent.
- Effective behavior remains equivalent to current behavior:
  - blocking holds stage completion until exit/timeout,
  - async does not block stage completion.

### E) `verify` command contract

- CLI command is `verify`.
- CLI command `validate` must return unknown-command error.
- Emacs command is `supervisor-verify` (not `supervisor-validate`).
- Internal low-level helper names may remain `supervisor--validate-*` where private.
- `blame` command name remains unchanged.

## Phase Plan (10 Phases)

### Phase 1: Unit-File Runtime Unification

Deliverables:

- remove legacy runtime indirection/merge path from effective loader,
- make unit-file loading canonical in core call sites.

Acceptance:

- no runtime code path depends on `supervisor-programs` for service loading.

### Phase 2: Remove Legacy Config Knobs and References

Deliverables:

- remove `supervisor-programs` and `supervisor-use-unit-files` configuration surface,
- update dashboard/timer/core call sites that still iterate legacy config directly.

Acceptance:

- no remaining references to removed runtime knobs in executable code paths.

### Phase 3: Test Fixture Migration to Unit Files

Deliverables:

- introduce shared test helper for temp unit directories/files,
- convert tests from in-memory `supervisor-programs` fixtures to unit-file fixtures.

Acceptance:

- converted tests remain deterministic and isolated,
- `make check` passes for converted subset before proceeding.

### Phase 4: Remove User-Facing `reconcile`

Deliverables:

- remove CLI dispatch/help/wrapper references,
- remove user docs references,
- ensure dashboard/transient help does not mention it.

Acceptance:

- `supervisorctl reconcile` is unknown command,
- no user-facing documentation mentions reconcile workflows.

### Phase 5: Internal Convergence/Config-Watch Stabilization

Deliverables:

- keep or refactor internal reconcile machinery as private-only implementation detail,
- ensure config-watch behavior remains coherent without user-facing reconcile command.

Acceptance:

- config-watch path remains functional,
- no public command or interactive entry point exposes reconcile.

### Phase 6: Restart Policy Data Model and Runtime Logic

Deliverables:

- parser and schema store restart policy symbol (not boolean),
- override persistence updated for symbol policies,
- sentinel restart decision uses canonical four-policy semantics.

Acceptance:

- restart behavior matches policy matrix for exit/signal scenarios,
- crash-loop gating still works.

### Phase 7: Restart Policy CLI and Dashboard UX

Deliverables:

- CLI grammar: `restart-policy (no|on-success|on-failure|always) ID...`,
- dashboard restart control updated to set/cycle policy values,
- help text and table output updated.

Acceptance:

- CLI and dashboard both expose all four policies consistently.

### Phase 8: Oneshot Keyword Rename (Hard Cut)

Deliverables:

- rename parser/validation/accessors/struct fields/default variables to canonical names,
- update unit keyword whitelist and scaffold templates,
- remove old keyword acceptance.

Acceptance:

- old keys `:async` and `:oneshot-wait` are rejected,
- new keys work everywhere (core, units, dashboard, tests).

### Phase 9: Oneshot Consistency + Documentation Note

Deliverables:

- full code/message/docs/test sweep for renamed oneshot terms,
- add systemd comparison note covering oneshot blocking/timeout mapping.

Acceptance:

- no stale old-key wording in user-facing docs/messages,
- README includes accurate oneshot/systemd comparison note.

### Phase 10: `validate` -> `verify` Command Cutover

Deliverables:

- rename CLI command path and help strings to `verify`,
- update wrapper docs/help,
- rename interactive command surface to `supervisor-verify`,
- keep `blame` unchanged and do not add `analyze` namespace.

Acceptance:

- `supervisorctl verify` works,
- `supervisorctl validate` fails as unknown command,
- no `analyze` command path introduced.

## Required File Categories to Update

- Core/runtime:
  - `supervisor-core.el`
  - `supervisor-units.el`
  - `supervisor-timer.el`
  - `supervisor-dashboard.el`
  - `supervisor-cli.el`
- Shell wrapper/help:
  - `sbin/supervisorctl`
  - `sbin/README.md`
- Tests:
  - `supervisor-test.el`
- Documentation:
  - `README.org`
  - `CLAUDE.md` (architecture notes that mention removed runtime model)

## Explicit Non-Goals

- No logging pipeline changes in this plan.
- No `analyze` namespace introduction.
- No `reset-failed` implementation.
- No journal-style log query functionality.

## Definition of Done

All must be true:

1. Runtime is unit-files-only (no legacy runtime path).
2. `reconcile` is not user-facing.
3. Restart policy is four-state model and enforced consistently.
4. Oneshot keywords are only `:oneshot-blocking` / `:oneshot-async`.
5. Full rename consistency pass is complete (code/docs/tests/messages).
6. README includes oneshot/systemd comparison note.
7. CLI command is `verify`; `validate` is removed.
8. `blame` remains unchanged.
9. `analyze` namespace is not introduced.
10. `make check` passes.

