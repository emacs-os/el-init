# PLAN: Project Rebrand from supervisor to elinit

Date: 2026-02-18
Status: Draft

## Final Naming Contract

- Elisp files: `elinit-*.el`
- Interactive UI: `M-x elinit`
- CLI tool: `elinitctl`

## What Users Type

UI:
- `M-x elinit`

CLI:
- `elinitctl status`
- `elinitctl start foo.service`
- `elinitctl restart default.target`
- `elinitctl logs foo.service --follow`
- `elinitctl --json status`

## Goal

Perform a full codebase refactor from `supervisor` naming to `elinit` naming,
including files, symbols, features, commands, scripts, binaries, tests,
documentation, and packaging metadata.

## Scope

In scope:
- Elisp file/module names.
- Public/private Elisp symbols and customization groups.
- Interactive command names and mode names.
- CLI script names and command output branding.
- libexec helper binary/source names.
- Test file names, test symbol names, and test docs.
- README/CLAUDE/docs and static-build documentation references.
- Build system/Makefile targets and package entry points.

Out of scope:
- Functional behavior redesign.
- New scheduler/process model changes.

## Rename Matrix (Authoritative)

Core package and modules:
- `supervisor.el` -> `elinit.el`
- `supervisor-core.el` -> `elinit-core.el`
- `supervisor-log.el` -> `elinit-log.el`
- `supervisor-overrides.el` -> `elinit-overrides.el`
- `supervisor-libexec.el` -> `elinit-libexec.el`
- `supervisor-sandbox.el` -> `elinit-sandbox.el`
- `supervisor-units.el` -> `elinit-units.el`
- `supervisor-timer.el` -> `elinit-timer.el`
- `supervisor-dashboard.el` -> `elinit-dashboard.el`
- `supervisor-cli.el` -> `elinit-cli.el`

Scripts and helpers:
- `sbin/supervisorctl` -> `sbin/elinitctl`
- `sbin/supervisor-import` -> `sbin/elinit-import`
- `sbin/supervisor-logrotate` -> `sbin/elinit-logrotate`
- `sbin/supervisor-log-prune` -> `sbin/elinit-log-prune`
- `libexec/supervisor-logd.c` -> `libexec/elinit-logd.c`
- `libexec/supervisor-runas.c` -> `libexec/elinit-runas.c`
- `libexec/supervisor-rlimits.c` -> `libexec/elinit-rlimits.c`
- built binaries match new source names.

Tests:
- `tests/supervisor-test-*.el` -> `tests/elinit-test-*.el`
- `libexec/tests/test_supervisor_*.c` -> `libexec/tests/test_elinit_*.c`
- `sbin/tests/test-supervisor*.sh` -> `sbin/tests/test-elinit*.sh`

Symbols and features:
- all public `supervisor-*` -> `elinit-*`
- all private `supervisor--*` -> `elinit--*`
- feature/provide names `supervisor*` -> `elinit*`
- customization group `supervisor` -> `elinit`
- user entry command `supervisor` -> `elinit`
- major mode names `supervisor-*mode` -> `elinit-*mode`

## Compatibility Policy

Recommended migration policy:
- Keep compatibility shims for one major cycle.
- Old commands and scripts remain callable as wrappers with deprecation warnings.
- Document cutoff version for shim removal.

Compatibility mechanisms:
- `defalias` for key interactive commands (`supervisor` -> `elinit`).
- wrapper scripts `supervisorctl` and old helper names exec new binaries.
- optional `define-obsolete-function-alias` / variable aliases where feasible.

## Phase Plan

### Phase 1 -- Inventory and Freeze

Code:
- Build authoritative rename map for files, symbols, binaries, scripts.
- Freeze public contract for naming (`elinit`, `elinitctl`).
- Add temporary allowlist of intentional `supervisor` references (migration docs only).

Deliverable:
- Reviewed rename matrix committed.

### Phase 2 -- Elisp Module and Symbol Refactor

Code:
- `git mv` all Elisp files to `elinit-*.el`.
- Rename package entry point, provides, requires, and declare-function strings.
- Mechanical symbol rename: `supervisor-`/`supervisor--` to `elinit-`/`elinit--`.
- Update autoload cookies and interactive command names.
- Update customization group names and defaults paths with `supervisor` prefixes.

Deliverable:
- `M-x elinit` is the primary entry point.
- Elisp byte-compile is clean under new names.

### Phase 3 -- CLI and sbin Refactor

Code:
- Rename `sbin/supervisorctl` to `sbin/elinitctl`.
- Rename related sbin tools (`import`, `logrotate`, `log-prune`) to `elinit-*`.
- Update CLI usage text, help output, error text, and docs to `elinitctl`.
- Ensure command examples in help match final naming contract.

Deliverable:
- `elinitctl` supports all existing command paths.
- JSON mode and status/show/logs commands unchanged semantically.

### Phase 4 -- libexec Refactor

Code:
- Rename C helper sources/binaries to `elinit-*`.
- Update libexec make targets, binary path macros, and compile/test flow.
- Update Elisp helper command path variables to new binary names.

Deliverable:
- Launch path works with `elinit-rlimits -> elinit-runas -> bwrap -> service`.
- `make -C libexec check` passes with renamed helpers.

### Phase 5 -- Test Suite Refactor and Expansion

Code:
- Rename Elisp, C, and shell test files to `elinit` naming.
- Rename test symbols/cases accordingly.
- Add regression tests for compatibility aliases/wrappers (if enabled).
- Ensure CI targets run renamed suites.

Deliverable:
- `make check` passes end-to-end.
- No loss of behavioral coverage due to rename churn.

### Phase 6 -- Documentation and Packaging

Code:
- Update `README.org`, `CLAUDE.md`, and other docs to `elinit` naming.
- Update static-build docs and patch references to new names where user-facing.
- Update badges, URLs, package metadata, and MELPA-facing identifiers if required.

Deliverable:
- Admin docs consistently show `M-x elinit` and `elinitctl` examples.
- No stale user-facing `supervisor` names outside migration notes.

### Phase 7 -- Compatibility Cleanup (Optional Timeboxed)

Code:
- Remove temporary wrappers/aliases after migration window.
- Remove allowlist exceptions for `supervisor` references.

Deliverable:
- Clean final state with only `elinit` branding.

## Acceptance Criteria

- User entry point is `M-x elinit`.
- CLI entry point is `elinitctl` and examples in this plan run as documented.
- Full repository search contains no user-facing `supervisor` strings except
  explicitly labeled migration compatibility notes.
- All modules load under new feature names without circular dependency regressions.
- `make check`, `make -C libexec check`, and `make -C sbin check` pass.

## Verification Checklist

- `rg -n "\\bsupervisor\\b" .` shows only approved migration references.
- `rg -n "\\belinit\\b" README.org CLAUDE.md sbin libexec tests` confirms coverage.
- `emacs -Q -l elinit.el` loads cleanly.
- `sbin/elinitctl status` works in expected runtime conditions.
- `sbin/elinitctl --json status` outputs valid JSON.

## Risks and Mitigations

Risk:
- Large mechanical rename causes hidden load-order or provide/require breaks.

Mitigation:
- Use staged `git mv` + compile/test gates at each phase.
- Keep temporary aliases during transition.

Risk:
- External scripts and administrator workflows rely on old names.

Mitigation:
- Ship wrappers plus explicit deprecation messaging.
- Provide migration section with exact command mapping.

Risk:
- Docs drift from implementation during high-churn rename.

Mitigation:
- Treat docs update as mandatory in same phase gate before completion.

## Recommended Execution Strategy

- Execute in small commits per phase.
- Run `make check` at end of each phase.
- Do not combine behavioral changes with rename commits.

