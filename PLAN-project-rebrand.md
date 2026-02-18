# PLAN: Project Rebrand from supervisor to elinit

Date: 2026-02-18
Status: Phase 1 Complete

## Final Naming Contract

- Elisp files: `elinit-*.el`
- Interactive UI: `M-x elinit`
- CLI tool: `elinitctl`
- Mode line: `Elinit`

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
- Face names.
- Logo/asset file names.
- .gitignore entries referencing old names.
- CI workflow gist filename reference.

Out of scope:
- Functional behavior redesign.
- New scheduler/process model changes.

## Rename Matrix (Authoritative)

### Elisp modules (10 files)

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

### Scripts and helpers (7 source files, binaries follow)

- `sbin/supervisorctl` -> `sbin/elinitctl`
- `sbin/supervisor-import` -> `sbin/elinit-import`
- `sbin/supervisor-logrotate` -> `sbin/elinit-logrotate`
- `sbin/supervisor-log-prune` -> `sbin/elinit-log-prune`
- `libexec/supervisor-logd.c` -> `libexec/elinit-logd.c`
- `libexec/supervisor-runas.c` -> `libexec/elinit-runas.c`
- `libexec/supervisor-rlimits.c` -> `libexec/elinit-rlimits.c`

Built binaries (`supervisor-logd`, `supervisor-runas`, `supervisor-rlimits`)
are derived from source names and follow automatically.

### Elisp test files (18 files)

- `tests/supervisor-test-helpers.el` -> `tests/elinit-test-helpers.el`
- `tests/supervisor-test-core.el` -> `tests/elinit-test-core.el`
- `tests/supervisor-test-restart.el` -> `tests/elinit-test-restart.el`
- `tests/supervisor-test-validation.el` -> `tests/elinit-test-validation.el`
- `tests/supervisor-test-dag.el` -> `tests/elinit-test-dag.el`
- `tests/supervisor-test-plan.el` -> `tests/elinit-test-plan.el`
- `tests/supervisor-test-units.el` -> `tests/elinit-test-units.el`
- `tests/supervisor-test-timer.el` -> `tests/elinit-test-timer.el`
- `tests/supervisor-test-cli.el` -> `tests/elinit-test-cli.el`
- `tests/supervisor-test-policy.el` -> `tests/elinit-test-policy.el`
- `tests/supervisor-test-dashboard.el` -> `tests/elinit-test-dashboard.el`
- `tests/supervisor-test-sandbox.el` -> `tests/elinit-test-sandbox.el`
- `tests/supervisor-test-targets.el` -> `tests/elinit-test-targets.el`
- `tests/supervisor-test-keywords.el` -> `tests/elinit-test-keywords.el`
- `tests/supervisor-test-identity.el` -> `tests/elinit-test-identity.el`
- `tests/supervisor-test-logformat.el` -> `tests/elinit-test-logformat.el`
- `tests/supervisor-test-logging.el` -> `tests/elinit-test-logging.el`
- `tests/supervisor-test-rlimits.el` -> `tests/elinit-test-rlimits.el`

### C test files (3 files)

- `libexec/tests/test_supervisor_logd.c` -> `libexec/tests/test_elinit_logd.c`
- `libexec/tests/test_supervisor_runas.c` -> `libexec/tests/test_elinit_runas.c`
- `libexec/tests/test_supervisor_rlimits.c` -> `libexec/tests/test_elinit_rlimits.c`

### Shell test files (2 renames, 2 unchanged)

Renamed:
- `sbin/tests/test-supervisorctl.sh` -> `sbin/tests/test-elinitctl.sh`
- `sbin/tests/test-supervisor-import.sh` -> `sbin/tests/test-elinit-import.sh`

No rename needed (no supervisor prefix in filename, internal references updated in place):
- `sbin/tests/test-logrotate.sh`
- `sbin/tests/test-log-prune.sh`

### Assets (2 files)

- `supervisor_logo.png` -> `elinit_logo.png`
- `supervisor_logo_small.png` -> `elinit_logo_small.png`

### Static-build files with supervisor in name (2 files)

- `static-builds/PKGBUILD-static-nox-supervisor-patched-for-pid1` -> `static-builds/PKGBUILD-static-nox-elinit-patched-for-pid1`
- `static-builds/emacs-static-nox-supervisor-patched-for-pid1.nix` -> `static-builds/emacs-static-nox-elinit-patched-for-pid1.nix`

### Build system and config files (content updates, no rename)

- `Makefile` -- update module/test file references
- `libexec/Makefile` -- update binary names, LOGD_PATH/RUNAS_PATH/RLIMITS_PATH macros
- `sbin/Makefile` -- update script references
- `libexec/.gitignore` -- update binary name entries
- `.github/workflows/ci.yml` -- update gist filename `supervisor-el-tests.json`

### Symbols and features

- All public `supervisor-*` -> `elinit-*` (140 public functions)
- All private `supervisor--*` -> `elinit--*` (325 private functions)
- Feature/provide names: `supervisor` -> `elinit`, `supervisor-core` -> `elinit-core`, etc. (10 features)
- Test feature/provide names: `supervisor-test-*` -> `elinit-test-*` (18 features)
- Customization groups: `supervisor` -> `elinit`, `supervisor-timer` -> `elinit-timer`
- User entry command: `supervisor` -> `elinit`
- Mode names: `supervisor-mode` -> `elinit-mode`, `supervisor-dashboard-mode` -> `elinit-dashboard-mode`, `supervisor-edit-mode` -> `elinit-edit-mode`, `supervisor-timer-subsystem-mode` -> `elinit-timer-subsystem-mode`
- Keymaps: `supervisor-dashboard-mode-map` -> `elinit-dashboard-mode-map`, `supervisor-edit-mode-map` -> `elinit-edit-mode-map`
- Mode line string: `"Supervisor"` -> `"Elinit"` (in define-derived-mode)
- Faces (16): `supervisor-status-*` -> `elinit-status-*`, `supervisor-type-*` -> `elinit-type-*`, `supervisor-enabled-*` -> `elinit-enabled-*`, `supervisor-reason` -> `elinit-reason`, `supervisor-section-separator` -> `elinit-section-separator`
- Defcustom variables (42): all `supervisor-*` -> `elinit-*`
- Defvar variables (140): all `supervisor-*`/`supervisor--*` -> `elinit-*`/`elinit--*`
- Hook variable: `supervisor-event-hook` -> `elinit-event-hook`
- Autoload-marked functions (11): all renamed with prefix
- Declare-function forms (101): all target names updated
- CLI branding: `"supervisorctl"` -> `"elinitctl"` in usage/version strings

### Internal string references

- C sources: `supervisor-logd`, `supervisor-runas`, `supervisor-rlimits` in usage/error messages
- Shell scripts: `supervisor-logrotate`, `supervisor-log-prune`, `supervisorctl` in help text
- sbin scripts internally reference `supervisor-logrotate`, `supervisor-log-prune` as helper names
- `sbin/tests/test-logrotate.sh` and `sbin/tests/test-log-prune.sh` reference old script names internally

## Symbol Counts Summary

| Category | Count |
|----------|-------|
| Elisp module files | 10 |
| Elisp test files | 18 |
| C source files | 3 |
| C test files | 3 |
| sbin scripts | 4 |
| sbin test scripts | 2 |
| Defun public | 140 |
| Defun private | 325 |
| Defcustom | 42 |
| Defvar | 140 |
| Defface | 16 |
| Defgroup | 2 |
| Mode definitions | 4 |
| Keymaps | 2 |
| Autoloads | 11 |
| Provide forms | 28 (10 modules + 18 tests) |
| Declare-function | 101 |

## Compatibility Policy

Recommended migration policy:
- Keep compatibility shims for one major cycle.
- Old commands and scripts remain callable as wrappers with deprecation warnings.
- Document cutoff version for shim removal.

Compatibility mechanisms:
- `defalias` for key interactive commands (`supervisor` -> `elinit`).
- Wrapper scripts `supervisorctl` and old helper names exec new binaries.
- Optional `define-obsolete-function-alias` / variable aliases where feasible.

## Intentional Supervisor References Allowlist

### Documentation references (retained now)

These files contain `supervisor` references that are historical or describe the
migration itself and are not renamed:

- `PLAN-project-rebrand.md` -- this plan file (references old names throughout)
- `PLAN-*.md` files in `sbin/`, `libexec/`, `static-builds/` -- historical dev docs
- `static-builds/patches/README-pid1-supervisor-integration.md` -- historical patch docs
- `.github/workflows/ci.yml` -- external gist filename `supervisor-el-tests.json` (rename separately)

### Future code aliases (created in Phase 2, removed in Phase 7)

These `supervisor` symbols will be introduced as compatibility shims during
Phase 2 and are Phase 7 removal candidates.  They do not exist yet:

- `(defalias 'supervisor #'elinit)` and similar key command aliases
- Any `define-obsolete-function-alias` / `define-obsolete-variable-alias` forms

### Boundary rule

All other `supervisor` references must be eliminated by Phase 6 completion.

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

