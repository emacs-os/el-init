# PLAN: Unit-File NICE-TO-HAVE Expansion (PT3)

Date: 2026-02-12
Status: Tightened draft
Source: `FINDINGS-PT2.md` Table 8 (`NICE TO HAVE` bucket only)

## Scope

Implement only NICE-TO-HAVE unit features from `FINDINGS-PT2.md`:

1. `:wants`
2. `:before`
3. `:success-exit-status`
4. `:kill-signal`
5. `:kill-mode`
6. `:remain-after-exit`
7. `:description`
8. `:documentation`

Out of scope here:

- All MUST-HAVE work from `PLAN-PT2.md`.
- All PROBABLY-BLOAT items from `FINDINGS-PT2.md`.

## Locked Decisions

1. Unit files stay Elisp plist format; no INI parser.
2. Feature names are hard-cut canonical names (no aliases/shims).
3. Multi-declarable systemd directives map to list values under one plist key.
4. Duplicate plist keys are invalid.
5. Stage model remains runit-like; no target-model rewrite in this plan.
6. `:kill-mode` supports only `process` and `mixed` (no `control-group`, no
   `none`).
7. `:success-exit-status` is valid only for `simple` units.
8. `:remain-after-exit` is valid only for `oneshot` units.

## Canonical Value Shapes

| Key | Accepted forms | Normalized form |
|---|---|---|
| `:description` | string or `nil` | string or `nil` |
| `:documentation` | string, list of strings, or `nil` | ordered list of strings |
| `:before` | string, list of strings, or `nil` | ordered list of unit IDs |
| `:wants` | string, list of strings, or `nil` | ordered list of unit IDs |
| `:kill-signal` | symbol/string signal name or `nil` | canonical signal symbol (default `SIGTERM`) |
| `:kill-mode` | symbol `process`/`mixed` or `nil` | symbol (`process` default) |
| `:remain-after-exit` | boolean or `nil` | boolean (`nil` default) |
| `:success-exit-status` | int, signal symbol/string, list mix, or `nil` | two sets: exit codes + signals |

Normalization rules:

- Single string forms become one-element lists for `:documentation`, `:before`,
  and `:wants`.
- Duplicate items in list-valued keys are de-duplicated with stable first-order
  preservation.
- Signal names accept `TERM`/`SIGTERM` form and normalize to `SIGTERM` symbol.

## Multi-Declaration Parity Rules

Systemd-repeatable directives and PT3 mapping:

- `Documentation=` -> `:documentation` list.
- `Before=` -> `:before` list.
- `Wants=` -> `:wants` list.
- `SuccessExitStatus=` -> `:success-exit-status` list.

Scalar-only directives:

- `Description=` -> `:description` string.
- `KillSignal=` -> `:kill-signal` scalar.
- `KillMode=` -> `:kill-mode` scalar.
- `RemainAfterExit=` -> `:remain-after-exit` boolean.

## Runtime Semantics Contract

### 1) Metadata (`:description`, `:documentation`)

- Metadata only; no lifecycle impact.
- Exposed in CLI `show`, JSON, and dashboard detail/info view.
- `:documentation` is treated as opaque strings (URIs/paths/text), no URI
  strict validation.

### 2) Dependency semantics (`:before`, `:wants`)

`before` contract:

- `A :before B` is normalized into an ordering edge equivalent to `B :after A`.
- Same-stage-only rule (matches current stage model constraints).
- Cross-stage `:before` references: warning + edge ignored.

`wants` contract:

- Soft dependency with ordering preference.
- Same-stage-only rule in this stage model.
- Missing/invalid/masked/disabled wanted unit does not fail or block the
  wanting unit.
- A wanted unit that starts and then fails does not block the wanting unit.
- This plan does not redefine global enable/disable semantics; `:wants` does not
  force-start disabled units.

Cycle behavior:

- Existing cycle-detection/fallback behavior remains authoritative.
- `:before` and `:wants` edges participate in cycle detection.

### 3) Stop semantics (`:kill-signal`, `:kill-mode`)

`kill-signal` contract:

- Default stop signal is `SIGTERM`.
- If `:kill-signal` is set, it replaces the default graceful stop signal.
- Applies to stop/restart/shutdown stop paths.

`kill-mode` contract:

- `process` (default): signal only main supervised process, then fallback
  `SIGKILL` to main process on timeout.
- `mixed`: send graceful `kill-signal` to main process first, then on timeout
  send `SIGKILL` to main process and discovered descendants.

Descendant discovery for `mixed`:

- Use PID-tree traversal via `list-system-processes` + `process-attributes`.
- Snapshot descendants at escalation time.
- If OS/process metadata is unavailable, log warning and fall back to `process`.

No cgroup semantics are introduced.

### 4) Oneshot status semantics (`:remain-after-exit`)

- Valid only for `oneshot` units.
- If `t` and oneshot exits successfully: status is `active` after exit.
- If exit non-zero/signal failure: status is `failed` (not `active`).
- Startup DAG readiness behavior is unchanged (dependents unlock on oneshot exit
  as today).
- `stop` on active remain-after-exit unit transitions it to `stopped` state.
- `start` on already-active remain-after-exit unit is a no-op.
- `restart` explicitly re-runs the oneshot.

### 5) Success criteria semantics (`:success-exit-status`)

- Valid only for `simple` units.
- Extends what is treated as a "clean" result for restart-policy evaluation.
- Accepts extra numeric exit codes and/or signal names.
- Default clean set remains existing baseline plus user additions from
  `:success-exit-status`.

Policy effect:

- `on-failure`: added success outcomes do not restart.
- `on-success`: added success outcomes do restart.
- `always`/`no`: unaffected except for status labeling consistency.

Dependency:

- This feature is blocked until four-state restart policy work is present
  (`PLAN-service-units.md`).

## Validation Contract Additions

- Add all eight keys to keyword whitelist(s).
- Enforce duplicate-key rejection.
- Enforce key-specific shape/type errors with precise messages.
- Enforce type gating:
  - `:remain-after-exit` invalid for `simple`.
  - `:success-exit-status` invalid for `oneshot`.
- Enforce value enums:
  - `:kill-mode` in `{process,mixed}` only.

## Phase Plan

### Phase N1: Parser/Schema/Validation

Deliverables:

- Add keys to unit and core whitelists.
- Extend service schema + accessors.
- Implement normalization helpers for repeatable keys.
- Implement duplicate-key and shape/type validation.

Acceptance:

- Parse/validate tests pass for each key.

### Phase N2: Metadata Surface

Deliverables:

- Wire `:description` and `:documentation` into CLI show/JSON and dashboard
  detail view.

Acceptance:

- Metadata appears consistently in all detail surfaces.

### Phase N3: Ordering and Soft Dependencies

Deliverables:

- Implement `:before` inversion into ordering graph.
- Implement `:wants` soft-edge semantics.
- Keep stage constraints and cycle fallback behavior explicit.

Acceptance:

- Tests prove ordering correctness and non-blocking soft dependency behavior.

### Phase N4: Kill Controls

Deliverables:

- Implement `:kill-signal` normalization and stop-path wiring.
- Implement `:kill-mode process|mixed` semantics with documented fallback.

Acceptance:

- Tests cover custom signal, process mode, mixed mode, and fallback path.

### Phase N5: Oneshot Active-Latch

Deliverables:

- Implement `:remain-after-exit` state behavior and start/stop/restart rules.
- Reflect status in CLI/dashboard consistently.

Acceptance:

- Tests cover success, failure, no-op start, explicit restart, and stop.

### Phase N6: Success Exit Status

Deliverables:

- Implement `:success-exit-status` restart-policy integration.

Acceptance:

- Tests cover numeric/signal additions and policy outcomes.
- Phase gated on four-state restart-policy completion.

### Phase N7: Handbook + Unplanned List

Deliverables:

- Update `README.org` (the handbook surfaced by `M-x supervisor-handbook`) with
  all PT3 options and exact semantics.
- Add explicit "Unplanned" section listing:
  - PROBABLY-BLOAT items from `FINDINGS-PT2.md`.
  - all other systemd service directives not implemented by PT2/PT3/approved
    plans.
- If project wiki is maintained separately, mirror the same unplanned table.

Acceptance:

- Handbook has no aspirational language and matches implemented behavior.
- Unplanned list is explicit and easy to audit.

## Required Test Matrix (Minimum)

1. Duplicate-key rejection for each new key.
2. String-or-list normalization for `:documentation`, `:before`, `:wants`.
3. `:before` inversion parity with explicit `:after`.
4. `:wants` non-blocking semantics on missing/disabled/failed wanted units.
5. `:kill-signal` normalization (`TERM` vs `SIGTERM`) and runtime behavior.
6. `:kill-mode` enum validation + process vs mixed behavior.
7. `:remain-after-exit` active latch and command semantics.
8. `:success-exit-status` numeric and signal handling.

## Explicit Non-Goals

- Any MUST-HAVE item from `PLAN-PT2.md`.
- Any PROBABLY-BLOAT item from `FINDINGS-PT2.md`, including:
  - `DefaultDependencies`,
  - boot-target/install graph semantics beyond simple enable/disable,
  - cgroup hardening matrix (`ProtectSystem`, `NoNewPrivileges`, namespace knobs),
  - deep init-only controls (`OOMPolicy`, `FailureAction`, etc.).
- systemd target model, cgroup model, or full unit-spec parity.
- INI `[Unit]/[Service]/[Install]` parser support.

## Definition of Done

1. All eight NICE-TO-HAVE keys parse and validate with unambiguous behavior.
2. Repeatable directives behave via list-value semantics.
3. Runtime semantics for wants/before/kill/remain/success-exit are implemented
   exactly as contracted here.
4. CLI/dashboard + handbook are updated.
5. Unplanned list is documented in handbook (and mirrored to wiki if used).
6. `make check` passes.
