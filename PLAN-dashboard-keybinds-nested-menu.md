# PLAN: Dashboard Keybind Redesign (Nested Menu)

Date: 2026-02-12
Status: Draft for implementation

## Scope

This plan covers only the first TODO item:

- redesign `M-x supervisor` interactive keybinds around a Magit-style nested menu,
- keep navigation/refresh/proced controls top-level,
- introduce canonical nested groups for lifecycle, policy, and inspect actions,
- normalize bindings for mnemonic clarity and systemctl-style verb behavior.

This plan does not include logging architecture, unit-file schema expansion, or CLI verb renames.

## Locked Decisions (From TODO)

1. Top-level keys are navigation/system controls only:
- `f` filter,
- `g` refresh,
- `G` live refresh,
- `t` open `proced` ("top"),
- `T` proced live refresh / auto-update toggle.

2. Free top-level `p` for policy namespace.
- Existing direct proced binding on `p` is moved to `t`.

3. Introduce nested action groups:
- `l` Lifecycle,
- `p` Policy,
- `i` Inspect.

4. Lifecycle canonical bindings:
- `s` start,
- `t` stop,
- `r` restart,
- `k` kill,
- `u` reload,
- `f` reset-failed.

5. Policy canonical bindings (explicit verbs, not blind toggles):
- `e` enable,
- `d` disable,
- `m` mask,
- `u` unmask,
- `r` restart-policy set,
- `l` logging set.

6. Inspect group contains:
- info,
- dependencies,
- full graph,
- blame,
- log view,
- cat unit,
- edit unit.

7. Migration ergonomics:
- preserve high-frequency direct keys during transition where practical,
- nested groups are canonical in help text and menu layout.

## Canonical Command Contracts

### A) Lifecycle group

Lifecycle keys must call the same canonical command paths as CLI verbs:

- start -> manual start path,
- stop -> manual stop path,
- restart -> stop+start semantics,
- kill -> signal semantics,
- reload -> per-unit reload semantics,
- reset-failed -> clear failure state only.

No lifecycle key may mutate unit definitions directly.

### B) Policy group

Policy keys must be explicit set/unset operations:

- no toggle-only hidden behavior,
- prompts/selectors must resolve to explicit target state,
- policy actions map 1:1 to canonical policy mutators.

Any setting that is unit-definition-only must route user to `edit` flow instead of runtime mutation.

### C) Inspect group

Inspect keys are read-only presentation workflows.

- They must not mutate runtime state.
- Output/behavior should match CLI intent where equivalent commands exist.

### D) Top-level key behavior

Top-level keys must remain lightweight and operationally safe:

- navigation/filter/refresh/proced only,
- no destructive state mutation outside nested groups,
- updated header/transient help reflects this model.

## Phase Plan

### Phase 1: Inventory and Binding Map

Deliverables:

- inventory all current dashboard keys and mutators,
- map old bindings to new top-level + nested model,
- identify collisions and transitional compatibility bindings.

Acceptance:

- complete binding matrix exists in-code comments or developer docs,
- no unassigned required action from locked decisions.

### Phase 2: Lifecycle Submenu Implementation

Deliverables:

- implement `l` submenu with canonical bindings (`s t r k u f`),
- wire each key to existing canonical lifecycle command paths,
- update transient labels/descriptions.

Acceptance:

- lifecycle actions reachable from submenu and execute correctly,
- no lifecycle action disappears from interactive surface.

### Phase 3: Policy Submenu Implementation

Deliverables:

- implement `p` submenu with canonical bindings (`e d m u r l`),
- replace toggle-style actions with explicit set operations where required,
- ensure restart-policy/logging interactions are explicit and clear.

Acceptance:

- policy actions are explicit verbs,
- no policy action relies on implicit state flip without user-visible target state.

### Phase 4: Inspect Submenu Implementation

Deliverables:

- implement `i` submenu with info/deps/graph/blame/log/cat/edit,
- preserve read-only semantics,
- align labels with CLI terminology where possible.

Acceptance:

- all inspect actions available under `i`,
- no inspect action mutates runtime/policy state.

### Phase 5: Top-Level Simplification + Transition Layer

Deliverables:

- enforce top-level set (`f g G t T` plus submenu dispatch keys),
- move proced from `p` to `t` and add `T` behavior,
- keep selected legacy direct keys as transition affordances where practical.

Acceptance:

- top-level no longer overloaded with lifecycle/policy/inspect actions,
- help text marks nested groups as canonical.

### Phase 6: Help, Tests, and Docs

Deliverables:

- update dashboard header/help/menu descriptions,
- add/adjust ERT tests for key dispatch, mutator semantics, and regressions,
- update handbook section for interactive controls.

Acceptance:

- tests cover new key paths and explicit policy semantics,
- documentation matches current interactive behavior.

## Required Files (Expected)

- `supervisor-dashboard.el`
- `supervisor-cli.el` (only if inspect/policy parity helpers are needed)
- `supervisor-core.el` (only if missing explicit mutators are required)
- `supervisor-test.el`
- `README.org`

## Non-Goals

- no CLI surface redesign in this plan,
- no unit-file format changes,
- no timer/logging architecture rework.

## Definition of Done

All must be true:

1. Nested groups `l`, `p`, and `i` exist and are canonical.
2. Top-level keeps navigation/filter/refresh/proced responsibilities.
3. Lifecycle binds are `s/t/r/k/u/f` under `l`.
4. Policy binds are explicit verbs under `p` (`e/d/m/u/r/l`).
5. Inspect actions are grouped under `i` and are non-mutating.
6. Help/menu/header text reflects the new layout.
7. Transition bindings (if any) are explicitly marked temporary.
8. `make check` passes.
