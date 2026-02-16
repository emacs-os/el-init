# PLAN: Optional bubblewrap Sandbox Integration

Date: 2026-02-15
Status: Final Locked
Authority: This file is the implementation and audit contract.

## Rule of Interpretation
All requirements in this document are mandatory.

- MUST means required for completion.
- MUST NOT means prohibited.
- SHOULD is not used in this file.
- MAY is not used in this file.

Any behavior not explicitly listed here is out of scope.

## Objective
Add optional Linux sandboxing via bubblewrap in a way that is practical for
users and supportable for maintainers.

The feature model is profile-first:

1. curated built-in profiles for common use,
2. constrained safe customization knobs,
3. explicitly gated expert raw-argument mode.

## Locked Decisions
1. Bubblewrap integration is Linux-only.
2. Sandbox is opt-in and disabled by default.
3. Existing units without sandbox settings keep current launch behavior.
4. Launch semantics remain service start only; no restart-policy coupling.
5. Initial profile set is fixed to: `none`, `strict`, `service`, `desktop`.
6. Profile selection is per-unit via `:sandbox-profile`.
7. `:sandbox-profile` default is `none`.
8. Safe per-unit knobs are limited to:
   - `:sandbox-network` (`shared` or `isolated`),
   - `:sandbox-ro-bind` (list of absolute paths),
   - `:sandbox-rw-bind` (list of absolute paths),
   - `:sandbox-tmpfs` (list of absolute paths).
9. Raw bubblewrap arguments are gated behind global defcustom
   `supervisor-sandbox-allow-raw-bwrap`.
10. Per-unit raw args key is `:sandbox-raw-args` and is rejected unless global
    gate is non-nil.
11. Command building remains shell-free (argv list only).
12. With `:user` or `:group`, launch order is fixed:
    `supervisor-runas` -> `bwrap` -> service executable.
13. Non-Linux hosts reject sandbox-enabled units with explicit invalid reason.
14. Missing `bwrap` binary rejects sandbox-enabled units with explicit reason.
15. Validation failures exclude only affected units; unrelated units continue.
16. `make check` is required at each phase gate.

## Canonical Data Model

### 1) New Unit Keys
Supported sandbox keys for service units:

- `:sandbox-profile` symbol: `none`, `strict`, `service`, `desktop`
- `:sandbox-network` symbol: `shared`, `isolated`
- `:sandbox-ro-bind` list of absolute path strings
- `:sandbox-rw-bind` list of absolute path strings
- `:sandbox-tmpfs` list of absolute path strings
- `:sandbox-raw-args` list of strings (expert mode only)

Target and timer definitions do not accept sandbox keys.

### 2) Profile Contract
Profiles are deterministic argument templates.

- `none`: no sandbox wrapper.
- `strict`: unshare all supported namespaces, no host home bind, network
  isolated unless explicitly overridden.
- `service`: restrictive filesystem baseline with shared network.
- `desktop`: service baseline plus runtime socket/bus paths required for
  desktop userland workflows.

Profile definitions are centralized in one registry function and are not
constructed ad hoc across call sites.

### 3) Path Validation Contract
For `:sandbox-ro-bind`, `:sandbox-rw-bind`, and `:sandbox-tmpfs`:

1. Each path MUST be absolute.
2. Empty string paths are invalid.
3. Duplicate paths are deduplicated deterministically (first occurrence wins).
4. Forbidden destinations (`/proc`, `/dev`) are rejected from custom bind lists.
5. Non-existent source path in bind lists is invalid unless explicitly declared
   as optional in the profile definition.

## Runtime Semantics

### 1) Launch Builder Integration
Sandbox command composition is centralized in launch command building.

1. Build base executable argv.
2. Apply identity wrapper (`supervisor-runas`) when requested.
3. Apply bubblewrap wrapper when sandbox is enabled.
4. Emit final argv to `make-process`.

### 2) Failure Surfaces
Sandbox setup failures are deterministic and user-visible.

- Validation-time failures: invalid unit state with reason in verify/dashboard.
- Launch-time failures: spawn failure reason recorded and surfaced in status.

### 3) Network Override Rules

1. Profile default network mode applies when `:sandbox-network` is omitted.
2. `:sandbox-network shared` forces shared network for that unit.
3. `:sandbox-network isolated` forces isolated network for that unit.

### 4) Expert Raw Mode Rules

1. `:sandbox-raw-args` is ignored and invalid unless global gate is enabled.
2. Even with global gate enabled, dangerous or conflicting combinations are
   rejected by validation policy.
3. Expert raw mode is unsupported on non-Linux and rejected.

## Validation Contract

1. Sandbox keys are valid only for `simple` and `oneshot` units.
2. Unknown sandbox values are invalid.
3. `:sandbox-profile` value outside fixed profile set is invalid.
4. `:sandbox-network` value outside fixed enum is invalid.
5. Path-list keys must be proper lists of absolute strings.
6. Sandbox-enabled unit on non-Linux is invalid.
7. Sandbox-enabled unit with missing `bwrap` executable is invalid.
8. `:sandbox-raw-args` without global gate is invalid.
9. `:sandbox-raw-args` must be a list of strings when allowed.
10. Validation reasons MUST be explicit and stable for test assertions.

## CLI and Dashboard Contract

1. Status/detail surfaces include sandbox profile and effective network mode.
2. Verify output includes sandbox validation failures.
3. Dashboard row detail includes sandbox-enabled indicator.
4. Existing commands remain stable; sandbox output fields are additive.

## Phase Plan

### Phase 1: Schema and Validation
Deliverables:

1. Add new sandbox keys to unit schema for service units.
2. Implement validation contract for profiles, enums, path lists, and OS/binary
   prerequisites.
3. Add deterministic invalid-reason messages.

Acceptance:

1. Valid profile units parse and validate.
2. Invalid profile or key shapes are rejected with explicit reasons.
3. Non-Linux and missing-binary gates are enforced.
4. `make check` passes.

### Phase 2: Profile Registry and Command Builder
Deliverables:

1. Implement centralized profile registry.
2. Implement sandbox argv builder from profile + safe knobs.
3. Integrate builder into launch command composition.

Acceptance:

1. `none` profile produces no bwrap wrapper.
2. `strict`, `service`, and `desktop` produce deterministic argv.
3. Existing non-sandbox launch behavior remains unchanged.
4. `make check` passes.

### Phase 3: Identity + Sandbox Interop
Deliverables:

1. Implement fixed wrapper ordering with runas and bwrap.
2. Ensure absolute executable path handling remains correct.
3. Preserve existing trust-gate behavior for `:user`/`:group`.

Acceptance:

1. Combined `:user`/`:group` + sandbox launches with expected wrapper order.
2. Identity trust-gate failures still block as before.
3. No regression for non-sandbox identity launches.
4. `make check` passes.

### Phase 4: Safe Knobs and Overrides
Deliverables:

1. Implement `:sandbox-network`, `:sandbox-ro-bind`, `:sandbox-rw-bind`,
   and `:sandbox-tmpfs`.
2. Enforce path validation and deterministic deduplication.
3. Apply knob overrides to profile defaults deterministically.

Acceptance:

1. Knobs modify effective sandbox as specified.
2. Invalid paths and forbidden destinations are rejected.
3. Duplicate path entries resolve deterministically.
4. `make check` passes.

### Phase 5: Expert Raw Mode (Gated)
Deliverables:

1. Add global defcustom `supervisor-sandbox-allow-raw-bwrap`.
2. Add per-unit `:sandbox-raw-args` support behind gate.
3. Add validation policy for conflicting/unsafe raw arg combinations.

Acceptance:

1. Raw mode is rejected when gate is off.
2. Raw mode works only when gate is on and validation passes.
3. Unsafe/conflicting raw args are rejected with explicit reasons.
4. `make check` passes.

### Phase 6: Test Expansion (Hard Requirement)
Deliverables:

1. Add schema and validation tests for all sandbox keys.
2. Add launch-argv tests for all built-in profiles.
3. Add interop tests for runas + bwrap wrapper ordering.
4. Add safe-knob tests for network and path list behavior.
5. Add expert-mode gate tests for allowed/rejected raw args.
6. Add CLI/dashboard surface tests for sandbox status fields.
7. Add non-regression tests for non-sandbox services.

Acceptance:

1. New tests pass.
2. Existing tests pass.
3. `make check` passes.

### Phase 7: Manual and Docs Update
Deliverables:

1. Update `README.org` with sandbox overview and profile table.
2. Document each profile’s guarantees and limitations.
3. Document safe knobs and validation errors.
4. Document expert mode gate and support boundaries.
5. Remove stale claims that no sandbox hardening is possible.

Acceptance:

1. Manual reflects implementation exactly.
2. Additive docs do not break existing command references.
3. No stale contradictions remain.
4. `make check` passes.

## Explicit Non-Goals
1. No systemd hardening parity matrix in this plan.
2. No cgroup policy management.
3. No seccomp policy language in v1.
4. No per-profile dynamic auto-detection of app dependencies.
5. No macOS sandbox backend.
6. No changes to timer model.
7. No D-Bus activation work.

## Audit Evidence Requirements
Each phase audit MUST include:

1. Code evidence:
   - exact files changed,
   - exact functions changed or added.
2. Validation evidence:
   - accepted and rejected sandbox configs,
   - exact invalid-reason strings.
3. Runtime evidence:
   - one successful launch per profile,
   - one failure path per profile/gate scenario.
4. Interop evidence:
   - runas + bwrap wrapper ordering proof.
5. Test evidence:
   - new ERT test names,
   - exact `make check` result,
   - explicit skipped test list.
6. Documentation evidence:
   - exact README headings updated,
   - confirmation stale statements were removed.

## Audit Checklist (Final Signoff)
All items MUST be true:

1. Sandbox schema keys are implemented and validated.
2. Profile registry is centralized and deterministic.
3. Safe knobs work and are strongly validated.
4. Non-Linux and missing-binary gates are enforced.
5. runas + bwrap ordering is implemented as specified.
6. Expert mode gate is enforced and validated.
7. CLI/dashboard sandbox visibility is present and accurate.
8. Full test suite passes with no regressions.
9. README manual is synchronized with behavior.
10. `make check` passes at release gate.
