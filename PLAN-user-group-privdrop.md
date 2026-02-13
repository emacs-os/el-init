# PLAN: Unit `:user` / `:group` Privilege Drop (Root-Only)

Date: 2026-02-12
Status: Draft for implementation

## At a Glance (Locked Direction)

This plan keeps control-plane routing simple in the first iteration:

1. Root/system manager use case:
   - Run supervisor as root (including pid2-style setups).
   - Use unit keys `:user` / `:group` to drop privileges per service.
   - Example: root manager launches `web.service` as `www-data`.
2. Per-user manager use case:
   - Each user runs their own supervisor manager in their own Emacs server.
   - `supervisorctl` uses existing emacsclient server discovery in that user context.
   - Example: `alice` runs `supervisorctl status` for Alice services.

Important boundary:

- `:user` / `:group` controls service runtime identity inside one manager.
- Manager selection remains existing emacsclient discovery behavior plus OS
  identity (`sudo`, `sudo -u`).
- No new `--system` / `--user` scope flags in this plan.
- No implicit bridge from unprivileged callers into a root manager.

## Scope

Add unit-level identity controls so a root-run supervisor can launch selected
services as non-root users, with behavior aligned to the systemd `User=` and
`Group=` model where reasonable for this project.

This plan covers:

- new unit keys `:user` and `:group`,
- safe process launch with privilege drop before `execve`,
- validation and UX behavior in CLI/dashboard,
- control-plane behavior documented as unchanged in this plan,
- security guardrails for unit-file trust when supervisor runs as root,
- tests and documentation.

This plan does not cover:

- cgroup integration,
- full systemd sandbox/hardening matrix,
- per-service namespace isolation,
- TTY/PTY architecture redesign.

## Emacs Capability Check (Native vs Helper)

Findings from local Emacs source (`~/repos/emacs`):

- `make-process` keyword contract does not expose user/group/uid/gid launch
  controls (`src/process.c`, `make-process` doc around lines 1767-1826).
- `create_process` and `emacs_spawn` code paths do not accept uid/gid inputs
  for child setup (`src/process.c` around lines 2160-2291,
  `src/callproc.c` around lines 1439-1660).
- `child_setup` performs fd/chdir/pgid/exec setup, but no `setuid`/`setgid`
  operations (`src/callproc.c` around lines 1236-1293).
- `user`/`group` symbols in Emacs process code are used for
  `process-attributes` reporting, not spawn identity control
  (`src/process.c` around lines 8546-8549).

Conclusion:

- Native Emacs subprocess API is not sufficient for proper per-service
  identity drop.
- A dedicated exec helper is required to stay close to the proper init model.

## Options and Decision

| Option | Description | Pros | Cons | Decision |
|---|---|---|---|---|
| A. Native Emacs only | Try to implement with `make-process` directly | No extra binary | Not supported by current Emacs API; cannot set target uid/gid per child | Rejected |
| B. `sudo`/`su` wrapper | Spawn through `sudo -u` or `su -c` | Fast to prototype | Shell/quoting/TTY/policy fragility; dependency on host config; diverges from init-style model | Rejected |
| C. Dedicated helper | Small `libexec/supervisor-runas` drops groups/gid/uid then `execve` | Closest to proper model; deterministic; no shell mediation | Extra component to maintain | Accepted |

## Control-Plane Model (First Iteration)

Running a unit as another uid (`:user`/`:group`) is separate from deciding
which manager instance `supervisorctl` is talking to.

This plan keeps control-plane behavior unchanged:

| Option | Description | Pros | Cons | Decision |
|---|---|---|---|---|
| A. Add `--system` / `--user` routing now | New scope flags pick transport endpoints automatically | Convenient UX | More wrapper logic and edge cases in first iteration | Deferred |
| B. Keep current discovery model | Use existing emacsclient server discovery and OS identity (`sudo`, `sudo -u`) | Zero new surface; least friction | Less explicit when multiple managers are running | Accepted |
| C. Root-manager bridge proxy | Unprivileged clients route through broker to root manager with ACLs | Single backend manager | Security-critical complexity and high risk | Rejected |

Adopted model for this plan:

- No new `--system` / `--user` flags.
- No required control-plane changes in `supervisorctl`.
- Manager selection uses existing emacsclient discovery behavior in current
  caller context.
- Admin controlling another user's manager remains OS-level identity switch
  (for example `sudo -u alice supervisorctl ...`).

Server bootstrap requirement (documented default):

```emacs-lisp
(require 'server)
(unless (server-running-p) (server-start))
```

Security expectation for this default:

- local server socket mode (`server-use-tcp` is nil) is the supported baseline.
- socket access remains OS/user-bound by default server directory permissions.
- TCP server mode is out of scope for this plan's security assumptions.

PTY note:

- PTY/baud issues are not the primary blocker here.
- Primary concern remains transport authorization and endpoint ownership.

## Locked Decisions

1. Canonical unit keys are `:user` and `:group`.
2. `:user`/`:group` are only meaningful when supervisor effective UID is root.
3. If supervisor is not root and either key is set to a different identity, the
   unit is invalid and must not spawn.
4. Launch path uses dedicated helper `libexec/supervisor-runas`; no `sudo`,
   no `su`, no shell wrapper.
5. Privilege-drop order in helper is:
   - resolve user/group,
   - set supplementary groups,
   - set primary gid,
   - set uid,
   - `execve` target command.
6. Default process I/O remains pipes (`:connection-type 'pipe` behavior stays).
7. No PTY requirement for this feature.
8. Restart/reload semantics remain unchanged except spawned identity follows
   resolved `:user`/`:group`.
9. Hard-fail on identity resolution errors with clear reason surfaced in status.
10. No fallback escalation behavior is allowed.
11. No new scope flags are added in this plan (`--system` / `--user` deferred).
12. Manager selection behavior remains unchanged from current emacsclient
    discovery semantics.
13. No implicit bridge/proxy from unprivileged callers into a root manager.
14. Cross-user control remains OS identity switch
    (for example `sudo -u alice ...`).

## Canonical Semantics

### Unit Keys

- `:user` accepts:
  - username string (for example `"postgres"`),
  - numeric uid integer.
- `:group` accepts:
  - group name string (for example `"postgres"`),
  - numeric gid integer.
- Both keys default to nil.

### Effective Identity Rules

- If both keys nil:
  - spawn exactly as current supervisor identity.
- If `:user` set and `:group` nil:
  - use target user and its primary group.
- If `:group` set and `:user` nil:
  - if supervisor euid is root, keep current uid and switch primary group.
  - if not root and requested group differs from current egid, unit invalid.
- If both set:
  - resolve both and apply exactly.

### Validation Rules

- `:user`/`:group` are allowed for `simple` and `oneshot`.
- Unknown or unresolvable users/groups are validation/runtime errors.
- Non-root supervisor with incompatible requested identity rejects unit.
- Error strings must identify unit id and requested identity.

### Execution Model

- Supervisor builds command vector as today.
- When effective identity change is requested, supervisor prepends helper:
  - helper receives resolved identity arguments and target argv.
- Helper performs privilege drop and direct `execve`.
- No shell parsing is introduced in the helper path.

### Control-Plane Routing (No New Scope Flags)

`supervisorctl` control-plane behavior remains unchanged in this plan.

Operational pattern:

- per-user management:
  - run `supervisorctl ...` in that user context.
- system/root management:
  - run `sudo supervisorctl ...` (or equivalent root shell context).
- admin controlling another user's manager:
  - use OS-level identity switch:
    `sudo -u alice supervisorctl ...`.

Identity vs control-plane clarification:

- `:user`/`:group` changes runtime identity of spawned service processes.
- emacsclient discovery and caller identity choose which manager instance
  receives the command.
- This plan intentionally does not add auto-routing scope flags.

Not supported in this plan:

- `--system` / `--user` scope flags.
- built-in cross-user bridge/proxy in a root manager.

## Root Security and Unit-Source Trust

When supervisor runs as root, unit source trust must be explicit.

Required policy for this plan:

- root mode must not execute `:user`/`:group` units from untrusted roots.
- loader must be able to expose source path/root metadata for each unit.
- if source trust cannot be determined, fail closed for identity-changing units.

Integration note:

- this policy should align with `PLAN-unit-file-authority-cascade.md`
  (authoritative source path/root metadata).

## Phase Plan (10 Phases)

### Phase 1: Schema and Parser Surface

Deliverables:

- add `:user` and `:group` to keyword whitelist(s),
- extend entry/service schema accessors,
- add parse-time shape validation (string or integer or nil).

Acceptance:

- invalid shapes are rejected with key-specific errors,
- existing units unaffected when keys are absent.

### Phase 2: Spawn Abstraction Refactor

Deliverables:

- isolate process launch command assembly behind one internal function,
- support optional launch-wrapper argv injection cleanly.

Acceptance:

- no behavior change without `:user`/`:group`,
- restart/reload/manual-start paths share the same launch assembly.

### Phase 3: Implement `libexec/supervisor-runas`

Deliverables:

- helper binary/script with strict argument contract,
- direct uid/gid/supplementary-group drop then `execve`,
- robust error codes and stderr diagnostics.

Acceptance:

- helper can run command as target non-root user when invoked by root,
- helper fails safely when not privileged or identities invalid.

### Phase 4: Core Integration

Deliverables:

- wire helper launch into `supervisor--start-process`,
- ensure restart timers and reload use same identity behavior,
- preserve current logging/sentinel/filter behavior.

Acceptance:

- services start/restart/reload under configured identity,
- no regression for units without `:user`/`:group`.

### Phase 5: Trust Enforcement in Root Mode

Deliverables:

- enforce source trust gate for identity-changing units,
- block launch with explicit error when trust policy fails.

Acceptance:

- root supervisor never runs identity-changing units from untrusted source.

### Phase 6: Control-Plane Documentation Alignment

Deliverables:

- keep `supervisorctl` control-plane behavior unchanged in this plan,
- document operational expectations for root/user manager selection using
  caller identity,
- explicitly defer `--system` / `--user` scope flags to future planning.

Acceptance:

- no new scope-routing flags added,
- documentation clearly states how manager targeting works today.

### Phase 7: CLI/Dashboard Visibility

Deliverables:

- include configured `:user`/`:group` in `show` and detail surfaces,
- surface launch failures with identity context.

Acceptance:

- operator can diagnose identity issues from CLI/dashboard without debug logs.

### Phase 8: Tests

Deliverables:

- unit tests for parse/validation and launch assembly,
- integration tests for helper success/failure paths,
- root-only tests gated by explicit env flag and euid check.

Acceptance:

- deterministic test behavior in non-root CI,
- optional privileged test lane validates full drop behavior.

### Phase 9: Documentation

Deliverables:

- update `README.org` unit key reference with `:user`/`:group`,
- document root-only semantics and trust requirements,
- add migration and troubleshooting examples.

Acceptance:

- docs match implemented behavior exactly, no aspirational gaps.

### Phase 10: Final Hardening Pass

Deliverables:

- ensure stale wording is removed,
- run full quality gate.

Acceptance:

- `make check` passes,
- no known bypass of identity/trust constraints.

## Required Test Matrix (Minimum)

1. Parse accepts string/int/nil for `:user` and `:group`.
2. Parse rejects invalid types (symbol/list/etc).
3. Non-root supervisor rejects incompatible identity requests.
4. Root supervisor launches with helper and correct argv contract.
5. Helper error propagation surfaces clear status/message.
6. Restart path preserves configured identity.
7. Reload path preserves configured identity.
8. Trust gate blocks untrusted source in root mode.
9. No new `--system` / `--user` flag behavior is introduced by this plan.
10. Caller identity (`user`, `root`, `sudo -u`) remains sufficient to select
    manager context as documented.
11. Control-plane behavior does not alter unit identity semantics.

## Explicit Non-Goals

- No shell-based privilege mediation (`sudo`, `su`, `doas`).
- No PTY-first process model change.
- No single-root-manager ACL bridge/proxy in this plan.
- No SELinux/AppArmor policy management.
- No full systemd sandbox feature parity.

## Definition of Done

All must be true:

1. `:user` and `:group` are implemented and validated.
2. Spawn path uses helper-based direct privilege drop, not shell mediation.
3. Root/non-root behavior is explicit and fail-safe.
4. Restart/reload behavior is identity-consistent.
5. Root-mode trust gate for identity-changing units is enforced.
6. `supervisorctl` control-plane behavior remains intentionally unchanged.
7. CLI/dashboard and docs expose final behavior clearly.
8. `make check` passes.
