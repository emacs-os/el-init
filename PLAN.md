# PLAN: supervisorctl/systemctl Terminology Normalization (No New Features)

Date: 2026-02-12
Based on: `FINDINGS.md`

## Objective
Normalize existing `supervisorctl` terminology and user-facing semantics to be as systemd-like as possible, without adding new feature surface.

Adopt a strict naming policy:
- If behavior matches mainstream/systemd behavior, and our name differs, rename to mainstream/systemd nomenclature.
- If behavior is only similar, mainstream naming is acceptable only when no future conflict is expected by feature development plans (roadmap/plan docs).
- If behavior is unique, do not reuse mainstream names; use a unique supervisor-specific name.

## Hard Constraints
- No new runtime capabilities.
- No new output fields in JSON contracts.
- No internal schema changes.
- Command renames/aliases are allowed only for normalization, not expansion.

## In Scope
- Wording, naming, and terminology normalization.
- Clarifying existing command semantics where names overlap with `systemctl` but behavior differs.
- Consistency across CLI, dashboard help, and README handbook.

## Out of Scope
- Adding runtime behavior that does not already exist.
- Adding status model fields (`active_state`/`sub_state`).
- Any new lifecycle behavior.

## Naming Decision Framework (Policy)
Use this decision order for every user-facing term/command.

1. Equivalent behavior:
- If supervisor behavior is functionally equivalent to systemd behavior and naming differs, rename to the systemd term/verb.
- Backward-compatible aliases may be used as transition support, but canonical naming must be systemd-aligned.

2. Similar-but-not-equivalent behavior:
- Use systemd term only if we do not plan to implement the true systemd behavior later.
- If future true behavior is likely (per roadmap/plan documents), use a distinct name now to avoid future collision.

3. Supervisor-unique behavior:
- Use supervisor-specific naming.
- Avoid reusing highly loaded mainstream verbs that imply different semantics.

4. Migration safety:
- Keep compatibility aliases/messages during transition.
- Keep JSON contract stable unless separately versioned.

## Normalization Principles
- Use `service` as the primary user-facing noun (not `entry`/`program`) where practical.
- Keep internal code identifiers untouched unless needed for clarity.
- Preserve existing JSON keys, status tokens, and exit codes.
- Prefer explicit semantic phrasing where systemctl-equivalent command names are not behavior-equivalent.

## Workstreams

## 1) Canonical Vocabulary
Define a canonical term map and apply it consistently in user-facing text:
- `entry` -> `service` (user-facing only)
- `program` -> `service` (user-facing only)
- Keep `oneshot` and `simple` as-is (already systemd-aligned)
- Keep `stage*` terminology, but describe it as supervisor-specific ordering model

Deliverables:
- Canonical term table in README.
- One terminology policy section for contributors.

Acceptance:
- No mixed `entry/service` wording in CLI help, dashboard help, or README command docs.

## 2) CLI Wording Normalization
Normalize wording in:
- usage banner text
- command descriptions/messages
- human output headings and diagnostics

Specific focus:
- `enable`/`disable`: explicitly described as supervisor startup policy overrides.
- `reload`: if kept with current semantics, explicitly described as reconciliation behavior.
- `kill`: if kept with current semantics, explicitly described as signal + restart suppression behavior.

Deliverables:
- Updated strings in `supervisor-cli.el` and `sbin/supervisorctl`.
- Updated CLI handbook sections in `README.org`.

Acceptance:
- Command semantics are unambiguous from help text alone.
- Existing command names and JSON shape remain unchanged.

## 3) Dashboard Wording Normalization
Normalize dashboard help and messages:
- key help descriptions
- status/reason legend phrasing
- action feedback messages (`start`/`kill`/invalid/disabled)

Deliverables:
- Updated strings in `supervisor-dashboard.el`.
- Dashboard handbook language synchronized with runtime help buffer.

Acceptance:
- Dashboard and CLI describe the same behavior with the same terminology.

## 4) Status Vocabulary Alignment (Non-Breaking)
Do not change machine status tokens, but normalize documentation language around them:
- document mapping to systemd mental model (example: `running ~ active`, `stopped ~ inactive`, `dead ~ failed/crash-loop`, `pending ~ activating/waiting`)
- keep current status values intact in CLI/JSON

Deliverables:
- Mapping table in README.
- Clarified status legend in dashboard help.

Acceptance:
- Users can interpret supervisor statuses through systemd vocabulary without changing APIs.

## 5) Semantic Divergence Disclosure
Add explicit section documenting same-name command differences from `systemctl`:
- `enable`/`disable`
- `reload`
- `kill`

Deliverables:
- New "systemctl compatibility notes" subsection in README.
- Cross-reference from CLI handbook command notes.

Acceptance:
- No hidden semantic differences for same-name commands.

## 7) Naming Normalization Pass (Command-Level)
Apply the naming framework to every existing command and classify each as:
- `equivalent` (must adopt systemd term),
- `similar` (keep with explicit caveat, or rename if collision risk),
- `unique` (rename to supervisor-specific term).

Deliverables:
- A command classification table in README.
- For each `equivalent` command whose current name differs from systemd:
  - a concrete rename plan,
  - transition alias behavior,
  - and deprecation wording/timeline.
- For each non-equivalent systemd-like name, either:
  - a rename proposal, or
  - an explicit decision note: \"kept intentionally, no future collision expected\".

Acceptance:
- Every command name has an explicit rationale under the naming framework.
- No `equivalent` command remains under a non-systemd canonical name.

## 6) Test and Regression Guardrails
Update/add tests only for normalization outcomes:
- help/usage text consistency checks
- key message consistency checks where tests already assert strings
- guarantee no JSON contract regression

Required verification:
- `make check` passes

Acceptance:
- All existing tests pass.
- Any updated string assertions reflect normalized terminology.

## Execution Order
1. Canonical vocabulary + naming decision framework
2. Command-by-command classification (`equivalent`/`similar`/`unique`)
3. CLI wording and naming normalization updates
4. Dashboard wording normalization updates
5. README compatibility notes + status mapping table + classification table
6. Test updates and `make check`

## Risks and Mitigations
Risk: Human-output wording changes may affect ad-hoc scripts parsing text.
Mitigation: Keep JSON interfaces unchanged and document human output as non-contractual.

Risk: Partial normalization creates mixed terminology.
Mitigation: apply terminology changes in one pass across CLI, dashboard, and README before merge.

## Completion Criteria
This plan is complete when:
- User-facing terminology is consistent and systemd-comparable.
- Naming decisions follow the explicit framework in this plan.
- Any command with equivalent systemd behavior is canonically named with systemd nomenclature.
- Same-name command semantic differences are explicitly documented.
- No new features/commands/options were introduced.
- `make check` passes.

## Open Decisions Requiring Maintainer Input
These are the key ambiguity points under the new naming policy:

1. `reload` current behavior is reconciliation, not service config reload:
- Option A: keep `reload` name and document divergence.
- Option B: rename behavior to `reconcile` and keep `reload` as compatibility alias.

2. `kill` currently also suppresses restart:
- Option A: keep `kill` name and document divergence.
- Option B: reserve `kill` for pure signal semantics and move current behavior name to a supervisor-specific verb.

3. `restart-policy` and `logging` are policy toggles without direct systemctl equivalents:
- Option A: keep names as supervisor policy commands.
- Option B: rename to explicitly supervisor-scoped verbs (for example prefixed policy verbs).
