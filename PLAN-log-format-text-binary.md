# PLAN: Structured Log Records and Optional Binary Mode

Date: 2026-02-16
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
Primary binary mode purpose:

1. to improve performance, searchability, and reliability over traditional
   text-based logs.

Add a per-service `:log-format` option with two supported values:

- `text` (default),
- `binary` (opt in).

Both formats MUST store structured records containing these fields:

- timestamp,
- unit id,
- pid,
- stream (`stdout` or `stderr`),
- exit markers,
- payload bytes.

This plan keeps existing logging architecture (external `supervisor-logd`,
rotate/prune scripts, split stdout/stderr support) while making record content
structured and deterministic.

## Locked Decisions
1. New unit keyword `:log-format` is introduced for service entries.
2. Allowed values are exactly `text` and `binary`.
3. Default log format is `text`.
4. `:log-format` is invalid for `:type target` entries.
5. Existing active log file naming is unchanged (`log-<id>.log` and
   `supervisor.log`).
6. Timestamped rotated suffix conventions are unchanged; optional `.tar.gz`
   archives may wrap rotated files after rotation.
7. `supervisor-logd` remains the writer for both formats.
8. Record timestamps are mandatory in both formats.
9. Exit markers are mandatory in both formats.
10. Stream attribution (`stdout` vs `stderr`) is mandatory in both formats.
11. Binary mode is a format choice, not a security boundary.
12. Existing `supervisorctl logs [--tail N] [--] ID` behavior remains
    unchanged.
13. New `supervisorctl journal` command is the canonical decoded reader for
    structured logs.
14. Dashboard log inspection uses the same decoded reader path as
    `supervisorctl journal`.
15. Existing unstructured legacy log files are read in backward-compatible
    fallback mode.
16. Performance-critical record encoding stays in `supervisor-logd` C code.
17. `supervisorctl journal` supports `-u/--unit`, `-f/--follow`,
    `--since`, `--until`, `-p err`, and `-n N`.
18. `supervisorctl journal` supports JSON output via existing global JSON mode.
19. Binary logs are rotated and vacuumed with byte-size rules; they are not
    exempt from maintenance.
20. `sbin/supervisor-log-prune` gains explicit vacuum feature flags as aliases
    of prune behavior.
21. Post-rotation compression is attempted only when `tar` is found on `PATH`.
22. Compression command is fixed to conservative `tar -czf` usage.
23. If `tar` is unavailable, rotation continues without compression.
24. Compression policy applies equally to rotated text logs and rotated binary
    logs.
25. Journal filtering is metadata-driven and operates on structured fields
    (`ts`, `unit`, `priority`, `stream`, `event`) rather than payload parsing.
26. Binary decode reliability includes recovery to the last valid record
    boundary when trailing partial data is encountered.
27. `make check` is required at every phase gate.

## Canonical Data Model

### 1) Unit Schema Extension
New key for service entries:

- `:log-format` with value symbol `text` or `binary`.

Defaults:

- If omitted, effective value is `text`.

Type constraints:

- For `simple` and `oneshot`: accepted.
- For `target`: invalid.

### 2) Parsed Entry Contract
Entry tuple extends by one field.

- index 33: `log-format`.

Required accessor:

- `supervisor-entry-log-format`.

`supervisor-service` struct gains field `log-format` and round-trip conversion
must preserve it.

### 3) Record Semantics (Common)
Every persisted output/exit record (text or binary) must represent one logical
event with these semantic fields:

- `timestamp` (event write time in `supervisor-logd`),
- `unit_id`,
- `pid`,
- `stream` (`stdout`, `stderr`, or `meta` for exit marker records),
- `event` (`output` or `exit`),
- `payload_bytes` (for `output`),
- `exit_status` and `exit_code` (for `exit`).

Derived priority classification for journal filtering is fixed:

- `err` when `stream=stderr` on `output` records,
- `err` when `event=exit` and termination is non-clean,
- otherwise `info`.

### 4) Text Format Contract
Text mode file content is line-oriented structured records.

Line format is fixed:

- `ts=<RFC3339NanoUTC> unit=<UNIT> pid=<PID> stream=<STREAM> event=<EVENT> status=<STATUS|-> code=<CODE|-> payload=<ESCAPED|->`

Rules:

1. `output` records must set `payload` to escaped bytes and `status/code` to
   `-`.
2. `exit` records must set `status/code` and `payload` to `-`.
3. `payload` escaping is lossless (`\\`, `\n`, `\r`, `\t`, and `\xNN` for
   all non-printable/non-ASCII bytes).
4. Each logical output fragment becomes exactly one `output` record line.

### 5) Binary Format Contract
Binary mode file content uses fixed magic and length-prefixed records.

File header:

- 4-byte magic: `SLG1`.

Record framing:

1. `u32be record_len` (bytes after this field).
2. `u8 version` (fixed `1`).
3. `u8 event` (`1=output`, `2=exit`).
4. `u8 stream` (`1=stdout`, `2=stderr`, `3=meta`).
5. `u8 reserved` (must be `0`).
6. `u64be timestamp_ns` (unix epoch nanoseconds).
7. `u32be pid`.
8. `u16be unit_len`.
9. `i32be exit_code` (set `0` for output records).
10. `u8 exit_status` (`0=none`, `1=exited`, `2=signaled`, `3=spawn-failed`).
11. `u8[3] reserved` (must be `0`).
12. `u32be payload_len`.
13. `unit_len` bytes `unit_id` UTF-8.
14. `payload_len` bytes payload.

Validation rules:

- Decoder must reject malformed lengths and unknown enums.
- Decoder must surface deterministic corruption diagnostics.
- Decoder must return all records up to the last valid boundary and surface a
  warning when EOF truncates a trailing record.

## Runtime Semantics

### 1) Supervisor to Writer Event Transport
Supervisor to log writer transport is framed and binary-safe.

Transport events:

- `output` event with explicit stream (`stdout` or `stderr`) and payload bytes.
- `exit` event with explicit exit status and exit code.

This transport must support exact payload bytes and never infer stream from
merged output.

### 2) Stream Handling
Stream semantics are fixed:

1. `stdout` and `stderr` events always retain stream identity.
2. If both streams target the same log file, records are still tagged by stream.
3. If streams target different log files, each file only contains its stream’s
   `output` records plus the service `exit` marker.

### 3) Exit Marker Semantics
Exit marker record must be written exactly once per service process termination
per destination file.

- Marker is emitted before writer teardown.
- Marker contains process exit status and code.

### 4) Logging Disabled Semantics
If effective logging is disabled for a service:

- no writer process is started,
- no structured records are written,
- service lifecycle behavior is otherwise unchanged.

### 5) Legacy File Read Semantics
Decoded readers (`supervisorctl journal`, dashboard inspection, telemetry tail)
must support:

1. structured text files,
2. structured binary files,
3. legacy unstructured plain text files (fallback path).

### 5a) Decode Reliability Semantics
Decode behavior is deterministic and fault-tolerant:

1. A malformed record header in the middle of a file is a hard decode error.
2. A truncated trailing record at end-of-file is recoverable:
   - valid prior records are returned,
   - a warning diagnostic is surfaced.
3. Unknown enum values are hard decode errors.

### 6) Rotation and Vacuum Semantics
Maintenance remains file-size-based for all log formats.

1. `sbin/supervisor-logrotate` and `sbin/supervisor-log-prune` continue to
   operate on byte size, file age, and filename conventions, not record
   internals.
2. Binary logs are not skipped; they are rotated and pruned under the same
   active/rotated naming rules.
3. `supervisor-logd` passes a format hint (`text` or `binary`) to maintenance
   scripts when invoking prune hooks.
4. Script-side format hint must not change size-based prune policy.
5. Script-side format hint may be used for diagnostics and future extension.
6. `sbin/supervisor-log-prune` exposes vacuum alias options:
   - `--vacuum`,
   - `--vacuum-max-total-bytes N` (alias of `--max-total-bytes`).
7. Vacuum and prune share the same algorithm:
   - total-byte cap,
   - oldest rotated files deleted first,
   - active files are never deleted.
8. Vacuum behavior applies equally to text and binary logs.
9. After each successful rotate rename, `sbin/supervisor-logrotate` attempts
   compression only if `tar` exists on `PATH`.
10. Compression output naming is fixed to `<rotated-log-file>.tar.gz`.
11. Compression command shape is fixed to conservative portable flags:
    - `tar -C <dir> -czf <archive> <basename>`.
12. On successful compression, the uncompressed rotated file is removed.
13. On compression failure, the uncompressed rotated file is preserved and a
    warning is emitted.
14. Prune/vacuum candidate matching includes both uncompressed rotated logs and
    compressed rotated archives.
15. Compression must not special-case by log format; rotated binary logs are
    compressed under the same tar-gated policy as rotated text logs.

## Performance Invariants
1. Structured record encoding for both text and binary modes MUST run in
   `supervisor-logd` C code.
2. Supervisor Elisp MUST only frame and forward events; it MUST NOT perform
   per-byte escaping or binary serialization of payload bytes.
3. `supervisorctl journal -f` MUST follow incrementally from file offset and
   MUST NOT rescan the full file on each refresh.
4. Journal decoding MUST be streaming/bounded and avoid unbounded memory growth
   on large logs.
5. Maintenance scripts MUST compute usage by file byte size for both text and
   binary files.
6. `--since`, `--until`, and `-p` filters MUST evaluate against structured
   metadata fields and MUST NOT parse payload text.

## CLI and Dashboard Contract
1. `supervisorctl logs [--tail N] [--] ID` behavior remains unchanged.
2. New command `supervisorctl journal` is added for decoded structured logs.
3. Supported journal option set is fixed:
   - `-u ID` or `--unit ID` selects the unit log.
   - `-f` or `--follow` follows appended records.
   - `--since TIMESTAMP` filters records from timestamp forward.
   - `--until TIMESTAMP` filters records up to timestamp.
   - `-p err` filters to error-priority records only.
   - `-n N` shows last N records.
   - Combined short form `-fu ID` is accepted.
4. Accepted `--since` and `--until` timestamp formats are fixed to:
   - RFC3339/RFC3339Nano (`2026-02-16T03:04:05Z`),
   - unix epoch seconds (integer).
5. Unsupported `journal` flags must return actionable errors.
6. `supervisorctl journal -u ID` shows full decoded history for that unit.
7. `supervisorctl journal -n 20 -u ID` shows last 20 decoded records.
8. `supervisorctl journal -fu ID` follows decoded records continuously.
9. `supervisorctl journal -f -n N -u ID` outputs last N records then follows.
10. `supervisorctl journal --since TIMESTAMP --until TIMESTAMP -u ID` applies
    inclusive timestamp delimiting before output.
11. `supervisorctl journal -p err -u ID` includes only records classified
    `err`.
12. Decoded output includes timestamp, stream, payload text, and explicit exit
   marker lines.
13. Dashboard log inspection uses the same decoder path as
   `supervisorctl journal`.
14. Dashboard must not open binary files as raw text buffers.
15. Telemetry log tail surfaces decoded output consistently across formats.

## JSON Output Contract
1. `supervisorctl --json journal` is supported in non-follow and follow modes.
2. Non-follow JSON output is a single object containing:
   - request fields (`unit`, `since`, `until`, `priority`, `limit`, `follow`),
   - `records` array of decoded entries.
3. Follow JSON output is newline-delimited JSON (one record object per line).
4. JSON record fields are fixed:
   - `ts`, `unit`, `pid`, `stream`, `event`, `priority`,
   - `status`, `code`, `payload`.
5. JSON follow mode emits no non-JSON banner lines.

## Validation Contract
1. `:log-format` must be symbol `text` or `binary` when present.
2. Unknown `:log-format` values are validation errors.
3. `:log-format` on `:type target` is validation error.
4. Validation errors are surfaced in existing invalid-entry surfaces.
5. Invalid entries are excluded from startup as today.

## Phase Plan

### Phase 1: Schema and Validation
Deliverables:

1. Add `:log-format` to unit keyword allowlists.
2. Extend parser, tuple, accessors, and service struct conversion.
3. Implement strict validation rules from Validation Contract.

Acceptance:

1. `simple` and `oneshot` parse and preserve `:log-format`.
2. `target` with `:log-format` is invalid.
3. Unknown `:log-format` values are rejected deterministically.
4. `make check` passes.

### Phase 2: Writer Event Transport
Deliverables:

1. Implement framed supervisor->logd event transport for `output` and `exit`.
2. Carry explicit stream identity per event.
3. Ensure payload bytes are lossless across transport.

Acceptance:

1. Output bytes containing NUL and control bytes are preserved.
2. Stream identity is correct for merged and split stream configurations.
3. Exit events arrive at writer before teardown.
4. `make check` passes.

### Phase 3: Text Structured Record Writer
Deliverables:

1. Implement text record encoder in `supervisor-logd` per Text Format Contract.
2. Add timestamp emission and lossless payload escaping.
3. Add exit marker record emission.

Acceptance:

1. Every text record includes required fields.
2. Escaping round-trip reconstructs exact payload bytes.
3. Exit marker lines include status and code.
4. `make check` passes.

### Phase 4: Binary Structured Record Writer
Deliverables:

1. Implement binary file header and record framing per Binary Format Contract.
2. Write output and exit records with fixed enum/value rules.
3. Preserve existing local rotate and prune trigger behavior.
4. Implement trailing-record truncation handling contract for binary decode.

Acceptance:

1. Binary logs begin with `SLG1` and valid records.
2. Malformed framing is detected and surfaced deterministically.
3. Rotation and prune continue to work unchanged on binary logs.
4. Truncated trailing records return prior valid records plus warning.
5. `make check` passes.

### Phase 5: Decoder and User Surfaces
Deliverables:

1. Add shared decoder path for text, binary, and legacy logs.
2. Add `supervisorctl journal` command with `-u/--unit`, `-f/--follow`, and
   `--since`/`--until` support, including `-fu` short form.
3. Add `-p err` and `-n N` journal options.
4. Add JSON output for journal in non-follow and follow modes.
5. Keep existing `supervisorctl logs` behavior unchanged.
6. Switch dashboard log inspection and telemetry tail to decoded rendering.

Acceptance:

1. Existing `supervisorctl logs` behavior is unchanged.
2. `supervisorctl journal -u ID` renders decoded content correctly.
3. `supervisorctl journal -n 20 -u ID` returns exactly 20 newest records.
4. `supervisorctl journal -p err -u ID` returns only error-priority records.
5. `supervisorctl journal -fu ID` follows new records without full-file
   rescans.
6. `supervisorctl journal --since TIMESTAMP --until TIMESTAMP -u ID` filters
   correctly.
7. `supervisorctl --json journal` non-follow output matches JSON contract.
8. `supervisorctl --json journal -fu` emits valid NDJSON records only.
9. Dashboard renders decoded content consistent with `journal`.
10. Binary logs are never displayed as raw gibberish by decoded surfaces.
11. Legacy text logs still display correctly.
12. `make check` passes.

### Phase 6: Integration Edge Cases
Deliverables:

1. Verify merged and split stream routing with structured records.
2. Verify writer failure behavior remains non-fatal for service startup.
3. Verify restart loops, oneshot completion, and manual stop paths emit correct
   exit markers.
4. Extend rotate/prune hooks to pass format hint from writer context.
5. Verify rotate/prune behavior remains deterministic for binary logs.
6. Add explicit vacuum alias support in prune script.
7. Add optional tar-based post-rotation compression step in rotate script.

Acceptance:

1. Exit marker count is exactly one per process exit per destination file.
2. No duplicate or missing markers in restart scenarios.
3. Existing lifecycle semantics are unchanged apart from richer log content.
4. Binary logs are rotated and pruned by byte-size policy.
5. Format hint presence does not alter prune ordering rules.
6. Vacuum alias behavior matches prune behavior exactly.
7. When `tar` is present, rotated files are archived as `.tar.gz`.
8. When `tar` is missing, rotation still succeeds without compression.
9. Rotated binary logs are compressed under the same rules as text logs.
10. `make check` passes.

### Phase 7: Tests, Docs, and Audit
Deliverables:

1. Add ERT coverage for schema/validation and tuple/service round-trip.
2. Add integration tests for text and binary record writing and decoding.
3. Add tests for merged/split streams, legacy fallback, and decode errors.
4. Add tests that rotate/prune scripts continue to function for both formats.
5. Update `README.org` logging sections with `:log-format`, record fields,
   and decoding behavior.
6. Add tests for journal `-p err`, `-n`, `--since`, `--until`, and follow mode.
7. Add tests for journal JSON object mode and NDJSON follow mode.
8. Add tests for prune vacuum alias flags and binary-file handling.
9. Document vacuum feature usage in prune script and README.
10. Add tests for logrotate compression-on-with-tar and skip-without-tar.
11. Document optional tar-based rotated-log compression behavior in README.
12. Add tests for metadata-only filter evaluation semantics.
13. Add tests for binary trailing-record recovery semantics.

Acceptance:

1. New tests pass.
2. Existing tests pass.
3. `README.org` reflects actual implementation exactly.
4. `make check` passes.

## Explicit Non-Goals
1. No encryption or access-control guarantees from binary mode.
2. No configurable compression algorithm matrix beyond `tar -czf`.
3. No change to unit file authority model.
4. No rotate/prune naming redesign beyond optional `<rotated>.tar.gz`.
5. No migration rewrite of existing historical log files.
6. No change to `supervisor.log` control-plane file format in this plan.
7. No full `journalctl` parity beyond `-u`, `-f`, `--since`, `--until`,
   `-p err`, and `-n`.
8. No separate content-aware vacuum algorithm distinct from prune.
