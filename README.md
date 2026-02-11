
# Table of Contents

1.  [supervisor.el](#orge3843e6)
    1.  [Codebase Layout](#orgcfe4292)
    2.  [Requirements](#org7237cd0)
    3.  [Install And Enable](#org5455217)
    4.  [Public Commands](#org130f383)
    5.  [CLI Control Plane (\`supervisorctl\`)](#org104d0c2)
        1.  [Requirements](#org908d034)
        2.  [Usage](#orgc08d638)
        3.  [Options](#org346d950)
        4.  [Commands](#org36cf5f8)
        5.  [Exit Codes](#orgeefb58f)
        6.  [JSON Output](#org23831ed)
    6.  [Configuration Model](#orgdff47e5)
        1.  [Entry Keywords](#org67ee89a)
        2.  [Dependency Model (Schema v1)](#org561140e)
        3.  [Migration](#org028c653)
        4.  [Validation Rules](#orgccb0f5a)
        5.  [Internal Normalized Shape](#org62d4064)
    7.  [Startup Execution Model](#org57f1b0a)
        1.  [Plan Build (Before Scheduling)](#org15c1731)
        2.  [Stage Scheduling](#org8d865de)
        3.  [Ready Semantics](#org5402cd5)
        4.  [Stage Completion Criteria](#org871baad)
        5.  [Timeouts](#orgbdd1ec1)
    8.  [Entry Lifecycle State Machine](#orgf20efae)
    9.  [Process Lifecycle And Restart](#orgec209e9)
        1.  [Spawn Model](#org1224d5a)
        2.  [Restart Model (\`simple\` only)](#org610a6ec)
        3.  [Runtime Overrides](#orga240991)
        4.  [Persistent Overrides](#orge58912b)
    10. [Reload Reconciler](#org7b34128)
    11. [Stop Semantics](#orgde316d7)
        1.  [\`supervisor-stop\` (asynchronous)](#org5870e9e)
        2.  [\`supervisor-stop-now\` (synchronous)](#org107d538)
    12. [Dashboard (\`M-x supervisor\`)](#orgf7254ed)
        1.  [Columns](#orgb6e424e)
        2.  [Status Values](#org23630d9)
        3.  [Reason Values](#org60b75ad)
        4.  [Dashboard Keys](#org825b539)
    13. [Logging](#org41dbdb1)
    14. [Config File Watch](#orgdd63864)
    15. [Structured Events And Hooks](#org2d37f52)
        1.  [Built-in Messages vs Hooks](#org1da7272)
        2.  [Unified Event Hook](#org74abc7c)
        3.  [Legacy Hooks (Compatibility)](#org6847d26)
    16. [Customization Variables](#org7efac26)
    17. [Development Commands](#org4ff20ef)
    18. [License](#orgbf50d04)

[![img](https://github.com/cypherpunk2001/supervisor.el/actions/workflows/ci.yml/badge.svg)](https://github.com/cypherpunk2001/supervisor.el/actions/workflows/ci.yml)


<a id="orge3843e6"></a>

# supervisor.el

\`supervisor.el\` is an Emacs user-session process supervisor.

It manages background programs from inside Emacs with:

-   validated config parsing
-   staged startup (\`stage1 -> stage2 -> stage3 -> stage4\`)
-   intra-stage dependency ordering (\`:after\`)
-   restart/crash-loop handling for long-running processes
-   oneshot orchestration with wait/timeout semantics
-   a tabulated dashboard for control and inspection
-   structured events and legacy compatibility hooks

This is user-session supervision, not a system init replacement.


<a id="orgcfe4292"></a>

## Codebase Layout

The package is modular, with \`supervisor.el\` as a stable facade:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">File</td>
<td class="org-left">Role</td>
</tr>

<tr>
<td class="org-left">`supervisor.el`</td>
<td class="org-left">Facade. Requires all modules and provides `supervisor`</td>
</tr>

<tr>
<td class="org-left">`supervisor-core.el`</td>
<td class="org-left">Parsing, validation, planning, scheduler, runtime state, process lifecycle, persistence</td>
</tr>

<tr>
<td class="org-left">`supervisor-dashboard.el`</td>
<td class="org-left">Dashboard UI, keymaps, rendering, dashboard actions</td>
</tr>

<tr>
<td class="org-left">`supervisor-cli.el`</td>
<td class="org-left">CLI parsing/dispatch/formatting and wrapper transport adapter</td>
</tr>
</tbody>
</table>

Dependency rules in code:

-   \`supervisor-core.el\` does not require dashboard or CLI modules
-   \`supervisor-dashboard.el\` requires only \`supervisor-core.el\`
-   \`supervisor-cli.el\` requires only \`supervisor-core.el\`
-   \`supervisor.el\` requires all modules in load order: core -> dashboard -> cli

Standalone module loading is supported:

-   \`(require 'supervisor-core)\` works in headless/batch contexts
-   \`(require 'supervisor-cli)\` works without loading dashboard


<a id="org7237cd0"></a>

## Requirements

-   Emacs 28.1+
-   \`supervisor.el\`, \`supervisor-core.el\`, \`supervisor-dashboard.el\`, \`supervisor-cli.el\` available in \`load-path\`
-   Optional: \`transient\` package for dashboard action menu (\`?\`)


<a id="org5455217"></a>

## Install And Enable

    (require 'supervisor)
    
    (setq supervisor-programs
          '(("xsetroot -cursor_name left_ptr" :type oneshot :stage stage1 :id "cursor")
            ("/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
             :type simple :stage stage2 :id "polkit" :restart t)
            ("nm-applet" :type simple :stage stage3 :id "nm" :restart t)
            ("pasystray" :type simple :stage stage4 :id "tray" :restart t)))
    
    (supervisor-mode 1)

When Emacs is your session owner (for example EXWM), typical hooks are:

    (add-hook 'elpaca-after-init-hook #'supervisor-start)
    (add-hook 'kill-emacs-hook #'supervisor-stop-now)

\`supervisor-stop-now\` is synchronous and intended for \`kill-emacs-hook\`.


<a id="org130f383"></a>

## Public Commands

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Command</td>
<td class="org-left">What it does</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-mode`</td>
<td class="org-left">Global mode. Enable -&gt; `supervisor-start`; disable -&gt; `supervisor-stop`; also starts/stops config watch</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-start`</td>
<td class="org-left">Build plan, validate, and start entries by stage</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-stop`</td>
<td class="org-left">Asynchronous graceful shutdown (`SIGTERM`, then timeout `SIGKILL`)</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-stop-now`</td>
<td class="org-left">Synchronous hard shutdown (`SIGKILL` + brief wait)</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-reload`</td>
<td class="org-left">Reconcile runtime state against current config</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor`</td>
<td class="org-left">Open dashboard buffer `*supervisor*`</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-validate`</td>
<td class="org-left">Validate config and show errors without starting</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-dry-run`</td>
<td class="org-left">Show computed startup order and metadata without starting</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-migrate-config`</td>
<td class="org-left">Show canonical schema v1 view of current config</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-overrides-load`</td>
<td class="org-left">Reload persisted runtime overrides from disk</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-overrides-save`</td>
<td class="org-left">Save current runtime overrides to disk</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-overrides-clear`</td>
<td class="org-left">Clear runtime overrides in memory and on disk</td>
</tr>
</tbody>
</table>


<a id="org104d0c2"></a>

## CLI Control Plane (\`supervisorctl\`)

A shell wrapper in \`sbin/supervisorctl\` provides command-line control via \`emacsclient\`.
This allows scripting and terminal-based management without the interactive dashboard.


<a id="org908d034"></a>

### Requirements

-   Emacs server running (\`M-x server-start\` or \`(server-start)\`)
-   \`emacsclient\` available in \`PATH\`


<a id="orgc08d638"></a>

### Usage

    sbin/supervisorctl [OPTIONS] COMMAND [ARGS...]


<a id="org346d950"></a>

### Options

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Option</td>
<td class="org-left">Purpose</td>
</tr>

<tr>
<td class="org-left">`&#x2013;help`</td>
<td class="org-left">Show wrapper help text</td>
</tr>

<tr>
<td class="org-left">`&#x2013;json`</td>
<td class="org-left">Output JSON instead of human-readable text</td>
</tr>

<tr>
<td class="org-left">`&#x2013;socket NAME`</td>
<td class="org-left">Use specific Emacs server socket</td>
</tr>

<tr>
<td class="org-left">`&#x2013;server-file PATH`</td>
<td class="org-left">Use server file for TCP</td>
</tr>

<tr>
<td class="org-left">`&#x2013;timeout N`</td>
<td class="org-left">Wait at most N seconds for response</td>
</tr>
</tbody>
</table>


<a id="org36cf5f8"></a>

### Commands

Lifecycle:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Command</td>
<td class="org-left">Purpose</td>
</tr>

<tr>
<td class="org-left">`start [ID&#x2026;]`</td>
<td class="org-left">Start entries (or all if none specified)</td>
</tr>

<tr>
<td class="org-left">`stop [ID&#x2026;]`</td>
<td class="org-left">Stop entries (or all if none specified)</td>
</tr>

<tr>
<td class="org-left">`restart [ID&#x2026;]`</td>
<td class="org-left">Restart entries (or all if none specified)</td>
</tr>

<tr>
<td class="org-left">`reload`</td>
<td class="org-left">Reconcile config without full restart</td>
</tr>

<tr>
<td class="org-left">`validate`</td>
<td class="org-left">Validate config and show errors</td>
</tr>
</tbody>
</table>

Inspection:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Command</td>
<td class="org-left">Purpose</td>
</tr>

<tr>
<td class="org-left">`status [ID&#x2026;]`</td>
<td class="org-left">Show status of entries</td>
</tr>

<tr>
<td class="org-left">`list [ID&#x2026;]`</td>
<td class="org-left">Alias for status</td>
</tr>

<tr>
<td class="org-left">`describe ID`</td>
<td class="org-left">Show detailed info for an entry</td>
</tr>

<tr>
<td class="org-left">`graph [ID]`</td>
<td class="org-left">Show dependency graph</td>
</tr>

<tr>
<td class="org-left">`blame`</td>
<td class="org-left">Show startup timing info</td>
</tr>

<tr>
<td class="org-left">`logs ID [&#x2013;tail N]`</td>
<td class="org-left">View entry log file</td>
</tr>
</tbody>
</table>

Runtime policy:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Command</td>
<td class="org-left">Purpose</td>
</tr>

<tr>
<td class="org-left">`enable ID&#x2026;`</td>
<td class="org-left">Enable entries</td>
</tr>

<tr>
<td class="org-left">`disable ID&#x2026;`</td>
<td class="org-left">Disable entries</td>
</tr>

<tr>
<td class="org-left">`restart-policy (on&vert;off) ID&#x2026;`</td>
<td class="org-left">Set restart policy</td>
</tr>

<tr>
<td class="org-left">`logging (on&vert;off) ID&#x2026;`</td>
<td class="org-left">Set logging policy</td>
</tr>
</tbody>
</table>

Utilities:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Command</td>
<td class="org-left">Purpose</td>
</tr>

<tr>
<td class="org-left">`kill ID [&#x2013;signal SIG]`</td>
<td class="org-left">Send signal to process</td>
</tr>

<tr>
<td class="org-left">`ping`</td>
<td class="org-left">Check if supervisor is responsive</td>
</tr>

<tr>
<td class="org-left">`version`</td>
<td class="org-left">Show version info</td>
</tr>
</tbody>
</table>


<a id="orgeefb58f"></a>

### Exit Codes

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Code</td>
<td class="org-left">Meaning</td>
</tr>

<tr>
<td class="org-left">`0`</td>
<td class="org-left">Success</td>
</tr>

<tr>
<td class="org-left">`1`</td>
<td class="org-left">Runtime failure</td>
</tr>

<tr>
<td class="org-left">`2`</td>
<td class="org-left">Invalid arguments</td>
</tr>

<tr>
<td class="org-left">`3`</td>
<td class="org-left">Emacs server unavailable</td>
</tr>

<tr>
<td class="org-left">`4`</td>
<td class="org-left">Validation failed</td>
</tr>
</tbody>
</table>


<a id="org23831ed"></a>

### JSON Output

When \`&#x2013;json\` is passed, all output is JSON with a stable schema.
Status output always includes \`entries\` and \`invalid\` arrays (empty arrays when no data).


<a id="orgdff47e5"></a>

## Configuration Model

Set \`supervisor-programs\` to a list of entries.

Entry forms:

-   String command: \`"nm-applet"\`
-   List command + plist: \`("nm-applet" :id "nm" :stage stage3 &#x2026;)\`


<a id="org67ee89a"></a>

### Entry Keywords

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Keyword</td>
<td class="org-left">Type</td>
<td class="org-left">Default</td>
<td class="org-left">Notes</td>
</tr>

<tr>
<td class="org-left">`:id`</td>
<td class="org-left">string</td>
<td class="org-left">executable basename</td>
<td class="org-left">Must be string when provided</td>
</tr>

<tr>
<td class="org-left">`:type`</td>
<td class="org-left">symbol</td>
<td class="org-left">`simple`</td>
<td class="org-left">`simple` or `oneshot`</td>
</tr>

<tr>
<td class="org-left">`:stage`</td>
<td class="org-left">symbol</td>
<td class="org-left">`stage3`</td>
<td class="org-left">`stage1`, `stage2`, `stage3`, `stage4`</td>
</tr>

<tr>
<td class="org-left">`:delay`</td>
<td class="org-left">number</td>
<td class="org-left">`0`</td>
<td class="org-left">Non-negative seconds</td>
</tr>

<tr>
<td class="org-left">`:after`</td>
<td class="org-left">string or list of strings</td>
<td class="org-left">`nil`</td>
<td class="org-left">Ordering dependencies (same stage)</td>
</tr>

<tr>
<td class="org-left">`:requires`</td>
<td class="org-left">string or list of strings</td>
<td class="org-left">`nil`</td>
<td class="org-left">Requirement dependencies (same stage, also pull-in)</td>
</tr>

<tr>
<td class="org-left">`:enabled`</td>
<td class="org-left">boolean</td>
<td class="org-left">`t`</td>
<td class="org-left">Enable entry</td>
</tr>

<tr>
<td class="org-left">`:disabled`</td>
<td class="org-left">boolean</td>
<td class="org-left">`nil`</td>
<td class="org-left">Inverse form of `:enabled`</td>
</tr>

<tr>
<td class="org-left">`:restart`</td>
<td class="org-left">boolean</td>
<td class="org-left">`t` for `simple`</td>
<td class="org-left">Invalid for `oneshot`</td>
</tr>

<tr>
<td class="org-left">`:no-restart`</td>
<td class="org-left">boolean</td>
<td class="org-left">`nil`</td>
<td class="org-left">Inverse form of `:restart`</td>
</tr>

<tr>
<td class="org-left">`:logging`</td>
<td class="org-left">boolean</td>
<td class="org-left">`t`</td>
<td class="org-left">Per-process stdout/stderr logging</td>
</tr>

<tr>
<td class="org-left">`:oneshot-wait`</td>
<td class="org-left">boolean</td>
<td class="org-left">`supervisor-oneshot-default-wait`</td>
<td class="org-left">Invalid for `simple`</td>
</tr>

<tr>
<td class="org-left">`:async`</td>
<td class="org-left">boolean</td>
<td class="org-left">`nil`</td>
<td class="org-left">Alias for non-blocking oneshot</td>
</tr>

<tr>
<td class="org-left">`:oneshot-timeout`</td>
<td class="org-left">number or `nil`</td>
<td class="org-left">`supervisor-oneshot-timeout`</td>
<td class="org-left">Invalid for `simple`</td>
</tr>

<tr>
<td class="org-left">`:tags`</td>
<td class="org-left">symbol/string/list</td>
<td class="org-left">`nil`</td>
<td class="org-left">Dashboard tag filtering labels</td>
</tr>
</tbody>
</table>


<a id="org561140e"></a>

### Dependency Model (Schema v1)

Two types of dependencies are supported:

-   \`:after\` (ordering only): controls start ORDER but does not pull in services
-   \`:requires\` (pull-in + ordering): implies ordering AND the required service should be active

Dependency constraints:

-   Both \`:after\` and \`:requires\` must reference services in the same stage
-   Cross-stage \`:after\` is ignored with a warning
-   Cross-stage \`:requires\` causes an error (entry marked invalid)
-   Missing dependencies are ignored with a warning


<a id="org028c653"></a>

### Migration

\`M-x supervisor-migrate-config\` displays current config in canonical schema v1 format.
This is useful for:

-   Normalizing shorthand entries to explicit form
-   Seeing computed defaults
-   Preparing config for future schema versions


<a id="orgccb0f5a"></a>

### Validation Rules

Entries are validated before startup and in validate/dry-run.

-   unknown keywords are rejected
-   \`:id\` must be a string
-   \`:type\` must be symbol and one of \`simple\`/\`oneshot\`
-   \`:stage\` must be symbol and one of \`stage1..stage4\`
-   \`:delay\` must be non-negative number
-   \`:oneshot-timeout\` must be number or \`nil\`
-   mutually exclusive pairs are rejected:
-   \`:enabled\` with \`:disabled\`
-   \`:restart\` with \`:no-restart\`
-   \`:oneshot-wait\` with \`:async\`
-   type-specific restrictions are enforced:
-   \`oneshot\` rejects \`:restart\` and \`:no-restart\`
-   \`simple\` rejects \`:oneshot-wait\`, \`:async\`, \`:oneshot-timeout\`

Invalid entries are skipped by startup/reload and shown as \`invalid\` in dashboard with reason.

Duplicate IDs: first valid occurrence wins; later duplicates are skipped.


<a id="org62d4064"></a>

### Internal Normalized Shape

Every valid entry is normalized internally to:

    (id cmd delay enabled-p restart-p logging-p type stage after
        oneshot-wait oneshot-timeout tags requires)

Accessor functions (\`supervisor-entry-id\`, \`supervisor-entry-command\`, etc.) abstract the tuple indexing.


<a id="org57f1b0a"></a>

## Startup Execution Model

\`supervisor-start\` runs an asynchronous staged DAG scheduler.


<a id="org15c1731"></a>

### Plan Build (Before Scheduling)

1.  Parse/validate all entries.
2.  Deduplicate IDs.
3.  Partition by stage.
4.  Validate \`:after\` dependencies against same-stage IDs (warn on missing/cross-stage).
5.  Validate \`:requires\` dependencies against same-stage IDs (error on cross-stage, warn on missing).
6.  Topologically sort each stage with stable input-order tie-break.
7.  If cycle detected, clear all \`:after\` edges in that stage and use list order.


<a id="org8d865de"></a>

### Stage Scheduling

-   Stages run sequentially: \`stage1\`, \`stage2\`, \`stage3\`, \`stage4\`.
-   Entries inside a stage may start in parallel (dependency constrained).
-   Optional \`supervisor-max-concurrent-starts\` limits concurrent spawn attempts.


<a id="org5402cd5"></a>

### Ready Semantics

Readiness (used to unblock dependents and complete stages):

-   \`simple\`: ready when spawned successfully
-   \`oneshot\`: ready when exited (success/failure) or timed out
-   disabled: treated as ready immediately
-   failed spawn: treated as ready immediately


<a id="org871baad"></a>

### Stage Completion Criteria

A stage is complete only when all are true:

-   all stage entries reached started/skipped/failed-to-spawn accounting
-   no pending delay timers
-   no pending blocking oneshots


<a id="orgbdd1ec1"></a>

### Timeouts

-   Per-entry oneshot timeout via \`:oneshot-timeout\` (or \`supervisor-oneshot-timeout\` default)
-   Optional \`supervisor-stage-timeout\` can force stage completion and mark unstarted entries as \`stage-timeout\`


<a id="orgf20efae"></a>

## Entry Lifecycle State Machine

\`supervisor&#x2013;entry-state\` tracks normalized lifecycle state.

States:

-   \`stage-not-started\`
-   \`waiting-on-deps\`
-   \`delayed\`
-   \`disabled\`
-   \`started\`
-   \`failed-to-spawn\`
-   \`stage-timeout\`

Transitions are validated by a central helper. Invalid transitions error unless forced.


<a id="orgec209e9"></a>

## Process Lifecycle And Restart


<a id="org1224d5a"></a>

### Spawn Model

Processes start via \`make-process\` with argv from \`split-string-and-unquote\`.
No implicit shell interpretation is performed.

If shell behavior is needed (pipes, redirects, expansion), call a shell explicitly,
for example \`"sh -c '&#x2026;"\`.


<a id="org610a6ec"></a>

### Restart Model (\`simple\` only)

-   restart allowed only when effective restart policy is enabled
-   restart delay uses \`supervisor-restart-delay\`
-   crash loop window uses \`supervisor-restart-window\`
-   when crash count reaches \`supervisor-max-restarts\`, entry is marked failed (\`dead\`) and restart stops

\`oneshot\` entries do not restart.


<a id="orga240991"></a>

### Runtime Overrides

Dashboard toggles set runtime overrides for next starts/restarts:

-   enabled override (\`e\`)
-   restart override (\`r\`)
-   logging override (\`l\`)


<a id="orge58912b"></a>

### Persistent Overrides

Runtime overrides survive Emacs restart when persistence is enabled.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Variable</td>
<td class="org-left">Default</td>
<td class="org-left">Purpose</td>
</tr>

<tr>
<td class="org-left">`supervisor-overrides-file`</td>
<td class="org-left">`~/.local/state/supervisor/overrides.eld`</td>
<td class="org-left">Path to overrides file, or nil to disable</td>
</tr>
</tbody>
</table>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Command</td>
<td class="org-left">Purpose</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-overrides-load`</td>
<td class="org-left">Reload overrides from file</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-overrides-save`</td>
<td class="org-left">Save current overrides to file</td>
</tr>

<tr>
<td class="org-left">`M-x supervisor-overrides-clear`</td>
<td class="org-left">Clear all overrides (memory and file)</td>
</tr>
</tbody>
</table>

Overrides are auto-loaded on \`supervisor-start\` and auto-saved when changed via dashboard toggles.
The file uses atomic writes (temp file + rename) for crash safety.
Corrupt files are preserved for inspection and do not block startup.


<a id="org7b34128"></a>

## Reload Reconciler

\`supervisor-reload\` uses a declarative reconcile pipeline:

1.  build plan
2.  build runtime snapshot
3.  compute actions (\`start\`, \`stop\`, \`noop\`, \`skip\`)
4.  apply actions

Behavior:

-   stop running entries removed from config
-   stop running entries now effectively disabled
-   start eligible entries present in config but not running
-   skip failed entries and completed oneshots
-   keep already converged entries as \`noop\`

It does not auto-restart already-running entries because command/plist changed.


<a id="orgde316d7"></a>

## Stop Semantics


<a id="org5870e9e"></a>

### \`supervisor-stop\` (asynchronous)

-   enters shutdown mode
-   cancels delayed/restart timers and DAG state
-   emits cleanup event
-   sends \`SIGTERM\` to live supervised processes
-   completes by sentinel accounting
-   after \`supervisor-shutdown-timeout\`, sends \`SIGKILL\` to survivors


<a id="org107d538"></a>

### \`supervisor-stop-now\` (synchronous)

-   enters shutdown mode
-   cancels delayed/restart timers and DAG state
-   emits cleanup event
-   sends immediate \`SIGKILL\`
-   waits briefly (up to ~0.5s) for process death
-   clears process table and sets completion flag


<a id="orgf7254ed"></a>

## Dashboard (\`M-x supervisor\`)

Dashboard is \`tabulated-list-mode\` in buffer \`\*supervisor\*\`.

Rows and header are built from a shared runtime snapshot per refresh.


<a id="orgb6e424e"></a>

### Columns

-   \`ID\`
-   \`Type\`
-   \`Stage\`
-   \`Enabled\`
-   \`Status\`
-   \`Restart\`
-   \`Log\`
-   \`PID\`
-   \`Reason\`


<a id="org23630d9"></a>

### Status Values

-   \`running\`
-   \`done\`
-   \`failed\`
-   \`dead\`
-   \`pending\`
-   \`stopped\`
-   \`invalid\`


<a id="org60b75ad"></a>

### Reason Values

-   \`disabled\`
-   \`delayed\`
-   \`waiting-on-deps\`
-   \`stage-not-started\`
-   \`failed-to-spawn\`
-   \`stage-timeout\`
-   \`crash-loop\`


<a id="org825b539"></a>

### Dashboard Keys

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Key</td>
<td class="org-left">Action</td>
</tr>

<tr>
<td class="org-left">`e`</td>
<td class="org-left">Toggle enabled override</td>
</tr>

<tr>
<td class="org-left">`f`</td>
<td class="org-left">Cycle stage filter</td>
</tr>

<tr>
<td class="org-left">`t`</td>
<td class="org-left">Cycle tag filter</td>
</tr>

<tr>
<td class="org-left">`s`</td>
<td class="org-left">Start entry</td>
</tr>

<tr>
<td class="org-left">`k`</td>
<td class="org-left">Kill entry (confirm)</td>
</tr>

<tr>
<td class="org-left">`K`</td>
<td class="org-left">Kill entry (no confirm)</td>
</tr>

<tr>
<td class="org-left">`r`</td>
<td class="org-left">Toggle restart override</td>
</tr>

<tr>
<td class="org-left">`l`</td>
<td class="org-left">Toggle logging override</td>
</tr>

<tr>
<td class="org-left">`L`</td>
<td class="org-left">View entry log file</td>
</tr>

<tr>
<td class="org-left">`p`</td>
<td class="org-left">Open `proced`</td>
</tr>

<tr>
<td class="org-left">`P`</td>
<td class="org-left">Toggle proced auto-update</td>
</tr>

<tr>
<td class="org-left">`d`</td>
<td class="org-left">Show dependency info for entry</td>
</tr>

<tr>
<td class="org-left">`D`</td>
<td class="org-left">Show full dependency graph</td>
</tr>

<tr>
<td class="org-left">`B`</td>
<td class="org-left">Show startup blame timings</td>
</tr>

<tr>
<td class="org-left">`g`</td>
<td class="org-left">Refresh dashboard</td>
</tr>

<tr>
<td class="org-left">`G`</td>
<td class="org-left">Toggle dashboard auto-refresh</td>
</tr>

<tr>
<td class="org-left">`?`</td>
<td class="org-left">Open action menu (transient)</td>
</tr>

<tr>
<td class="org-left">`i`</td>
<td class="org-left">Show entry details (`C-u i` shows status legend)</td>
</tr>

<tr>
<td class="org-left">`h`</td>
<td class="org-left">Open full dashboard help</td>
</tr>

<tr>
<td class="org-left">`q`</td>
<td class="org-left">Quit dashboard</td>
</tr>
</tbody>
</table>

Notes:

-   separator rows reject process actions with user error
-   dependency graph and blame need startup metadata from \`supervisor-start\`
-   invalid entries are shown when stage filter is not active
-   \`?\` requires \`transient\`; if missing, dashboard reports a user error


<a id="org41dbdb1"></a>

## Logging

Two logging layers exist:

-   Supervisor-level log (\`supervisor.log\`) controlled by \`supervisor-log-to-file\`
-   Per-process logs \`log-<id>.log\` controlled by entry/effective logging

On session start, existing per-process \`log-\*.log\` files are rotated to timestamped names.


<a id="orgdd63864"></a>

## Config File Watch

\`supervisor-watch-config\` options:

-   \`nil\`: disabled
-   \`t\`: watch \`user-init-file\`
-   string path: watch that file

On file change, reload is debounced by 1 second, then \`supervisor-reload\` runs.
\`supervisor-mode\` starts/stops watching automatically.


<a id="org2d37f52"></a>

## Structured Events And Hooks


<a id="org1da7272"></a>

### Built-in Messages vs Hooks

Core logs stage lifecycle to \`\*Messages\*\` automatically (\`Supervisor: stageN starting/complete\`). No hook needed.

Hooks are for automation and integration:

-   trigger external actions (notifications, commands)
-   log failures to custom destinations
-   react to process exits
-   integrate with other packages

Do not use hooks to re-log what core already emits.


<a id="org74abc7c"></a>

### Unified Event Hook

\`supervisor-event-hook\` receives one plist argument:

-   \`:type\` event type symbol
-   \`:ts\` float timestamp
-   \`:id\` process ID or \`nil\`
-   \`:stage\` stage symbol or \`nil\`
-   \`:data\` event-specific payload plist

Event types:

-   \`stage-start\`
-   \`stage-complete\`
-   \`process-started\`
-   \`process-ready\`
-   \`process-exit\`
-   \`process-failed\`
-   \`cleanup\`

\`process-ready\` semantics:

-   simple process spawned and ready
-   oneshot exited (success/failure) or timed out
-   not emitted for disabled entries
-   not emitted for failed spawn attempts


<a id="org6847d26"></a>

### Legacy Hooks (Compatibility)

Legacy hooks are still supported and dispatched by the event adapter:

-   \`supervisor-cleanup-hook\`
-   \`supervisor-stage-start-hook\`
-   \`supervisor-stage-complete-hook\`
-   \`supervisor-process-exit-hook\` (\`id status code\`)

\`supervisor-process-exit-hook\` status values:

-   \`exited\`
-   \`signal\`
-   \`unknown\`


<a id="org7efac26"></a>

## Customization Variables

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Variable</td>
<td class="org-left">Default</td>
<td class="org-left">Purpose</td>
</tr>

<tr>
<td class="org-left">`supervisor-programs`</td>
<td class="org-left">`nil`</td>
<td class="org-left">Entry list to supervise</td>
</tr>

<tr>
<td class="org-left">`supervisor-log-directory`</td>
<td class="org-left">`~/.emacs.d/supervisor/`</td>
<td class="org-left">Directory for logs</td>
</tr>

<tr>
<td class="org-left">`supervisor-restart-delay`</td>
<td class="org-left">`2`</td>
<td class="org-left">Delay before restart</td>
</tr>

<tr>
<td class="org-left">`supervisor-max-restarts`</td>
<td class="org-left">`3`</td>
<td class="org-left">Crash-loop threshold</td>
</tr>

<tr>
<td class="org-left">`supervisor-restart-window`</td>
<td class="org-left">`60`</td>
<td class="org-left">Crash-loop time window (seconds)</td>
</tr>

<tr>
<td class="org-left">`supervisor-shutdown-timeout`</td>
<td class="org-left">`3`</td>
<td class="org-left">Graceful stop timeout before SIGKILL</td>
</tr>

<tr>
<td class="org-left">`supervisor-oneshot-default-wait`</td>
<td class="org-left">`t`</td>
<td class="org-left">Default blocking mode for oneshots</td>
</tr>

<tr>
<td class="org-left">`supervisor-oneshot-timeout`</td>
<td class="org-left">`30`</td>
<td class="org-left">Default oneshot timeout</td>
</tr>

<tr>
<td class="org-left">`supervisor-stage-timeout`</td>
<td class="org-left">`nil`</td>
<td class="org-left">Optional per-stage timeout</td>
</tr>

<tr>
<td class="org-left">`supervisor-max-concurrent-starts`</td>
<td class="org-left">`nil`</td>
<td class="org-left">Optional start concurrency limit</td>
</tr>

<tr>
<td class="org-left">`supervisor-verbose`</td>
<td class="org-left">`nil`</td>
<td class="org-left">Show info-level messages</td>
</tr>

<tr>
<td class="org-left">`supervisor-log-to-file`</td>
<td class="org-left">`nil`</td>
<td class="org-left">Enable supervisor-level log file</td>
</tr>

<tr>
<td class="org-left">`supervisor-watch-config`</td>
<td class="org-left">`nil`</td>
<td class="org-left">Config file watch and auto-reload</td>
</tr>

<tr>
<td class="org-left">`supervisor-stage-descriptions`</td>
<td class="org-left">`((stage1 . "X Setup") (stage2 . "System") (stage3 . "Services") (stage4 . "Applets"))`</td>
<td class="org-left">Stage labels in grouped dashboard</td>
</tr>

<tr>
<td class="org-left">`supervisor-dashboard-group-by-stage`</td>
<td class="org-left">`t`</td>
<td class="org-left">Show stage separator rows</td>
</tr>

<tr>
<td class="org-left">`supervisor-dashboard-show-header-hints`</td>
<td class="org-left">`nil`</td>
<td class="org-left">Show key hints in dashboard header</td>
</tr>

<tr>
<td class="org-left">`supervisor-auto-refresh-interval`</td>
<td class="org-left">`2`</td>
<td class="org-left">Dashboard live-refresh interval</td>
</tr>

<tr>
<td class="org-left">`supervisor-overrides-file`</td>
<td class="org-left">`~/.local/state/supervisor/overrides.eld`</td>
<td class="org-left">Override persistence file (nil to disable)</td>
</tr>
</tbody>
</table>


<a id="org4ff20ef"></a>

## Development Commands

    make check
    make lint
    make test
    make test-one TEST=supervisor-test-parse-string-entry


<a id="orgbf50d04"></a>

## License

GPL-3.0-or-later. See \`LICENSE\`.

