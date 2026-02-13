# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

supervisor.el is an Emacs Lisp package for managing background processes. It provides staged startup with dependency ordering, crash recovery, and a dashboard UI.

## Module Structure

The package is split into focused modules:

| File | Purpose |
|------|---------|
| `supervisor-core.el` | Engine, parsing, scheduling, process lifecycle, state management |
| `supervisor-units.el` | Unit-file loading, validation, merge with legacy config |
| `supervisor-dashboard.el` | UI rendering, keymaps, interactive commands |
| `supervisor-cli.el` | CLI dispatcher, formatters, command handlers |
| `supervisor.el` | Entry point that loads all modules and provides the `supervisor` feature |

**Load order:** core → units → timer → dashboard → cli (loaded by entry point).

**Dependency rules:**
- `supervisor-core.el` has no dependencies on other modules (can load standalone)
- `supervisor-units.el` uses `declare-function` for core (no hard require)
- `supervisor-dashboard.el` requires only `supervisor-core`
- `supervisor-cli.el` requires only `supervisor-core`
- `supervisor.el` requires all modules

Cross-module calls use `declare-function` for proper byte-compilation.

## Emacs Lisp Standards (MANDATORY)

**CRITICAL:** All code must adhere to STANDARDS in this repository. This document contains:
- GNU Coding Standards for Emacs Lisp
- Emacs Lisp coding conventions from the official manual
- Naming conventions, predicate suffixes, library loading rules
- Minor/major mode conventions
- Documentation and commenting standards

This is a **hard requirement** for all Elisp projects. Read before writing or modifying any code.

## Git Workflow (MANDATORY)

Use only basic git commands. **Never** use `git commit --amend`, `git rebase`,
`git reset`, or any history-rewriting commands. Every commit is a new commit.

```bash
git add <specific-files>
git commit -m "message"
git push
```

Rules:
- **No amending**: if a commit needs a fix, make a new commit on top.
- **No resets**: if something went wrong, fix forward with a new commit.
- **No rebasing**: keep a linear append-only history.
- **Always push** after committing to keep local and remote in sync.
- **Always `make check`** before committing.

## Development Commands

```bash
make check          # Run all CI checks (byte-compile, checkdoc, package-lint, ERT tests)
make lint           # Run byte-compile, checkdoc, package-lint only
make test           # Run ERT tests only
make test-one TEST=supervisor-test-parse-string-entry  # Run single test

# Load and test interactively
emacs -Q -l supervisor.el
```

## Emacs as a Reference Tool

Look up function documentation from the shell when you need to confirm behavior:

```bash
emacs --batch --eval "(princ (documentation 'process-live-p))"
```

More generally, use `emacs --batch --eval "..."` to test expressions and confirm
assumptions. Do this often rather than guessing.

The full Emacs source code is available locally at `~/repos/emacs` for reading
implementation details, understanding C primitives, or checking how built-in
modes work.

## Pre-commit Requirement

**IMPORTANT:** Always run `make check` and ensure it passes before:
- Committing changes
- Moving to the next task or phase
- Declaring work complete

This matches CI exactly (GitHub Actions runs `make check` on Emacs 28.2, 29.4, and snapshot).
CI failures that pass locally often involve Emacs version differences (e.g., `when-let` → `when-let*` for Emacs 31+).

## Architecture

### Entry Parsing (Schema v1)
`supervisor--parse-entry` converts user config into a 13-element list:
`(id cmd delay enabled-p restart-p logging-p type stage after oneshot-wait oneshot-timeout tags requires)`

Use accessor functions (`supervisor-entry-id`, `supervisor-entry-command`, etc.) instead of direct indexing.
The `supervisor-service` struct provides a canonical schema v1 representation with conversion functions.

### Staged Startup Flow
1. `supervisor-start` builds a plan using `supervisor--build-plan` (pure, deterministic)
2. Plan contains entries partitioned by stage (stage1→stage2→stage3→stage4), pre-sorted
3. `supervisor--start-stages-from-plan` processes stages sequentially via continuation-passing
4. Within each stage, `supervisor--dag-init` builds a dependency graph from `:after` and `:requires` declarations
5. Entries with in-degree 0 start immediately; others wait for dependencies
6. `supervisor--dag-mark-ready` is called when a process is ready (spawned for simple, exited for oneshot)

### Dependency Model
- `:after` - ordering only (controls start order, same stage)
- `:requires` - pull-in + ordering (same stage, cross-stage is an error)
Both are combined for topological sorting.

### Process Types
- **simple**: Long-running daemons. "Ready" when spawned. Restarts on crash.
- **oneshot**: Run-once scripts. "Ready" when exited (success or failure) or timed out. No restart.

### State Management
All runtime state lives in `supervisor--*` hash tables:
- `supervisor--processes`: id → process object
- `supervisor--failed`: ids that have crash-looped
- `supervisor--restart-override`: runtime restart policy overrides
- `supervisor--enabled-override`: runtime enabled policy overrides
- `supervisor--mask-override`: id → `masked` or nil
- `supervisor--oneshot-completed`: id → exit code
- `supervisor--manually-stopped`: ids stopped via CLI/dashboard (suppresses restart)
- `supervisor--manually-started`: ids started via CLI/dashboard (session-only, does not change enabled state)

DAG scheduler state (`supervisor--dag-*` variables) is per-stage and reset between stages.

### Persistent Overrides
Override state is persisted to `supervisor-overrides-file` (default: `~/.local/state/supervisor/overrides.eld`).
- Loaded on `supervisor-start` after clearing runtime state
- Saved on dashboard toggle actions
- Uses atomic write pattern (temp file + rename)
- Corrupt files are logged and preserved, not deleted

### Dashboard
`supervisor-dashboard-mode` extends `tabulated-list-mode`. The dashboard reads from unit files (via `supervisor--effective-programs`) and runtime state hash tables to display current status.

## Code Quality Standards

See PLAN-INIT-followups.md for the authoritative spec. Key requirements:

### Validation
- Entry options must be validated against a whitelist before starting
- Invalid entries are skipped but surfaced in the dashboard with status `invalid` and a reason
- Mutually exclusive options (`:restart`/`:no-restart`, `:enabled`/`:disabled`) must be detected
- Type-specific options must be enforced (`:restart` invalid for oneshot, `:oneshot-wait` invalid for simple)

### Async Scheduling
- No polling loops—use sentinels and timers only
  - Exception: `supervisor-stop-now` uses a brief polling loop (max 0.5s) for synchronous
    shutdown in `kill-emacs-hook`, where async completion is not possible
- Cycle detection must fall back to list order and clear `:after` edges
- Disabled entries are immediately ready
- Start failures must not block dependents or stages
- Delayed entries must not allow early stage completion

### Stage Completion Criteria
A stage is complete only when:
1. All simple processes have spawned
2. All blocking oneshots have exited or timed out
3. All delayed entries have actually started

### Testing
- Use ERT for all tests
- If a behavior can be tested, it must be tested
- Required test coverage: whitelist validation, cycle fallback, stable ordering, oneshot timeout unlock, delayed entry handling, async oneshot non-blocking

## Documentation Files

- **README.org**: The authoritative user handbook. Requirements:
  - **Source of truth**: Must accurately reflect the codebase (the actual source of truth)
  - **Two-part style**: The intro through Quick Start is beginner-friendly and welcoming,
    explaining what the package does and who it's for. Everything after Quick Start is
    thorough technical reference handbook style.
  - **Complete coverage**: Document every single feature, option, and configuration possible
  - **Indexed and searchable**: Well-organized with clear headings for navigation
  - **No aspirational content**: Only document what currently exists in the code
  - **Human-readable**: Clear prose suitable for end users, not developer notes
  - **Keep in sync**: When modifying code, update the associated handbook documentation
- **ROADMAP.md**: Future plans and ideas. Not yet implemented.
- **CLAUDE.md**: Development guidance for AI assistants.

## MELPA Standards

This package must be structured for [MELPA](https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org) submission.

### Required Checks
```bash
make check   # Must pass before commits (runs lint + test)
```

### Code Style
- Use `#'function` syntax (not `'function`) for function references
- Decompose long functions into smaller documented functions
- All public functions and variables need docstrings
- Follow Emacs Lisp naming conventions (`supervisor-` public, `supervisor--` private)

### Package Structure
- Lexical binding required in all module files
- Standard headers: Author, Version, Package-Requires, Keywords, URL (in entry point)
- GPL-compatible license with boilerplate above `;;; Commentary:`
- Must include LICENSE file
- Main feature provided by entry point: `(provide 'supervisor)`
- Each module provides its own feature: `supervisor-core`, `supervisor-units`, `supervisor-timer`, `supervisor-dashboard`, `supervisor-cli`

**CRITICAL: Claude / Codex and all AGENTS must adhere to the strict standards and best practices laid out in this document**

53.1 Coding Standards

Contributed code should follow the GNU Coding Standards https://www.gnu.org/prep/standards/. This may also be available in info on your system.

If it doesn’t, we’ll need to find someone to fix the code before we can use it.

Emacs has additional style and coding conventions:

    the “Tips and Conventions" Appendix in the Emacs Lisp Reference https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html.
    Avoid using defadvice or with-eval-after-load for Lisp code to be included in Emacs.
    Remove all trailing whitespace in all source and text files.
    Emacs has no convention on whether to use tabs in source code; please don’t change whitespace in the files you edit.
    Use ?\s instead of ? in Lisp code for a space character.

---

https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html expanded:



Next: Key Binding Conventions, Up: Tips and Conventions   [Contents][Index]
D.1 Emacs Lisp Coding Conventions

Here are conventions that you should follow when writing Emacs Lisp code intended for widespread use:

    Simply loading a package should not change Emacs’s editing behavior. Include a command or commands to enable and disable the feature, or to invoke it.

    This convention is mandatory for any file that includes custom definitions. If fixing such a file to follow this convention requires an incompatible change, go ahead and make the incompatible change; don’t postpone it.
    You should choose a short word to distinguish your program from other Lisp programs. The names of all global symbols in your program, that is the names of variables, constants, and functions, should begin with that chosen prefix. Separate the prefix from the rest of the name with a hyphen, ‘-’. This practice helps avoid name conflicts, since all global variables in Emacs Lisp share the same name space, and all functions share another name space35. Use two hyphens to separate prefix and name if the symbol is not meant to be used by other packages.

    Occasionally, for a command name intended for users to use, it is more convenient if some words come before the package’s name prefix. For example, it is our convention to have commands that list objects named as ‘list-something’, e.g., a package called ‘frob’ could have a command ‘list-frobs’, when its other global symbols begin with ‘frob-’. Also, constructs that define functions, variables, etc., may work better if they start with ‘define-’, so it’s okay to put the name prefix later on in the name. Outside of these well-established cases, however, err on the side of prepending your name prefix.

    If you write a function that you think ought to be added to Emacs under a certain name, such as twiddle-files, don’t call it by that name in your program. Call it mylib-twiddle-files in your program, and send mail to ‘bug-gnu-emacs@gnu.org’ suggesting we add it to Emacs. If and when we do, we can change the name easily enough.

    If one prefix is insufficient, your package can use two or three alternative common prefixes, so long as they make sense.
    We recommend enabling lexical-binding in new code, and converting existing Emacs Lisp code to enable lexical-binding if it doesn’t already. See Selecting Lisp Dialect.
    Put a call to provide at the end of each separate Lisp file. See Features.
    If a file requires certain other Lisp programs to be loaded beforehand, then the comments at the beginning of the file should say so. Also, use require to make sure they are loaded. See Features.
    If a file foo uses a macro defined in another file bar, but does not use any functions or variables defined in bar, then foo should contain the following expression:

    (eval-when-compile (require 'bar))

    This tells Emacs to load bar just before byte-compiling foo, so that the macro definition is available during compilation. Using eval-when-compile avoids loading bar when the compiled version of foo is used. It should be called before the first use of the macro in the file. See Macros and Byte Compilation.
    Avoid loading additional libraries at run time unless they are really needed. If your file simply cannot work without some other library, then just require that library at the top-level and be done with it. But if your file contains several independent features, and only one or two require the extra library, then consider putting require statements inside the relevant functions rather than at the top-level. Or use autoload statements to load the extra library when needed. This way people who don’t use those aspects of your file do not need to load the extra library.
    If you need Common Lisp extensions, use the cl-lib library rather than the old cl library. The latter library is deprecated and will be removed in a future version of Emacs.
    When defining a major mode, please follow the major mode conventions. See Major Mode Conventions.
    When defining a minor mode, please follow the minor mode conventions. See Conventions for Writing Minor Modes.
    If the purpose of a function is to tell you whether a certain condition is true or false, give the function a name that ends in ‘p’ (which stands for “predicate”). If the name is one word, add just ‘p’; if the name is multiple words, add ‘-p’. Examples are framep and frame-live-p. We recommend to avoid using this -p suffix in boolean variable names, unless the variable is bound to a predicate function; instead, use a -flag suffix or names like is-foo.
    If the purpose of a variable is to store a single function, give it a name that ends in ‘-function’. If the purpose of a variable is to store a list of functions (i.e., the variable is a hook), please follow the naming conventions for hooks. See Hooks.
    Using unload-feature will undo the changes usually done by loading a feature (like adding functions to hooks). However, if loading feature does something unusual and more complex, you can define a function named feature-unload-function, and make it undo any such special changes. unload-feature will then automatically run this function if it exists. See Unloading.
    It is a bad idea to define aliases for the Emacs primitives. Normally you should use the standard names instead. The case where an alias may be useful is where it facilitates backwards compatibility or portability.
    If a package needs to define an alias or a new function for compatibility with some other version of Emacs, name it with the package prefix, not with the raw name with which it occurs in the other version. Here is an example from Gnus, which provides many examples of such compatibility issues.

    (defalias 'gnus-point-at-bol
      (if (fboundp 'point-at-bol)
          'point-at-bol
        'line-beginning-position))

    Redefining or advising an Emacs primitive is a bad idea. It may do the right thing for a particular program, but there is no telling what other programs might break as a result.
    It is likewise a bad idea for one Lisp package to advise a function in another Lisp package (see Advising Emacs Lisp Functions).
    Avoid using eval-after-load and with-eval-after-load in libraries and packages (see Hooks for Loading). This feature is meant for personal customizations; using it in a Lisp program is unclean, because it modifies the behavior of another Lisp file in a way that’s not visible in that file. This is an obstacle for debugging, much like advising a function in the other package.
    If a file does replace any of the standard functions or library programs of Emacs, prominent comments at the beginning of the file should say which functions are replaced, and how the behavior of the replacements differs from that of the originals.
    Constructs that define a function or variable should be macros, not functions, and their names should start with ‘define-’. The macro should receive the name to be defined as the first argument. That will help various tools find the definition automatically. Avoid constructing the names in the macro itself, since that would confuse these tools.
    In some other systems there is a convention of choosing variable names that begin and end with ‘*’. We don’t use that convention in Emacs Lisp, so please don’t use it in your programs. (Emacs uses such names only for special-purpose buffers.) People will find Emacs more coherent if all libraries use the same conventions.
    The default file coding system for Emacs Lisp source files is UTF-8 (see Text Representations). In the rare event that your program contains characters which are not in UTF-8, you should specify an appropriate coding system in the source file’s ‘-*-’ line or local variables list. See Local Variables in Files in The GNU Emacs Manual.
    Indent the file using the default indentation parameters.
    Don’t make a habit of putting close-parentheses on lines by themselves; Lisp programmers find this disconcerting.
    Please put a copyright notice and copying permission notice on the file if you distribute copies. See Conventional Headers for Emacs Libraries.
    For variables holding (or functions returning) a file or directory name, avoid using path in its name, preferring file, file-name, or directory instead, since Emacs follows the GNU convention to use the term path only for search paths, which are lists of directory names.

Footnotes
(35)

The benefits of a Common Lisp-style package system are considered not to outweigh the costs.

Next: Key Binding Conventions, Up: Tips and Conventions   [Contents][Index]



Next: Emacs Programming Tips, Previous: Emacs Lisp Coding Conventions, Up: Tips and Conventions   [Contents][Index]
D.2 Key Binding Conventions

    Many special major modes, like Dired, Info, Compilation, and Occur, are designed to handle read-only text that contains hyper-links. Such a major mode should redefine mouse-2 and RET to follow the links. It should also set up a follow-link condition, so that the link obeys mouse-1-click-follows-link. See Defining Clickable Text. See Buttons, for an easy method of implementing such clickable links.
    Don’t define C-c letter as a key in Lisp programs. Sequences consisting of C-c and a letter (either upper or lower case; ASCII or non-ASCII) are reserved for users; they are the only sequences reserved for users, so do not block them.

    Changing all the Emacs major modes to respect this convention was a lot of work; abandoning this convention would make that work go to waste, and inconvenience users. Please comply with it.
    Function keys F5 through F9 without modifier keys are also reserved for users to define.
    Sequences consisting of C-c followed by a control character or a digit are reserved for major modes.
    Sequences consisting of C-c followed by {, }, <, >, : or ; are also reserved for major modes.
    Sequences consisting of C-c followed by any other ASCII punctuation or symbol character are allocated for minor modes. Using them in a major mode is not absolutely prohibited, but if you do that, the major mode binding may be shadowed from time to time by minor modes.
    Don’t bind C-h following any prefix character (including C-c). If you don’t bind C-h, it is automatically available as a help character for listing the subcommands of the prefix character.
    Don’t bind a key sequence ending in ESC except following another ESC. (That is, it is OK to bind a sequence ending in ESC ESC.)

    The reason for this rule is that a non-prefix binding for ESC in any context prevents recognition of escape sequences as function keys in that context.
    Similarly, don’t bind a key sequence ending in C-g, since that is commonly used to cancel a key sequence.
    Anything that acts like a temporary mode or state that the user can enter and leave should define ESC ESC or ESC ESC ESC as a way to escape.

    For a state that accepts ordinary Emacs commands, or more generally any kind of state in which ESC followed by a function key or arrow key is potentially meaningful, then you must not define ESC ESC, since that would preclude recognizing an escape sequence after ESC. In these states, you should define ESC ESC ESC as the way to escape. Otherwise, define ESC ESC instead.

Next: Emacs Programming Tips, Previous: Emacs Lisp Coding Conventions, Up: Tips and Conventions   [Contents][Index]



Next: Tips for Making Compiled Code Fast, Previous: Key Binding Conventions, Up: Tips and Conventions   [Contents][Index]
D.3 Emacs Programming Tips

Following these conventions will make your program fit better into Emacs when it runs.

    Don’t use next-line or previous-line in programs; nearly always, forward-line is more convenient as well as more predictable and robust. See Motion by Text Lines.
    Don’t call functions that set the mark, unless setting the mark is one of the intended features of your program. The mark is a user-level feature, so it is incorrect to change the mark except to supply a value for the user’s benefit. See The Mark.

    In particular, don’t use any of these functions:
        beginning-of-buffer, end-of-buffer
        replace-string, replace-regexp
        insert-file, insert-buffer

    If you just want to move point, or replace a certain string, or insert a file or buffer’s contents, without any of the other features intended for interactive users, you can replace these functions with one or two lines of simple Lisp code.
    Use lists rather than vectors, except when there is a particular reason to use a vector. Lisp has more facilities for manipulating lists than for vectors, and working with lists is usually more convenient.

    Vectors are advantageous for tables that are substantial in size and are accessed in random order (not searched front to back), provided there is no need to insert or delete elements (only lists allow that).
    The recommended way to show a message in the echo area is with the message function, not princ. See The Echo Area.
    When you encounter an error condition, call the function error (or signal). The function error does not return. See How to Signal an Error.

    Don’t use message, throw, sleep-for, or beep to report errors.
    An error message should start with a capital letter but should not end with a period or other punctuation.

    It is occasionally useful to tell the user where an error originated, even if debug-on-error is nil. In such cases, a lower-case Lisp symbol can be prepended to the error message. For example, the error message “Invalid input” could be extended to say “some-function: Invalid input”.
    A question asked in the minibuffer with yes-or-no-p or y-or-n-p should start with a capital letter and end with ‘?’.
    When you mention a default value in a minibuffer prompt, put it and the word ‘default’ inside parentheses. It should look like this:

    Enter the answer (default 42):

    In interactive, if you use a Lisp expression to produce a list of arguments, don’t try to provide the correct default values for region or position arguments. Instead, provide nil for those arguments if they were not specified, and have the function body compute the default value when the argument is nil. For instance, write this:

    (defun foo (pos)
      (interactive
       (list (if specified specified-pos)))
      (unless pos (setq pos default-pos))
      ...)

    rather than this:

    (defun foo (pos)
      (interactive
       (list (if specified specified-pos
                 default-pos)))
      ...)

    This is so that repetition of the command will recompute these defaults based on the current circumstances.

    You do not need to take such precautions when you use interactive specs ‘d’, ‘m’ and ‘r’, because they make special arrangements to recompute the argument values on repetition of the command.
    Many commands that take a long time to execute display a message that says something like ‘Operating...’ when they start, and change it to ‘Operating...done’ when they finish. Please keep the style of these messages uniform: no space around the ellipsis, and no period after ‘done’. See Reporting Operation Progress, for an easy way to generate such messages.
    Try to avoid using recursive edits. Instead, do what the Rmail e command does: use a new local keymap that contains a command defined to switch back to the old local keymap. Or simply switch to another buffer and let the user switch back at will. See Recursive Editing.

Next: Tips for Making Compiled Code Fast, Previous: Key Binding Conventions, Up: Tips and Conventions   [Contents][Index]



Next: Tips for Avoiding Compiler Warnings, Previous: Emacs Programming Tips, Up: Tips and Conventions   [Contents][Index]
D.4 Tips for Making Compiled Code Fast

Here are ways of improving the execution speed of byte-compiled Lisp programs.

    Profile your program, to find out where the time is being spent. See Profiling.
    Use iteration rather than recursion whenever possible. Function calls are slow in Emacs Lisp even when a compiled function is calling another compiled function.
    Using the primitive list-searching functions memq, member, assq, or assoc is even faster than explicit iteration. It can be worth rearranging a data structure so that one of these primitive search functions can be used.
    Certain built-in functions are handled specially in byte-compiled code, avoiding the need for an ordinary function call. It is a good idea to use these functions rather than alternatives. To see whether a function is handled specially by the compiler, examine its byte-compile property. If the property is non-nil, then the function is handled specially.

    For example, the following input will show you that aref is compiled specially (see Functions that Operate on Arrays):

    (get 'aref 'byte-compile)
         ⇒ byte-compile-two-args

    Note that in this case (and many others), you must first load the bytecomp library, which defines the byte-compile property.
    If calling a small function accounts for a substantial part of your program’s running time, make the function inline. This eliminates the function call overhead. Since making a function inline reduces the flexibility of changing the program, don’t do it unless it gives a noticeable speedup in something slow enough that users care about the speed. See Inline Functions.

Next: Tips for Avoiding Compiler Warnings, Previous: Emacs Programming Tips, Up: Tips and Conventions   [Contents][Index]



Next: Tips for Documentation Strings, Previous: Tips for Making Compiled Code Fast, Up: Tips and Conventions   [Contents][Index]
D.5 Tips for Avoiding Compiler Warnings

    Try to avoid compiler warnings about undefined free variables, by adding dummy defvar definitions for these variables, like this:

    (defvar foo)

    Such a definition has no effect except to tell the compiler not to warn about uses of the variable foo in this file.
    Similarly, to avoid a compiler warning about an undefined function that you know will be defined, use a declare-function statement (see Telling the Compiler that a Function is Defined).
    If you use many functions, macros, and variables from a certain file, you can add a require (see require) for that package to avoid compilation warnings for them, like this:

    (require 'foo)

    If you need only macros from some file, you can require it only at compile time (see Evaluation During Compilation). For instance,

    (eval-when-compile
      (require 'foo))

    If you bind a variable in one function, and use it or set it in another function, the compiler warns about the latter function unless the variable has a definition. But adding a definition would be unclean if the variable has a short name, since Lisp packages should not define short variable names. The right thing to do is to rename this variable to start with the name prefix used for the other functions and variables in your package.
    The last resort for avoiding a warning, when you want to do something that is usually a mistake but you know is not a mistake in your usage, is to put it inside with-no-warnings. See Compiler Errors.

Next: Tips for Documentation Strings, Previous: Tips for Making Compiled Code Fast, Up: Tips and Conventions   [Contents][Index]



Next: Tips on Writing Comments, Previous: Tips for Avoiding Compiler Warnings, Up: Tips and Conventions   [Contents][Index]
D.6 Tips for Documentation Strings

Here are some tips and conventions for the writing of documentation strings. You can check many of these conventions by running the command M-x checkdoc-minor-mode.

    Every command, function, or variable intended for users to know about should have a documentation string.
    An internal variable or subroutine of a Lisp program might as well have a documentation string. Documentation strings take up very little space in a running Emacs.
    Format the documentation string so that it fits in an Emacs window on an 80-column screen. It is a good idea for most lines to be no wider than 60 characters. The first line should not be wider than 74 characters, or it will look bad in the output of apropos.

    You can fill the text if that looks good. Emacs Lisp mode fills documentation strings to the width specified by emacs-lisp-docstring-fill-column. However, you can sometimes make a documentation string much more readable by adjusting its line breaks with care. Use blank lines between sections if the documentation string is long.
    The first line of the documentation string should consist of one or two complete sentences that stand on their own as a summary. M-x apropos displays just the first line, and if that line’s contents don’t stand on their own, the result looks bad. In particular, start the first line with a capital letter and end it with a period.

    For a function, the first line should briefly answer the question, “What does this function do?” For a variable, the first line should briefly answer the question, “What does this value mean?” Prefer to answer these questions in a way that will make sense to users and callers of the function or the variable. In particular, do not tell what the function does by enumerating the actions of its code; instead, describe the role of these actions and the function’s contract.

    Don’t limit the documentation string to one line; use as many lines as you need to explain the details of how to use the function or variable. Please use complete sentences for the rest of the text too.
    When the user tries to use a disabled command, Emacs displays just the first paragraph of its documentation string—everything through the first blank line. If you wish, you can choose which information to include before the first blank line so as to make this display useful.
    The first line should mention all the important arguments of the function (in particular, the mandatory arguments), and should mention them in the order that they are written in a function call. If the function has many arguments, then it is not feasible to mention them all in the first line; in that case, the first line should mention the first few arguments, including the most important arguments.
    When a function’s documentation string mentions the value of an argument of the function, use the argument name in capital letters as if it were a name for that value. Thus, the documentation string of the function eval refers to its first argument as ‘FORM’, because the actual argument name is form:

    Evaluate FORM and return its value.

    Also write metasyntactic variables in capital letters, such as when you show the decomposition of a list or vector into subunits, some of which may vary. ‘KEY’ and ‘VALUE’ in the following example illustrate this practice:

    The argument TABLE should be an alist whose elements
    have the form (KEY . VALUE).  Here, KEY is ...

    Never change the case of a Lisp symbol when you mention it in a doc string. If the symbol’s name is foo, write “foo”, not “Foo” (which is a different symbol).

    This might appear to contradict the policy of writing function argument values, but there is no real contradiction; the argument value is not the same thing as the symbol that the function uses to hold the value.

    If this puts a lower-case letter at the beginning of a sentence and that annoys you, rewrite the sentence so that the symbol is not at the start of it.
    Do not start or end a documentation string with whitespace.
    Do not indent subsequent lines of a documentation string so that the text is lined up in the source code with the text of the first line. This looks nice in the source code, but looks bizarre when users view the documentation. Remember that the indentation before the starting double-quote is not part of the string!
    When documentation should display an ASCII apostrophe or grave accent, use ‘\\='’ or ‘\\=`’ in the documentation string literal so that the character is displayed as-is.
    In documentation strings, do not quote expressions that are not Lisp symbols, as these expressions can stand for themselves. For example, write ‘Return the list (NAME TYPE RANGE) ...’ instead of ‘Return the list `(NAME TYPE RANGE)' ...’ or ‘Return the list \\='(NAME TYPE RANGE) ...’.
    When a documentation string refers to a Lisp symbol, write it as it would be printed (which usually means in lower case), with a grave accent ‘`’ before and apostrophe ‘'’ after it. There are two exceptions: write t and nil without surrounding punctuation. For example:

    CODE can be `lambda', nil, or t.

    Note that when Emacs displays these doc strings, Emacs will usually display ‘`’ (grave accent) as ‘‘’ (left single quotation mark) and ‘'’ (apostrophe) as ‘’’ (right single quotation mark), if the display supports displaying these characters. See Substituting Key Bindings in Documentation. (Some previous versions of this section recommended using the non-ASCII single quotation marks directly in doc strings, but this is now discouraged, since that leads to broken help string displays on terminals that don’t support displaying those characters.)

    Help mode automatically creates a hyperlink when a documentation string uses a single-quoted symbol name, if the symbol has either a function or a variable definition. You do not need to do anything special to make use of this feature. However, when a symbol has both a function definition and a variable definition, and you want to refer to just one of them, you can specify which one by writing one of the words ‘variable’, ‘option’, ‘function’, or ‘command’, immediately before the symbol name. (Case makes no difference in recognizing these indicator words.) For example, if you write

    This function sets the variable `buffer-file-name'.

    then the hyperlink will refer only to the variable documentation of buffer-file-name, and not to its function documentation.

    If a symbol has a function definition and/or a variable definition, but those are irrelevant to the use of the symbol that you are documenting, you can write the words ‘symbol’ or ‘program’ before the symbol name to prevent making any hyperlink. For example,

    If the argument KIND-OF-RESULT is the symbol `list',
    this function returns a list of all the objects
    that satisfy the criterion.

    does not make a hyperlink to the documentation, irrelevant here, of the function list.

    Normally, no hyperlink is made for a variable without variable documentation. You can force a hyperlink for such variables by preceding them with one of the words ‘variable’ or ‘option’.

    Hyperlinks for faces are only made if the face name is preceded or followed by the word ‘face’. In that case, only the face documentation will be shown, even if the symbol is also defined as a variable or as a function.

    To make a hyperlink to Info documentation, write the single-quoted name of the Info node (or anchor), preceded by ‘info node’, ‘Info node’, ‘info anchor’ or ‘Info anchor’. The Info file name defaults to ‘emacs’. For example,

    See Info node `Font Lock' and Info node `(elisp)Font Lock Basics'.

    To make a hyperlink to a man page, write the single-quoted name of the man page, preceded by ‘Man page’, ‘man page’, or ‘man page for’. For example,

    See the man page `chmod(1)' for details.

    The Info documentation is always preferable to man pages, so be sure to link to an Info manual where available. For example, chmod is documented in the GNU Coreutils manual, so it is better to link to that instead of the man page.

    To link to a customization group, write the single-quoted name of the group, preceded by ‘customization group’ (the first character in each word is case-insensitive). For example,

    See the customization group `whitespace' for details.

    Finally, to create a hyperlink to URLs, write the single-quoted URL, preceded by ‘URL’. For example,

    The GNU project website has more information (see URL
    `https://www.gnu.org/').

    Don’t write key sequences directly in documentation strings. Instead, use the ‘\\[…]’ construct to stand for them. For example, instead of writing ‘C-f’, write the construct ‘\\[forward-char]’. When Emacs displays the documentation string, it substitutes whatever key is currently bound to forward-char. (This is normally ‘C-f’, but it may be some other character if the user has moved key bindings.) See Substituting Key Bindings in Documentation.
    In documentation strings for a major mode, you will want to refer to the key bindings of that mode’s local map, rather than global ones. Therefore, use the construct ‘\\<…>’ once in the documentation string to specify which key map to use. Do this before the first use of ‘\\[…]’, and not in the middle of a sentence (since if the map is not loaded, the reference to the map will be replaced with a sentence saying the map is not currently defined). The text inside the ‘\\<…>’ should be the name of the variable containing the local keymap for the major mode.

    Each use of ‘\\[…]’ slows the display of the documentation string by a tiny amount. If you use a lot of them, these tiny slowdowns will add up, and might become tangible, especially on slow systems. So our recommendation is not to over-use them; e.g., try to avoid using more than one reference to the same command in the same doc string.
    For consistency, phrase the verb in the first sentence of a function’s documentation string as an imperative—for instance, use “Return the cons of A and B.” in preference to “Returns the cons of A and B.” Usually it looks good to do likewise for the rest of the first paragraph. Subsequent paragraphs usually look better if each sentence is indicative and has a proper subject.
    The documentation string for a function that is a yes-or-no predicate should start with words such as “Return t if”, to indicate explicitly what constitutes truth. The word “return” avoids starting the sentence with lower-case “t”, which could be somewhat distracting.
    Write documentation strings in the active voice, not the passive, and in the present tense, not the future. For instance, use “Return a list containing A and B.” instead of “A list containing A and B will be returned.”
    Avoid using the word “cause” (or its equivalents) unnecessarily. Instead of, “Cause Emacs to display text in boldface”, write just “Display text in boldface”.
    Avoid using “iff” (a mathematics term meaning “if and only if”), since many people are unfamiliar with it and mistake it for a typo. In most cases, the meaning is clear with just “if”. Otherwise, try to find an alternate phrasing that conveys the meaning.
    Try to avoid using abbreviations such as “e.g.” (for “for example”), “i.e.” (for “that is”), “no.” (for “number”), “cf.” (for “compare”/“see also”) and “w.r.t.” (for “with respect to”) as much as possible. It is almost always clearer and easier to read the expanded version.36
    When a command is meaningful only in a certain mode or situation, do mention that in the documentation string. For example, the documentation of dired-find-file is:

    In Dired, visit the file or directory named on this line.

    When you define a variable that represents an option users might want to set, use defcustom. See Defining Global Variables.
    The documentation string for a variable that is a yes-or-no flag should start with words such as “Non-nil means”, to make it clear that all non-nil values are equivalent and indicate explicitly what nil and non-nil mean.
    If a line in a documentation string begins with an open-parenthesis, consider writing a backslash before the open-parenthesis, like this:

    The argument FOO can be either a number
    \(a buffer position) or a string (a file name).

    This avoids a bug in Emacs versions older than 27.1, where the ‘(’ was treated as the start of a defun (see Defuns in The GNU Emacs Manual). If you do not anticipate anyone editing your code with older Emacs versions, there is no need for this work-around.

Footnotes
(36)

We do use these occasionally, but try not to overdo it.

Next: Tips on Writing Comments, Previous: Tips for Avoiding Compiler Warnings, Up: Tips and Conventions   [Contents][Index]



Next: Conventional Headers for Emacs Libraries, Previous: Tips for Documentation Strings, Up: Tips and Conventions   [Contents][Index]
D.7 Tips on Writing Comments

We recommend these conventions for comments:

‘;’

    Comments that start with a single semicolon, ‘;’, should all be aligned to the same column on the right of the source code. Such comments usually explain how the code on that line does its job. For example:

    (setq base-version-list                 ; There was a base
          (assoc (substring fn 0 start-vn)  ; version to which
                 file-version-assoc-list))  ; this looks like
                                            ; a subversion.

‘;;’

    Comments that start with two semicolons, ‘;;’, should be aligned to the same level of indentation as the code. Such comments usually describe the purpose of the following lines or the state of the program at that point. For example:

    (prog1 (setq auto-fill-function
                 …
                 …
      ;; Update mode line.
      (force-mode-line-update)))

    We also normally use two semicolons for comments outside functions.

    ;; This Lisp code is run in Emacs when it is to operate as
    ;; a server for other processes.

    If a function has no documentation string, it should instead have a two-semicolon comment right before the function, explaining what the function does and how to call it properly. Explain precisely what each argument means and how the function interprets its possible values. It is much better to convert such comments to documentation strings, though.
‘;;;’

    Comments that start with three (or more) semicolons, ‘;;;’, should start at the left margin. We use them for comments that should be considered a heading by Outline minor mode. By default, comments starting with at least three semicolons (followed by a single space and a non-whitespace character) are considered section headings, comments starting with two or fewer are not.

    (Historically, triple-semicolon comments have also been used for commenting out lines within a function, but this use is discouraged in favor of using just two semicolons. This also applies when commenting out entire functions; when doing that use two semicolons as well.)

    Three semicolons are used for top-level sections, four for sub-sections, five for sub-sub-sections and so on.

    Typically libraries have at least four top-level sections. For example when the bodies of all of these sections are hidden:

    ;;; backquote.el --- implement the ` Lisp construct...
    ;;; Commentary:...
    ;;; Code:...
    ;;; backquote.el ends here

    (In a sense the last line is not a section heading as it must never be followed by any text; after all it marks the end of the file.)

    For longer libraries it is advisable to split the code into multiple sections. This can be done by splitting the ‘Code:’ section into multiple sub-sections. Even though that was the only recommended approach for a long time, many people have chosen to use multiple top-level code sections instead. You may chose either style.

    Using multiple top-level code sections has the advantage that it avoids introducing an additional nesting level but it also means that the section named ‘Code’ does not contain all the code, which is awkward. To avoid that, you should put no code at all inside that section; that way it can be considered a separator instead of a section heading.

    Finally, we recommend that you don’t end headings with a colon or any other punctuation for that matter. For historic reasons the ‘Code:’ and ‘Commentary:’ headings end with a colon, but we recommend that you don’t do the same for other headings anyway.

Generally speaking, the M-; (comment-dwim) command automatically starts a comment of the appropriate type; or indents an existing comment to the right place, depending on the number of semicolons. See Manipulating Comments in The GNU Emacs Manual.

Next: Conventional Headers for Emacs Libraries, Previous: Tips for Documentation Strings, Up: Tips and Conventions   [Contents][Index]



Previous: Tips on Writing Comments, Up: Tips and Conventions   [Contents][Index]
D.8 Conventional Headers for Emacs Libraries

Emacs has conventions for using special comments in Lisp libraries to divide them into sections and give information such as who wrote them. Using a standard format for these items makes it easier for tools (and people) to extract the relevant information. This section explains these conventions, starting with an example:

;;; foo.el --- Support for the Foo programming language  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2025 Your Name


;; Author: Your Name <yourname@example.com>
;; Maintainer: Someone Else <someone@example.com>
;; Created: 14 Jul 2010

;; Keywords: languages
;; URL: https://example.com/foo

;; This file is not part of GNU Emacs.

;; This file is free software…
…
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

The very first line should have this format:

;;; filename --- description  -*- lexical-binding: t; -*-

The description should be contained in one line. If the file needs to set more variables in the ‘-*-’ specification, add it after lexical-binding. If this would make the first line too long, use a Local Variables section at the end of the file.

The copyright notice usually lists your name (if you wrote the file). If you have an employer who claims copyright on your work, you might need to list them instead. Do not say that the copyright holder is the Free Software Foundation (or that the file is part of GNU Emacs) unless your file has been accepted into the Emacs distribution or GNU ELPA. For more information on the form of copyright and license notices, see the guide on the GNU website.

After the copyright notice come several header comment lines, each beginning with ‘;; header-name:’. Here is a table of the conventional possibilities for header-name:

‘Author’

    This header states the name and email address of at least the principal author of the library. If there are multiple authors, list them on continuation lines led by ;; and a tab or at least two spaces. We recommend including a contact email address, of the form ‘<…>’. For example:

    ;; Author: Your Name <yourname@example.com>
    ;;      Someone Else <someone@example.com>
    ;;      Another Person <another@example.com>

‘Maintainer’

    This header has the same format as the Author header. It lists the person(s) who currently maintain(s) the file (respond to bug reports, etc.).

    If there is no Maintainer header, the person(s) in the Author header is/are presumed to be the maintainer(s). Some files in Emacs use ‘emacs-devel@gnu.org’ for the maintainer, which means the author is no longer responsible for the file, and that it is maintained as part of Emacs.
‘Created’

    This optional line gives the original creation date of the file, and is for historical interest only.
‘Version’

    If you wish to record version numbers for the individual Lisp program, put them in this line. Lisp files distributed with Emacs generally do not have a ‘Version’ header, since the version number of Emacs itself serves the same purpose. If you are distributing a collection of multiple files, we recommend not writing the version in every file, but only the main one.
‘Keywords’ ¶

    This line lists keywords for the finder-by-keyword help command. Please use that command to see a list of the meaningful keywords. The command M-x checkdoc-package-keywords RET will find and display any keywords that are not in finder-known-keywords. If you set the variable checkdoc-package-keywords-flag non-nil, checkdoc commands will include the keyword verification in its checks.

    This field is how people will find your package when they’re looking for things by topic. To separate the keywords, you can use spaces, commas, or both.

    The name of this field is unfortunate, since people often assume it is the place to write arbitrary keywords that describe their package, rather than just the relevant Finder keywords.
‘URL’
‘Homepage’

    These lines state the website of the library.
‘Package-Version’

    If ‘Version’ is not suitable for use by the package manager, then a package can define ‘Package-Version’; it will be used instead. This is handy if ‘Version’ is an RCS id or something else that cannot be parsed by version-to-list. See Packaging Basics.
‘Package-Requires’

    If this exists, it names packages on which the current package depends for proper operation. See Packaging Basics. This is used by the package manager both at download time (to ensure that a complete set of packages is downloaded) and at activation time (to ensure that a package is only activated if all its dependencies have been).

    Its format is a list of lists on a single line. The car of each sub-list is the name of a package, as a symbol. The cadr of each sub-list is the minimum acceptable version number, as a string that can be parsed by version-to-list. An entry that lacks a version (i.e., an entry which is just a symbol, or a sub-list of one element) is equivalent to entry with version "0". For instance:

    ;; Package-Requires: ((gnus "1.0") (bubbles "2.7.2") cl-lib (seq))

    Packages that don’t need to support Emacs versions older than Emacs 27 can have the ‘Package-Requires’ header split across multiple lines, like this:

    ;; Package-Requires: ((emacs "27.1")
    ;;                    (compat "29.1.4.1"))

    Note that with this format, you still need to start the list on the same line as ‘Package-Requires’.

    The package code automatically defines a package named ‘emacs’ with the version number of the currently running Emacs. This can be used to require a minimal version of Emacs for a package.

Just about every Lisp library ought to have the ‘Author’ and ‘Keywords’ header comment lines. Use the others if they are appropriate. You can also put in header lines with other header names—they have no standard meanings, so they can’t do any harm.

We use additional stylized comments to subdivide the contents of the library file. These should be separated from anything else by blank lines. Here is a table of them:

‘;;; Commentary:’

    This begins introductory comments that explain how the library works. It should come right after the copying permissions, and is terminated by one of the comment lines described below: ‘Change Log’, ‘History’ or ‘Code’. This text is used by the Finder package, so it should make sense in that context.
‘;;; Change Log:’

    This begins an optional log of changes to the file over time. Don’t put too much information in this section—it is better to keep the detailed logs in a version control system (as Emacs does) or in a separate ChangeLog file. ‘History’ is an alternative to ‘Change Log’.
‘;;; Code:’

    This begins the actual code of the program.
‘;;; filename ends here’

    This is the footer line; it appears at the very end of the file. Its purpose is to enable people to detect truncated versions of the file from the lack of a footer line.

Previous: Tips on Writing Comments, Up: Tips and Conventions   [Contents][Index]


---

    7 The Release Process
        7.1 How Configuration Should Work
        7.2 Makefile Conventions
            7.2.1 General Conventions for Makefiles
            7.2.2 Utilities in Makefiles
            7.2.3 Variables for Specifying Commands
            7.2.4 DESTDIR: Support for Staged Installs
            7.2.5 Variables for Installation Directories
            7.2.6 Standard Targets for Users
            7.2.7 Install Command Categories
        7.3 Making Releases


7 The Release Process

Making a release is more than just bundling up your source files in a tar file and putting it up for FTP. You should set up your software so that it can be configured to run on a variety of systems. Your Makefile should conform to the GNU standards described below, and your directory layout should also conform to the standards discussed below. Doing so makes it easy to include your package into the larger framework of all GNU software.

    How Configuration Should Work
    Makefile Conventions
    Making Releases

Next: Makefile Conventions, Up: The Release Process   [Contents][Index]
7.1 How Configuration Should Work

Each GNU distribution should come with a shell script named configure. This script is given arguments which describe the kind of machine and system you want to compile the program for. The configure script must record the configuration options so that they affect compilation.

The description here is the specification of the interface for the configure script in GNU packages. Many packages implement it using GNU Autoconf (see Introduction in Autoconf) and/or GNU Automake (see Introduction in Automake), but you do not have to use these tools. You can implement it any way you like; for instance, by making configure be a wrapper around a completely different configuration system.

Another way for the configure script to operate is to make a link from a standard name such as config.h to the proper configuration file for the chosen system. If you use this technique, the distribution should not contain a file named config.h. This is so that people won’t be able to build the program without configuring it first.

Another thing that configure can do is to edit the Makefile. If you do this, the distribution should not contain a file named Makefile. Instead, it should include a file Makefile.in which contains the input used for editing. Once again, this is so that people won’t be able to build the program without configuring it first.

If configure does write the Makefile, then Makefile should have a target named Makefile which causes configure to be rerun, setting up the same configuration that was set up last time. The files that configure reads should be listed as dependencies of Makefile.

All the files which are output from the configure script should have comments at the beginning stating that they were generated automatically using configure. This is so that users won’t think of trying to edit them by hand.

The configure script should write a file named config.status which describes which configuration options were specified when the program was last configured. This file should be a shell script which, if run, will recreate the same configuration.

The configure script should accept an option of the form ‘--srcdir=dirname’ to specify the directory where sources are found (if it is not the current directory). This makes it possible to build the program in a separate directory, so that the actual source directory is not modified.

If the user does not specify ‘--srcdir’, then configure should check both . and .. to see if it can find the sources. If it finds the sources in one of these places, it should use them from there. Otherwise, it should report that it cannot find the sources, and should exit with nonzero status.

Usually the easy way to support ‘--srcdir’ is by editing a definition of VPATH into the Makefile. Some rules may need to refer explicitly to the specified source directory. To make this possible, configure can add to the Makefile a variable named srcdir whose value is precisely the specified directory.

In addition, the ‘configure’ script should take options corresponding to most of the standard directory variables (see Variables for Installation Directories). Here is the list:

--prefix --exec-prefix --bindir --sbindir --libexecdir --sysconfdir
--sharedstatedir --localstatedir --runstatedir
--libdir --includedir --oldincludedir
--datarootdir --datadir --infodir --localedir --mandir --docdir
--htmldir --pdfdir --psdir

The configure script should also take an argument which specifies the type of system to build the program for. This argument should look like this:

cpu-company-system

For example, an Athlon-based GNU/Linux system might be ‘i686-pc-linux-gnu’.

The configure script needs to be able to decode all plausible alternatives for how to describe a machine. Thus, ‘athlon-pc-gnu/linux’ would be a valid alias. There is a shell script called config.sub that you can use as a subroutine to validate system types and canonicalize aliases.

The configure script should also take the option --build=buildtype, which should be equivalent to a plain buildtype argument. For example, ‘configure --build=i686-pc-linux-gnu’ is equivalent to ‘configure i686-pc-linux-gnu’. When the build type is not specified by an option or argument, the configure script should normally guess it using the shell script config.guess.

Other options are permitted to specify in more detail the software or hardware present on the machine, to include or exclude optional parts of the package, or to adjust the name of some tools or arguments to them:

‘--enable-feature[=parameter]’

    Configure the package to build and install an optional user-level facility called feature. This allows users to choose which optional features to include. Giving an optional parameter of ‘no’ should omit feature, if it is built by default.

    No ‘--enable’ option should ever cause one feature to replace another. No ‘--enable’ option should ever substitute one useful behavior for another useful behavior. The only proper use for ‘--enable’ is for questions of whether to build part of the program or exclude it.
‘--with-package’

    The package package will be installed, so configure this package to work with package.

    Possible values of package include ‘gnu-as’ (or ‘gas’), ‘gnu-ld’, ‘gnu-libc’, ‘gdb’, ‘x’, and ‘x-toolkit’.

    Do not use a ‘--with’ option to specify the file name to use to find certain files. That is outside the scope of what ‘--with’ options are for.
‘variable=value’

    Set the value of the variable variable to value. This is used to override the default values of commands or arguments in the build process. For example, the user could issue ‘configure CFLAGS=-g CXXFLAGS=-g’ to build with debugging information and without the default optimization.

    Specifying variables as arguments to configure, like this:

    ./configure CC=gcc

    is preferable to setting them in environment variables:

    CC=gcc ./configure

    as it helps to recreate the same configuration later with config.status. However, both methods should be supported.

All configure scripts should accept all of the “detail” options and the variable settings, whether or not they make any difference to the particular package at hand. In particular, they should accept any option that starts with ‘--with-’ or ‘--enable-’. This is so users will be able to configure an entire GNU source tree at once with a single set of options.

You will note that the categories ‘--with-’ and ‘--enable-’ are narrow: they do not provide a place for any sort of option you might think of. That is deliberate. We want to limit the possible configuration options in GNU software. We do not want GNU programs to have idiosyncratic configuration options.

Packages that perform part of the compilation process may support cross-compilation. In such a case, the host and target machines for the program may be different.

The configure script should normally treat the specified type of system as both the host and the target, thus producing a program which works for the same type of machine that it runs on.

To compile a program to run on a host type that differs from the build type, use the configure option --host=hosttype, where hosttype uses the same syntax as buildtype. The host type normally defaults to the build type.

To configure a cross-compiler, cross-assembler, or what have you, you should specify a target different from the host, using the configure option ‘--target=targettype’. The syntax for targettype is the same as for the host type. So the command would look like this:

./configure --host=hosttype --target=targettype

The target type normally defaults to the host type. Programs for which cross-operation is not meaningful need not accept the ‘--target’ option, because configuring an entire operating system for cross-operation is not a meaningful operation.

Some programs have ways of configuring themselves automatically. If your program is set up to do this, your configure script can simply ignore most of its arguments.

Next: Making Releases, Previous: How Configuration Should Work, Up: The Release Process   [Contents][Index]
