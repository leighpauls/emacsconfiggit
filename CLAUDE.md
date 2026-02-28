# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Personal Emacs configuration repository. Deployed by symlinking `.emacs` and `.emacs.d/` from `$HOME` to this repo.

## Architecture

- `.emacs` — Main init file. Loads packages, sets up eglot/LSP for C++ and Rust, configures magit, vterm, clang-format, and company-mode. Contains custom magit tracing and a custom `leigh/magit-remote-update-limited-fetch-spec` for managing fetch refspecs.
- `.emacs.d/leighpauls/` — Personal elisp modules, all loaded from `.emacs` via `require`:
  - `leigh-env.el` — PATH additions, macOS key modifiers (Command=Meta, Option=Super), editor env vars
  - `leigh-keys.el` — Global keybindings (e.g., `C-x a` for magit-status, `C-c b` for compile-from-dir). Defines a minor mode to override major-mode bindings.
  - `leigh-styling.el` — Font, colors, whitespace display, indentation settings (spaces, no tabs)
  - `leigh-compilation.el` — Compilation mode customizations: `compile-from-dir` (prompted directory + command), buck/slash spam filters, ANSI color support, IntelliJ/PyCharm error navigation
  - `leigh-load-modes.el` — Loads color-theme and dired-details
  - `leigh-lpass.el` — LastPass integration helpers
  - `leigh-file-assocs.el` — File association overrides
- `.emacs.d/elpa/` — MELPA packages (managed by package.el, not checked in meaningfully)
- `.emacs.d/cedet-1.1/`, `color-theme-6.6.0/`, `emms/`, `dired-details/` — Vendored third-party packages

## Key Conventions

- Elisp uses `lexical-binding:t` in `.emacs`
- Custom functions use `leigh-` or `leigh/` prefix
- No build system or tests — this is a dotfiles repo. Test changes by evaluating buffers in Emacs (`C-c C-e`).
- Byte-compiled `.emacs.elc` exists; after changing `.emacs`, recompile with `emacs --batch -f batch-byte-compile .emacs`
