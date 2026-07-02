# Emacs Config Modernization — Analysis

## Context

A review of this `~/.emacs.d` config to find packages/settings that are **outdated**, have
**state-of-the-art replacements**, or are **obsolete on Emacs 30**.

Target: **GNU Emacs 30.2.50** (a 30.x pretest build), so every Emacs 29/30 built-in feature
is available. The config is small and clean (~2000 lines across `init.el` + 18
`initializers/*.el`), uses `use-package` throughout, and a **mixed `package.el` +
`straight.el`** setup.

Findings are grouped from low-risk to invasive so they can be picked off per item. Each item
lists the location, the issue, and the recommendation.

---

## Tier 1 — Bugs & obsolete symbols (fix regardless of taste)

### 1.1 Two git-gutter packages enabled at once ⚠️ — **done**
- `initializer-editing.el:120-195` enabled **`git-gutter`** (`global-git-gutter-mode`, plus
  `git-gutter-fringe` bitmaps).
- `initializer-vcs.el:26-37` enables **`diff-hl`** (`global-diff-hl-mode` + `diff-hl-flydiff-mode`).
- Both drew diff indicators in the fringe — two overlapping systems were running.
- **Resolution:** kept **`diff-hl`** (better Magit integration, already wired to
  `magit-post-refresh-hook`), dropped the whole `git-gutter`/`git-gutter-fringe` block,
  including the manual `fringe-helper-define` bitmaps. The `git-gutter`/`git-gutter-fringe`/
  `fringe-helper` packages remain installed under `elpa/` but are unreferenced; safe to
  `package-delete` whenever convenient.

### 1.2 Obsolete native-comp variables ⚠️ — **done**
- `initializer-system.el:32-34`: `comp-deferred-compilation` and
  `comp-deferred-compilation-black-list` were renamed and are **obsolete since Emacs 28**.
- **Resolution:** dropped the whole block. `native-comp-jit-compilation` already defaults
  to `t`, so setting it was redundant; the `(when (fboundp 'native-compile-async) ...)`
  guard is unneeded on this build. The `mu4e*.el` deny-list entry was dropped rather than
  ported: this config has no mu4e setup (added in commit `262d522`, likely copied from
  another config as a workaround for early native-comp/mu4e crashes), so it was dead
  weight.

### 1.3 Deprecated `cl` library + `remove-if` ⚠️ — **done**
- `initializer-windowing.el:133-134` (`kill-all-buffers`): `(require 'cl)` and `remove-if`
  were deprecated and emitted warnings.
- **Resolution:** switched to `seq-remove` (built-in, no `require` needed). Also fixed a
  latent escaping bug found along the way: the regex literal `"^\*.*\*$"` collapsed to
  `"^*.**$"` at read time (`\*` is not a recognized Elisp string escape), so it never
  actually matched buffer names like `*scratch*` as intended. Now `"^\\*.*\\*$"` produces
  the correct regex.

### 1.4 Legacy advice & `goto-line` in Lisp — **done**
- `initializer-editing.el:69` (`undo-tree-undo` keep-region), `306-307` (`grep`/`rgrep`
  header): used the legacy **`defadvice`**; modern equivalent is `advice-add`.
- `initializer-editing.el:303`: `goto-line` inside Lisp — byte-compiler warns; use
  `(forward-line 4)` after `(goto-char (point-min))`.
- **Resolution:** the `undo-tree-undo` keep-region advice is now a named function
  `undo-tree-undo--keep-region` installed via `(advice-add 'undo-tree-undo :around ...)`.
  The `grep`/`rgrep` header-trim advice (`delete-grep-header` + its two `advice-add` calls)
  was removed outright instead of modernized — `rg` (bound to `C-S-f`) is the ripgrep
  entry point in actual use, and plain `grep`/`rgrep` are no longer used.

### 1.5 `yes-or-no-p` alias — **done**
- `initializer-editing.el:257`: `(fset 'yes-or-no-p 'y-or-n-p)` → replaced with the built-in
  `(setq use-short-answers t)` (Emacs 28+).

### 1.6 Stale Emacs-27 workaround ⚠️ — **done**
- `initializer-style.el:16-17`: `custom--inhibit-theme-enable` "Required for Emacs 27" — no
  longer needed on 30.
- **Resolution:** dropped the `(setq custom--inhibit-theme-enable nil)` line from the
  `monokai-theme` `:init` block; `load-theme` + `custom-theme-set-faces` work fine without it.
- `init.el:24-26` + `95-106`: the `(package-initialize)` line is commented out with a note
  that `auto-package-update` will call it — fragile ordering; worth revisiting (see 4.1).

---

## Tier 2 — Safe modern swaps (little/no workflow change)

### 2.1 `use-package` bootstrap is redundant
- `init.el:45-59`: manually installs `diminish` + `use-package`. **`use-package` is built
  into Emacs 29+**; only `(require 'use-package)` (or nothing) is needed. `diminish` still
  needs installing if `:diminish` is used.

### 2.2 Third-party tree-sitter → built-in `treesit`
- `initializer-editing.el:219-233`: uses the **`tree-sitter` / `tree-sitter-langs`**
  packages (the old `emacs-tree-sitter` project), and only enables highlighting for
  `typescript-mode`.
- Emacs 29+ ships **built-in `treesit`** with `*-ts-mode` major modes; the third-party
  package is effectively superseded and unmaintained.
- **Recommendation:** drop `tree-sitter`/`tree-sitter-langs`; use `treesit-install-language-grammar`
  and the built-in `treesit`. Ties into 3.x language-mode changes below. (The existing
  `tree-sitter/` grammar dir is likely from the old package.)

### 2.3 `undo-tree` → `vundo` + `undo-fu`
- `initializer-editing.el:57-78`: `undo-tree` is heavy, persists undo-history files, and has
  a history of perf/corruption issues.
- **Recommendation:** `undo-fu` (linear undo/redo) + `vundo` (on-demand visual tree). Modern,
  lightweight, no global mode. The keep-region `defadvice` becomes unnecessary.

### 2.4 Complete the Vertico stack with `consult` + `embark`
- Already present: `vertico` + `orderless` + `marginalia` + `savehist`
  (`initializer-completion.el`) — an excellent, current stack — but **missing `consult` and
  `embark`**, its natural companions. Adding them lets several older packages retire:
  - `ctrlf` (`initializer-completion.el:152` — already `:disabled`) → `consult-line`.
  - `browse-kill-ring` (`initializer-editing.el:81`) → `consult-yank-pop`.
  - the `rg` package (`initializer-editing.el:391`) → `consult-ripgrep`.
  - `embark` + `embark-consult` add act-on-candidate (the modern replacement for many
    bespoke bindings).

### 2.5 `prettier-js` → `apheleia`
- `initializer-editing.el:317-332`: `prettier-js` works but is single-formatter.
- **`apheleia`** is the current standard: async format-on-save, cursor-stable, multi-language
  (prettier, rubocop, gofmt, …). Would also unify with Ruby (`rubocop-autocorrect-on-save`)
  and could replace `elm-format-on-save-mode`.

### 2.6 `default-text-scale`
- `initializer-windowing.el:95`: still fine, but Emacs 29+ has `global-text-scale-adjust`
  (`C-x C-M-+` / `-`). Optional built-in replacement.

---

## Tier 3 — Big migrations (state-of-the-art, but change muscle memory / need re-testing)

These are all legitimate current tools too — migrate only to lean on built-ins.

### 3.1 `lsp-mode` + `lsp-ui` — **keep (decided)**
- `initializer-lsp.el`. Eglot (built into Emacs 29+) is the lighter "built-in" alternative,
  **but the decision is to stay on `lsp-mode` + `lsp-ui`** — keep the existing Angular/mise
  server-path setup as-is. No migration. Consequence: `flycheck` also stays (see 3.2).

### 3.2 `flycheck` → built-in **`flymake`** — de-prioritized (lsp-mode kept)
- `initializer-editing.el:95-116` + the eslint chaining in `initializer-javascript.el:141-151`
  and rubocop/standard selection in `initializer-ruby.el`. Flymake's main draw was native
  integration with eglot; since `lsp-mode` is being kept (3.1), **keep `flycheck`** — it's the
  better-integrated pairing for `lsp-mode` and preserves the eslint chaining and
  `flycheck-status-emoji`. Leave as-is.

### 3.3 `projectile` → built-in **`project.el`**
- `initializer-projectile.el`, plus `treemacs-projectile` (`initializer-treemacs.el:73`) and
  the `projectile-find-file` binding. `project.el` is what the consult ecosystem assumes, but
  Projectile still has richer commands. With consult (2.4), `consult-project-buffer` etc.
  cover most daily use. Medium effort — independent of the lsp-mode decision.

### 3.4 Per-language: dedicated modes → built-in `*-ts-mode` (with 2.2)
- `initializer-javascript.el`: `typescript-mode` → **`typescript-ts-mode`/`tsx-ts-mode`**;
  `js-mode` → `js-ts-mode`. `typescript-mode` is essentially frozen upstream.
- `initializer-ruby.el:15`: `enh-ruby-mode` → built-in **`ruby-ts-mode`** (Emacs 30).
- `initializer-web.el`, `initializer-languages.el`: `json-mode` → built-in `json-ts-mode`;
  `yaml-mode` → `yaml-ts-mode`; CSS → `css-ts-mode`. `web-mode` has no built-in equivalent
  for templating and should stay.

---

## Tier 4 — Minor / cosmetic / taste

- **4.1 `auto-package-update` fork** (`init.el:96-106`): pinned to a personal
  `hupf/auto-package-update.el` `preview-updates` branch via straight — a maintenance
  burden. Emacs 30 has no direct built-in equivalent, but consider upstream
  `auto-package-update` or manual `package-upgrade-all` (Emacs 29+).
- **4.2 Consolidate package managers**: straight.el is used for only 3 active packages
  (`ligature`, `flycheck-standardrb`, `auto-package-update` fork) — see
  `initializer-style.el:49`, `initializer-ruby.el:75`, `init.el:97`. Everything else is
  `package.el`. Emacs 30's **`package-vc-install` / `:vc` in use-package** can fetch
  Git-only packages, letting straight.el be dropped entirely.
- **4.3 `rvm`** (`initializer-ruby.el:10`): fine if RVM is in use; most have moved to
  chruby/rbenv/mise. mise is already used for Node (`initializer-javascript.el:10`) — mise
  can manage Ruby too, which would unify version management.
- **4.4 `add-node-modules-path` npm-bin workaround** (`initializer-javascript.el:16-18`):
  the `npm v9` shim comment — recent versions of the package handle this; the custom
  `add-node-modules-path-command` override may be removable.
- **4.5 `volatile-highlights`** (`initializer-editing.el:52`): lightly maintained; low
  priority, still works.
- **4.6 `exec-path-from-shell-initialize-once`** (`initializer-system.el:54-59`): defined but
  its only callers are commented-out (Haskell/Go/Rust). Effectively dead code now.
- **4.7 Theme/modeline (pure taste)**: `monokai-theme` + hand-rolled modeline faces
  (`initializer-modeline.el`) are fine. Modern options if ever wanted: built-in
  **`modus-themes`** / `ef-themes` (no install needed) and `doom-modeline`.
- **4.8 `coffee-mode`, `php-mode`, `adoc-mode`, `haml-mode`**: legacy-language modes — keep
  only what is actually edited.
- **4.9 ripgrep executable resolution** (`initializer-editing.el:391-397`) — _to discuss_:
  the hardcoded `rg-executable` (`~/.cargo/bin/rg`) was removed (commit 3c10f9c), so `rg` now
  relies on being found on `PATH`. Open question for later: is `rg` reliably on `PATH` in the
  GUI Emacs environment (via `exec-path-from-shell` / mise), or does it need explicit setup?
  Also relevant if 2.4 lands — `consult-ripgrep` has the same executable dependency.

---

## Suggested priority order

1. **Do now (Tier 1):** ~~duplicate git-gutter~~ (done, 1.1), ~~obsolete comp vars~~ (done, 1.2),
   ~~`cl`/`remove-if`~~ (done, 1.3), `defadvice`/`goto-line`, `use-short-answers`.
2. **Easy wins (Tier 2):** drop use-package bootstrap; built-in `treesit`; `vundo`+`undo-fu`;
   add `consult`+`embark`; `apheleia`.
3. **When there's appetite (Tier 3):** project.el, `*-ts-mode` — per language, testing each.
   (lsp-mode + flycheck are kept — see 3.1/3.2.)
4. **Cleanup (Tier 4):** consolidate to one package manager; prune dead code and unused modes.

## Verification (if/when any of these are applied)

- Start clean: `emacs -Q --debug-init` then load, or restart Emacs and watch `*Messages*` and
  the startup-time line for byte-compile/obsoletion warnings.
- `M-x package-lint` / check `*Warnings*` for remaining obsolete-symbol warnings (Tier 1).
- Per language changed in Tier 3: open a real project file, confirm LSP connects
  (`M-x lsp`), diagnostics appear, format-on-save works, and Treemacs/project navigation
  still resolves the project root.
