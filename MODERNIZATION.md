# Emacs Config Modernization — Analysis

## Context

A review of this `~/.emacs.d` config to find packages/settings that are **outdated**, have
**state-of-the-art replacements**, or are **obsolete on Emacs 30**.

Target: **GNU Emacs 30.2.50** (a 30.x pretest build), so every Emacs 29/30 built-in feature
is available. The config is small and clean (~2000 lines across `init.el` + 18
`initializers/*.el`), uses `use-package` throughout, on **`package.el`** (straight.el was
consolidated away — see git history).

Findings are grouped from low-risk to invasive so they can be picked off per item. Completed
items have been removed from this doc; see git history for what changed and why.

---

## Tier 3 — Big migrations (state-of-the-art, but change muscle memory / need re-testing)

These are all legitimate current tools too — migrate only to lean on built-ins.

### 3.1 `lsp-mode` + `lsp-ui` — **keep (decided)**
- `initializer-lsp.el`. Eglot (built into Emacs 29+) is the lighter "built-in" alternative,
  **but the decision is to stay on `lsp-mode` + `lsp-ui`** — keep the existing Angular/mise
  server-path setup as-is. No migration. Consequence: `flycheck` also stays (see 3.2).

### 3.2 `flycheck` → built-in **`flymake`** — de-prioritized (lsp-mode kept)
- `initializer-editing.el:90-108` + the eslint chaining in `initializer-javascript.el:141-151`
  and rubocop/standard selection in `initializer-ruby.el`. Flymake's main draw was native
  integration with eglot; since `lsp-mode` is being kept (3.1), **keep `flycheck`** — it's the
  better-integrated pairing for `lsp-mode` and preserves the eslint chaining and
  `flycheck-status-emoji`. Leave as-is.

### 3.4.1 `web-mode` → `html-ts-mode` — _to discuss, not migrated_
- `web-mode` handles `.phtml`/`.erb`/`.hbs`/`.astro`/JSP/ASP templating
  (`initializer-web.el:36-59`) where a single buffer mixes HTML with embedded PHP/Ruby/JS.
  Built-in `html-ts-mode` exists on this Emacs 30.2 build, but it's unclear whether it
  handles multi-language embedded templating the way `web-mode` does, or would need
  per-template-language experimentation (treesit's parser-embedding support, similar to how
  `php-ts-mode` embeds `phpdoc`). Left as a decision for later — no code changed here. (The
  rest of the per-language `*-ts-mode` migration is done.)

---

## Tier 4 — Minor / cosmetic / taste

- **4.1 `auto-package-update` fork** (`init.el:96-106`): pinned to a personal
  `hupf/auto-package-update.el` `preview-updates` branch (now via the built-in `:vc`
  use-package keyword) — a maintenance burden. Emacs 30 has no direct built-in equivalent,
  but consider upstream `auto-package-update` or manual `package-upgrade-all` (Emacs 29+).
- **4.3 `rvm`** (`initializer-ruby.el:10`): fine if RVM is in use; most have moved to
  chruby/rbenv/mise. mise is already used for Node (`initializer-javascript.el:10`) — mise
  can manage Ruby too, which would unify version management.
- **4.4 `add-node-modules-path` npm-bin workaround** (`initializer-javascript.el:16-18`):
  the `npm v9` shim comment — recent versions of the package handle this; the custom
  `add-node-modules-path-command` override may be removable.
- **4.5 `volatile-highlights`** (`initializer-editing.el:52-55`): lightly maintained; low
  priority, still works.
- **4.6 `exec-path-from-shell-initialize-once`** (`initializer-system.el:54-59`): defined but
  its only callers are commented-out (Haskell/Go/Rust). Effectively dead code now.
- **4.7 Theme/modeline (pure taste)**: `monokai-theme` + hand-rolled modeline faces
  (`initializer-modeline.el`) are fine. Modern options if ever wanted: built-in
  **`modus-themes`** / `ef-themes` (no install needed) and `doom-modeline`.
- **4.8 `adoc-mode`, `haml-mode`**: legacy-language modes with no built-in tree-sitter
  equivalent — keep only what is actually edited.
- **4.9 ripgrep executable resolution** (`initializer-completion.el`) — _to discuss_: the
  hardcoded `rg-executable` (`~/.cargo/bin/rg`) was removed (commit 3c10f9c) back when the
  `rg` package was still in use; now `consult-ripgrep` relies on the `rg` binary being found on
  `PATH`. Open question for later: is `rg` reliably on `PATH` in the GUI Emacs environment (via
  `exec-path-from-shell` / mise), or does it need explicit setup?

---

## Suggested priority order

1. **When there's appetite (Tier 3):** `web-mode`→`html-ts-mode` (3.4.1, deferred). (lsp-mode +
   flycheck are kept — see 3.1/3.2.)
2. **Cleanup (Tier 4):** prune dead code and unused modes.

## Verification (if/when any of these are applied)

- Start clean: `emacs -Q --debug-init` then load, or restart Emacs and watch `*Messages*` and
  the startup-time line for byte-compile/obsoletion warnings.
- Per language changed in Tier 3: open a real project file, confirm LSP connects
  (`M-x lsp`), diagnostics appear, format-on-save works, and Treemacs/project navigation
  still resolves the project root.
- If working on tree-sitter-related items: run `M-x my/treesit-install-all-grammars` once per
  machine (needs `git` + a C compiler on `PATH`) before opening `.ts`/`.tsx`/`.js`/`.json`/
  `.yml`/`.css`/`.rb`/`.php` files — without a compiled grammar, the corresponding
  `*-ts-mode` throws a hard error instead of falling back gracefully.
