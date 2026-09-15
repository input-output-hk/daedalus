# 01. Measured Divergence Between `yarn prettier` and `nix fmt`

All measurements in this note were taken on a clean checkout of `master` at
`50e6b84b3` on 2026-08-26, on `x86_64-linux`. Each is reproducible with the
command shown.

## 1. Two prettier versions govern the same files

| Entry point | Binary | Version | Reached from |
|---|---|---|---|
| `yarn prettier:check` | `./node_modules/.bin/prettier` | 2.1.2 | `yarn check:all` (`package.json:17`) |
| `nix fmt`, `checks.treefmt` | nixpkgs `prettier` | 3.6.2 | flake check set, required on `x86_64-linux` |

The repository pin is `package.json:163`:

```
"prettier": "2.1.2",
```

The treefmt side resolves through `nixpkgs/nixos-25.11` (`flake.nix:5`). The
generated treefmt config names the binary explicitly:

```
$ nix eval --raw .#formatter.x86_64-linux.outPath
/nix/store/fgh9nw1gwph85g8rqih2pg5m9a28jqya-treefmt

$ grep -A1 '\[formatter.prettier\]' \
    /nix/store/ahr8168hfk6vpg7d51apddqx7vrws4zm-treefmt.toml
[formatter.prettier]
command = "/nix/store/b5bx6qi8b2fza6b6sdd792c79iinln67-prettier-3.6.2/bin/prettier"
```

`checks.treefmt` is in the check set, and every `x86_64-linux` check is a
constituent of the `required` aggregate (`flake.nix`, `hydraJobs.required`), so
it gates a merge:

```
$ nix eval .#checks.x86_64-linux --apply builtins.attrNames
[ "bundle-integrity" "compile" "cucumber-unit" "drt-clippy" "i18n" "jest"
  "lint" "shellcheck" "storybook" "stylelint" "treefmt" "watchdog-clippy"
  "watchdog-test" ]
```

Note that `perSystem/checks.nix` does not define `treefmt`. It arrives from
`inputs.treefmt-nix.flakeModule`, which is why reading `checks.nix` alone
understates the check set.

## 2. The committed tree is prettier 3 shaped

```
$ ./node_modules/.bin/prettier --check "**/*.*"    # prettier 2.1.2
[warn] Code style issues found in the above file(s). Forgot to run Prettier?
# 210 files listed

$ nix fmt -- --ci                                   # prettier 3.6.2 via treefmt
formatted 1950 files (0 changed) in 6.987s
```

The 210 files break down as:

| Directory | Files |
|---|---|
| `source/` | 166 |
| `tests/` | 29 |
| `storybook/` | 9 |
| `hardware-wallet-tests/` | 5 |
| `declaration.d.ts` | 1 |

So `yarn check:all` fails on a clean checkout, and the merge gate passes on the
same checkout.

## 3. The failure loop

Following the failure that `yarn check:all` reports produces a tree that CI
rejects:

```
$ ./node_modules/.bin/prettier --write "**/*.*"   # what yarn prettier:format does
$ git diff --name-only | wc -l
210

$ nix fmt -- --ci
formatted 1950 files (209 changed) in 7.04s
Error: unexpected changes detected, --fail-on-change is enabled
```

209 of the 210 files are reverted by treefmt. One is not:
`source/renderer/app/api/news/requests/getNews.ts`. Prettier 2 breaks a
template literal that prettier 3 leaves on one line, and prettier 3 accepts
both shapes as already formatted, so the prettier 2 rewrite survives the
formatter that is supposed to be authoritative and lands in the tree.

## 4. What the diff consists of

Classifying the 334 hunks in the prettier 2 rewrite:

| Shape | Hunks |
|---|---|
| Assignment break layout, changed in prettier 2.3 | 197 |
| Other line-break and wrapping differences | 132 |
| Trailing comma in a broken type parameter list | 5 |

Two representative hunks:

```diff
-  const hardwareWalletConnectionChannel =
-    createHardwareWalletConnectionChannel();
+  const hardwareWalletConnectionChannel = createHardwareWalletConnectionChannel();
```

```diff
 export type EnumMap<
   K extends string,
   V,
-  O extends Record<string, any> = any,
+  O extends Record<string, any> = any
 > = O & Record<K, V & $ElementType<O, K>>;
```

None of it is a style decision anyone made. It is the difference between two
prettier majors.

## 5. Bumping to 3.6.2 changes zero files

Running the treefmt prettier against the repository's own `.prettierrc` and
`.prettierignore`, with the same glob `yarn prettier:check` uses:

```
$ /nix/store/b5bx6qi8b2fza6b6sdd792c79iinln67-prettier-3.6.2/bin/prettier \
    --check "**/*.*"
Checking formatting...
All matched files use Prettier code style!
```

This is the fact that makes the bump low risk: the version move is a
dependency change with no accompanying reformat.

## 6. `.prettierrc` is not the config treefmt uses

`perSystem/formatter.nix` sets `programs.prettier.settings`, and treefmt-nix
turns those settings into its own generated file:

```
$ cat /nix/store/f803fccm98zcfp3hf4hnszpsg8b25rk7-prettierrc.json
{
  "singleQuote": true,
  "trailingComma": "es5"
}
```

which it passes as `--config`. From `treefmt-nix/programs/prettier.nix`:

```nix
settingsFile =
  if settings != { } then configFormat.generate "prettierrc.json" settings else null;
...
options = lib.optionals (settingsFile != null) [
  "--config"
  (toString settingsFile)
];
```

An explicit `--config` stops prettier discovering `.prettierrc`, so treefmt has
never read the repository's config file. The two happen to hold the same values
today. Editing `.prettierrc` alone would change `yarn prettier` and not
`nix fmt`, which is the same class of divergence as the version skew.

Dropping `programs.prettier.settings` leaves `settingsFile` null, no `--config`
is passed, and prettier discovers `.prettierrc` itself.

## 7. The prettier `includes` list in `formatter.nix` is inert

`formatter.nix` writes to `settings.formatter.prettier.includes`, which is not
the option that controls the prettier module's include list. The module owns
`programs.prettier.includes`, whose default is treefmt-nix's own list, and
`mkFormatterModule` copies that default into `settings.formatter.prettier.includes`
as a second definition (`treefmt-nix/default.nix`, `mkFormatterModule`). Two
definitions of one `listOf str` option merge by concatenation, so the
repository's list is appended to the defaults rather than replacing them.

The effective include set therefore covers `*.md`, `*.mdx`, `*.yaml`, `*.yml`,
`*.html`, `*.css`, `*.json5` and `*.vue` anywhere in the tree. Setting
`programs.prettier.includes` instead of `settings.formatter.prettier.includes`
would override the defaults, which is what the block appears to be trying to do.
The same applies to `excludes`, and to `settings.global.excludes`, whose
generated form repeats several entries for the same reason.

Nothing in those categories is formatted, because prettier applies
`.prettierignore` even to paths passed explicitly on the command line:

```
$ prettier-3.6.2 --check README.md CHANGELOG.md tsconfig.json \
    .github/workflows/windows-tests.yml
Checking formatting...
All matched files use Prettier code style!

$ prettier-3.6.2 --ignore-path /dev/null --check README.md tsconfig.json
[warn] README.md
[warn] tsconfig.json
[warn] Code style issues found in 2 files.
```

`README.md` and `tsconfig.json` are not prettier formatted and treefmt does not
format them. `.prettierignore` is the single gate that both entry points pass
through, which is also why their scopes agree exactly. The `includes` and
`excludes` blocks in `formatter.nix` reproduce that gate a second and third
time without effect, and read as though Markdown were formatted.

## 8. `--loglevel` was removed in prettier 3

```
$ prettier-3.6.2 --loglevel warn --check declaration.d.ts
[warn] Ignored unknown option --loglevel=warn. Did you mean --log-level?
```

Three scripts pass it (`package.json:49`, `:58`, `:59`), as does
`.agent/skills/theme-management/SKILL.md:404`. The flag is ignored with a
warning rather than rejected, so nothing fails, but every invocation gains a
spurious warning line and the intended quieting stops working.

## 9. No programmatic use of prettier

Prettier 3 made its Node API asynchronous and ESM only. Nothing in the
repository imports it:

```
$ grep -rn "require(['\"]prettier\|from ['\"]prettier" \
    --include='*.ts' --include='*.tsx' --include='*.js' \
    source storybook utils tests scripts gulpfile.js
# no matches
```

Every use is through the CLI, so the API change does not apply here.

## 10. The bump needs no Nix hash update

`node_modules` is built from an offline cache derived from the lockfile at
evaluation time (`nix/internal/common.nix:298`):

```nix
offlineCache = yarn2nix.importOfflineCache (yarn2nix.mkYarnNix {
  yarnLock = srcLockfiles + "/yarn.lock";
```

There is no fixed-output hash to bump alongside `yarn.lock`. The flake sets
`allow-import-from-derivation = "true"` (`flake.nix`, `nixConfig`), which is
what makes that work.

## What these measurements do not cover

- Only `x86_64-linux` was measured. The prettier version comes from the same
  nixpkgs on every system, so it should not vary, but that was not checked on
  darwin.
- `checks.treefmt` was not built through Nix. treefmt was run directly from the
  built formatter wrapper against a scratch worktree. The binary and config are
  the same ones the check uses; the sandbox around them was not exercised.
- Whether prettier 3.6.2 changes any file that is currently ignored was not
  measured, because those files are ignored under both versions.
- No timing comparison was made. Neither formatter is slow enough for it to
  matter here.
