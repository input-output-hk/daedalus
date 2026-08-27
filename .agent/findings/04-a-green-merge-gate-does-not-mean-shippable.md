# 04. A green merge gate does not mean the branch ships

Demonstrated on 2026-08-27. `ci/hydra-build:required` was green on a commit that
could not produce a working Linux installer on any network.

## What happened

The `bignumber.js` 9.3.1 bump added an `exports` field permitting only `"."` and
`"./package.json"`. Four files imported `bignumber.js/bignumber`, a subpath that
9.0.1 allowed and 9.3.1 forbids. Webpack refused to resolve it and
`daedalus-js` failed to build, taking all four Linux installers with it.

Everything else stayed green:

| | |
|---|---|
| `yarn compile` | passed |
| `yarn lint` | passed |
| Jest, 35 files, 403 tests | passed |
| Cucumber `@unit` | passed |
| `yarn storybook:build` | passed |
| every `checks.x86_64-linux` derivation | passed |
| `ci/hydra-build:required` | **SUCCESS** |
| `installer.x86_64-linux.*` | **FAILURE**, all four |

## Two independent causes

**The installer does not gate a merge.** `flake.nix` puts `hydraJobs.installer`
in the `nonrequired` aggregate. The reasoning in the comment is about darwin:
scarce and flaky mac builders should not hold up work, which is sound. But it
was applied to the whole `installer` set, and the Linux installer is neither
scarce nor flaky. It is also the only job in the entire configuration that runs
webpack over the renderer, which makes it the only thing that can catch a module
that resolves everywhere except in a bundle.

**The type checker structurally cannot see this class of error.** `tsconfig.json`
sets `moduleResolution: "node"`, the node10 algorithm, which ignores `exports`
entirely. Node's own resolver rejects the same import with
`ERR_PACKAGE_PATH_NOT_EXPORTED`, so three of the four broken files would also
fail at runtime; they are e2e steps, which CI does not run.

## Options, none taken here

Moving `installer.x86_64-linux.*` into `required` while leaving darwin in
`nonrequired` would close the gap directly. It is a CI policy change and needs a
decision about build time on the gate.

Moving `moduleResolution` to `node16` or `bundler` would let `yarn compile` see
`exports` maps. That is a wider change with its own fallout across a codebase
this size, and it deserves its own branch.

Either would have caught this. Neither is dependency work.

## The cheap habit in the meantime

Building `hydraJobs.installer.x86_64-linux.mainnet` after a dependency change
takes one command and catches what no local check does. The failing derivation
is `daedalus-js`, shared by all four networks, so one build settles all of them.
