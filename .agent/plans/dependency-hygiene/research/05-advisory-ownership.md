# 05. Who owns the remaining advisories

Measured after the tier 1 bumps, at `f98b068b9`. 286 distinct advisories remain.
Attributed by the top-level dependency each advisory is reachable through, so
the work that would clear them can be assigned to the arc that owns it rather
than rediscovered later.

An advisory reachable from two roots is counted under both, so the columns sum
to more than 286.

| Arc | Advisories | critical | high | moderate | low |
|---|---|---|---|---|---|
| Storybook | 119 | 12 | 52 | 34 | 21 |
| Not attributable to a tooling arc | 88 | 0 | 46 | 34 | 8 |
| Build tooling | 53 | 2 | 23 | 25 | 3 |
| Hardware wallet | 50 | 6 | 16 | 21 | 7 |
| Test tooling | 43 | 3 | 19 | 17 | 4 |

## The Storybook arc carries most of it

119 of 286, and 12 of the 16 criticals, are reachable only through
`@storybook/*`. `@babel/traverse`, `handlebars`, `loader-utils` twice, `webpack`,
and `tar` are Storybook's build chain rather than anything Daedalus ships.

That is a useful thing for the Storybook epic to know before it starts: the
audit number improves substantially as a side effect of that work, and does not
improve much without it. It also means the headline advisory count is a poor
measure of the shipped application's exposure.

## The hardware wallet arc carries the ones with a path to funds

50 advisories, 6 critical, through `@trezor/connect` and
`@cardano-foundation/ledgerjs-hw-app-cardano`. Three are the same shape and are
the ones worth naming:

- `elliptic`, private key extraction in ECDSA, currently held at 6.5.4 by a
  `resolutions` entry
- `cipher-base`, missing type checks
- `sha.js`, missing type checks leading to hash rewind

These are reachable through `crypto-browserify`, which is the renderer's
`resolve.fallback` for `crypto`, as well as through `@trezor/connect`. So unlike
the Storybook set, they are in the shipped bundle. This is the exposure the PRD
flagged as unquantified pending three Trezor questions, now with the paths
written down.

## The two this branch can reach, and why they behave differently

`pbkdf2` and `lodash` each have **exactly one copy** in the entire tree:

```
node_modules/pbkdf2   3.1.2
node_modules/lodash   4.17.21
```

That is the `resolutions` block working as designed. `"pbkdf2": "3.1.2"` and
`"**/**/lodash": "4.17.21"` override every request in the tree, so there are no
nested copies to leave behind.

It cuts both ways, and the direction is the useful one here. It is why bumping
only the `dependencies` line would leave the old version installed everywhere,
which is the trap the plan was built around. It is also why moving both lines
clears those advisories across `@trezor/connect`, `@cardano-sdk/core`,
`crypto-browserify` and Storybook simultaneously, rather than only under the
direct dependency.

`form-data` and `semver` have no `resolutions` entry, which is why the tier 1
bump moved the direct dependency and left five nested copies untouched. The two
mechanisms sit side by side in the same `package.json`, and produce opposite
outcomes.

## What this does not say

The 88 not attributable to a tooling arc are not therefore shipping. The bucket
includes `wait-on`, `electron-connect` and other development tools that the
classification did not recognise. Establishing which of them reach the packaged
application needs the bundle examined rather than the dependency graph read, and
that is the unused-dependency investigation in task-012, not this note.
