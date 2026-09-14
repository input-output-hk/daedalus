# 04. Advisory baseline

`yarn audit` on `chore/dependency-hygiene` at `8bd9b052f`, before any dependency
moves. Recorded so the effect of the bumps is measured rather than asserted.

## Counts

| | |
|---|---|
| Distinct advisories | **288** |
| critical | 16 |
| high | 131 |
| moderate | 108 |
| low | 33 |

Distinct advisory identifiers, not occurrences. `yarn audit`'s own summary
reports 2,085 occurrences, which counts the same advisory once per path through
the dependency tree and moves when unrelated packages shift. The distinct count
is the number to compare against after.

## Advisories against packages this branch moves

| Package | Severity | Advisory | Patched |
|---|---|---|---|
| `pbkdf2` | critical | predictable uninitialized or zero-filled output | `>=3.1.3` |
| `pbkdf2` | critical | silently disregards `Uint8Array` input, returns static keys | `>=3.1.3` |
| `form-data` | critical | uses an unsafe random function to choose the boundary | `>=3.0.4` |
| `form-data` | high | CRLF injection via unescaped name and filename | `>=3.0.5` |
| `validator` | high | incomplete filtering | `>=13.15.22` |
| `validator` | moderate | URL validation bypass | `>=13.15.20` |
| `semver` | high | ReDoS | `>=7.5.2` |
| `lodash` | high | code injection via `_.template` | `>=4.18.0` |
| `lodash` | moderate | prototype pollution via array path in `_.unset` and `_.omit` | `>=4.18.0` |
| `lodash` | moderate | prototype pollution in `_.unset` and `_.omit` | `>=4.17.23` |
| `lodash-es` | high | prototype pollution | `>=4.17.20` |
| `lodash-es` | high | command injection | `>=4.17.21` |
| `lodash-es` | high | code injection via `_.template` | `>=4.18.0` |
| `lodash-es` | moderate | ReDoS | `>=4.17.21` |
| `lodash-es` | moderate | prototype pollution, two entries | `>=4.18.0`, `>=4.17.23` |

## What this changes about the plan

**Tier 1 is not purely mechanical.** The inventory classified `form-data`,
`validator` and `semver` as utility packages with no key material and no
arithmetic on balances, which is true of what they do and not true of what they
carry. `form-data` 3.0.0 holds two advisories, one critical, and the planned
bump to 3.0.5 clears both. `validator` 13.7.0 holds a high and a moderate, and
13.15.35 clears both. `semver` 7.3.5 holds a high, cleared by 7.8.5.

The tier is still correct as a grouping, because the risk of *taking* those
bumps is low and the check set covers them. But the commit body should say what
the group clears rather than describing it as housekeeping.

**`form-data`'s critical advisory is the same class of defect this branch's
crypto work is about.** It chooses its multipart boundary with an unsafe random
function. Nothing about Daedalus's security depends on that boundary being
unpredictable, so the practical exposure here is low, but it is worth naming
rather than passing over: the shape is identical to the entropy problem the
branch already fixed in `crypto.ts`.

**`lodash-es` is further behind than `lodash`.** It sits at 4.17.15 against
`lodash`'s 4.17.21, so it carries four advisories the main package does not,
including a high-severity command injection. Both move to 4.18.1.

## Reproducing this

```bash
yarn audit --json
```

Counting distinct advisory identifiers rather than reading the summary line.
