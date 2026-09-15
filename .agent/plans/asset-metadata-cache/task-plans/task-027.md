## Task ID and Title

`task-027` — Manual QA across the three platforms.

## Why Chosen Now

Everything phases 1 to 5 built is in place and every automated check is green.
What no automated check reaches is the process boundary, the on-disk path, and
what a person sees on the first launch after an update. CI runs the suites on
Linux and on aarch64-darwin and the static checks on `x86_64-linux` alone, so no
job executes on Windows at all.

## Interaction Mode

`manual_execution`.

This environment is a WSL2 machine with no display, no macOS, no Windows, no
mainnet wallet and no funded selfnode. Nothing below can be run here, and the
deliverable of this task is therefore the procedure, the evidence each step must
produce, and a checklist an operator signs. **No step below has been executed.**

## Scope

Nine scenarios, on Linux, macOS and Windows, plus one selfnode scenario that
needs no second platform. The evidence each one produces, stated so that a person
who did not write this plan can tell a pass from a fail.

## Non-Goals

- No screenshots as the primary evidence. A screenshot proves a render; several
  of these scenarios are about what does **not** happen, and the evidence for
  those is a file on disk or a log line.
- No new automation. The e2e suite cannot execute: `spectron@14` resolves
  `electron-chromedriver@12` against Electron 41.3.0, and Cucumber is not in the
  check set.
- No change to any source file. If a scenario fails, it opens a defect; it does
  not get fixed inside this task.

## Dependencies

`task-018`, `task-023`, `task-024`, `task-025`.

## Research Consulted

- `asset-metadata-cache-prd.md:1482-1497`, the Manual QA list, which scenarios 1
  to 6 and 9 reproduce.
- `asset-metadata-cache-prd.md:1514-1517`, platform verification: the database
  path differs per platform and no CI job runs on Windows.

## Docs, Workflows, and Skills Consulted

- `README.md:165-184`, the mock token metadata server and how a developer run is
  pointed at it.

## Live Repo Findings Verified For Planning

1. **The database path is built from one export and one constant.**
   `assetMetadataDirectoryPath()` is
   `path.join(stateDirectoryPath, 'asset-metadata-cache')`
   (`source/main/assets/assetMetadataDb.ts:192-196`), and `stateDirectoryPath` is
   `launcherConfig.stateDir` (`source/main/config.ts:115-125`).
2. **The launcher sets `stateDir` per platform** at
   `nix/internal/launcher-config.nix:193-199`:
   `${XDG_DATA_HOME}/Daedalus/<network>` on Linux,
   `${HOME}/Library/Application Support/<spacedName>` on macOS, and
   `%APPDATA%\<spacedName>` on Windows. So the three paths to confirm are:

   | Platform | Expected |
   |---|---|
   | Linux | `~/.local/share/Daedalus/<network>/asset-metadata-cache/assets.sqlite` |
   | macOS | `~/Library/Application Support/<spacedName>/asset-metadata-cache/assets.sqlite` |
   | Windows | `%APPDATA%\<spacedName>\asset-metadata-cache\assets.sqlite` |

   With `assets.sqlite-wal` and `assets.sqlite-shm` beside it while the
   application is running, because the database opens in WAL mode.
3. **The log the evidence is read from is at `<stateDir>/Logs/pub/Daedalus.json`
   on Linux and macOS and `Logs\pub\Daedalus.json` on Windows**
   (`nix/internal/launcher-config.nix:210-213` and `:244`), and the file
   transport logs at `debug` (`source/main/utils/setupLogging.ts:30`), so every
   line these modules emit reaches it.
4. **The selfnode endpoint is chosen in code, not in configuration.**
   `nix/internal/launcher-config.nix:448-450` adds `metadataUrl` only when the
   network is not selfnode, so on selfnode `launcherConfig.metadataUrl` is
   absent. `assetRegistryClient.ts:97-107` therefore falls through to
   `${MOCK_TOKEN_METADATA_SERVER_URL}:${MOCK_TOKEN_METADATA_SERVER_PORT}` when
   `environment.isSelfnode`, and only then to the mainnet literal. If that branch
   were removed or reordered, a selfnode run would query the mainnet registry,
   and **the failure is silent**: mainnet answers a selfnode query with a
   perfectly well-formed response. Nothing else in this plan catches it.
5. **The port comes from the environment, defaulting to 0.**
   `source/main/config.ts:170-172` is
   `MOCK_TOKEN_METADATA_SERVER_PORT = process.env.MOCK_TOKEN_METADATA_SERVER_PORT || 0`,
   and `README.md:170-177` is the procedure for starting the server and passing
   the port.
6. **The mock registry knows one subject and it is editable.**
   `utils/cardano/native-tokens/registry.json` carries
   `789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1` as NiceCoin, ticker
   NCN. It has no `policy` field, so its decimals cannot verify and must not be
   applied. Its `logo` value decodes to the ASCII text "Almost a logo", which is
   not a raster image, so the image store refuses it on media type and no logo
   appears: that is the correct behaviour, not a defect.
7. **The cache directory is its own directory** precisely so that deleting it is
   the whole reset procedure, which is what scenario 6 exercises.

## Files Expected To Change

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

No source file changes. `source/main/assets/assetMetadataDb.ts` is the task
graph's target path because it is the module whose behaviour is under test, not
because it changes.

## Implementation Approach

The procedure below is the deliverable. An operator runs it, fills in the
checklist, and attaches the artifacts each step names.

### Preparation, once per platform

- A release build, not a development run, except where a step says otherwise.
  `yarn package` produces the installer; the scenarios are about what a user
  gets.
- A wallet holding at least four tokens: one whose issuer published decimal
  places bound to the minting policy, one whose issuer published decimal places
  with no `policy` field, one the registry has never heard of, and one whose
  issuer published a logo.
- On this development machine only, Electron needs
  `ELECTRON_DISABLE_SANDBOX=true`, exported in the shell profile rather than in
  `.envrc`, which is tracked.

### Scenario 1 — First run, empty cache, offline

Delete the `asset-metadata-cache` directory. Disconnect the network. Start
Daedalus and open the token list, the wallet summary, the send form and the
transaction list.

**Expected:** every held token has a row, immediately. Each row shows its
fingerprint and its quantity in whole ledger units. No spinner appears on any of
the four surfaces at any point. No error dialog. The list is complete rather than
shorter than the wallet's holdings.

**Evidence:** a screenshot of the token list showing a row per holding, and the
log filtered for `Asset metadata: query failed`, which should appear, because
being offline is a state rather than an error.

### Scenario 2 — The same wallet, online

Reconnect the network without restarting Daedalus. Stay on the token list.

**Expected:** tickers and formatted amounts appear in place. The list does not
blank, remount or reorder wholesale, and no row disappears while it resolves.

**Evidence:** a screenshot before and after, taken without navigating away.

### Scenario 3 — Three tokens side by side

With the cache warm, look at the verified token, the unverified token and the
unknown token in the token list and then in the send form.

**Expected:**

- The verified token shows its amount with the issuer's decimal places applied.
- The unverified token shows its amount as whole ledger units. Opening its
  settings dialog shows the sentence naming the published figure and saying it
  could not be checked against the token's minting policy.
- The unknown token shows its fingerprint and whole ledger units, and its
  settings dialog shows no such sentence, because nothing was published.

**Evidence:** a screenshot of the three rows together and one of the settings
dialog for the unverified token.

### Scenario 4 — The send form refuses a decimal it cannot interpret

Open the send form, add the token whose decimal places are unresolved, and try to
enter `1.5`, first by typing and then by pasting. Repeat with a comma as the
separator and with a grouped value such as `1,500,000`.

**Expected:** the field holds `1` after typing `1.5`, and rejects the paste
outright. The label under the field says the amount is entered as a whole number
of the token's units and that its decimal places are unknown.

**Evidence:** a screenshot of the field and its label after each attempt. This is
one of the two send-path safety rules; a failure here is a defect of the highest
severity in this plan and stops the release.

### Scenario 5 — The migration notice

Install the previous release, create or restore a wallet holding tokens, accept
the terms of use, and close. Install this build over it. Start Daedalus and open
the token list.

**Expected:** the notice appears once, above the list, and says that amounts for
tokens with verified decimal places are now entered in those units, with the
example. Dismiss it. Navigate away and back: it does not return. Restart
Daedalus: it does not return.

Then, on a profile created **after** installing this build, open the token list:
the notice never appears, because a profile created now has no habit to correct.

**Evidence:** a screenshot of the notice, a screenshot of the same screen after a
restart, and a note of which profile each was taken in.

### Scenario 6 — The cache directory deleted underneath a running application

With Daedalus running and the token list open, delete the whole
`asset-metadata-cache` directory. Navigate away from the token list and back.

**Expected:** every row still renders, with fingerprints and quantities. No
crash, no error dialog. Tickers may be gone until they are fetched again, and
they come back.

**Evidence:** the log filtered for
`Asset metadata cache: unavailable, answering as empty` or
`Asset metadata cache: recreating the database`, plus a screenshot of the list
after the deletion, plus a directory listing showing the directory has been
recreated.

### Scenario 7 — The database path

On each platform, with Daedalus running, list the directory from finding 2.

**Expected:** `assets.sqlite` exists at the documented path, with
`assets.sqlite-wal` and `assets.sqlite-shm` beside it while the application is
running. Windows is the one no contributor exercises daily and is the one most
likely to disagree.

**Evidence:** the full path as printed by the platform's own directory listing,
pasted into the checklist. Not a screenshot of a file manager: the path is the
artifact.

### Scenario 8 — Selfnode reaches the bundled mock and not mainnet

This scenario exists because nothing else in this plan or in the suite catches
the failure it looks for, and the failure is silent.

1. In `nix develop`, add the selfnode wallet's own token to
   `utils/cardano/native-tokens/registry.json` with a ticker no real issuer would
   publish, for example `ZZMOCK`.
2. Start the mock server:
   `mock-token-metadata-server --port 65432 ./utils/cardano/native-tokens/registry.json`
3. Start Daedalus against selfnode with the port passed through:
   `MOCK_TOKEN_METADATA_SERVER_PORT=65432 yarn nix:selfnode` and then
   `yarn dev` in that shell, or the equivalent for the operator's setup.
4. Hold the token and open the token list.

**Expected:** the row shows `ZZMOCK`. Its amount stays in whole ledger units,
because the mock's entry carries no `policy` field and an unverified decimal
count is never applied. No logo appears for it: the mock's `logo` value is ASCII
text rather than raster bytes and is refused on media type.

Then stop the mock server, delete the cache directory, and restart.

**Expected:** the row shows its fingerprint and no ticker. **If it shows a ticker
with the mock server stopped, the fetcher reached the public registry and the
selfnode branch is broken.** That is the failure this scenario exists for.

**Evidence:** two screenshots of the same row, one with the mock running and one
without, and the log filtered for `Asset registry: request refused` in the second
case.

### Scenario 9 — The logo

With a wallet holding a token whose issuer published a PNG logo, open the token
list and scroll it.

**Expected:** the logo appears in the row header at a fixed size. A row whose
issuer published no logo looks exactly as it did before this change, with no
placeholder and no broken-image mark. Scrolling the list does not make the logo
flicker or reload.

**Evidence:** a screenshot of a list containing both kinds of row.

## Acceptance Criteria

1. Every scenario passes on Linux, macOS and Windows, except scenario 8, which is
   run once on any platform.
2. The database appears at the documented path on each platform, recorded as the
   path itself rather than as a screenshot.
3. The checklist below is completed and signed, with the build identifier and the
   date, and the evidence each step names attached.
4. Any failure is opened as a defect with its scenario number, before the
   checklist is signed.

## Verification Plan

The checklist is the verification. It is filled in by the operator and is what
makes the claim checkable afterwards by someone who was not there.

```
Asset metadata cache — manual QA
Build:            ____________________   Date: ____________
Operator:         ____________________

                                    Linux    macOS    Windows
1  Cold cache, offline               [  ]     [  ]     [  ]
2  Resolving in place, online        [  ]     [  ]     [  ]
3  Verified / unverified / unknown   [  ]     [  ]     [  ]
4  Send form refuses a decimal       [  ]     [  ]     [  ]
5  Migration notice, once            [  ]     [  ]     [  ]
6  Cache directory deleted           [  ]     [  ]     [  ]
7  Database path                     [  ]     [  ]     [  ]
9  Logo in the row header            [  ]     [  ]     [  ]

8  Selfnode reaches the mock         [  ]  (once, platform: __________ )

Database path observed
  Linux    ______________________________________________
  macOS    ______________________________________________
  Windows  ______________________________________________

Defects opened (scenario number and issue reference)
  ____________________________________________________________

Signed: ____________________
```

## Risks and Open Questions

- **This task cannot be completed by an agent and must not be recorded as
  passing.** Its status in the task graph is `blocked`, with the reason stated,
  until an operator returns a signed checklist.
- **Scenario 5 needs the previous release**, which means an operator who can
  install two builds in sequence on each platform. It is the longest step and the
  one most likely to be skipped; it is also the only check of a notice that can
  only ever be shown once per profile.
- **Scenario 8 needs a selfnode cluster and a funded wallet**, which is a
  developer setup rather than a QA one. It can be run by whoever is running
  selfnode for another reason, and it takes a few minutes once the cluster is up.
- Nothing here needs a decision from the project owner beyond assigning an
  operator.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-027.status` to `blocked` with the
  reason, and its `implementationNotes` extended with the selfnode scenario and
  the evidence each step produces.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-027-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-027-impl-review.md`

## Planning Status

approved

## Build Status

blocked — the procedure is complete and no step has been executed. An operator
with Linux, macOS and Windows returns the signed checklist.

## Current Outcome

A procedure a person can follow, with a pass condition for each step that does
not depend on having written it.

## Final Outcome

Pending an operator.

## Self-Review

The temptation in a task like this is to run what can be run here, report those
steps as passing, and leave the rest implied. None of it can be run here: there
is no display, no second platform, and no wallet. Recording any of it as done
would put a claim into the record that nobody checked, which is worse than an
empty checklist. The scenario worth defending is the eighth: it is not on the
PRD's list, it catches a failure that produces a plausible-looking result, and
nothing else in this plan would notice.
