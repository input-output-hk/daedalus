## Task ID and Title

`task-030` — Asset metadata source presets, types and URL validator, wired from
launcher config.

## Why Chosen Now

It is the only phase 7 task with no dependencies, and three later ones need what
it declares. `task-031` maps a stored URL back to a preset, `task-032` renders
the presets in a `Select`, and `task-033` composes its request URLs from the
selected one. Nothing in phases 1 to 6 reads a Koios URL, so the value has no
path from the launcher to the renderer yet.

## Interaction Mode

`agent_execution`.

## Scope

The asset equivalent of the SMASH configuration block: a source type union, a
preset list, a type map and a URL validator, with the default URL carried from
the launcher configuration to `global` the way `smashUrl` already is.

## Non-Goals

- No probe, no persistence and no settings surface. Those are `task-031` and
  `task-032`, and a preset list is what both of them read.
- No client. `task-033` composes request URLs; this task declares the base.
- **No second configuration surface.** The launcher value is the default and a
  stored user value overrides it, which is the relation `metadataUrl` already
  has at `source/main/config.ts:71`.
- Not the `@ts-ignore` at `source/renderer/app/config/stakingConfig.ts:8`. It is
  there because `smashUrl` is missing from the global declarations, which is the
  same gap this task closes for `koiosUrl`. Recorded below rather than fixed
  here.

## Dependencies

None.

## Research Consulted

- `asset-metadata-cache-prd.md:821-861`, the metadata source setting: the preset
  list, the type map and the validator, with the one respect in which the
  validator differs from the SMASH one.
- `asset-metadata-cache-prd.md:229`, locked decision 11: three options, and
  `direct` carried in the enum from the start and rendered unavailable.
- `research/01-koios-pointer-option.md`, for the instance URLs and the `/api/v1`
  path prefix.

## Docs, Workflows, and Skills Consulted

- `.agent/system/architecture.md` for where a renderer config module sits.
- `nix fmt` is treefmt over prettier and alejandra, and
  `nix/internal/launcher-config.nix` is inside it.

## Live Repo Findings Verified For Planning

1. **The block being copied is three declarations, not one.**
   `SMASH_SERVERS_LIST` at `source/renderer/app/config/stakingConfig.ts:12-28`,
   `SMASH_SERVER_TYPES` at `:29-33` and `SMASH_URL_VALIDATOR` at `:43-45`. The
   PRD's line references are still accurate.
2. **`SMASH_SERVERS_LIST` holds two entries and the type map holds three.**
   `iohk` and `direct` are in the list; `custom` is in the map alone, because a
   custom entry has no fixed URL. The asset copy has the same shape.
3. **`assetsConfig.ts` is two lines today.** `MAX_DECIMAL_PRECISION = 20` and
   `DEFAULT_DECIMAL_PRECISION = 0`, both read across the renderer.
4. **There is no `source/renderer/app/types/assetTypes.ts`.** The types
   directory holds nineteen files and none of them is about assets;
   `SmashServerType` lives at `source/renderer/app/types/stakingTypes.ts:2`. The
   file is created.
5. **`smashUrl` reaches the renderer in four steps and one of them is a
   suppression.** `nix/internal/launcher-config.nix:456` assigns it,
   `source/main/config.ts:70` types it, `:121` re-exports it,
   `source/main/preload.ts:8,40` puts it on `global`, and
   `source/renderer/app/config/stakingConfig.ts:8-9` reads it behind an
   `@ts-ignore`, because `declaration.d.ts:52-66` declares `isFlight`,
   `legacyStateDir`, `environment`, `http`, `https`, `ipcRenderer` and
   `daedalus` on `global` and does not declare `smashUrl`. **Divergence
   recorded:** the suppression exists only because the declaration was never
   added. `koiosUrl` is declared instead, so this task adds no suppression.
6. **`smashServers` covers three networks and the assignment is conditional.**
   `nix/internal/launcher-config.nix:31-35` is `mainnet`, `preprod` and
   `preview`, and `:455-457` assigns `smashUrl` only under
   `__hasAttr network smashServers`. `installer-clusters.cfg` ships `mainnet
   preprod preview selfnode`, and `isFlight` is `network == "mainnet_flight"`
   (`:269`), which is a fifth network the launcher configures and which
   `smashServers` does not cover. So on Flight, and on selfnode, `smashUrl` is
   absent and `SMASH_SERVERS_LIST.iohk.url` is `undefined` today.
7. **`tokenMetadataServers` is assigned differently.** It is read at `:76`
   inside `fromCardanoPlayground` into `envCfg.metadataUrl` and assigned at
   `:449` under `network != "selfnode"`, so it follows the playground
   environments rather than a hard-coded network list. Koios follows the
   `smashServers` shape rather than this one, because there is no Koios instance
   for a playground environment that Koios does not run.
8. **Koios runs three public instances that match three of those networks.**
   `https://api.koios.rest/api/v1`, `https://preprod.koios.rest/api/v1` and
   `https://preview.koios.rest/api/v1`. Confirmed live on 2026-09-14:
   `GET https://preprod.koios.rest/api/v1/tip` answers `200`, and a
   `POST .../tx_cbor` for a preprod transaction answers with `block_hash`,
   `absolute_slot` and the transaction CBOR. There is no instance for
   `shelley_qa`, `vasil_dev` or a selfnode cluster, and there cannot be one for
   selfnode, whose chain exists only on the user's machine.
9. **The validator's one difference is load-bearing and testable.**
   `SMASH_URL_VALIDATOR` is
   `^(direct|https://[a-zA-Z0-9-_~.]+(:[0-9]+)?/?)$`, which rejects
   `https://api.koios.rest/api/v1` because of the path.
10. **Jest runs under jsdom with `globals: { environment: { network: {} } }`**
    (`jest.config.js`), so `global.koiosUrl` is `undefined` in a spec unless the
    spec sets it before the module is loaded. `stakingConfig.ts` has the same
    property and no spec, so there is no precedent to copy; the spec sets the
    global and loads the module through `jest.isolateModules`.

## Files Expected To Change

- `source/renderer/app/types/assetTypes.ts` (new)
- `source/renderer/app/config/assetsConfig.ts`
- `source/renderer/app/config/assetsConfig.spec.ts` (new)
- `declaration.d.ts`
- `nix/internal/launcher-config.nix`
- `source/main/config.ts`
- `source/main/preload.ts`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

**One type, three declarations.** `AssetMetadataSourceType` is
`'koios' | 'custom' | 'direct'`. It has no `none` member, which
`SmashServerType` does: `none` exists there for a wallet with no SMASH server
configured, and there is no server-side counterpart here to be unconfigured.

`ASSET_METADATA_SERVERS_LIST` holds `koios` and `direct`.
`ASSET_METADATA_SOURCE_TYPES` holds all three. `ASSET_METADATA_URL_VALIDATOR`
takes the PRD's pattern verbatim: the SMASH pattern with
`(/[a-zA-Z0-9-_~.]+)*` inserted before the optional trailing slash.

**`koiosUrl` follows `smashUrl` and closes one gap on the way.** A `koiosServers`
attrset beside `smashServers`, assigned under `__hasAttr network koiosServers`
beside `smashUrl`, typed at `source/main/config.ts:70`, re-exported at `:121`
and put on `global` at `source/main/preload.ts:40`. It is also **declared** in
`declaration.d.ts`, as `string | undefined`, so the renderer reads it without a
suppression. The `undefined` is not defensive: it is the real type on a network
with no instance.

`koiosServers` covers `mainnet`, `mainnet_flight`, `preprod` and `preview`.
`mainnet_flight` is one more network than `smashServers` covers, deliberately:
Flight is a mainnet client, `clustersAvailable` maps `mainnet_flight` to the
mainnet environment at `:24`, and omitting it would leave the chain channel dead
on Flight for no reason other than matching a gap. The equivalent gap in
`smashServers` is a finding, not this task's to fix.

**The spec drives the validator and the shape of the list.** The four cases the
task graph names, plus the negative the graph does not: a URL whose host carries
a character the class excludes.

## Acceptance Criteria

1. `ASSET_METADATA_URL_VALIDATOR` accepts the Koios default including
   `/api/v1`, accepts a custom instance with a port and with a path, accepts the
   literal `direct`, and rejects an `http://` URL and a URL carrying a query
   string.
2. `ASSET_METADATA_SERVERS_LIST` holds `koios` and `direct` and no `custom`
   entry; `ASSET_METADATA_SOURCE_TYPES` holds all three.
3. `koiosUrl` is present on `global` for `mainnet`, `mainnet_flight`, `preprod`
   and `preview`, and absent for `selfnode`, which has no instance and can have
   none. The reason the set is not "every network" is recorded here rather than
   the criterion being read as met.
4. `nix fmt` leaves `nix/internal/launcher-config.nix` unchanged.
5. `compile`, `lint`, `stylelint`, `jest`, `i18n` and `cucumber-unit` pass from
   `nix build`.
6. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- `assetsConfig.spec.ts` drives criterion 1 as one case per input class, with
  the accepted and the rejected forms in separate cases so a pattern that
  accepted everything would fail rather than pass half.
- Criterion 2 is asserted on the exported objects: the key sets, and that
  `custom` is absent from the list. A preset list that grew a `custom` entry
  would break the mapping in `task-031`, which falls back to `CUSTOM` for
  anything unmatched.
- Criterion 3 by evaluating the launcher configuration for each network rather
  than by reading the expression:
  `nix eval --impure --expr` over `internal.x86_64-linux.launcherConfigs.<network>`
  is not exposed as a flake output, so the check is
  `nix build .#daedalus` per network, which is minutes each. Instead the
  assignment is read at its site and the network list is compared against
  `clustersAvailable` and `installer-clusters.cfg`, and `nix fmt` is what
  confirms the file still parses. Stated as the weaker check it is.
- Criterion 4 by running `nix fmt` and then `git diff --exit-code` over the nix
  file.
- All six Nix checks, `stylelint` and `i18n` included even though neither a
  stylesheet nor a message changes, because they are cheap and the branch's
  convention is to run the set.

## Risks and Open Questions

- **A preset URL that goes stale.** Koios instance hostnames are a third party's
  to change, and a wrong default is a channel that silently answers nothing. The
  probe in `task-031` is what turns that into a visible refusal for a URL the
  user typed; for the default it would take a release. Named, not mitigated.
- **`mainnet_flight` diverges from the SMASH precedent by one line.** The
  divergence is deliberate and argued above. If a reviewer prefers exact
  symmetry, removing the line is the whole change.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-030.targetPaths` widened by
  `declaration.d.ts` and the new spec; `task-030.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-030-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-030-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

The renderer can name the three sources and knows the default URL for the
network it is running on.

## Final Outcome

Complete.

## Self-Review

The temptation in a structural copy is to copy the suppression with the
structure. `stakingConfig.ts` reads its launcher value behind an `@ts-ignore`
only because nobody added the global declaration, and reproducing that would add
a suppression this branch has spent six phases not adding. Declaring `koiosUrl`
costs one line and makes the absence on a network with no instance a type rather
than a surprise.
