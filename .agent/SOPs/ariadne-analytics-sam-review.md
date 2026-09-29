# PR #3415: local response to Sam's review

Review: https://github.com/input-output-hk/daedalus/pull/3415#pullrequestreview-5338353487

Base: `a0357152585a11022ab6819759f0090e4a00ff7f`, branch
`feature/ariadne-analytics-upstream`. Reviewed the complete review body; its
review-specific comments endpoint returned no inline comments. The numbering
skips #4. Nothing here has been posted to GitHub.

| Item | Response and evidence | Remaining decision |
| --- | --- | --- |
| 1. Flight network mismatch | `normalizeEvent` maps Daedalus `mainnet-flight` to Ariadne's existing `mainnet_flight`. Other accepted wire names are unchanged; unknown names still drop. Regression checks every allowed network plus the renamed cluster. Verified against `support-ariadne/src/core/analytics/vocabulary.ts` and the cross-repository harness. | None. Do not change the wire vocabulary unilaterally. |
| 2. Signed endpoint | Added the URL to the main webpack EnvironmentPlugin with an empty default. A direct `process.env` expression is compiled into the module; packaged configuration uses only that value. Unpackaged development retains its explicit loopback exception. The webpack regression compiles the actual configuration module with the actual main plugins and executes it with a different runtime URL. Unit tests cover empty and invalid compiled destinations. Linux, Windows and Darwin installer builders now explicitly forward the shared empty-default build-time setting; no watchdog/runtime profile changes. | Team must approve the production destination before release. |
| 3. Privacy policy | No IOG policy is reused and no policy URL or agreement is invented. The separate team-decision document identifies the required Se7en Labs policy, agreement, retention and rights/contact decisions. | Release blocker: publish/approve the policy, provide its URL, confirm legal entity and agreement, then integrate the link. |
| 5. Consent API seam | `AnalyticsConsentApi` exposes get/set IPC operations. `LocalStorageApi` no longer hides Ariadne IPC; its explicitly named legacy Matomo reader only reads legacy storage. ProfileStore delegates the transition to AnalyticsConsentStore. | None. |
| 6. Shared observable | `AnalyticsConsentStore.view` is the one MobX consent snapshot. Startup reads once; writes consume their acknowledgement without a second read. Tracker reads this snapshot through its injected callback. Store tests cover observations, reordered reads, stale acceptance versus revoke, refresh after failure, retry, and teardown; the tracker reads a gated tracking view. | None. |
| 7. Component IPC | The form receives availability/saving/error props from the observer container. It no longer imports IPC or runs an effect to fetch consent. Component tests rerender availability and exercise explicit choices and failure UI. | None. |
| 8. Inactive Matomo | Marked the tracker, client, dimensions helper, renderer configuration, identity reader and key registry explicitly inactive. Existing settings and identifiers are preserved. The active entry point has no Matomo fallback. | Sam/Adam replacement-versus-fallback decision remains outstanding; deletion is not presumed approved. |
| 9. Privacy-link prop | Removed the unused required `onExternalLinkClick` prop and its container/test references. | Add it with an actual approved policy link, rather than an invented target. |
| 10. BOMs | Removed UTF-8 BOMs from both message files; message content is unchanged. | None. |
| 11. Provisional wording | Prepared proposed final English wording and required policy-link label in the team-decision document. Existing draft translations are not silently represented as approved. | Release blocker: approve factual copy, policy and Japanese translation, replace provisional UI/catalogs, advance consent version. |
| 12. Owner responsibilities | Extracted `FunnelAttempts`, including capacity, expiry, monotonic handles and flow/order validation; UUID generation is shared in a small helper. Prepare/commit keeps ownership changes after queue admission. Existing funnel regressions cover bounds, expiry, ordering and revoke. Queue and HTTP cancellation remain with the owner. | None. |
| 13. Registration singleton | Replaced module-level cleanup state with an app-owned registration factory injected into window creation/recovery. A replacement window reuses the owner/queue/consent/rate budget; old close cannot dispose the replacement. Final close/quit cancels work and removes handlers. Tests cover sender transfer, invalidation of captured old handlers, queue preservation, final disposal and cancellation. Window recovery also transfers the app reference/close wiring, ignores old window errors/close, and routes disk-space results to the replacement. | None. |

## Validation (Node 24 final review supersedes the earlier Node 22 pass)

The installer toolchain selects `pkgs.nodejs_24` in `perSystem/common.nix`;
Windows CI explicitly selects Node 24. `package.json` declares a looser Node >=22
minimum, which does not identify the actual release toolchain. This review used
Windows Node **24.21.0** and the flake-pinned Linux Node **24.15.0**.

- Full Jest: **85 suites / 1,164 passed tests / 3 skipped / 6 snapshots**, including
  stale responses, revoke/failure/retry, observed teardown, captured old IPC
  handlers, replacement-window events and current-window disk notifications.
- Windows TypeScript, full ESLint and changed-code Prettier pass. The Ariadne
  cross-repository harness passes under Node 24.21.0, with disposable data and no
  network listener or live telemetry.
- Actual Nix `checks.x86_64-linux` derivations for compile, Jest, lint,
  Cucumber unit, stylelint, Storybook and i18n consistency all pass under Node
  24.15.0 on an isolated LF source export. The protected checkout whitelist files
  were not written by the translation generator.
- The literal Nix format gate was run on exact-byte source exports: HEAD flags
  **1,724 files**, the reviewed source flags **1,677**, with **no newly failing
  paths**. The gate still fails; the composite release gate is not claimed green.
  No line-ending cleanup was applied to the checkout. Nix formatter mutations
  occurred only in its disposable check copies; installer/check exports were
  freshly extracted separately, then normalized to LF to avoid the existing
  CRLF shell-fragment failure.

## Actual installer endpoint proof

Pure Nix derivation evaluation confirms the staging URL in the build environment
for Linux, Windows, Intel Darwin and ARM Darwin. All use the same
`common.ariadneAnalyticsUrl`, passed to webpack by their actual installer builders.
The proposed committed value and webpack fallback remain **empty**.

The staging test used `https://daedalus-staging.support.se7enlabs.com/api/analytics/event`,
combining the staging origin documented in Ariadne's `docs/hosting-strategy.md`
with its documented ingestion route. This was set only in a disposable source
export; no production destination was chosen and no request was sent.

The actual Windows `daedalusJs.preprod` derivation (the installer dependency) ran
its existing `yarn package` build phase unchanged with Node 24.15.0. Both webpack
bundles compiled. The next packaging step failed at `rcedit-x64.exe` with WSL's
`UtilGetPpid /proc/1/stat` error, the same signature recorded in prior baseline
work. A complete installer is therefore **not** claimed.

From that derivation's actual full main bundle, the two configuration definitions
were extracted with the TypeScript parser and evaluated in an isolated VM with
`packaged=true`. A different HTTPS runtime URL, an empty runtime URL, and loopback
HTTP each retained the compiled staging destination. Turning off the enabled
switch still returned null. A Node 24 Windows full main build independently
passes the same proof; an empty-default full build stays disabled even with a
runtime URL. The Electron entry point was never executed.

The Nix staging main bundle SHA-256 is
`ab1806e25a701c6c7999317394a2fe9c7febbc59a02162b0835d8e40886972a7`.
This bundle predates only the final teardown snapshot invalidation and its tests;
those final edits are covered by the passing Nix checks and Windows main build.
Its endpoint configuration and installer inputs are identical to the proposal.

Logs, derivation JSON, exact bundle hashes and runtime cases are under
`.git/ariadne-sam-review/final-audit`. The final proposed patch and file manifest
are under `.git/ariadne-sam-review/proposal`; these are private review artifacts.

## Remaining checks and decisions

1. Build a complete installer on a supported native Linux builder; validate
   packaged UI behavior and the signed release artifact. Darwin was evaluated
   for endpoint forwarding, not built on a Mac here.
2. Operator-run consent/restart/revocation/recovery acceptance remains outstanding;
   neither application was started. No real wallet, hardware or live telemetry
   test was performed.
3. The repository-wide formatting gate still has baseline failures. Leave its
   line endings alone for this review; this patch introduces no newly failing path.
4. Approve the production URL, Se7en Labs policy and actual policy link, IOG/Se7en
   agreement, retention/deletion details, final English/Japanese wording and
   consent-version bump. The provisional UI is still a release blocker.
5. Sam/Adam must decide Matomo replacement versus fallback; the retained inactive
   code is explicitly marked and never used as an Ariadne fallback.

## Preserved state

The two translation whitelist files are checked against SHA-256 hashes captured
before editing. Their bytes and pre-existing Git state are retained. No settings,
credentials, environment files, watchdog configuration, wallet data or test
profiles were edited. No staging, commit, push, GitHub reply/thread action, merge
or deployment was performed.
