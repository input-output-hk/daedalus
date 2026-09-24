# Ariadne analytics: local implementation and coordinated rollout

Base: official upstream `https://github.com/input-output-hk/daedalus.git`, commit
`6c57eb94753211f66d3a63f49d031bf746044755` (11.4.0). Local origin remains
`https://github.com/Liberty-Chris/daedalus.git`; its master is preserved. The local
branch is `feature/ariadne-analytics-upstream`. The event vocabulary was rechecked
against the earlier integration-plan revision; its v1 literals are unchanged.

This replaces the active Matomo tracker at the existing AnalyticsTracker boundary.
Old Matomo settings/UUID and implementation files remain untouched and are not
used by the new tracker. The feature is disabled by default, with no built-in
endpoint, new dependency or wallet-data migration. No ticket identity linking or release anomaly claims are implemented. v2
instruments delegation submission and voting registration setup as explicit
attempts; neither means on-chain success or a cast ballot. See the coordinated
Ariadne `docs/analytics-measurements.md` for exact cohorts and deduplication.

**Unresolved for Sam and Adam:** Matomo replacement versus fallback is not
approved. The current branch selects Ariadne with no Matomo fallback, including
when disabled/rejected/unavailable; legacy Matomo code and settings remain.

## Configuration and consent

The main process reads these explicit launcher environment values:

| Variable | Default / rule |
| --- | --- |
| `DAEDALUS_ARIADNE_ANALYTICS_ENABLED` | Off; only exact `true` opts in |
| `DAEDALUS_ARIADNE_ANALYTICS_URL` | Unset; exact `/api/analytics/event` path, no credentials, query or fragment |
| `DAEDALUS_ARIADNE_ALLOW_LOOPBACK_HTTP` | Off; exact `true` allows HTTP only to `localhost`, `127.0.0.1` or `[::1]`, only with `NODE_ENV=development` and `app.isPackaged === false` |

Packaged applications and production configurations require HTTPS even if the
loopback exception is set. Invalid configuration fails closed. Renderer messages
cannot supply a URL, UUID, network or device fingerprint. Changing the configured
recipient requires fresh consent. Release/launcher owners must choose and review
the recipient; this patch enables no release endpoint.

Consent notice version 2 is explicitly **provisional for local review**. It
describes pseudonymous data, purpose, fields, retention and revocation. Final
recipient/privacy wording, backup retention, an authenticated deletion-request
procedure and Japanese translation require review before rollout. English and actual Japanese translations are provisional human-review drafts,
not approved copy; see `ariadne-analytics-consent.md`. Version 2 requires fresh
acceptance even after a local version-1 acceptance because attempt IDs are new.
The old Matomo privacy link is not presented as covering Ariadne.

The main process owns a separate `electron-store` file named
`ariadne-analytics.json` under Electron's existing userData directory, keyed by
network. It stores consent version, recipient, decision and a separate random v4
UUID only when accepted. It is outside the generic renderer settings IPC and is
never included in telemetry logs. No wallet data is read or changed by this store.
Old/unrecognized consent versions, unreadable storage and pending/rejected consent
send nothing. Acceptance is persisted before sending; accepted restarts reuse the
UUID. Revoke clears it and the unsent queue immediately; later acceptance creates
a new UUID (installation counts can therefore increase after reconsent).

Revocation stops collection; it **does not delete already received events**. An
in-flight event may already be committed at the server. A failed settings write
stops collection for the session and shows a save error; retry before restarting,
since failed persistence cannot promise that an earlier on-disk choice changed.

## Process boundary and normalization

Only the registered wallet window's main frame at the exact expected document
URL may invoke the new IPC handlers (hash navigation is allowed). Payloads reject
unknown fields, invalid generations/times/actions/labels, arbitrary routes/text
and renderer-controlled identities/destinations. These checks follow
[Electron's sender-validation guidance](https://www.electronjs.org/docs/latest/tutorial/security#17-validate-the-sender-of-all-ipc-messages).
They use the current application trust boundary; they do not introduce a new
Electron sandbox or change unrelated IPC channels.

Renderer normalization maps existing enum display names to v1 category keys and
keeps exact allowlisted page/action literals. Dynamic labels and numeric values
are discarded before IPC. Only four wallet actions may carry the exact
`Hardware wallet` or `Software wallet` label. Both event types have full dimensions.
Wallet flags are synchronously derived from the already-loaded wallet-store
snapshot at occurrence; no extra wallet API request/cache is added. Unknown,
loading or failed snapshots drop the event. No wallet records enter IPC.

The sender adds trusted network/package version/device dimensions: Windows,
Linux or macOS; numeric OS version or null; allowlisted CPU family or Other; RAM
rounded upward to integer GiB (1–2048); exact three-part numeric application
version. Unsupported prerelease versions drop events rather than masquerading as
stable releases. The UUID is random, never derived from wallet/device/support data.
Event time is captured at occurrence; events older than 30 seconds or in the future
are dropped. The cross-repository test checks vocabulary equality with Ariadne.

## Bounded delivery and shutdown

- Renderer has one ordinary event IPC request and drops ordinary events while busy.
  A separate ordered lane holds at most eight funnel steps, bound to consent
  generation; the sender owns eight active attempts expiring after 30 minutes.
  Main owns at most 32 waiting events / 64 KiB, 2 KiB per payload, plus one active
  POST. Drop newest on overload. Queue lifetime is 30 seconds. No disk spool/replay.
- Main independently requires enabled configuration, persisted consent and the
  current consent generation before admission and again before dispatch. Revoke
  invalidates renderer enable work and sender generations, clears queue/timers and
  aborts the active request. Window close/app quit cancels all work.
- At most 30 dispatches per rolling minute, including failed attempts. Consent
  toggles do not reset this budget. No retry of any attempted event, including an
  ambiguous disconnect/timeout. 429 discards the item and delays future dispatches
  by integer Retry-After seconds clamped to 1–60; missing/date/text values use 1.
  Other failures use capped 1–60 second backoff for future events only.
- Node HTTP(S) sends a JSON POST without cookies/authentication or redirects. A
  five-second total timer destroys the request; response headers are bounded to
  8 KiB and response bodies are destroyed without reading/logging. This explicitly
  aborts rather than relying on [Node's non-aborting timeout event](https://nodejs.org/api/http.html).
- Telemetry is never awaited by wallet actions and never changes their result.
  There are no payload/URL/UUID/raw-provider-error logs. Memory admission is not
  durable delivery; restart loses unsent events. 204 completes an attempt, not a
  receipt: Ariadne also uses it for disabled/malformed drops. No exactly-once claim.

## Local validation and manual testing

Automated tests use synthetic events and mocked HTTP only:

```text
corepack yarn test:jest source/main/analytics source/main/ipc/ariadneAnalytics.test.ts --runInBand --coverage=false
```

From the Ariadne checkout, with neither development server required:

```powershell
npx.cmd tsx scripts/verify-daedalus-analytics.ts --daedalus-dir ../daedalus
```

That harness uses the actual client tracker/owner/normalizer, an in-process mocked
transport to Ariadne's real POST handler, temporary DuckDB, database close/reopen
and the authorized dashboard handler. It verifies totals, daily/weekly
installations, hardware use, distributions, no UUID disclosure and no sends for
pending/rejected/revoked consent. It deletes its own temporary database, never
loads `.env.local` or touches configured support data. It is not a running-wallet,
real HTTP, Electron UI or packaged installer test.

For operator-controlled manual testing, follow the prerequisites and isolated
profile walkthrough in [ariadne-analytics-review.md](ariadne-analytics-review.md).
At upstream 6c57eb9 the watchdog launches Electron and supplies backend IPC and
TLS paths. Plain `yarn dev`, the older `dev:windows` launcher-config shortcut,
or setting only the analytics variables does not provide that environment.
Node 24, Electron 41.3.0, Nix-built HID/USB and matching Windows watchdog/node/
wallet binaries are now prepared. Native modules load in Electron without
launching the UI. Interactive wallet testing remains for the operator.

See [ariadne-analytics-validation.md](ariadne-analytics-validation.md) for current
check results and exact remaining baseline/environment blockers. That record
supersedes the earlier Node 22 / missing-Nix/native-tool report.

Validation limitations are also recorded in the coordinated Ariadne integration plan.
Issue #9 remains open. Ariadne ingestion/dashboard stay in one eventual PR to
`staging`; this Daedalus branch requires its own coordinated PR and local approval.
