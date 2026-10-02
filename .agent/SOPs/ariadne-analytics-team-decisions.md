# Ariadne analytics: decisions required before release

Prepared 2026-09-28 for Sam's PR #3415 review. These are proposals and release
prerequisites, not approvals or a privacy policy. No production URL is supplied.

## Production destination

Sam/Adam and the release owners must confirm the single HTTPS Ariadne ingestion
URL, ending in `/api/analytics/event`, for every network. Supply it at **build
time**: set the reviewed `ariadneAnalyticsUrl` in `perSystem/common.nix` for
pure Nix installer builds. All three platform builders export it as
`DAEDALUS_ARIADNE_ANALYTICS_URL` to the main webpack EnvironmentPlugin. Merely
setting an environment variable outside a Nix sandbox does not configure installers.
Direct webpack builds accept that environment variable at build time.
The default is empty, so packaged collection is unavailable until it is chosen.
Do not put the destination in `common.nix`'s `electron.env` or a watchdog profile.
A packaged process cannot override the compiled destination at runtime.

## Policy and agreement

The consent dialog requires a link labelled **Se7en Labs privacy policy** to an
approved, publicly accessible Se7en Labs policy covering both Ariadne analytics
and the support platform. Sam identifies Se7en Labs as the operator determining
the collection and its purposes. The team must confirm the responsible legal
entity and contact details; the old IOG policy is not a substitute.

The policy must settle event retention (the current draft says up to 24 months),
backup retention/deletion, infrastructure IP/access logs and their retention,
recipients/processors and international transfers where applicable, and a usable
rights/deletion-request contact and verification procedure. Do not promise that
withdrawing consent deletes previously received events. Verify the notice against
the actual deployment before approval. IOG and Se7en Labs must confirm the written
data handling agreement Sam requested before a signed release routes telemetry.
No agreement or policy has been created or asserted to exist by this change.

## Proposed final English consent wording

The following is ready for team/editorial review, not approved for publication.
Bracketed retention text must be replaced with the agreed factual statement.

**Help improve Daedalus with optional analytics**

If you allow analytics, Daedalus sends pseudonymous usage data to Se7en Labs,
which operates Ariadne, to help improve Daedalus. This is optional. You can use
all wallet features without allowing analytics and change your choice in Settings.

A separate random installation identifier connects your usage events across
restarts while consent remains active. The data is pseudonymous, not anonymous.
Analytics events are not linked to your support tickets. Previous permission for
Matomo does not apply.

**What is sent**

Approved page names and actions, event time and network; operating system and
numeric version where supported, CPU family, rounded RAM size and Daedalus
version; whether legacy or hardware wallets are present, and hardware/software
wallet type for supported actions. Delegation submission and voting registration
setup send start, completion or pre-submission cancellation steps with a separate
random identifier for each attempt. Completion describes the application step,
not an on-chain confirmation or a cast vote.

Analytics events do not contain wallet addresses, balances, transaction IDs,
messages, recovery phrases, spending passwords, raw routes or free-text labels.
IP addresses are not analytics event fields; network infrastructure receives your
IP address and separate access logs may record it, as described in the policy.

**Your choice**

[Insert the approved event, infrastructure-log and backup retention statement.]
Withdrawing consent stops collection, discards unsent events and clears this
installation identifier. It does not delete events already received; a request
already in flight may have arrived. Allowing analytics again creates a new
identifier. See the **Se7en Labs privacy policy** for retention, your rights and
how to contact the operator.

Buttons: **Allow analytics** / **Keep analytics off**. For an existing acceptance,
the withdrawal action should read **Turn analytics off**.

## Copy, link and consent version

Once the policy URL and facts are approved, replace the provisional English and
Japanese catalogs and source messages together, add the policy link and its
external-link callback through the container, and review Japanese wording with a
human translator. Bump the consent notice version for the final recipient/policy
notice so local draft acceptance cannot stand in for acceptance of final terms.
The current provisional UI is deliberately still a release blocker. Removing a
warning or replacing the title alone would not make the underlying notice final.
The unused external-link prop has been removed until a real link can use it.

## Matomo

Sam/Adam still need to decide replacement versus fallback. The current active
path uses Ariadne only. The retained Matomo files and legacy settings reader/key
registry are explicitly marked inactive; no fallback, migration, setting deletion
or change to existing identifiers is implemented. Delete the legacy implementation
and dependencies in a deliberate follow-up if replacement is approved.
