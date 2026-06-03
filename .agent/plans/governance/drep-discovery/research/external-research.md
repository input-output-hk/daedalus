# DRep Discovery — External Research

**Purpose:** Catalog how the ecosystem presents DRep directories today and extract interaction patterns Daedalus should adopt (or reject), without using any of these surfaces as a data source. Daedalus remains local-first.

## Sources Reviewed

| Source | URL | Notes |
|---|---|---|
| GovTool DRep Directory | Ecosystem DRep directory | Live, canonical reference UX. Patterns authoritative. |
| 1694.io | https://www.1694.io/ | CIP-1694 explainer + voting-power calculator. Not a directory, but useful tone/voice. |
| AdaStat Governance | https://adastat.net/governance | Returned HTTP 404 at fetch time; superseded by 1694.io for ecosystem framing. |
| CIP-1694 | https://cips.cardano.org/cip/CIP-1694 | DRep model, active/inactive, anchor structure, ratification mechanics. |
| CIP-119 | https://cips.cardano.org/cip/CIP-0119 | DRep metadata vocabulary (`givenName`, `image`, `objectives`, `motivations`, `qualifications`, `references` with `Link`/`Identity` types, `doNotList`, `paymentAddress`). |

## GovTool DRep Directory — What It Does

Observed page elements:

- Top action: **Find a DRep** search input.
- Top right: **Filter** (single facet drawer with a "1" badge for active filters) and **Sort** (default `Sort by: Voting Activity`).
- Cards show: avatar (default placeholder if no anchor image), `givenName`, CIP-129 DRep ID (newer 56-char form) AND CIP-105 legacy `drep1…` ID stacked, **Voting power** in `₳`, **Status: Active/Inactive** badge, **View details** + **Connect to delegate** CTAs.
- Pagination footer: `Rows per page: 5 · 1–5 of 275 · pages 1…`.
- "Default DRep" icon is reused for any DRep without an image — no broken-image artifacts.

### Strong patterns to adopt

1. **Dual-ID display.** Showing both CIP-129 and CIP-105 IDs reduces user error during the bech32 transition period. Daedalus should follow.
2. **Default avatar placeholder.** Never render a broken image; always fall back to a neutral SVG. (Daedalus will only render avatars in anchor-1/anchor-2 after anchor verification — but the same fallback contract applies.)
3. **Status badge always visible at card level**, not only in detail. Aligns with the plan's "active/inactive + expiry" first-class fields.
4. **Sort + Filter are separate, discoverable buttons** rather than buried in a dropdown. Active-filter count badge is a small but high-value cue.
5. **Two CTAs per card: "View details" and the action ("Connect to delegate").** Eliminates a click for users who already know they want to delegate.

### Patterns to reject

1. **Default sort by "Voting Activity" / by stake.** This is exactly the popularity-bias amplifier the Daedalus plan exists to remove. Daedalus default sort is **randomized within the eligible cohort** (top-35 excluded).
2. **No source labelling on rendered name/avatar.** GovTool renders the off-chain `givenName` and image directly alongside on-chain ID with no visual distinction. A naïve user can't tell what's verified vs anchor-claimed. Daedalus must label all anchor-derived content explicitly (anchor-1/anchor-2). See `shared-design-tokens.md` → "Source label."
3. **Voting power displayed without precision context.** GovTool shows `₳ 23,137,980` with no thousands-suffix-vs-decimal clarity; this is fine for browsing but Daedalus also needs detail-view exact lovelace for evaluators. Shared tokens specify both rounded and exact representations.
4. **"Connect to delegate" requires wallet-connect handshake.** Daedalus already has the wallet in-process; the CTA wording must be different ("Select DRep", "Delegate to this DRep") and must not imply an external handshake.

## 1694.io — What's Useful

- The **Voting Power Calculator** ("Your ADA × current total voting power = your share %") is a powerful evaluator-side affordance. **Out of scope for v1**, but flag for a post-anchor-2 phase on detail view ("if you delegated your X ADA to this DRep, their voting power would become Y").
- Heavy use of `Note` and `Warning` callouts with a single accent color and an icon. Daedalus already has a similar inline-warning pattern in voting screens — reuse it for the randomization indicator and source labels.

## CIP-1694 — Direct UX Implications

| Concept | UX impact |
|---|---|
| DRep activity / `drepActivity` epochs | "Inactive" badge is normative, not cosmetic. Show on directory cards. |
| Anchor = `(URL, blake2b-256 hash)` pair, optional, no on-chain rules check URL or hash | Source label MUST distinguish "on-chain (verified by hash)" from "off-chain (not yet verified)". |
| `Abstain` and `No Confidence` are pre-defined options, not DReps | Do not list them as directory entries; keep as `VotingPowerDelegation` form choices. |
| DReps can be Ed25519 keys OR scripts (`drep_script`) | Filter facet "Type: key / script" useful in detail but low priority for slice-4/5 cards. |
| Voting power = total lovelace delegated to that DRep at next epoch boundary | Always render lovelace via `BigNumber`-safe formatter. Never coerce through `Number`. |

## CIP-119 — Direct UX Implications

| Field | Required? | UX treatment |
|---|---|---|
| `givenName` (≤80 chars, no markdown) | **Compulsory** in metadata | Display as DRep name once verified; truncate with ellipsis + tooltip beyond ~32 chars in cards. |
| `image` / `imageObject` (base64 or URL+sha256) | Optional | anchor-2 only. Render after the image-file hash is also verified. Default placeholder otherwise. |
| `objectives`, `motivations`, `qualifications` (≤1000 chars each) | Optional | Detail view only. Render as separate labeled sections. |
| `references[].@type = Link` | Optional | Render as outbound link with explicit external-link icon and "Opens in browser" confirmation. |
| `references[].@type = Identity` | Optional | "Identity claim" sub-section — must say "Verify by visiting the URL and confirming DRep ID is published there." Never auto-verify. |
| `paymentAddress` | Optional | Detail view only. Render with copy button and clear "DRep's stated payment address" label — never auto-fill anywhere. |
| `doNotList` | Optional (default `false`) | **anchor-2 acceptance gap (task-153).** If `true`, DRep is excluded from default cohort but reachable via direct ID entry and search. Accepted interim gap from slice-5 to anchor-2. |

## Patterns Daedalus Must Reject Outright

- **Aggregate "trust score" / star ratings.** No source surfaces this; doing so would imply Daedalus endorsement and is incompatible with the local-first / no-centralization stance.
- **Inline social media embeds.** All anchor-claimed `references` URLs must be rendered as explicit links with a confirmation step before the OS browser opens, consistent with the rest of Daedalus.
- **Identity verification claims as facts.** Every CIP-119 `Identity` reference is unverified by Daedalus; render as "claimed identity, please verify manually."

## Pattern Summary

The DRep Discovery design adopts:

- Default sort = randomized within eligible cohort; user can switch to ascending DRep ID or alphabetical (when name verified).
- Dual ID display.
- Status badges and source labels per `shared-design-tokens.md`.
- Two card CTAs: "View details" and "Select for delegation".
- No external-portal links anywhere except CIP reference links in help text.

## Citations

- GovTool DRep Directory page, fetched 2026-05-21.
- 1694.io — CIP-1694 explainer with Voting Power Calculator, fetched 2026-05-21.
- AdaStat — `/governance` returned 404, fetched 2026-05-21; alternative DRep listing surface to be re-evaluated if a working URL is provided.
- CIP-1694 — Cardano Foundation CIPs site, status `Active`.
- CIP-119 — Cardano Foundation CIPs site, status `Proposed`.
