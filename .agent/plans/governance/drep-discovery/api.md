# DRep API Reference

All requests target the cardano-wallet REST API via the standard TLS-authenticated client.
Request files live in `source/renderer/app/api/governance/requests/`.
Types live in `source/renderer/app/api/governance/types.ts`.

The DRep surface below exists in the wallet revision this branch pins in `flake.nix`,
`cardano-foundation/cardano-wallet` at `26c79b4194c71167f28281237541203feaa28b40`. It is
absent from the revision master pins. Both builds report the same version string,
`v2026-07-23`, and differ only by git revision, so `cardano-wallet version` alone does not
tell the two apart. Running against the older build makes `/v2/dreps/summary` fail as an
invalid bech32 DRep id, because the path is matched against the `{drepId}` route.

## Endpoints

### `GET /v2/dreps/suggested?count={n}`

**File:** `requests/listSuggestedDReps.ts`

Returns a randomized cohort of DReps suitable for delegation. The server applies its own
scoring/ranking logic; the result is deterministic per session (seeded by the server).

**Query params:**
- `count` (default 20) — Number of DReps to return

**Response:** `ApiDRepInfo[]`

**Store call:** `governanceStore.fetchSuggestedDReps(count?)` → populates `suggestedDReps`

---

### `GET /v2/dreps`

**File:** `requests/listDReps.ts`

Returns the full registered DRep index. This is a potentially large response (hundreds/thousands
of entries). The store lazy-loads it only when show-all or search is active.

**Response:** `ApiDRepInfo[]`

**Store call:** `governanceStore.loadAllDReps()` → populates `allDReps`

---

### `GET /v2/dreps/{drepId}`

**File:** `requests/getDRep.ts`

Returns detail for a single DRep including off-chain metadata fetched by the wallet backend.

**Path params:**
- `drepId` — CIP-129 bech32 DRep ID

**Response:** `ApiDRepInfo & { metadata?: ApiDRepMetadata }`

**Store call:** `governanceStore.fetchDRep(drepId)` → returned as `AppDRepDetail` (not stored)

---

### `GET /v2/dreps/summary`

**File:** `requests/getDRepSummary.ts`

Returns aggregate statistics across all registered DReps.

**Response:** `ApiDRepSummary`

```json
{
  "active_drep_count": 59,
  "inactive_drep_count": 221,
  "total_drep_count": 280,
  "total_drep_stake": { "quantity": "511304929746789", "unit": "lovelace" }
}
```

`total_drep_stake` and `total_drep_count` cover registered DReps only. The predefined
`always_abstain` and `always_no_confidence` targets are excluded from both, verified on
preprod against Koios on 2026-08-20: the wallet's figure matched the sum over real DReps to
the lovelace, while `always_abstain` alone held a further 422,670,965,669,387 lovelace. The
value is therefore usable directly as a share denominator with no adjustment.

**Store call:** `governanceStore.fetchDRepSummary()` → populates `drepSummary` and
`drepSummaryState`. Called unconditionally from `fetchSuggestedDReps`, so it runs on every
directory refresh.

---

### `PUT /v2/dreps/{drepId}/wallets/{walletId}`

**File:** `../voting/requests/delegateVotes.ts`

Submits a vote delegation certificate for the wallet. `drepId` may be a bech32 DRep ID or one
of the predefined targets. This certificate delegates voting power only: it does not carry a
stake delegation and does not disturb one already in place, confirmed on preprod by inspecting
transaction `470c10c5abcb0b685fbed229aa25f8985cffbc8081c5bd79129a75d6c4344494`, which carried a
single `vote_delegation` certificate.

**Body:** `{ passphrase }`

**Response:** `Transaction`

---

## TypeScript types

### `ApiDRepInfo`

```typescript
interface ApiDRepInfo {
  id: string;                    // CIP-129 bech32 DRep ID
  voting_power?: string;         // Lovelace as string (absent if 0 delegation)
  status: 'active' | 'inactive';
  drep_activity?: number;        // Epochs until expiry
  anchor?: {
    url: string;
    hash: string;                // SHA-256 hex of the metadata document
  };
  metadata?: ApiDRepMetadata;    // Only on detail endpoint
}
```

### `ApiDRepMetadata`

```typescript
interface ApiDRepMetadata {
  verified_name?: string;        // Human-readable name from off-chain doc
  do_not_list?: boolean;         // DRep opted out of directory listing
  objectives?: string;
  motivations?: string;
  qualifications?: string;
  payment_address?: string;      // Cardano payment address
  references?: ApiDRepMetaReference[];
}
```

### `ApiDRepMetaReference`

```typescript
interface ApiDRepMetaReference {
  '@type': string;   // e.g. 'Identity', 'Link', 'GovernanceMetadata'
  label?: string;
  uri?: string;
}
```

## API → store mapping

| API field | `AppDRepDirectoryEntry` field | Notes |
|-----------|------------------------------|-------|
| `id` | `drepId` | CIP-129 form |
| `voting_power` | `votingPower` | `BigNumber` parsed from string, `null` if absent |
| `status` | `status` | Passed through |
| `drep_activity` | `drepActivity` | `null` if absent |
| `anchor` | `anchor` | `null` if absent |
| `metadata.verified_name` | `verifiedName` | Clamped to 80 chars |
| `metadata.do_not_list` | `doNotList` | Defaults to `false` |

The full `metadata` object (objectives, motivations, etc.) is only available on `AppDRepDetail`,
which is the return type of `fetchDRep`.
