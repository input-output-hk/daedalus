# DRep API Reference

All requests target the cardano-wallet REST API via the standard TLS-authenticated client.
Request files live in `source/renderer/app/api/governance/requests/`.
Types live in `source/renderer/app/api/governance/types.ts`.

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
