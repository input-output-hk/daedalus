# Slice-1 Implementation Guide: Walking Skeleton + Sanitization Floor

> **Companion PRD:** [slice-1-PRD.md](./slice-1-PRD.md) | **Task Tracker:** [governance-drep-discovery-plan-tasks.json](./governance-drep-discovery-plan-tasks.json)

> ⚠️ **SUPERSEDED IN PARTS (2026-06-15).** This guide is the original *pre-implementation* plan. Several sections were overtaken by the as-built code during the 2026-06-12 parser/schema repair and the slice-1 final pass. Trust the shipped code and the [code review](./slice-1-code-review.md) (finding R5) over the pseudocode below for:
> - **task-101 types** — `DRepStatus` shipped as `'active' | 'expired'` (canonical term is `inactive`; rename tracked as FP-9), **not** the four-value `'active' | 'inactive' | 'expired' | 'retired'` union shown here. Voting power is `Lovelace | null`. See [governance.types.ts](../../../../../source/common/types/governance.types.ts).
> - **task-103 parser** — the real CLI output is a `[[credential, state], …]` tuple array (not a flat object with `drepId`/`votingPower`/`status` fields). The shipped service uses `drep-state --all-dreps --include-stake` + `query tip`, derives CIP-129 IDs from credentials, derives status from `expiry` vs current epoch, and **must also pass the network flag** (`--mainnet`/`--testnet-magic`, FP-1). See [GovernanceQueryService.ts](../../../../../source/main/governance/GovernanceQueryService.ts).
> - **task-107 status badge** — slice-1 ships `Active`/`Inactive` only (the code's `active`/`expired`); `Expiring`/`Retired` are deferred per [shared-design-tokens.md](../designs/shared-design-tokens.md) §1.
>
> The task-102 / 104 / 105 / 106 / 108 / 109 / 110 / 111 sections remain accurate. The remaining executable gaps (FP-1 network flag, FP-2 error details in UI, FP-3 history-push guard, FP-9 status rename) are owned by the [final pass PRD](./slice-1-final-pass-PRD.md).

---

## Implementation Order

Tasks must be executed in dependency order. The critical path is:

```
task-101 (types) → task-102 (socket path) → task-103 (query service)
→ task-104 (IPC channels) → task-105 (renderer IPC clients)
→ task-106 (GovernanceStore) → task-107 (directory list component)
→ task-108 (route wiring)
→ task-109 (filterLogData) → task-110 (analytics) → task-111 (spy floor)
```

## Cross-Cutting Renderer Note

The renderer currently uses `react-intl@2.9.0`. Slice-1 governance UI should follow the existing `injectIntl` / `intlShape` / `FormattedMessage` patterns and must not introduce hooks such as `useIntl()` or `FormattedRelativeTime`.

---

## task-101: Define shared DRep Discovery TypeScript types

**Directory:** `source/common/types/`  
**New files:**
- `source/common/types/governance.types.ts` — Core DRep types
- `source/common/types/governance.types.ts` must re-export from an index or be imported directly

### Types to Define

```typescript
// source/common/types/governance.types.ts

// ---- DRep Identity ----

/** Raw on-wire bech32 string as returned by cardano-cli */
export type DRepId = string;

/** Discriminated DRep identity with all known encodings */
export interface DRepIdentity {
  raw: DRepId;                    // original bech32 from CLI
  cip129?: string;                // CIP-129 encoding (if derivable)
  cip105?: string;                // CIP-105 encoding (if derivable)
  credentialHex?: string;         // raw credential hex
  credentialType: 'key' | 'script';
}

// ---- DRep Registration State ----

export type DRepStatus = 'active' | 'inactive' | 'expired' | 'retired';

/** Number of epochs until expiry; null if unknown or retired */
export type DrepActivity = number | null;

// ---- Voting Power ----

/** lovelace as BigNumber | null (null = ranking unavailable) */
// Serialized as decimal string across IPC
export type Lovelace = string; // decimal-string representation

// ---- DRep Directory Entry (bare, slice-1) ----

export interface DRepDirectoryEntry {
  drepId: DRepId;
  /** Bech32-encoded DRep ID */
  votingPower: Lovelace;
  /** Decimal string, e.g. "688964123456" */
  status: DRepStatus;
  /** Active / Inactive / Expired / Retired */
  drepActivity: DrepActivity;
  /** Remaining epochs until expiry */
  anchor: DRepAnchorPresence | null;
  /** Anchor presence (URL + hash) from on-chain */
}

// ---- Anchor Presence (on-chain reference only, NO fetch in slice-1) ----

export interface DRepAnchorPresence {
  url: string;
  hash: string; // Blake2b-256 hex digest
}

// ---- Wallet Governance Status ----

export type GovernanceVoteKind = 'drep' | 'abstain' | 'no_confidence';

/** Wallet's current governance delegation */
export interface WalletGovernanceStatus {
  voteKind: GovernanceVoteKind | null;
  /** null = no delegation */
  drepId: DRepId | null;
  /** present only when voteKind === 'drep' */
}

// ---- Query Payloads ----

export interface DRepListQueryPayload {
  dreps: DRepDirectoryEntry[];
  fetchedAt: number;
  /** Unix timestamp ms */
  epoch: number | null;
  /** Current epoch number, null if unavailable */
}

// ---- Error Types ----

export enum GovernanceQueryErrorType {
  SocketUnavailable = 'SOCKET_UNAVAILABLE',
  CliNotFound = 'CLI_NOT_FOUND',
  QueryFailed = 'QUERY_FAILED',
  ParseFailed = 'PARSE_FAILED',
  SelfnodeCliUnsupported = 'SELFNODE_CLI_UNSUPPORTED',
  Timeout = 'TIMEOUT',
  Unknown = 'UNKNOWN',
}

export interface GovernanceQueryError {
  type: GovernanceQueryErrorType;
  message: string;
  details?: string;
}
```

### Key Rules

1. All `Lovelace` values are **decimal strings** (e.g., `"688964123456"`) when crossing IPC. The renderer rehydrates to `BigNumber`.
2. `DRepDirectoryEntry` in slice-1 is **bare**: ID, voting power, status, activity, anchor presence. No metadata fields, no category badges, no favorited flag.
3. The `WalletGovernanceStatus` type is defined now for IPC contract completeness but is not wired until cv-1.

### Acceptance

- [ ] Types compile with `yarn compile`
- [ ] `BigNumber | null` for voting power in app-domain models
- [ ] All lovelace fields carry JSDoc noting decimal-string serialization

---

## task-102: Own cardano-node socket path across launcher lifecycle

**Directory:** `source/main/cardano/`  
**Files to modify:**
- `source/main/cardano/CardanoWalletLauncher.ts` — Set `nodeConfig.socketFile` before launcher construction
- `source/main/cardano/CardanoNode.ts` — Capture and expose `socketPath` after `node.start()`

### Step-by-Step

#### Step 1: Verify launcher support

Before writing code, verify `cardano-launcher@0.20220119.0` supports `nodeConfig.socketFile`:

```bash
grep -r "socketFile" node_modules/cardano-launcher/ --include="*.js" --include="*.ts" --include="*.d.ts"
```

If the launcher does NOT support `socketFile`, escalate and pause.

#### Step 2: Set `socketFile` in `CardanoWalletLauncher`

In `CardanoWalletLauncher.ts`, add `socketFile` to the shared launcher config before constructing the launcher:

```typescript
// Inside CardanoWalletLauncher function, in the shared launcherConfig object:

const launcherConfig = {
  networkName: cluster,
  stateDir,
  nodeConfig: {
    kind: nodeImplementation,
    configurationDir: '',
    network: {
      configFile: configPath,
    },
    // ADD: specify the socket file path so cardano-launcher creates it
    socketFile: path.join(stateDir, 'node.socket'),
  },
  // ... rest unchanged
};
```

#### Step 3: Capture `socketPath` in `CardanoNode`

In `CardanoNode.ts`:

1. Add a private field:
```typescript
_nodeSocketPath: string | null = null;
```

2. Add a public getter:
```typescript
get nodeSocketPath(): string | null {
  return this._nodeSocketPath;
}
```

3. After `node.start()` resolves (in the `.then()` callback), capture:
```typescript
node.start().then((api) => {
  // ... existing process setup ...

  // ADD: capture the resolved socket path from the launcher
  this._nodeSocketPath = node.nodeService.socketPath;

  // ... rest unchanged ...
});
```

If `cardano-launcher` exposes the socket path differently (e.g., `node.nodeService.getSocketPath()`), adapt accordingly after verifying the actual launcher API.

#### Step 4: Verify

- [ ] `nodeSocketPath` is `null` before `node.start()`
- [ ] `nodeSocketPath` is a valid filesystem path after `node.start()` resolves
- [ ] Path points to the actual socket file created by cardano-node
- [ ] No `nodeSocketPath` field added to `WalletOptions` or `CardanoNodeConfig`

---

## task-103: Add main-process DRep query service

**Directory:** `source/main/`  
**New files:**
- `source/main/governance/GovernanceQueryService.ts` — Singleton query service
- `source/main/governance/parseDRepState.ts` — CLI JSON parsing with `json-bigint`

### Step-by-Step

#### Step 1: Create directory structure

```bash
mkdir -p source/main/governance
```

#### Step 2: Implement `GovernanceQueryService`

```typescript
// source/main/governance/GovernanceQueryService.ts

import { spawn } from 'child_process';
import JSONbig from 'json-bigint';
import { logger } from '../utils/logging';
import type { DRepListQueryPayload, GovernanceQueryError, DRepDirectoryEntry } from '../../common/types/governance.types';
import { GovernanceQueryErrorType } from '../../common/types/governance.types';

// Use json-bigint with storeAsString: true so all large numbers become strings
const JSONBig = JSONbig({ storeAsString: true });

export class GovernanceQueryService {
  private static instance: GovernanceQueryService | null = null;
  private lastSuccessfulData: DRepListQueryPayload | null = null;
  private inFlightRefresh: Promise<DRepListQueryPayload> | null = null;
  private cliBin: string = 'cardano-cli';
  private nodeSocketPath: string | null = null;

  private constructor() {}

  static getInstance(): GovernanceQueryService {
    if (!GovernanceQueryService.instance) {
      GovernanceQueryService.instance = new GovernanceQueryService();
    }
    return GovernanceQueryService.instance;
  }

  setCliBin(path: string): void {
    this.cliBin = path;
  }

  setNodeSocketPath(socketPath: string | null): void {
    this.nodeSocketPath = socketPath;
  }

  async fetchDRepList(): Promise<DRepListQueryPayload> {
    // Deduplicate in-flight requests
    if (this.inFlightRefresh) {
      return this.inFlightRefresh;
    }

    this.inFlightRefresh = this._doFetchDRepList();

    try {
      const result = await this.inFlightRefresh;
      this.lastSuccessfulData = result;
      return result;
    } finally {
      this.inFlightRefresh = null;
    }
  }

  getLastSuccessfulData(): DRepListQueryPayload | null {
    return this.lastSuccessfulData;
  }

  private async _doFetchDRepList(): Promise<DRepListQueryPayload> {
    if (!this.nodeSocketPath) {
      throw this._makeError(GovernanceQueryErrorType.SocketUnavailable, 
        'Cardano node socket path is not available');
    }

    try {
      const stdout = await this._runCliQuery();
      const parsed = JSONBig.parse(stdout);
      const dreps = this._parseDRepState(parsed);
      
      return {
        dreps,
        fetchedAt: Date.now(),
        epoch: null, // Will be enriched with gov-state query in later slices
      };
    } catch (error) {
      if (error instanceof Error && error.message.includes('GovernanceQueryError')) {
        throw error;
      }
      throw this._makeError(GovernanceQueryErrorType.QueryFailed,
        `DRep query failed: ${error instanceof Error ? error.message : String(error)}`);
    }
  }

  private _runCliQuery(): Promise<string> {
    return new Promise((resolve, reject) => {
      const child = spawn(this.cliBin, [
        'latest', 'query', 'drep-state',
        '--all-dreps',
        '--output-json',
      ], {
        env: {
          ...process.env,
          CARDANO_NODE_SOCKET_PATH: this.nodeSocketPath!,
        },
        stdio: ['ignore', 'pipe', 'pipe'],
      });

      let stdout = '';
      let stderr = '';

      child.stdout.on('data', (data: Buffer) => {
        stdout += data.toString('utf-8');
      });

      child.stderr.on('data', (data: Buffer) => {
        stderr += data.toString('utf-8');
      });

      child.on('error', (err) => {
        reject(this._makeError(GovernanceQueryErrorType.CliNotFound,
          `cardano-cli not found: ${err.message}`));
      });

      child.on('close', (code) => {
        if (code !== 0) {
          reject(this._makeError(GovernanceQueryErrorType.QueryFailed,
            `cardano-cli exited with code ${code}: ${stderr}`));
          return;
        }
        resolve(stdout);
      });
    });
  }

  private _parseDRepState(raw: unknown): DRepDirectoryEntry[] {
    if (!Array.isArray(raw)) {
      throw this._makeError(GovernanceQueryErrorType.ParseFailed,
        'Expected array from drep-state query');
    }

    return raw.map((entry: Record<string, unknown>, index: number) => {
      try {
        return {
          drepId: String(entry.drepId ?? entry.key ?? `unknown-${index}`),
          votingPower: String(entry.votingPower ?? entry.stake ?? entry.amount ?? '0'),
          status: this._parseStatus(entry),
          drepActivity: this._parseActivity(entry),
          anchor: this._parseAnchor(entry),
        };
      } catch (err) {
        logger.error('GovernanceQueryService: failed to parse DRep entry', { entry, error: err });
        throw this._makeError(GovernanceQueryErrorType.ParseFailed,
          `Failed to parse DRep entry at index ${index}`);
      }
    });
  }

  private _parseStatus(entry: Record<string, unknown>): DRepDirectoryEntry['status'] {
    // cardano-cli returns the status in the drep-state output
    // Actual field names depend on CLI output schema — adapt after fixture capture
    const rawStatus = String(entry.status ?? entry.drepStatus ?? 'active');
    switch (rawStatus.toLowerCase()) {
      case 'active': return 'active';
      case 'inactive': return 'inactive';
      case 'expired': return 'expired';
      case 'retired': return 'retired';
      default: return 'active';
    }
  }

  private _parseActivity(entry: Record<string, unknown>): DRepDirectoryEntry['drepActivity'] {
    const raw = entry.drepActivity ?? entry.remainingEpochs ?? entry.expiry;
    if (raw === null || raw === undefined) return null;
    const num = Number(raw);
    return Number.isNaN(num) ? null : num;
  }

  private _parseAnchor(entry: Record<string, unknown>): DRepDirectoryEntry['anchor'] {
    const anchor = entry.anchor as Record<string, unknown> | null | undefined;
    if (!anchor) return null;
    return {
      url: String(anchor.url ?? anchor.anchorUrl ?? ''),
      hash: String(anchor.hash ?? anchor.anchorHash ?? anchor.dataHash ?? ''),
    };
  }

  private _makeError(type: GovernanceQueryErrorType, message: string, details?: string): Error & { queryErrorType: GovernanceQueryErrorType } {
    const err = new Error(message) as Error & { queryErrorType: GovernanceQueryErrorType };
    err.queryErrorType = type;
    return err;
  }
}
```

#### Step 3: Wire into `CardanoNode`

In `CardanoNode.ts`, after `node.start()` resolves and `_nodeSocketPath` is set, notify the query service:

```typescript
import { GovernanceQueryService } from '../governance/GovernanceQueryService';

// In the node.start().then() callback, after setting this._nodeSocketPath:
GovernanceQueryService.getInstance().setNodeSocketPath(this._nodeSocketPath);
GovernanceQueryService.getInstance().setCliBin(this._config.cliBin);
```

#### Step 4: Capture test fixtures

Before merging, capture real fixtures from a synced node:

```bash
# Start a synced node
CARDANO_NODE_SOCKET_PATH=/path/to/node.socket cardano-cli latest query drep-state --all-dreps --output-json > tests/mocks/governance/drep-state.json
CARDANO_NODE_SOCKET_PATH=/path/to/node.socket cardano-cli latest query drep-stake-distribution --all-dreps --output-json > tests/mocks/governance/drep-stake-distribution.json
CARDANO_NODE_SOCKET_PATH=/path/to/node.socket cardano-cli latest query gov-state --output-json > tests/mocks/governance/gov-state.json
```

Verify at least one fixture contains a lovelace value above `Number.MAX_SAFE_INTEGER` (9,007,199,254,740,991).

### Acceptance

- [ ] Service is a singleton
- [ ] Bulk `--all-dreps` only; no per-DRep CLI invocations
- [ ] `CARDANO_NODE_SOCKET_PATH` set in spawn env, not as argv
- [ ] `json-bigint` with `storeAsString: true` used for parsing
- [ ] All lovelace fields are decimal strings
- [ ] In-flight requests are deduplicated
- [ ] Last-successful data retained for stale-while-refresh
- [ ] Typed errors for socket-unavailable, CLI-not-found, query-failed, parse-failed
- [ ] Test fixtures committed under `tests/mocks/governance/`

---

## task-104: Add governance IPC channels for DRep Discovery

**Files to modify:**
- `source/common/ipc/api.ts` — Add channel constants and types
- `source/main/ipc/index.ts` — Register main-process handlers
- `source/main/ipc/governanceChannel.ts` — New file with channel handler

### Step-by-Step

#### Step 1: Add channel definitions to `api.ts`

In `source/common/ipc/api.ts`, add at the end (before the final export):

```typescript
// ======================= GOVERNANCE IPC CHANNELS ======================

import type { DRepListQueryPayload, GovernanceQueryError } from '../types/governance.types';

export const GOVERNANCE_DREP_LIST_CHANNEL = 'GOVERNANCE_DREP_LIST_CHANNEL';
export type GovernanceDRepListRendererRequest = void;
export type GovernanceDRepListMainResponse = DRepListQueryPayload;
```

#### Step 2: Create main-process handler

Create `source/main/ipc/governanceChannel.ts`:

```typescript
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  GOVERNANCE_DREP_LIST_CHANNEL,
} from '../../common/ipc/api';
import type {
  GovernanceDRepListRendererRequest,
  GovernanceDRepListMainResponse,
} from '../../common/ipc/api';
import { GovernanceQueryService } from '../governance/GovernanceQueryService';
import { logger } from '../utils/logging';

const governanceDRepListChannel: MainIpcChannel<
  GovernanceDRepListRendererRequest,
  GovernanceDRepListMainResponse
> = new MainIpcChannel(GOVERNANCE_DREP_LIST_CHANNEL);

export const handleGovernanceRequests = () => {
  governanceDRepListChannel.onRequest(async (_request) => {
    logger.info('Governance IPC: DRep list requested');
    try {
      const result = await GovernanceQueryService.getInstance().fetchDRepList();
      return result;
    } catch (error) {
      logger.error('Governance IPC: DRep list query failed', { error });
      const queryError = error as Error & { queryErrorType?: string };
      throw {
        type: queryError.queryErrorType ?? 'UNKNOWN',
        message: queryError.message ?? 'Unknown error',
      };
    }
  });
};
```

#### Step 3: Register in main IPC index

In `source/main/ipc/index.ts`:

```typescript
import { handleGovernanceRequests } from './governanceChannel';

// Inside the default export function, add:
  handleGovernanceRequests();
```

### Acceptance

- [ ] Channel constant follows naming convention (`GOVERNANCE_DREP_LIST_CHANNEL`)
- [ ] Types follow `{ChannelName}RendererRequest` / `{ChannelName}MainResponse` pattern
- [ ] Main handler is registered in `source/main/ipc/index.ts`
- [ ] Handler logs the request and catches errors with typed fallback

---

## task-105: Add renderer IPC clients for DRep Discovery

**New files:**
- `source/renderer/app/ipc/governanceChannel.ts` — Renderer IPC client

### Step-by-Step

Create `source/renderer/app/ipc/governanceChannel.ts`:

```typescript
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  GOVERNANCE_DREP_LIST_CHANNEL,
} from '../../../common/ipc/api';
import type {
  GovernanceDRepListMainResponse,
  GovernanceDRepListRendererRequest,
} from '../../../common/ipc/api';

export const governanceDRepListChannel: RendererIpcChannel<
  GovernanceDRepListMainResponse,
  GovernanceDRepListRendererRequest
> = new RendererIpcChannel(GOVERNANCE_DREP_LIST_CHANNEL);
```

### Acceptance

- [ ] Follows existing Mithril IPC client pattern
- [ ] Generic params are `<MainResponse, RendererRequest>` (reverse of main-side)
- [ ] Uses `RendererIpcChannel` from `./lib/RendererIpcChannel`

---

## task-106: Add bare GovernanceStore (list + drepIndex + loading/error)

**New files:**
- `source/renderer/app/stores/GovernanceStore.ts`

**Files to modify:**
- `source/renderer/app/stores/index.ts` — Register GovernanceStore

### Step-by-Step

#### Step 1: Create GovernanceStore

```typescript
// source/renderer/app/stores/GovernanceStore.ts

import { action, observable, computed, runInAction } from 'mobx';
import BigNumber from 'bignumber.js';
import Store from './lib/Store';
import { governanceDRepListChannel } from '../ipc/governanceChannel';
import { logger } from '../utils/logging';
import type { DRepDirectoryEntry, GovernanceQueryError as GovernanceQueryErrorType } from '../../../common/types/governance.types';
import { GovernanceQueryErrorType as GovErr } from '../../../common/types/governance.types';

/** App-domain DRep entry with BigNumber voting power */
export interface AppDRepDirectoryEntry {
  drepId: string;
  votingPower: BigNumber | null;
  status: DRepDirectoryEntry['status'];
  drepActivity: DRepDirectoryEntry['drepActivity'];
  anchor: DRepDirectoryEntry['anchor'];
}

export enum GovernanceRefreshState {
  Idle = 'idle',
  Loading = 'loading',
  Refreshing = 'refreshing', // Stale-while-refresh in later slices
  Loaded = 'loaded',
  Failed = 'failed',
}

export interface GovernanceStoreError {
  type: string;
  message: string;
  details?: string;
}

export default class GovernanceStore extends Store {
  // ---- Observables ----

  @observable drepIndex: Map<string, AppDRepDirectoryEntry> = new Map();
  @observable drepList: AppDRepDirectoryEntry[] = [];
  @observable refreshState: GovernanceRefreshState = GovernanceRefreshState.Idle;
  @observable error: GovernanceStoreError | null = null;
  @observable lastFetchedAt: number | null = null;

  // ---- Computed ----

  @computed get isLoading(): boolean {
    return this.refreshState === GovernanceRefreshState.Loading;
  }

  @computed get isLoaded(): boolean {
    return this.refreshState === GovernanceRefreshState.Loaded;
  }

  @computed get hasError(): boolean {
    return this.refreshState === GovernanceRefreshState.Failed;
  }

  @computed get drepCount(): number {
    return this.drepList.length;
  }

  // ---- Actions ----

  @action
  async fetchDRepList(): Promise<void> {
    if (this.refreshState === GovernanceRefreshState.Loading) return;

    const hasExistingData = this.drepList.length > 0;
    
    runInAction(() => {
      this.refreshState = hasExistingData
        ? GovernanceRefreshState.Refreshing
        : GovernanceRefreshState.Loading;
      this.error = null;
    });

    try {
      const payload = await governanceDRepListChannel.request();
      
      runInAction(() => {
        const entries = this._rehydrateDReps(payload.dreps);
        this.drepList = entries;
        this.drepIndex = new Map(entries.map(e => [e.drepId, e]));
        this.refreshState = GovernanceRefreshState.Loaded;
        this.lastFetchedAt = payload.fetchedAt;
        this.error = null;
      });
    } catch (err) {
      logger.error('GovernanceStore: fetchDRepList failed', { error: err });
      runInAction(() => {
        this.refreshState = GovernanceRefreshState.Failed;
        this.error = this._normalizeError(err);
      });
    }
  }

  @action
  refresh(): Promise<void> {
    return this.fetchDRepList();
  }

  // ---- Private Helpers ----

  private _rehydrateDReps(raw: DRepDirectoryEntry[]): AppDRepDirectoryEntry[] {
    return raw.map(entry => ({
      drepId: entry.drepId,
      votingPower: entry.votingPower ? new BigNumber(entry.votingPower) : null,
      status: entry.status,
      drepActivity: entry.drepActivity,
      anchor: entry.anchor,
    }));
  }

  private _normalizeError(err: unknown): GovernanceStoreError {
    if (err instanceof Error) {
      const queryErr = err as Error & { queryErrorType?: string };
      return {
        type: queryErr.queryErrorType ?? GovErr.Unknown,
        message: err.message,
      };
    }
    return {
      type: GovErr.Unknown,
      message: String(err),
    };
  }

  // ---- Lifecycle ----

  setup(): void {
    super.setup();
    // Fetch DRep list on store initialization
    this.fetchDRepList();
  }
}
```

#### Step 2: Register in `stores/index.ts`

In `source/renderer/app/stores/index.ts`:

1. Add import:
```typescript
import GovernanceStore from './GovernanceStore';
```

2. Add to `storeClasses`:
```typescript
governance: GovernanceStore,
```

3. Add to `StoresMap` type:
```typescript
governance: GovernanceStore;
```

### Acceptance

- [ ] Store exports `AppDRepDirectoryEntry` with `BigNumber | null` voting power
- [ ] `drepIndex` is a `Map<string, AppDRepDirectoryEntry>` for O(1) lookups (used by cv-2)
- [ ] Refresh state covers: idle / loading / refreshing / loaded / failed
- [ ] `fetchDRepList()` deduplicates in-flight requests locally
- [ ] `BigNumber` rehydration happens in `_rehydrateDReps`, never before IPC
- [ ] Store registered in `storeClasses` and `StoresMap`
- [ ] VotingStore must NOT import or read GovernanceStore

---

## task-107: Build the BARE DRep directory list (id / power / status)

**New files:**
- `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx`
- `source/renderer/app/components/governance/drep-directory/DRepCard.tsx`
- `source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx`
- `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.tsx` (placeholder)
- `source/renderer/app/components/governance/_shared/DRepStatusBadge.tsx`
- `source/renderer/app/components/governance/_shared/DRepIdDisplay.tsx`
- `source/renderer/app/i18n/locales/governance/DRepDirectory.messages.ts`

### Component Design

#### DRepStatusBadge

A presentational badge component. In slice-1, renders:
- **Active**: green dot + "Active" label
- **Inactive**: grey dot + "Inactive" label
- **Expired**: grey + "Expired"
- **Retired**: grey + "Retired"

Follows shared-design-tokens §1 (reuses existing theme tokens).

#### DRepIdDisplay

Renders truncated DRep ID with copy button. In slice-1:
- Show first 8 + "…" + last 6 characters
- Copy button copies full ID to clipboard
- Monospace font

#### DRepCard

Bare card for slice-1:
```
┌──────────────────────────────────────────┐
│ ●Active   drep1yg7s…aj8ras   📋         │
│            Voting power: ₳ 688K          │
└──────────────────────────────────────────┘
```

Props: `{ entry: AppDRepDirectoryEntry }`

#### DRepDirectoryList

Paginated list of DRepCards. Bare in slice-1: flat list, 25 per page, no filters.

#### DRepDirectory

Container that orchestrates loading/error/empty/list states:

```tsx
// States:
// - loading: skeleton placeholder cards
// - loaded: DRepDirectoryList
// - empty: "No DReps found" message
// - error: error banner with retry button
// - refreshing: list visible + spinner badge
```

### DRepDirectoryBanner (placeholder)

In slice-1: a simple header with "DRep Directory" title and a refresh button. The cohort banner, BMVG citation, and reshuffle control are added in slice-5.

### i18n Keys

Add messages file with keys under `governance.drepDirectory.*`:

```typescript
// source/renderer/app/i18n/locales/governance/DRepDirectory.messages.ts
export default {
  title: { id: 'governance.drepDirectory.title', defaultMessage: '!!!DRep Directory' },
  refresh: { id: 'governance.drepDirectory.refresh', defaultMessage: '!!!Refresh' },
  lastUpdated: { id: 'governance.drepDirectory.lastUpdated', defaultMessage: '!!!Last updated {time}' },
  empty: { id: 'governance.drepDirectory.empty', defaultMessage: '!!!No DReps found on-chain.' },
  error: { id: 'governance.drepDirectory.error', defaultMessage: '!!!Could not load DRep data.' },
  retry: { id: 'governance.drepDirectory.retry', defaultMessage: '!!!Retry' },
  statusActive: { id: 'governance.drepDirectory.status.active', defaultMessage: '!!!Active' },
  statusInactive: { id: 'governance.drepDirectory.status.inactive', defaultMessage: '!!!Inactive' },
  statusExpired: { id: 'governance.drepDirectory.status.expired', defaultMessage: '!!!Expired' },
  statusRetired: { id: 'governance.drepDirectory.status.retired', defaultMessage: '!!!Retired' },
  votingPowerColumn: { id: 'governance.drepDirectory.votingPower', defaultMessage: '!!!Voting power' },
  drepIdColumn: { id: 'governance.drepDirectory.drepId', defaultMessage: '!!!DRep ID' },
  copyId: { id: 'governance.drepDirectory.copyId', defaultMessage: '!!!DRep ID copied' },
};
```

After defining messages, run `yarn i18n:manage` to populate en-US.json and ja-JP.json, and keep all generated copy preliminary with the leading `!!!` marker until the final end-of-feature manual review.

### Acceptance

- [ ] All five component states render correctly (loading / loaded / empty / error / refreshing)
- [ ] DRep cards show: truncated ID, formatted voting power, status badge
- [ ] Copy button works with toast confirmation
- [ ] Pagination at 25 cards per page
- [ ] No favorite toggle (slice-7)
- [ ] No category badges (slice-5)
- [ ] No search/filter controls (slice-6)
- [ ] No "Select for delegation" button (slice-2)
- [ ] Storybook stories cover all five states

---

## task-108: Wire the DRep directory route

**Files to modify:**
- `source/renderer/app/routes-config.ts` — Add GOVERNANCE routes
- `source/renderer/app/Routes.tsx` — Add route component
- `source/renderer/app/containers/voting/Voting.tsx` — Rename to Governance or create wrapper
- `source/renderer/app/components/sidebar/Sidebar.tsx` or equivalent — Rename "Voting" to "Governance"

### Step-by-Step

#### Step 1: Add GOVERNANCE route constants

In `source/renderer/app/routes-config.ts`:

```typescript
GOVERNANCE: {
  ROOT: '/governance',
  DREPS: '/governance/dreps',
},
```

#### Step 2: Create Governance container (or reuse Voting)

Two options, per the design doc:

**Option A (recommended if Voting container is simple):** Rename `Voting` container to `Governance` and expand it.

**Option B:** Create a new `Governance` container that wraps both the existing Voting routes and the new DRep routes.

Since the plan says "Rename the existing Voting sidebar entry to Governance," we follow the simplest path:

1. In the sidebar component, change the "Voting" label to "Governance"
2. The Governance section highlights for both `/governance/*` and `/voting/*` paths
3. Add the new DRep directory route

#### Step 3: Add route in Routes.tsx

```tsx
import DRepDirectoryPage from './containers/governance/DRepDirectoryPage';

// Add inside the Governance/Voting section:
<Route path={ROUTES.GOVERNANCE.ROOT}>
  <Governance>
    <Route
      exact
      path={ROUTES.GOVERNANCE.ROOT}
      component={() => <Redirect to={ROUTES.GOVERNANCE.DREPS} />}
    />
    <TrackedRoute
      pageTitle="DRep Directory"
      path={ROUTES.GOVERNANCE.DREPS}
      component={DRepDirectoryPage}
    />
    {/* Existing voting routes remain */}
    <TrackedRoute
      pageTitle="Voting Registration"
      path={ROUTES.VOTING.REGISTRATION}
      component={VotingRegistrationPage}
    />
    <TrackedRoute
      pageTitle="Voting Governance"
      path={ROUTES.VOTING.GOVERNANCE}
      component={VotingGovernancePage}
    />
  </Governance>
</Route>
```

#### Step 4: Create DRepDirectoryPage container

```typescript
// source/renderer/app/containers/governance/DRepDirectoryPage.tsx

import React from 'react';
import { observer, inject } from 'mobx-react';
import DRepDirectory from '../../components/governance/drep-directory/DRepDirectory';
import type { StoresMap } from '../../stores';

interface Props {
  stores?: StoresMap;
}

@inject('stores')
@observer
class DRepDirectoryPage extends React.Component<Props> {
  render() {
    const { stores } = this.props;
    const governanceStore = stores?.governance;
    
    if (!governanceStore) return null;
    
    return (
      <DRepDirectory
        drepList={governanceStore.drepList}
        refreshState={governanceStore.refreshState}
        error={governanceStore.error}
        lastFetchedAt={governanceStore.lastFetchedAt}
        onRefresh={() => governanceStore.fetchDRepList()}
      />
    );
  }
}

export default DRepDirectoryPage;
```

#### Step 5: Update sidebar

Find the sidebar component (likely `source/renderer/app/components/sidebar/`) and:
1. Change the "Voting" label to "Governance" (i18n key)
2. Update the active-state logic to highlight for both `/governance` and `/voting` paths

### Acceptance

- [ ] `/governance` redirects to `/governance/dreps`
- [ ] `/governance/dreps` renders the DRep directory
- [ ] Sidebar shows "Governance" label (en-US) and appropriate ja-JP translation
- [ ] Existing `/voting/governance` route still works
- [ ] Sidebar Governance entry is active for both `/governance/*` and `/voting/*`

---

## task-109: Redact governance vote targets in filterLogData

**File to modify:** `source/common/utils/logging.ts`

### Step-by-Step

In the `sensitiveData` array inside `filterLogData`, add the three governance-related keys:

```typescript
const sensitiveData = [
  // ... existing keys ...
  'spendingPassword',
  'oldPassword',
  'newPassword',
  'mnemonic',
  'recoveryPhrase',
  'passphrase',
  'password',
  'votingKey',
  'stakeKey',
  'signature',
  'accountPublicKey',
  'extendedPublicKey',
  'publicKeyHex',
  'chainCodeHex',
  'signedTransactionBlob',
  'withdrawal',
  // ADD: governance vote target redaction
  'dRepId',
  'vote',
  'voting',
];
```

Because `omit-deep-lodash` recurses by key name at ANY depth, adding `dRepId`, `vote`, and `voting` to the flat key list will redact them wherever they appear in logged objects, including `delegation.active.voting`, `delegation.next[*].voting`, `certificates[*].vote`, etc.

### Acceptance

- [ ] `dRepId`, `vote`, `voting` keys are redacted at any nesting depth
- [ ] No path-aware visitor needed; flat key list + omit-deep-lodash recursion is sufficient
- [ ] Existing sensitive data continues to be redacted

---

## task-110: Reduce 'Casted governance vote' analytics payload to drepOption only

**File to modify:** `source/renderer/app/stores/VotingStore.ts`

### Step-by-Step

Find the two `'Casted governance vote'` analytics calls in `VotingStore.ts` (around lines 386 and 416).

**Change from:**
```typescript
this.analytics.sendEvent(
  EventCategories.VOTING,
  'Casted governance vote',
  chosenOption, // Raw DRep ID or 'abstain' | 'no_confidence'
  wallet.amount.toNumber()
);
```

**Change to:**
```typescript
this.analytics.sendEvent(
  EventCategories.VOTING,
  'Casted governance vote',
  this._getDrepOption(chosenOption) // 'drep' | 'abstain' | 'no_confidence'
);
```

Add a helper method to derive `drepOption`:

```typescript
private _getDrepOption(chosenOption: string): 'drep' | 'abstain' | 'no_confidence' {
  if (chosenOption === 'abstain') return 'abstain';
  if (chosenOption === 'no_confidence') return 'no_confidence';
  // Bech32-encoded DRep ID — map to 'drep' without exposing the raw ID
  return 'drep';
}
```

### Acceptance

- [ ] Both `'Casted governance vote'` analytics calls pass `drepOption` only (never raw DRep ID)
- [ ] Abstain and No-Confidence sentinel strings are also reduced to their kind
- [ ] The `value` argument is omitted for this event
- [ ] CHANGELOG note added: `"analytics: 'Casted governance vote' event action field now carries the drepOption only (drep/abstain/no_confidence) instead of the raw DRep id — telemetry dashboards keyed on event action must be updated."`

---

## task-111: Establish the sanitization regression spy floor

**New files:**
- `tests/jest/security/governance-sanitization.spec.ts`

### Step-by-Step

Create a Jest test that:

1. **Spies on `logger.debug`, `logger.info`, `logger.warn`, `logger.error`** — asserts no call contains a CIP-129/CIP-105 bech32 string or `abstain`/`no_confidence` literal.
2. **Spies on `AnalyticsTracker.sendEvent`** — asserts the `'Casted governance vote'` event action is always one of `'drep' | 'abstain' | 'no_confidence'`, never a raw DRep ID, and that the event does not send a `value` argument.
3. **Uses representative data** — a mock DRep ID (`drep1yg7shg8...`), the abstain/no-confidence sentinels, wallet objects with `delegation.active.voting`, and certificate arrays.

```typescript
// tests/jest/security/governance-sanitization.spec.ts

import { filterLogData } from '../../../source/common/utils/logging';

// CIP-129 test vector
const CIP129_DREP = 'drep1yg7shg8raj8f0q0ra0v6q5q3q6z8qkqz7q9q8q7q6q5q4q3q2q1q0qz7q9q8q7q6q5q4q3q2q1q0qz7q9q8';
// CIP-105 test vectors
const CIP105_KEY = 'drep_vkh1abc123def456ghi789jkl012mno345pqr678stu901vwx234yz';
const CIP105_SCRIPT = 'drep_script1abc123def456ghi789jkl012mno345pqr678stu901vwx234yz';

describe('Governance sanitization', () => {
  describe('filterLogData', () => {
    it('redacts dRepId at any depth', () => {
      const data = {
        delegation: {
          active: { voting: CIP129_DREP },
          next: [{ voting: CIP129_DREP }],
        },
        certificates: [{ vote: CIP129_DREP }],
      };
      const result = filterLogData(data);
      // After redaction, the CIP-129 string should not appear
      const resultStr = JSON.stringify(result);
      expect(resultStr).not.toContain(CIP129_DREP);
    });

    it('redacts vote key at any depth', () => {
      const data = { certificates: [{ vote: CIP129_DREP }] };
      const result = filterLogData(data);
      expect(JSON.stringify(result)).not.toContain(CIP129_DREP);
    });

    it('redacts voting key at any depth', () => {
      const data = { delegation: { active: { voting: CIP129_DREP } } };
      const result = filterLogData(data);
      expect(JSON.stringify(result)).not.toContain(CIP129_DREP);
    });

    it('redacts abstain sentinel', () => {
      const data = { delegation: { active: { voting: 'abstain' } } };
      const result = filterLogData(data);
      expect(JSON.stringify(result)).not.toContain('abstain');
    });

    it('redacts no_confidence sentinel', () => {
      const data = { delegation: { active: { voting: 'no_confidence' } } };
      const result = filterLogData(data);
      expect(JSON.stringify(result)).not.toContain('no_confidence');
    });

    it('redacts CIP-105 key hash DRep ID', () => {
      const data = { delegation: { active: { voting: CIP105_KEY } } };
      const result = filterLogData(data);
      expect(JSON.stringify(result)).not.toContain(CIP105_KEY);
    });

    it('redacts CIP-105 script DRep ID', () => {
      const data = { delegation: { active: { voting: CIP105_SCRIPT } } };
      const result = filterLogData(data);
      expect(JSON.stringify(result)).not.toContain(CIP105_SCRIPT);
    });
  });
});
```

### Acceptance

- [ ] Tests pass with `yarn test:jest`
- [ ] Covers: CIP-129 bech32, CIP-105 key bech32, CIP-105 script bech32, `abstain` literal, `no_confidence` literal
- [ ] Covers: nested paths (`delegation.active.voting`, `delegation.next[*].voting`, `certificates[*].vote`)
- [ ] Test file located under `tests/jest/security/`

---

## Cross-Cutting Acceptance (All Tasks)

After all tasks are complete, run:

```bash
yarn compile          # Zero TypeScript errors
yarn lint             # Zero ESLint errors
yarn prettier:check   # Zero formatting errors
yarn test:jest        # All tests pass
yarn i18n:manage      # i18n keys populated
```

Manual verification:
- [ ] Start Daedalus with a synced node
- [ ] Navigate to Governance → DRep Directory
- [ ] Directory loads with real DRep data
- [ ] Voting power displays correctly (₳-formatted)
- [ ] Refresh button triggers re-query
- [ ] Error state renders when socket is unavailable
- [ ] No DRep IDs or vote literals in console logs
- [ ] Sidebar shows "Governance" label

---

## References

- PRD: [slice-1-PRD.md](./slice-1-PRD.md)
- Plan: [governance-drep-discovery-plan.md](./governance-drep-discovery-plan.md)
- Tasks: [governance-drep-discovery-plan-tasks.json](./governance-drep-discovery-plan-tasks.json)
- Design: [drep-discovery-design.md](./designs/drep-discovery-design.md)
- Shared tokens: [shared-design-tokens.md](./designs/shared-design-tokens.md)
- Mithril IPC reference: `source/main/ipc/mithrilBootstrapChannel.ts`
- Store reference: `source/renderer/app/stores/MithrilBootstrapStore.ts`
