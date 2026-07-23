/**
 * DRep Discovery — Shared Governance Types
 *
 * These types are shared between the Electron main process and the React renderer.
 * All lovelace values are serialized as decimal strings across IPC and rehydrated
 * to BigNumber in the renderer. Never pass raw json-bigint objects through IPC.
 *
 * @see .agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md
 */

// ---- DRep Identity ----

/** Raw bech32-encoded DRep ID as returned by cardano-cli. */
export type DRepId = string;

/**
 * Discriminated DRep identity with all known encodings.
 * Populated by normalizeDRepIdentity (cv-1, task-129).
 */
export interface DRepIdentity {
  /** Original bech32 string from the CLI / wallet API. */
  raw: DRepId;
  /** CIP-129 encoding, derivable from the raw credential. */
  cip129?: string;
  /** CIP-105 encoding, derivable from the raw credential. */
  cip105?: string;
  /** Raw credential hex (without bech32 HRP). */
  credentialHex?: string;
  /** Whether this is a key-hash or script-hash DRep. */
  credentialType: 'key' | 'script';
}

// ---- DRep Registration State ----

export type DRepStatus = 'active' | 'inactive';

/** Remaining epochs until expiry; 0 when inactive, null if unknown. */
export type DrepActivity = number | null;

// ---- Voting Power ----

/**
 * Lovelace amount as a decimal string (e.g., "688964123456").
 * Serialized as string across IPC to preserve precision beyond Number.MAX_SAFE_INTEGER.
 * Rehydrated to BigNumber in the renderer.
 */
export type Lovelace = string;

// ---- DRep Directory Entry (bare, slice-1) ----

export interface DRepDirectoryEntry {
  /** CIP-129 bech32-encoded DRep ID derived from on-chain credential. */
  drepId: DRepId;
  /** Voting power in lovelace as a decimal string; null when no stake is available. */
  votingPower: Lovelace | null;
  /** Active / Inactive. */
  status: DRepStatus;
  /** Remaining epochs until expiry (null if unknown). */
  drepActivity: DrepActivity;
  /** Anchor presence (URL + hash) from on-chain data. No fetch performed in slice-1. */
  anchor: DRepAnchorPresence | null;
}

// ---- Anchor Presence (on-chain reference only, NO fetch in slice-1) ----

export interface DRepAnchorPresence {
  /** The raw anchor URL recorded on-chain. */
  url: string;
  /** Blake2b-256 hex digest of the anchor content. */
  hash: string;
}

// ---- Wallet Governance Status ----

export type GovernanceVoteKind = 'drep' | 'abstain' | 'no_confidence';

/** A wallet's current on-chain governance delegation. */
export interface WalletGovernanceStatus {
  /** The kind of governance vote. null = no delegation. */
  voteKind: GovernanceVoteKind | null;
  /** DRep ID, present only when voteKind === 'drep'. */
  drepId: DRepId | null;
}

// ---- Query Payloads ----

export interface DRepListQueryPayload {
  /** All DRep entries from the ledger state. */
  dreps: DRepDirectoryEntry[];
  /** Unix timestamp (ms) when the data was fetched. */
  fetchedAt: number;
  /** Current epoch number returned by `query tip`; nullable for compatibility. */
  epoch: number | null;
}

export interface DRepStakeQueryPayload {
  /** Voting power in lovelace (decimal string) keyed by CIP-129 DRep id. */
  stakeByDRepId: Record<DRepId, Lovelace>;
  /** Unix timestamp (ms) when the stake distribution was fetched. */
  fetchedAt: number;
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
