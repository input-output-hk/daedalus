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
 * Populated by normalizeDRepIdentity.
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
  /** CIP-119 body.givenName, only ever set from Blake2b-256-verified anchor content. */
  verifiedName: string | null;
}

// ---- Anchor Presence (on-chain reference only, NO fetch in slice-1) ----

export interface DRepAnchorPresence {
  /** The raw anchor URL recorded on-chain. */
  url: string;
  /** Blake2b-256 hex digest of the anchor content. */
  hash: string;
}

// ---- Anchor Fetch (transport outcomes) ----

export enum AnchorFetchErrorType {
  UnsupportedScheme = 'ANCHOR_UNSUPPORTED_SCHEME',
  BlockedAddress = 'ANCHOR_BLOCKED_ADDRESS',
  DnsFailed = 'ANCHOR_DNS_FAILED',
  Redirected = 'ANCHOR_REDIRECTED',
  HttpStatus = 'ANCHOR_HTTP_STATUS',
  ContentType = 'ANCHOR_CONTENT_TYPE',
  TooLarge = 'ANCHOR_TOO_LARGE',
  Timeout = 'ANCHOR_TIMEOUT',
  TlsFailed = 'ANCHOR_TLS_FAILED',
  Network = 'ANCHOR_NETWORK',
  HashMismatch = 'ANCHOR_HASH_MISMATCH',
  ParseFailed = 'ANCHOR_PARSE_FAILED',
  InvalidRequest = 'ANCHOR_INVALID_REQUEST',
}

/** CIP-119 `references[].@type`, normalised to the buckets the detail view renders. */
export type VerifiedDRepReferenceType = 'link' | 'identity' | 'other';

export interface VerifiedDRepReference {
  /** Normalised bucket; unrecognised and missing types collapse to 'other'. */
  type: VerifiedDRepReferenceType;
  /** Human-readable label from the anchor, or null when none was supplied. */
  label: string | null;
  uri: string;
}

/** CIP-119 fields extracted from anchor bytes that passed Blake2b-256 verification. */
export interface VerifiedDRepAnchorContent {
  givenName: string | null;
  objectives: string | null;
  motivations: string | null;
  qualifications: string | null;
  references: VerifiedDRepReference[];
  paymentAddress: string | null;
  /** CIP-119 opt-out from listing. Absent means false; consumed by the cohort filter. */
  doNotList: boolean;
}

export type DRepAnchorResult =
  | {
      status: 'verified';
      content: VerifiedDRepAnchorContent;
      host: string;
      fetchedAt: number;
    }
  | { status: 'unavailable'; reason: AnchorFetchErrorType };

// ---- Wallet Governance Status ----

export type GovernanceVoteKind = 'drep' | 'abstain' | 'no_confidence';

/** A wallet's current on-chain governance delegation. */
export interface WalletGovernanceStatus {
  /** The kind of governance vote. null = no delegation. */
  voteKind: GovernanceVoteKind | null;
  /** DRep ID, present only when voteKind === 'drep'. */
  drepId: DRepId | null;
}
