# ANCHOR-2 Implementation Guide: Anchor Enrichment Completion

Slice: **`anchor-2` — Anchor Enrichment Completion**, six tasks —
**task-157, task-153, task-174, task-154, task-155, task-156**.
Companion docs: [anchor-2-PRD.md](./anchor-2-PRD.md) ·
[anchor-2-code-review.md](./anchor-2-code-review.md).

---

## How to read this guide

**This guide is self-contained.** An implementer reads *this document plus the files it tells them
to edit* — nothing else. Do not open the PRD, do not open
`governance-drep-discovery-plan.md`, do not open the design docs to look up a rule: every locked
invariant, every design decision, every corrected line anchor and every quoted seam that a task
depends on is inlined into that task's section. Where a task genuinely must read a live file (a
design doc it verifies, a spec whose baseline it measures), the section names the file and the
exact lines.

Every `path:line` in this guide was re-read in the worktree at commit `55e8985bf`. **Line anchors
quoted in the tasks JSON and in the design docs are frequently stale**; each task section states
the correction inline and the live file always wins.

One task = one section = one commit. Finish a task's Verify block green before starting the next.

---

## Build order (binding)

```
task-157 → task-153 → task-174 → task-154 → task-155 → task-156
```

This **deviates deliberately from the tasks-JSON listing order** (`153, 174, 154, 155, 156, 157`).
anchor-2 has zero intra-slice dependency edges — every dependency of every row is cross-slice and
already `complete` — so the JSON order encodes nothing and is not a contract. The order above is
forced by file-level coupling:

- **157 before 153** — both extend `VerifiedDRepAnchorContent`
  (`source/common/types/governance.types.ts:93-96`) and `parseVerifiedContent`
  (`source/main/governance/AnchorVerificationService.ts:43-62`). Widening once, in the wider row,
  means task-153 adds one boolean to an already-widened contract instead of the reverse. task-153
  AC-1 is also unsatisfiable for a `doNotList: true` document that omits `givenName` until task-157
  relaxes the parser's `null` return at `:60`.
- **174 and 154 before 155** — task-155 is a sweep; a sweep that runs before the surfaces it audits
  guarantees a second sweep. task-154's confirmation-dialog name line is itself anchor-derived
  content that task-155 AC-2 must confirm carries a label.
- **155 after 157** — task-157 labels every field it adds, so running 155 first would label a
  detail view about to grow five unlabelled fields.
- **156 last** — it verifies the *end state* of the directory, its empty state and the confirmation
  dialog, after five earlier commits have changed all three.

The five parts below appear in build order.

---

## Shared conventions every task inherits

These apply to all six sections and are not repeated as rules inside them. Each task section does
restate the gate commands with its **own measured baselines** — that repetition is deliberate,
because the delta is the contract and the deltas differ per task.

### Code comments

The default is **no comment**. Add one only when the logic or constraint is not self-evident and
cannot be made self-evident by better naming. When warranted: 1-3 lines, plain sentence case,
stating the invariant or the reason — never the *what*, never change history, never a defence of
correctness. **Never cite process artefacts** (task IDs, `CAT-*`/`CP-*` labels, plan names, PR
numbers) in source comments **or in test names**. No ALL-CAPS emphasis. In a spec file the
`describe`/`it` name carries the intent, so comment only a non-obvious fixture or mock constraint.
The live governance code already follows this — see `helpers.ts:177-180`,
`GovernanceStore.ts:404-406`, `DRepDetailAnchorSection.tsx:44-45`.

### Commit messages

One commit per task. A **single Conventional Commits subject line** — no body, no trailers, no
`Co-Authored-By`:

```
<type>(gov): task-NNN <short imperative summary>
```

Task IDs *do* belong in the commit subject (and only there). This matches the live log at
`55e8985bf`, `74bf92cdd`, `351467833`.

### Formatting

`nix` is unavailable in this devcontainer, so the mandated `nix fmt` cannot run and **remains a
user-owned pre-merge obligation**. The substitute, with explicit changed paths only:

```bash
node_modules/.bin/prettier --write <path> [<path> …]
```

**Never `yarn prettier`.** Its `package.json` script embeds a repo-wide `"**/*.*"` glob and
reformats ~250 unrelated files even when handed a path. Never format tool-managed JSON: the tasks
tracker, the locale catalogs, `translations/messages.json`.

### Discarding unwanted changes

```bash
git restore <path>          # or: git checkout -- <path>
```

**Never `git stash`** — the stash stack is shared across worktrees and concurrent sessions.

### Pre-review gate

Run these before opening a task for review. Every task section repeats them with its measured
`baseline → expected` numbers; those numbers, not these commands alone, are the contract.

```bash
# 1. Typecheck. The precompile hook regenerates the gitignored *.scss.d.ts first.
yarn compile
#    Equivalent without the hook:
#      node_modules/.bin/typed-scss-modules source/renderer/app
#      node_modules/.bin/tsc --noEmit
#    tsconfig.json has no "include", so this covers source/, tests/ AND storybook/.

# 2. Focused test runs. `jest tests/jest` is only ~8% of the suite.
node_modules/.bin/jest --testPathPattern=<p> --no-coverage --runInBand
#    The real full run, before the task is called done:
node_modules/.bin/jest --runInBand
#    tests/jest/governance/GovernanceCliArgvSmoke.spec.ts self-skips when cardano-cli is
#    off PATH (1 skipped suite / 12 skipped tests) — expected, not a regression.

# 3. Sanitization floor — the two-anchor rule. Cite BOTH runs together; citing
#    one alone is a false green.
node_modules/.bin/jest --testPathPattern=security/governance-sanitization --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern=<the sibling logging suite named in the task> --no-coverage --runInBand

# 4. Lint the touched surfaces. Errors are the gate; warnings are not — there are
#    ~5591 pre-existing warnings at HEAD and the count rises whenever a new file
#    lands under source/ or storybook/ (tests/ is eslint-ignored).
yarn lint

# 5. i18n — run whenever copy changed, and run it even when a task claims to add
#    zero keys, to prove it.
yarn i18n:manage
#    This WRITES files (defaultMessages.json, whitelist_*.json, the locale catalogs).
#    Revert unintended mutations surgically on paths that were clean at HEAD:
#      git restore <path>          (NEVER git stash)
```

Storybook: `yarn storybook:build` is **red at HEAD** for a pre-existing manager-webpack reason
unrelated to any change (`storybook/main.ts:13` registers a `.tsx` addon into the manager graph
while the swc-loader rule sits in the preview-only `webpackFinal` hook), which makes
`yarn check:all` red too. The usable floor is the dev server, `yarn storybook`.

### Storybook conventions

Never wrap a story in its own `IntlProvider` and never create separate `(en-US)` / `(ja-JP)` story
variants — `storybook/preview.tsx` applies a global `StoryWrapper` decorator driven by the
English/Japanese toggle, and a local `IntlProvider` shadows it. Where a task adds stories, provide
an integrated "Connected flow" story modelled on `Voting / Governance > Connected flow` in
`storybook/stories/voting/Governance.stories.tsx`.

### Environment facts (given — do not re-derive)

No `nix`, no `gh`, no git push credentials: work stays local. `.vscode/` is gitignored and absent.
Any visual pass (browser rendering, ja-JP overflow) cannot execute here and is **owed, never
reported green**.

---

## task-157 — Render remaining verified CIP-119 profile fields

**Build position:** 1 of 6 in `anchor-2`. Nothing in this slice precedes it.
**Estimate:** 5 h. **Priority:** high. **Dependencies:** task-151 ✔, task-152 ✔.
**Commit subject (single line, no body, no trailers):**
`feat(gov): task-157 render the remaining verified cip-119 profile fields`

### 1. What this task is

This is a **full-stack change**, not a renderer-only change. The tasks-JSON
`targetPath` says `source/renderer/app/components/` — that is indicative only.
Five of the six CIP-119 fields this slice needs are neither parsed in main nor
carried across IPC today. Verified at `55e8985bf`,
`source/main/governance/AnchorVerificationService.ts:57-61` extracts exactly one
field:

```ts
  const givenName = readCip119String(
    (body as Record<string, unknown>).givenName
  );
  if (givenName === null) return null;
  return { givenName };
```

and `source/common/types/governance.types.ts:93-96` is:

```ts
/** CIP-119 fields extracted from anchor bytes that passed Blake2b-256 verification. */
export interface VerifiedDRepAnchorContent {
  givenName: string | null;
}
```

task-157 therefore widens the whole pipeline **once**: the wire type, the main
parser, the log-redaction key list, the store's `AnchorEnrichEntry`, and the
detail-view render.

### 2. What this task is NOT — read this before you start

These boundaries exist because five other tasks land after this one and they
own the surfaces listed here. Crossing a boundary produces a duplicate edit,
which is a defect.

- **DETAIL VIEW ONLY.** AC-2 ("Delegation confirmation shows the verified
  display name only when verified metadata is available") is **jointly
  discharged**. task-157 makes the data available; the confirmation-dialog
  gating logic ships in **task-154's** commit. **Do not open
  `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx`
  in this task.** Record the hand-off in the task's `statusReason` instead.
- **You add `doNotList` to the wire type and the parser, and you stop there.**
  You do **not** project it onto `AppDRepDirectoryEntry`, you do **not** touch
  `defaultCohort`, `showAllList`, `isStaleFavorite`, or any filter/sort/search
  code. That is task-153's work and it consumes what you build.
- **No `image` / `imageObject`.** It is deferred for this release. The deferral
  is *already written* at `.agent/plans/governance/drep-discovery/designs/drep-discovery-design.md:218`.
  Verify it is there; do **not** add a second paragraph.
- **No verified content on `DRepCard`.** `drep-discovery-design.md:216` —
  "`DRepCard` does **not** render verified anchor content even after
  anchor-1/anchor-2 (cards stay on-chain-only)". Reinforced at `:251-259`.
- **No new IPC channel.** `GOVERNANCE_DREP_ANCHOR_CHANNEL`
  (`source/common/ipc/api.ts:670-672`) already carries `DRepAnchorResult`; you
  widen the payload type it already references, nothing else.
- **No new `DRepSourceLabel` variant.** Compose the existing
  `verified-off-chain` variant.
- **No section-level on-chain label on `DRepDetailOnchainSection`.** That is
  task-155's single edit.
- **No dual CIP-129/CIP-105 identity rendering.** That is task-174.
- **`paymentAddress` never pre-populates a send-form or delegation-form field.**
  It is read-only text plus a copy button. Nothing else.

### 3. Locked invariants this change must not break (inlined — do not look them up)

1. **Local-first.** Discovery data comes only from the local node via the
   main-process `GovernanceQueryService`. No hosted explorers, indexers,
   GovTool, Koios, Blockfrost or public governance APIs. The per-DRep anchor
   fetch is the one sanctioned outbound path and it stays in main.
2. **Sanitization floor.** No DRep id, no `abstain`/`no_confidence` literal, no
   CIP-129/CIP-105 bech32 string in any logger, analytics or electron-store
   payload — re-asserted via the task-111 spy suite in every slice. The
   task-168 DRep-state snapshot is the one documented exception. **A
   `paymentAddress` is a bech32 address: treat it as sensitive-shaped even
   though it is public data.**
3. **Anchor transport-security floor, never thinned.** TLS on, redirects off,
   <=10s timeouts, ~1 MB cap, JSON content-type allow-list, SSRF +
   DNS-rebinding mitigation, Blake2b-256 hash-verify before parse/cache/render,
   immutable hash-keyed cache. **No anchor-derived content renders without
   verification AND a verified off-chain source label.** Anchor URLs — and
   every reference URI you add — open only through the HTTPS-only-hardened
   `open-external-url` path from task-152.
4. **No second delegation backend.** Selection supplies a DRep ID to the
   existing `delegateVotes` / `VotingStore` signing paths via React Router
   `location.state` only. `VotingStore` never reads `GovernanceStore` directly.
5. **Lovelace losslessness.** json-bigint lossless parse -> decimal-string IPC
   -> renderer `BigNumber` rehydration. Never route raw JSONbig objects across
   IPC or into observables. (You add only strings and one boolean; do not
   disturb the existing rehydration at `GovernanceStore.ts:505-514`.)
9. **No auto-delegation.** A verified profile field is display only. Nothing
   here initiates or pre-selects a delegation.
11. **Preliminary copy.** Every new en-US and ja-JP string keeps the leading
    `!!!` marker. Removing `!!!` is a release-end manual review, never a
    per-slice task.

Two design-doc rules that are equally binding here:

- `.agent/plans/governance/drep-discovery/designs/shared-design-tokens.md:45`:
  "Every rendered field gets an explicit provenance label. This is the single
  most important anti-misleading-content control."
- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md:335`:
  "Hash verification proves only that the registrant authored the blob — which
  an impersonator satisfies exactly — so it is not on its own a mitigation for
  a claimed identity." **This is the entire reason AC-4 and AC-5 exist.**

### 4. Corrected line anchors (the tasks JSON and the design docs are stale)

| cited in | stale anchor | correct anchor at `55e8985bf` |
|---|---|---|
| task-157 AC-1 | `drep-discovery-design.md:215` (render list) | **`:216`** |
| task-157 AC-1 | image deferral "recorded in that design doc" | **`:218`** — already present |
| references `@type` rule | — | `drep-discovery-design.md:220-224` |
| `paymentAddress` rule | — | `drep-discovery-design.md:226` |

### 5. Design decisions already made — implement them, do not re-derive

- **D-A `references` is a required array, never `| null`.** Every other new
  field is `| null`. `references` is `VerifiedDRepReference[]`, empty when the
  anchor carries none. An empty array and a null array render identically, and
  a nullable array doubles the guard at every call site. It is a *required*
  property, so no construction site can silently omit it — which is the reason
  the codebase prefers `| null` over `?` in the first place (see
  `governance.types.ts:55`, `:61`).
- **D-B `doNotList` is `boolean`, not `boolean | null`.** CIP-119 treats an
  absent `doNotList` as "not opted out", so absent and `false` are the same
  answer. task-153 distinguishes "never fetched" from "fetched and false" by
  the presence of an `anchorStateByDRepId` entry, not by a third value here.
- **D-C The 80-character clamp is a `givenName` rule and must not spread.**
  `readCip119String` is generic but `GIVEN_NAME_MAX_LENGTH = 80`
  (`AnchorVerificationService.ts:17`) is not. Reusing it unchanged for
  `objectives`/`motivations`/`qualifications` would silently clip long-form
  prose at 80 characters — CIP-119 caps none of them. Step 2 introduces an
  explicit per-field bound. **The `paymentAddress` bound rejects rather than
  clamps**: the value is rendered *with a copy button*, and a truncated address
  a user can copy is worse than an absent one.
- **D-D `AnchorEnrichEntry` carries the whole verified payload and exactly one
  name.** After this task the verified member is
  `{ state: 'verified'; hash; host; content: VerifiedDRepAnchorContent }`. The
  store clamps `content.givenName` on ingest with the existing
  `clampVerifiedName`. There is no separate `givenName` member left, so there
  is no "which name do I read" question anywhere in the renderer. **The detail
  view's single source of truth for every verified field is
  `state.content.<field>` off `AnchorEnrichEntry`** — not
  `AppDRepDirectoryEntry.verifiedName`, which stays exactly as it is for
  task-153/154 to consume on their own surfaces.
- **D-E The renderer-side https gate is lifted into one shared predicate.**
  `DRepDetailAnchorSection.tsx:44-52` already holds it; you move it to
  `source/renderer/app/utils/governance/isHttpsUrl.ts` and import it in both
  places rather than copying a three-line security guard. The gate is
  load-bearing, not belt-and-braces: main's rejection is fire-and-forget, so a
  non-https URL offered as a link would surface as an unhandled promise
  rejection instead of a visible error.
- **D-F Label placement.** One `<DRepSourceLabel source="verified-off-chain">`
  per rendered field row (name, objectives, motivations, qualifications,
  payment address) and one per `references` sub-section heading (Links,
  Claimed identities, Other references). Not one per reference entry — that
  would put up to twenty pills on one screen and fight the §2 "small pill"
  visual.
- **D-G The shipped `anchorContent.caption` copy is not edited.** Its text is
  "This name is the DRep's own claim…", so it renders **only when a name is
  rendered**. Do not reword it, do not make it a general profile caption; that
  would churn both locale catalogs for no acceptance criterion.
- **D-H A verified parse counts as completed metadata even with no
  `givenName`.** Step 2 relaxes the parser, so a CIP-119 body that carries
  prose but no name now resolves `verified` where it previously resolved
  `unavailable`. `verifiedMetadataIds` (`GovernanceStore.ts:283-291`) keys on
  `entry.state === 'verified'` and nothing else, and this task does **not**
  change that predicate to require a name. Consequences, all intended:
  - `cohortContext.verifiedMetadataIds` (`:245-252`) gains those DReps, so the
    "completed metadata when available" leg of invariant 7's eligibility rule
    now treats a nameless verified anchor as complete;
  - the task-172 **High value** category badge, which reads that same set,
    can now classify a nameless-but-verified DRep as high value.

  The alternative — requiring `givenName` for completeness — would re-introduce
  exactly the anchor-1 behaviour Step 2 exists to remove, and would make the
  cohort's metadata signal depend on one optional CIP-119 field rather than on
  whether the bytes hash-matched. The badge stays informational only
  (invariant 8): it reorders and filters nothing. **Proof of this decision is
  the second assertion in Step 10c's `keeps verifiedName null when the anchor
  carries no givenName` test** —
  `expect(store.verifiedMetadataIds.has(ANCHOR_DREP_ID)).toBe(true)` — which is
  deliberate, not incidental. Do not weaken it to `.toBe(false)` if the
  behaviour surprises you; change nothing and re-read this bullet.

---

### Step 1 — Widen `VerifiedDRepAnchorContent`

**File:** `source/renderer/app/../../common/types/governance.types.ts` →
absolute: `source/common/types/governance.types.ts`

**Replace lines 93-96 exactly.** Current text:

```ts
/** CIP-119 fields extracted from anchor bytes that passed Blake2b-256 verification. */
export interface VerifiedDRepAnchorContent {
  givenName: string | null;
}
```

New text:

```ts
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
```

Nothing else in this file changes. `DRepAnchorResult` at `:98-105` already
references `VerifiedDRepAnchorContent` and widens with it.

---

### Step 2 — Relax and widen `parseVerifiedContent`

**File:** `source/main/governance/AnchorVerificationService.ts`

This is a **behavioural change to an anchor-1 file** and it is deliberate: today
a CIP-119 document that carries `doNotList: true` but no `givenName` resolves as
`ParseFailed` → `unavailable`, so the DRep stays in the default cohort — the
exact opposite of what its author asked for. After this step, a body that parses
as a JSON object always yields content, every field independently nullable, and
each renderer guards its own field. The digest gate is untouched: unverified
bytes still never reach `JSON.parse` (see `:85-92` and the comment at `:64-65`).

#### 2a. Import the new reference type

**Replace lines 3-7:**

```ts
import type {
  DRepAnchorPresence,
  DRepAnchorResult,
  VerifiedDRepAnchorContent,
} from '../../common/types/governance.types';
```

with:

```ts
import type {
  DRepAnchorPresence,
  DRepAnchorResult,
  VerifiedDRepAnchorContent,
  VerifiedDRepReference,
  VerifiedDRepReferenceType,
} from '../../common/types/governance.types';
```

#### 2b. Replace the length constant at `:17`

**Replace line 17:**

```ts
const GIVEN_NAME_MAX_LENGTH = 80;
```

with:

```ts
// CIP-119 caps only givenName. The other bounds are rendering-safety limits so
// one hostile anchor cannot produce an unbounded detail view; the transport's
// body cap bounds the aggregate, these bound the individual field.
const GIVEN_NAME_MAX_LENGTH = 80;
const PROSE_MAX_LENGTH = 1000;
const REFERENCE_LABEL_MAX_LENGTH = 200;
const REFERENCE_URI_MAX_LENGTH = 2048;
const MAX_REFERENCES = 20;
const PAYMENT_ADDRESS_MAX_LENGTH = 128;
```

#### 2c. Replace the whole block from `:29` through `:62`

Current text (lines 29-62) — quoted so you do not have to hunt:

```ts
function readCip119String(raw: unknown): string | null {
  let value: string | null = null;
  if (typeof raw === 'string') {
    value = raw;
  } else if (raw !== null && typeof raw === 'object') {
    const wrapped = (raw as Record<string, unknown>)['@value'];
    if (typeof wrapped === 'string') value = wrapped;
  }
  if (value === null) return null;
  const trimmed = value.trim();
  if (trimmed === '') return null;
  return trimmed.slice(0, GIVEN_NAME_MAX_LENGTH);
}

function parseVerifiedContent(bytes: Buffer): VerifiedDRepAnchorContent | null {
  let parsed: unknown;
  try {
    parsed = JSON.parse(bytes.toString('utf8'));
  } catch {
    return null;
  }
  if (parsed === null || typeof parsed !== 'object' || Array.isArray(parsed)) {
    return null;
  }
  const body = (parsed as Record<string, unknown>).body;
  if (body === null || typeof body !== 'object' || Array.isArray(body)) {
    return null;
  }
  const givenName = readCip119String(
    (body as Record<string, unknown>).givenName
  );
  if (givenName === null) return null;
  return { givenName };
}
```

Replacement:

```ts
function readCip119Raw(raw: unknown): string | null {
  let value: string | null = null;
  if (typeof raw === 'string') {
    value = raw;
  } else if (raw !== null && typeof raw === 'object') {
    const wrapped = (raw as Record<string, unknown>)['@value'];
    if (typeof wrapped === 'string') value = wrapped;
  }
  if (value === null) return null;
  const trimmed = value.trim();
  return trimmed === '' ? null : trimmed;
}

function readCip119String(raw: unknown, maxLength: number): string | null {
  const value = readCip119Raw(raw);
  return value === null ? null : value.slice(0, maxLength);
}

// The address is rendered with a copy button, so an over-length value is dropped
// rather than clamped: a truncated address a user can copy is worse than none.
function readPaymentAddress(raw: unknown): string | null {
  const value = readCip119Raw(raw);
  if (value === null) return null;
  return value.length > PAYMENT_ADDRESS_MAX_LENGTH ? null : value;
}

function readCip119Boolean(raw: unknown): boolean {
  if (typeof raw === 'boolean') return raw;
  if (raw !== null && typeof raw === 'object') {
    const wrapped = (raw as Record<string, unknown>)['@value'];
    if (typeof wrapped === 'boolean') return wrapped;
  }
  return false;
}

// An Identity claim outranks a Link claim so the cautionary sub-section wins
// whenever an anchor asserts both; anything unrecognised collapses to 'other',
// which the renderer treats as a plain link and never as a claimed identity.
function readReferenceType(raw: unknown): VerifiedDRepReferenceType {
  const candidates = Array.isArray(raw) ? raw : [raw];
  let sawLink = false;
  for (const candidate of candidates) {
    const value = readCip119Raw(candidate);
    if (value === null) continue;
    const localName = value.toLowerCase().split(/[:/#]/).pop() ?? '';
    if (localName === 'identity') return 'identity';
    if (localName === 'link') sawLink = true;
  }
  return sawLink ? 'link' : 'other';
}

function readCip119References(raw: unknown): VerifiedDRepReference[] {
  if (!Array.isArray(raw)) return [];
  const references: VerifiedDRepReference[] = [];
  for (const item of raw) {
    if (references.length >= MAX_REFERENCES) break;
    if (item === null || typeof item !== 'object' || Array.isArray(item)) {
      continue;
    }
    const record = item as Record<string, unknown>;
    const uri = readCip119Raw(record.uri);
    if (uri === null || uri.length > REFERENCE_URI_MAX_LENGTH) continue;
    references.push({
      type: readReferenceType(record['@type']),
      label: readCip119String(record.label, REFERENCE_LABEL_MAX_LENGTH),
      uri,
    });
  }
  return references;
}

// Every CIP-119 field is optional. A document that omits givenName still carries
// doNotList and the profile fields, so it parses; each renderer guards its own
// field rather than the whole document failing on one absent value.
function parseVerifiedContent(bytes: Buffer): VerifiedDRepAnchorContent | null {
  let parsed: unknown;
  try {
    parsed = JSON.parse(bytes.toString('utf8'));
  } catch {
    return null;
  }
  if (parsed === null || typeof parsed !== 'object' || Array.isArray(parsed)) {
    return null;
  }
  const body = (parsed as Record<string, unknown>).body;
  if (body === null || typeof body !== 'object' || Array.isArray(body)) {
    return null;
  }
  const fields = body as Record<string, unknown>;
  return {
    givenName: readCip119String(fields.givenName, GIVEN_NAME_MAX_LENGTH),
    objectives: readCip119String(fields.objectives, PROSE_MAX_LENGTH),
    motivations: readCip119String(fields.motivations, PROSE_MAX_LENGTH),
    qualifications: readCip119String(
      fields.qualifications,
      PROSE_MAX_LENGTH
    ),
    references: readCip119References(fields.references),
    paymentAddress: readPaymentAddress(fields.paymentAddress),
    doNotList: readCip119Boolean(fields.doNotList),
  };
}
```

**What still returns `ParseFailed` after this step** (do not weaken these):
bytes that are not valid JSON; a top-level value that is not a plain object; a
missing, null, non-object or array `body`. Only the "no `givenName`" rejection
is removed.

**Do not touch** `resolveFromCacheOrFetch` (`:66-95`) or `resolveVerifiedAnchor`
(`:97-122`). Nothing about the cache, the digest check or the in-flight dedupe
changes.

---

### Step 3 — Extend the log-redaction key list

**File:** `source/common/utils/logging.ts`

The list at `:44-63` already carries `drepId`, `dRepId`, `vote`, `voting`,
`drepIdentity`, `currentVote`, `votingTarget`, `chosenOption`, `raw`, `cip105`,
`cip129`, `credentialHex`, `anchorUrl`, `anchorContent`, `givenName`,
`verifiedName`. It carries none of the six names this task introduces.

**Replace lines 62-63:**

```ts
    'givenName',
    'verifiedName',
```

with:

```ts
    'givenName',
    'verifiedName',
    'objectives',
    'motivations',
    'qualifications',
    'references',
    'paymentAddress',
    'doNotList',
```

`filterLogData` recurses by key name at any depth and removes the key entirely,
so redacting `references` removes the whole array including every `uri` and
`label` inside it. Do not add `uri` or `label` to this list — they are generic
names used across the app and a blanket removal there would silently strip
unrelated diagnostics.

---

### Step 4 — Widen `AnchorEnrichEntry` and the store ingest

**File:** `source/renderer/app/stores/GovernanceStore.ts`

#### 4a. Import the content type

Lines `:10-16` are the single import block from
`'../../../common/types/governance.types'`:

```ts
import {
  GovernanceQueryErrorType,
  AnchorFetchErrorType,
  DRepDirectoryEntry,
  DRepAnchorPresence,
  DRepAnchorResult,
} from '../../../common/types/governance.types';
```

Add `VerifiedDRepAnchorContent` to that list (after `DRepAnchorResult`). Do not
split it into a separate `import type` statement — the file mixes value and type
imports in this one block and `AnchorFetchErrorType` is a runtime enum.

#### 4b. Replace lines 49-52

Current:

```ts
export type AnchorEnrichEntry =
  | { state: 'loading'; hash: string }
  | { state: 'verified'; hash: string; givenName: string | null; host: string }
  | { state: 'unavailable'; hash: string; reason: AnchorFetchErrorType };
```

New:

```ts
export type AnchorEnrichEntry =
  | { state: 'loading'; hash: string }
  | {
      state: 'verified';
      hash: string;
      host: string;
      content: VerifiedDRepAnchorContent;
    }
  | { state: 'unavailable'; hash: string; reason: AnchorFetchErrorType };
```

#### 4c. Replace the verified-branch construction at `:434-442`

Current:

```ts
      const next: AnchorEnrichEntry =
        result.status === 'verified'
          ? {
              state: 'verified',
              hash: anchor.hash,
              givenName: clampVerifiedName(result.content.givenName),
              host: result.host,
            }
          : { state: 'unavailable', hash: anchor.hash, reason: result.reason };
```

New:

```ts
      const next: AnchorEnrichEntry =
        result.status === 'verified'
          ? {
              state: 'verified',
              hash: anchor.hash,
              host: result.host,
              content: {
                ...result.content,
                givenName: clampVerifiedName(result.content.givenName),
              },
            }
          : { state: 'unavailable', hash: anchor.hash, reason: result.reason };
```

The clamp stays in the renderer even though main now bounds `givenName` too:
`MAX_VERIFIED_NAME_LENGTH` (`:86`) exists because "nothing on the wire enforces
it", and the renderer clamp is what adds the trailing ellipsis the existing
spec asserts.

#### 4d. Update the single reader at `:532`

Inside `_applyVerifiedNames` (`:521-538`), replace:

```ts
          ? state.givenName
```

with:

```ts
          ? state.content.givenName
```

Leave everything else in `_applyVerifiedNames` alone — the hash guard at
`:530-531` and the JSDoc at `:516-520` are unchanged, and **you do not rename
this method** (task-153 renames it when it starts applying `doNotList` too).

**Do not add a `doNotList` field to `AppDRepDirectoryEntry` (`:23-36`).** That
projection belongs to task-153.

---

### Step 5 — Lift the https gate into a shared predicate

#### 5a. New file `source/renderer/app/utils/governance/isHttpsUrl.ts`

```ts
/**
 * Main rejects every non-https external URL and the rejection is fire-and-forget,
 * so a non-https link would silently do nothing. The renderer therefore offers a
 * link only for schemes main will actually open, and renders the rest as text.
 */
export function isHttpsUrl(url: string): boolean {
  try {
    return new URL(url).protocol === 'https:';
  } catch {
    return false;
  }
}
```

#### 5b. `source/renderer/app/components/governance/drep-detail/DRepDetailAnchorSection.tsx`

**Delete lines 44-52** (the local copy):

```ts
// The renderer offers the link only for schemes main will actually open, so a
// non-https anchor stays inert text instead of a link that does nothing.
const isHttpsAnchorUrl = (url: string): boolean => {
  try {
    return new URL(url).protocol === 'https:';
  } catch {
    return false;
  }
};
```

**Add the import** after line 4 (`import DRepDetailAnchorContent …`):

```ts
import { isHttpsUrl } from '../../../utils/governance/isHttpsUrl';
```

**At `:76`** change `isHttpsAnchorUrl(anchor.url)` to `isHttpsUrl(anchor.url)`.

**At `:111`** change the mount point so the content block can open reference
URIs through the same handler:

```tsx
          <DRepDetailAnchorContent
            state={anchorState}
            onOpenExternalLink={onOpenExternalLink}
          />
```

`onOpenExternalLink` is already a prop of this component (`:40`, destructured at
`:57`) and already arrives from `DRepDetail.tsx:123` ←
`DRepDetailPage.tsx` `onOpenExternalLink={stores.app.openExternalLink}`. No
change is needed in `DRepDetail.tsx` or `DRepDetailPage.tsx`.

---

### Step 6 — Rewrite `DRepDetailAnchorContent.tsx`

**File:**
`source/renderer/app/components/governance/drep-detail/DRepDetailAnchorContent.tsx`

Replace the whole file. The five existing `defineMessages` entries keep their
ids and their exact `defaultMessage` strings — do not reword them.

```tsx
import React, { useCallback, useState } from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepSourceLabel from '../_shared/DRepSourceLabel';
import { isHttpsUrl } from '../../../utils/governance/isHttpsUrl';
import type { AnchorEnrichEntry } from '../../../stores/GovernanceStore';
import type { VerifiedDRepReference } from '../../../../../common/types/governance.types';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDetail.anchorContent.title',
    defaultMessage: '!!!Off-chain profile',
    description: 'Heading of the verified anchor content block',
  },
  givenName: {
    id: 'governance.drepDetail.anchorContent.givenName',
    defaultMessage: '!!!Name',
    description: 'Label for the verified CIP-119 givenName field',
  },
  objectives: {
    id: 'governance.drepDetail.anchorContent.objectives',
    defaultMessage: '!!!Objectives',
    description: 'Label for the verified CIP-119 objectives field',
  },
  motivations: {
    id: 'governance.drepDetail.anchorContent.motivations',
    defaultMessage: '!!!Motivations',
    description: 'Label for the verified CIP-119 motivations field',
  },
  qualifications: {
    id: 'governance.drepDetail.anchorContent.qualifications',
    defaultMessage: '!!!Qualifications',
    description: 'Label for the verified CIP-119 qualifications field',
  },
  referencesTitle: {
    id: 'governance.drepDetail.anchorContent.references.title',
    defaultMessage: '!!!References',
    description: 'Heading of the verified CIP-119 references block',
  },
  referencesLinks: {
    id: 'governance.drepDetail.anchorContent.references.links',
    defaultMessage: '!!!Links',
    description: 'Sub-heading for references typed as Link',
  },
  referencesIdentity: {
    id: 'governance.drepDetail.anchorContent.references.identity',
    defaultMessage: '!!!Claimed identities',
    description: 'Sub-heading for references typed as Identity',
  },
  referencesIdentityCaption: {
    id: 'governance.drepDetail.anchorContent.references.identityCaption',
    defaultMessage:
      '!!!These identities are claimed by the DRep and are not verified by Daedalus. Open the link and confirm that this DRep ID is published there before you rely on it.',
    description:
      'Caption stating that an Identity reference is a claim, not a verified identity',
  },
  referencesOther: {
    id: 'governance.drepDetail.anchorContent.references.other',
    defaultMessage: '!!!Other references',
    description:
      'Sub-heading for references whose type is missing or unrecognised',
  },
  paymentAddressLabel: {
    id: 'governance.drepDetail.anchorContent.paymentAddress.label',
    defaultMessage: '!!!Stated payment address',
    description: 'Label for the verified CIP-119 paymentAddress field',
  },
  paymentAddressCaption: {
    id: 'governance.drepDetail.anchorContent.paymentAddress.caption',
    defaultMessage:
      "!!!This address is the DRep's own claim. Delegating your voting power requires no payment to any address.",
    description:
      'Caption warning that no payment is required to delegate voting power',
  },
  paymentAddressCopyButton: {
    id: 'governance.drepDetail.anchorContent.paymentAddress.copyButton',
    defaultMessage: '!!!Copy',
    description: 'Copy button label for the stated payment address',
  },
  paymentAddressCopyLabel: {
    id: 'governance.drepDetail.anchorContent.paymentAddress.copyLabel',
    defaultMessage: '!!!Copy stated payment address',
    description: 'Accessible label for the payment address copy button',
  },
  paymentAddressCopiedToast: {
    id: 'governance.drepDetail.anchorContent.paymentAddress.copiedToast',
    defaultMessage: '!!!Payment address copied',
    description: 'Inline confirmation shown after copying the payment address',
  },
  loading: {
    id: 'governance.drepDetail.anchorContent.loading',
    defaultMessage: '!!!Checking the anchor…',
    description: 'Shown while the anchor is being fetched and verified',
  },
  unavailable: {
    id: 'governance.drepDetail.anchorContent.unavailable',
    defaultMessage:
      '!!!The off-chain profile could not be verified. Only on-chain data is shown.',
    description: 'Shown when the anchor fetch or hash verification failed',
  },
  caption: {
    id: 'governance.drepDetail.anchorContent.caption',
    defaultMessage:
      "!!!This name is the DRep's own claim, hash-matched to the anchor recorded on-chain. Daedalus does not verify identity.",
    description:
      'Caption stating that a verified name is not verified identity',
  },
});

interface Props {
  state: AnchorEnrichEntry | null;
  onOpenExternalLink: (url: string) => void;
  intl: intlShape.isRequired;
}

function VerifiedFieldRow({
  host,
  label,
  value,
}: {
  host: string;
  label: string;
  value: string;
}) {
  return (
    <div className={styles.fieldRow}>
      <dt className={styles.fieldLabel}>{label}</dt>
      <dd className={styles.fieldValue}>
        {value}{' '}
        <DRepSourceLabel
          source="verified-off-chain"
          host={host}
          className={styles.sourceLabel}
        />
      </dd>
    </div>
  );
}

function ReferenceList({
  onOpenExternalLink,
  references,
}: {
  onOpenExternalLink: (url: string) => void;
  references: VerifiedDRepReference[];
}) {
  return (
    <ul className={styles.referenceList}>
      {references.map((reference, index) => (
        <li
          className={styles.referenceItem}
          key={`${reference.uri}-${index}`} // eslint-disable-line react/no-array-index-key
        >
          {isHttpsUrl(reference.uri) ? (
            <a
              href={reference.uri}
              target="_blank"
              rel="noopener noreferrer"
              onClick={(event: React.MouseEvent<HTMLAnchorElement>) => {
                event.preventDefault();
                onOpenExternalLink(reference.uri);
              }}
            >
              {reference.label ?? reference.uri}
            </a>
          ) : (
            <span className={styles.anchorValue}>
              {reference.label ?? reference.uri}
            </span>
          )}
        </li>
      ))}
    </ul>
  );
}

function DRepDetailAnchorContent({ state, onOpenExternalLink, intl }: Props) {
  const [addressCopied, setAddressCopied] = useState(false);
  const paymentAddress =
    state != null && state.state === 'verified'
      ? state.content.paymentAddress
      : null;

  // Nothing on this path is logged: a payment address is a bech32 string and the
  // sanitization floor forbids it in any logger payload, including a length.
  const handleCopyPaymentAddress = useCallback(() => {
    if (paymentAddress == null) return;
    if (!navigator.clipboard || !navigator.clipboard.writeText) return;
    navigator.clipboard
      .writeText(paymentAddress)
      .then(() => setAddressCopied(true))
      .catch(() => undefined);
  }, [paymentAddress]);

  if (!state) return null;

  if (state.state === 'loading') {
    return (
      <p className={styles.mutedValue}>
        {intl.formatMessage(messages.loading)}
      </p>
    );
  }

  if (state.state === 'unavailable') {
    return (
      <p className={styles.mutedValue}>
        {intl.formatMessage(messages.unavailable)}{' '}
        <DRepSourceLabel
          source="anchor-unavailable"
          className={styles.sourceLabel}
        />
      </p>
    );
  }

  const { content, host } = state;
  const linkReferences = content.references.filter((r) => r.type === 'link');
  const identityReferences = content.references.filter(
    (r) => r.type === 'identity'
  );
  const otherReferences = content.references.filter((r) => r.type === 'other');
  const hasFieldRows =
    content.givenName != null ||
    content.objectives != null ||
    content.motivations != null ||
    content.qualifications != null;
  const hasAnyContent =
    hasFieldRows ||
    content.references.length > 0 ||
    content.paymentAddress != null;

  if (!hasAnyContent) return null;

  return (
    <>
      <h3 className={styles.sectionTitle}>
        {intl.formatMessage(messages.title)}
      </h3>
      {hasFieldRows && (
        <dl className={styles.fieldList}>
          {content.givenName != null && (
            <VerifiedFieldRow
              host={host}
              label={intl.formatMessage(messages.givenName)}
              value={content.givenName}
            />
          )}
          {content.objectives != null && (
            <VerifiedFieldRow
              host={host}
              label={intl.formatMessage(messages.objectives)}
              value={content.objectives}
            />
          )}
          {content.motivations != null && (
            <VerifiedFieldRow
              host={host}
              label={intl.formatMessage(messages.motivations)}
              value={content.motivations}
            />
          )}
          {content.qualifications != null && (
            <VerifiedFieldRow
              host={host}
              label={intl.formatMessage(messages.qualifications)}
              value={content.qualifications}
            />
          )}
        </dl>
      )}
      {content.givenName != null && (
        <p className={styles.mutedValue}>
          {intl.formatMessage(messages.caption)}
        </p>
      )}
      {content.references.length > 0 && (
        <>
          <h4 className={styles.subSectionTitle}>
            {intl.formatMessage(messages.referencesTitle)}
          </h4>
          {linkReferences.length > 0 && (
            <>
              <h5 className={styles.subSectionTitle}>
                {intl.formatMessage(messages.referencesLinks)}{' '}
                <DRepSourceLabel
                  source="verified-off-chain"
                  host={host}
                  className={styles.sourceLabel}
                />
              </h5>
              <ReferenceList
                onOpenExternalLink={onOpenExternalLink}
                references={linkReferences}
              />
            </>
          )}
          {identityReferences.length > 0 && (
            <>
              <h5 className={styles.subSectionTitle}>
                {intl.formatMessage(messages.referencesIdentity)}{' '}
                <DRepSourceLabel
                  source="verified-off-chain"
                  host={host}
                  className={styles.sourceLabel}
                />
              </h5>
              <p className={styles.mutedValue}>
                {intl.formatMessage(messages.referencesIdentityCaption)}
              </p>
              <ReferenceList
                onOpenExternalLink={onOpenExternalLink}
                references={identityReferences}
              />
            </>
          )}
          {otherReferences.length > 0 && (
            <>
              <h5 className={styles.subSectionTitle}>
                {intl.formatMessage(messages.referencesOther)}{' '}
                <DRepSourceLabel
                  source="verified-off-chain"
                  host={host}
                  className={styles.sourceLabel}
                />
              </h5>
              <ReferenceList
                onOpenExternalLink={onOpenExternalLink}
                references={otherReferences}
              />
            </>
          )}
        </>
      )}
      {content.paymentAddress != null && (
        <>
          <h4 className={styles.subSectionTitle}>
            {intl.formatMessage(messages.paymentAddressLabel)}{' '}
            <DRepSourceLabel
              source="verified-off-chain"
              host={host}
              className={styles.sourceLabel}
            />
          </h4>
          <p className={styles.paymentAddressValue}>
            <span>{content.paymentAddress}</span>
            <Button
              className={styles.copyButton}
              onClick={handleCopyPaymentAddress}
              label={intl.formatMessage(messages.paymentAddressCopyButton)}
              skin={ButtonSkin}
              aria-label={intl.formatMessage(messages.paymentAddressCopyLabel)}
            />
            {addressCopied && (
              <span
                className={styles.copiedConfirmation}
                role="status"
                aria-live="polite"
              >
                {intl.formatMessage(messages.paymentAddressCopiedToast)}
              </span>
            )}
          </p>
        </>
      )}
    </>
  );
}

export default injectIntl(DRepDetailAnchorContent);
```

**Ordering rule you must preserve:** the identity sub-section renders its
caption *before* its list, and an `identity` reference is never emitted into
`linkReferences` or `otherReferences`. AC-4 tests this directly.

**AC-6 rule you must preserve:** the payment address is a `<span>` plus a copy
`<Button>`. It is never an `<input>`, never a form field, and its value is
never passed to `onSelectForDelegation`, `history.push` state, or any voting
component.

---

### Step 7 — SCSS additions

**File:** `source/renderer/app/components/governance/drep-detail/DRepDetail.scss`

Append these five classes (the file currently ends at `.errorMessage`, line
109-112). Reuse existing classes everywhere else — `.sectionTitle`,
`.fieldList`, `.fieldRow`, `.fieldLabel`, `.fieldValue`, `.mutedValue`,
`.anchorValue` and `.sourceLabel` all already exist.

```scss
.subSectionTitle {
  margin: 12px 0 6px;
  font-size: 13px;
  font-weight: 600;
  color: var(--theme-text-secondary, #6b7384);
}

.referenceList {
  display: flex;
  flex-direction: column;
  gap: 6px;
  margin: 0;
  padding: 0;
  font-size: 14px;
  list-style: none;
  word-break: break-all;
}

.referenceItem {
  margin: 0;
}

.paymentAddressValue {
  display: flex;
  flex-wrap: wrap;
  align-items: baseline;
  gap: 8px;
  margin: 0;
  font-family: var(--font-mono, 'SF Mono', 'Fira Code', monospace);
  font-size: 13px;
  word-break: break-all;
  color: var(--theme-text-primary);
}

.copyButton {
  font-size: 12px;
}

.copiedConfirmation {
  font-size: 12px;
  color: var(--theme-text-secondary, #6b7384);
}
```

`*.scss.d.ts` files are gitignored and generated. After editing the SCSS you
must regenerate before typechecking:

```bash
node_modules/.bin/typed-scss-modules source/renderer/app
```

(`yarn compile` does this for you through its `precompile` hook.)

---

### Step 8 — i18n catalogs

**Files:** `source/renderer/app/i18n/locales/en-US.json` and
`source/renderer/app/i18n/locales/ja-JP.json`.

Both catalogs are flat, alphabetically sorted objects. The existing
`governance.drepDetail.anchorContent.*` block sits at **en-US.json:289-293** and
**ja-JP.json:289-293**. Insert the 13 new keys so the block stays sorted:
`caption`, `givenName`, `loading`, `motivations`, `objectives`,
`paymentAddress.caption`, `paymentAddress.copiedToast`,
`paymentAddress.copyButton`, `paymentAddress.copyLabel`,
`paymentAddress.label`, `qualifications`, `references.identity`,
`references.identityCaption`, `references.links`, `references.other`,
`references.title`, `title`, `unavailable`.

**en-US.json — add exactly these lines:**

```json
  "governance.drepDetail.anchorContent.motivations": "!!!Motivations",
  "governance.drepDetail.anchorContent.objectives": "!!!Objectives",
  "governance.drepDetail.anchorContent.paymentAddress.caption": "!!!This address is the DRep's own claim. Delegating your voting power requires no payment to any address.",
  "governance.drepDetail.anchorContent.paymentAddress.copiedToast": "!!!Payment address copied",
  "governance.drepDetail.anchorContent.paymentAddress.copyButton": "!!!Copy",
  "governance.drepDetail.anchorContent.paymentAddress.copyLabel": "!!!Copy stated payment address",
  "governance.drepDetail.anchorContent.paymentAddress.label": "!!!Stated payment address",
  "governance.drepDetail.anchorContent.qualifications": "!!!Qualifications",
  "governance.drepDetail.anchorContent.references.identity": "!!!Claimed identities",
  "governance.drepDetail.anchorContent.references.identityCaption": "!!!These identities are claimed by the DRep and are not verified by Daedalus. Open the link and confirm that this DRep ID is published there before you rely on it.",
  "governance.drepDetail.anchorContent.references.links": "!!!Links",
  "governance.drepDetail.anchorContent.references.other": "!!!Other references",
  "governance.drepDetail.anchorContent.references.title": "!!!References",
```

**ja-JP.json — add exactly these lines:**

```json
  "governance.drepDetail.anchorContent.motivations": "!!!動機",
  "governance.drepDetail.anchorContent.objectives": "!!!目的",
  "governance.drepDetail.anchorContent.paymentAddress.caption": "!!!このアドレスはDRep自身による申告です。投票権の委任に、いかなるアドレスへの支払いも必要ありません。",
  "governance.drepDetail.anchorContent.paymentAddress.copiedToast": "!!!支払いアドレスをコピーしました",
  "governance.drepDetail.anchorContent.paymentAddress.copyButton": "!!!コピー",
  "governance.drepDetail.anchorContent.paymentAddress.copyLabel": "!!!申告された支払いアドレスをコピー",
  "governance.drepDetail.anchorContent.paymentAddress.label": "!!!申告された支払いアドレス",
  "governance.drepDetail.anchorContent.qualifications": "!!!資格",
  "governance.drepDetail.anchorContent.references.identity": "!!!申告されたアイデンティティ",
  "governance.drepDetail.anchorContent.references.identityCaption": "!!!これらのアイデンティティはDRep自身による申告であり、Daedalusは検証していません。リンク先を開き、このDRep IDがそこに公開されていることを確認してから信頼してください。",
  "governance.drepDetail.anchorContent.references.links": "!!!リンク",
  "governance.drepDetail.anchorContent.references.other": "!!!その他の参照",
  "governance.drepDetail.anchorContent.references.title": "!!!参照",
```

Every string keeps the leading `!!!`. `tests/jest/i18n/preliminaryCopyMarkers.spec.ts:55-62`
asserts every `governance.`-prefixed key is `!!!`-marked in both locales and
`:36-44` asserts key-set parity, so both are covered automatically — **no edit
to that spec is required for this task** (that requirement belongs to task-154's
`voting.governance.confirmationDialog.*` key).

**Gate step, run after the JSON edits:**

```bash
yarn i18n:manage
```

It runs `i18n:extract` then `i18n:check` and it **writes files**:
`source/renderer/app/i18n/locales/defaultMessages.json`,
`whitelist_en-US.json`, `whitelist_ja-JP.json`, and potentially the catalogs
themselves. Inspect `git status`, keep the regenerated
`defaultMessages.json` / `whitelist_*.json` changes that correspond to your 13
new keys, and revert anything else surgically:

```bash
git restore <path-that-was-clean-at-HEAD>
```

**Never `git stash`** — the stash stack is shared across worktrees and
concurrent sessions. **Never run `node_modules/.bin/prettier` over the locale
JSON, `defaultMessages.json` or `whitelist_*.json`** — those are tool-managed.

---

### Step 9 — Update `tests/jest/governance/AnchorVerificationService.spec.ts`

The committed fixture `tests/mocks/governance/anchor-cip119-sample.json`
**already carries** `objectives`, `motivations` and `qualifications` under
`body`. **Do not edit that fixture** — its Blake2b-256 digest is committed
beside it in `anchor-cip119-sample.hash` and any byte change invalidates every
test in this suite. All new bodies go inline via
`Buffer.from(JSON.stringify(…))` plus `anchorDigest(bytes)`, exactly as the
existing test at `:137-149` does.

#### 9a. Extend the happy-path assertion at `:79-91`

Replace the `expect(result).toMatchObject({…})` payload so the three prose
fields the fixture already carries are asserted:

```ts
    expect(result).toMatchObject({
      status: 'verified',
      content: {
        givenName: 'Daedalus Test DRep',
        objectives:
          'Synthetic fixture objectives for offline anchor verification tests.',
        motivations:
          'Synthetic fixture motivations for offline anchor verification tests.',
        qualifications:
          'Synthetic fixture qualifications for offline anchor verification tests.',
        references: [],
        paymentAddress: null,
        doNotList: false,
      },
      host: 'raw.githubusercontent.com',
    });
```

#### 9b. Rewrite the test at `:137-149`

Current title and body (this is the behaviour this task deliberately reverses):

```ts
  it('treats a body without a givenName as a parse failure', async () => {
    const bytes = Buffer.from(JSON.stringify({ body: {} }));
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect(result).toEqual({
      status: 'unavailable',
      reason: AnchorFetchErrorType.ParseFailed,
    });
  });
```

Replacement:

```ts
  it('verifies a body with no givenName and leaves every field at its empty value', async () => {
    const bytes = Buffer.from(JSON.stringify({ body: {} }));
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect(result).toMatchObject({
      status: 'verified',
      content: {
        givenName: null,
        objectives: null,
        motivations: null,
        qualifications: null,
        references: [],
        paymentAddress: null,
        doNotList: false,
      },
    });
  });

  it('keeps a doNotList opt-out from a body that carries no givenName', async () => {
    const bytes = Buffer.from(
      JSON.stringify({ body: { doNotList: true } })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect(result).toMatchObject({
      status: 'verified',
      content: { doNotList: true },
    });
  });
```

#### 9c. Add these tests to the same `describe` block

```ts
  it('still fails to parse a body that is missing, null or not an object', async () => {
    for (const body of [undefined, null, [], 'text']) {
      const bytes = Buffer.from(JSON.stringify({ body }));
      mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
      const result = await resolveVerifiedAnchor({
        url: ONCHAIN_URL,
        hash: anchorDigest(bytes),
      });
      expect(result).toEqual({
        status: 'unavailable',
        reason: AnchorFetchErrorType.ParseFailed,
      });
    }
  });

  it('splits references into link, identity and default buckets', async () => {
    const bytes = Buffer.from(
      JSON.stringify({
        body: {
          references: [
            { '@type': 'Link', label: 'Blog', uri: 'https://example.org/blog' },
            {
              '@type': 'Identity',
              label: 'Profile',
              uri: 'https://example.org/id',
            },
            { '@type': 'CIP119:Identity', uri: 'https://example.org/id2' },
            { '@type': 'Something', uri: 'https://example.org/other' },
            { uri: 'https://example.org/untyped' },
          ],
        },
      })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect((result as any).content.references).toEqual([
      { type: 'link', label: 'Blog', uri: 'https://example.org/blog' },
      { type: 'identity', label: 'Profile', uri: 'https://example.org/id' },
      { type: 'identity', label: null, uri: 'https://example.org/id2' },
      { type: 'other', label: null, uri: 'https://example.org/other' },
      { type: 'other', label: null, uri: 'https://example.org/untyped' },
    ]);
  });

  it('drops reference entries with no uri and caps the list at twenty', async () => {
    const many = Array.from({ length: 25 }, (_unused, index) => ({
      '@type': 'Link',
      uri: `https://example.org/${index}`,
    }));
    const bytes = Buffer.from(
      JSON.stringify({ body: { references: [{ label: 'no uri' }, ...many] } })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect((result as any).content.references).toHaveLength(20);
  });

  it('clamps long-form prose at one thousand characters, not at eighty', async () => {
    const bytes = Buffer.from(
      JSON.stringify({ body: { objectives: 'o'.repeat(5000) } })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect((result as any).content.objectives).toHaveLength(1000);
  });

  it('clamps givenName at eighty characters', async () => {
    const bytes = Buffer.from(
      JSON.stringify({ body: { givenName: 'n'.repeat(500) } })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect((result as any).content.givenName).toHaveLength(80);
  });

  it('drops an over-length payment address instead of truncating it', async () => {
    const bytes = Buffer.from(
      JSON.stringify({ body: { paymentAddress: `addr1${'q'.repeat(200)}` } })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect((result as any).content.paymentAddress).toBeNull();
  });

  it('reads the JSON-LD @value wrapper form for strings and booleans', async () => {
    const bytes = Buffer.from(
      JSON.stringify({
        body: {
          givenName: { '@value': 'Wrapped Name' },
          doNotList: { '@value': true },
        },
      })
    );
    mockFetchAnchorBytes.mockResolvedValue(okResult(bytes));
    const result = await resolveVerifiedAnchor({
      url: ONCHAIN_URL,
      hash: anchorDigest(bytes),
    });
    expect(result).toMatchObject({
      content: { givenName: 'Wrapped Name', doNotList: true },
    });
  });
```

The existing test at `:249` ("logs no anchor url, host, hash or identity on any
resolution path") must stay green untouched — you added no logging.

---

### Step 10 — Update `tests/jest/governance/GovernanceStore.spec.ts`

#### 10a. `verifiedResponse` helper at `:995-1000`

Current:

```ts
  const verifiedResponse = (givenName: string | null = 'Cardano Academy') => ({
    status: 'verified' as const,
    content: { givenName },
    host: 'raw.githubusercontent.com',
    fetchedAt: 1_750_000_001_000,
  });
```

Replacement:

```ts
  const verifiedContent = (
    overrides: Partial<VerifiedDRepAnchorContent> = {}
  ): VerifiedDRepAnchorContent => ({
    givenName: 'Cardano Academy',
    objectives: null,
    motivations: null,
    qualifications: null,
    references: [],
    paymentAddress: null,
    doNotList: false,
    ...overrides,
  });

  const verifiedResponse = (givenName: string | null = 'Cardano Academy') => ({
    status: 'verified' as const,
    content: verifiedContent({ givenName }),
    host: 'raw.githubusercontent.com',
    fetchedAt: 1_750_000_001_000,
  });
```

Add `VerifiedDRepAnchorContent` to the import block from
`'../../../source/common/types/governance.types'` that closes at `:18`.

#### 10b. Fixture at `:671-680`

Replace:

```ts
          {
            state: 'verified',
            hash: 'a'.repeat(64),
            givenName: 'Ada',
            host: 'governance-preview.example.org',
          },
```

with:

```ts
          {
            state: 'verified',
            hash: 'a'.repeat(64),
            host: 'governance-preview.example.org',
            content: verifiedContent({ givenName: 'Ada' }),
          },
```

`verifiedContent` is declared inside a later `describe` block. Hoist it to
module scope (immediately after the other module-level helpers near the top of
the file) so both call sites can use it, and leave `verifiedResponse` where it
is.

#### 10c. Add two tests to the anchor `describe` block

```ts
  it('stores the whole verified payload on the anchor state', async () => {
    const store = await loadStore();
    mockAnchorRequest.mockResolvedValue({
      status: 'verified' as const,
      content: verifiedContent({
        objectives: 'Objectives text',
        paymentAddress: 'addr1qexamplepaymentaddress',
        references: [
          { type: 'identity', label: null, uri: 'https://example.org/id' },
        ],
        doNotList: true,
      }),
      host: 'raw.githubusercontent.com',
      fetchedAt: 1_750_000_001_000,
    });

    await store.fetchAnchorContent(ANCHOR_DREP_ID, ANCHOR);

    const state = store.anchorStateByDRepId.get(ANCHOR_DREP_ID);
    expect(state).toMatchObject({
      state: 'verified',
      content: {
        objectives: 'Objectives text',
        paymentAddress: 'addr1qexamplepaymentaddress',
        doNotList: true,
      },
    });
  });

  it('keeps verifiedName null when the anchor carries no givenName', async () => {
    const store = await loadStore();
    mockAnchorRequest.mockResolvedValue({
      status: 'verified' as const,
      content: verifiedContent({ givenName: null, objectives: 'Only prose' }),
      host: 'raw.githubusercontent.com',
      fetchedAt: 1_750_000_001_000,
    });

    await store.fetchAnchorContent(ANCHOR_DREP_ID, ANCHOR);

    expect(store.drepIndex.get(ANCHOR_DREP_ID)?.verifiedName).toBeNull();
    // A hash-matched parse is completed metadata whether or not it names the
    // DRep; the cohort's completeness signal follows the digest, not one
    // optional CIP-119 field.
    expect(store.verifiedMetadataIds.has(ANCHOR_DREP_ID)).toBe(true);
  });
```

The second assertion is the proof of decision **D-H** in §5, not an incidental
check: it pins that a nameless verified anchor counts as completed metadata for
`cohortContext` and therefore for the task-172 High value badge.

The existing test at `:1112` ("clamps an oversized givenName to eighty
characters with a trailing ellipsis") must stay green unchanged — the renderer
clamp is still applied at ingest.

---

### Step 11 — Update `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx`

#### 11a. Add a shared verified-content helper

Add after `baseEntry` (`:31-41`):

```ts
const verifiedContent = (
  overrides: Partial<VerifiedDRepAnchorContent> = {}
): VerifiedDRepAnchorContent => ({
  givenName: 'Daedalus Test DRep',
  objectives: null,
  motivations: null,
  qualifications: null,
  references: [],
  paymentAddress: null,
  doNotList: false,
  ...overrides,
});

const verifiedState = (overrides: Partial<VerifiedDRepAnchorContent> = {}) =>
  new Map([
    [
      DREP_ID,
      {
        state: 'verified',
        hash: baseEntry.anchor!.hash,
        host: 'raw.githubusercontent.com',
        content: verifiedContent(overrides),
      },
    ],
  ]);
```

Extend the type import at `:16` to
`import { AnchorFetchErrorType } from '…/governance.types';` plus a
`import type { VerifiedDRepAnchorContent } from '../../../../common/types/governance.types';`
line.

The logger-spy test below needs the renderer logger in scope. Add one more
import beside it:

```ts
import { logger as rendererLogger } from '../../utils/logging';
```

That is the same module the task-111 spy suite imports
(`tests/jest/security/governance-sanitization.spec.ts:40`), so the two suites
spy the identical sink.

#### 11b. Migrate the two existing verified fixtures

Tests at `:333` ("renders the verified name with the verified off-chain label
and host tooltip") and `:406` ("renders the verified block in ja-JP") each build
an inline map with `givenName: 'Daedalus Test DRep'` at `:342` and `:416`.
Replace each `anchorStateByDRepId: new Map([[DREP_ID, { … }]])` with
`anchorStateByDRepId: verifiedState()`. Their assertions are unchanged and must
stay green.

#### 11c. Add these tests

```ts
  it('renders every verified profile field with a verified off-chain label', () => {
    renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          objectives: 'Fixture objectives',
          motivations: 'Fixture motivations',
          qualifications: 'Fixture qualifications',
        }),
      },
    });

    expect(screen.getByText('!!!Objectives')).toBeInTheDocument();
    expect(screen.getByText('Fixture objectives')).toBeInTheDocument();
    expect(screen.getByText('!!!Motivations')).toBeInTheDocument();
    expect(screen.getByText('Fixture motivations')).toBeInTheDocument();
    expect(screen.getByText('!!!Qualifications')).toBeInTheDocument();
    expect(screen.getByText('Fixture qualifications')).toBeInTheDocument();
    expect(
      screen.getAllByText('!!!Verified off-chain content').length
    ).toBeGreaterThanOrEqual(4);
  });

  it('renders an identity reference under the claim caption and never as a plain link', () => {
    renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          references: [
            {
              type: 'link',
              label: 'Blog',
              uri: 'https://example.org/blog',
            },
            {
              type: 'identity',
              label: 'X profile',
              uri: 'https://example.org/id',
            },
          ],
        }),
      },
    });

    const identityHeading = screen.getByText('!!!Claimed identities');
    const linkHeading = screen.getByText('!!!Links');
    expect(identityHeading).toBeInTheDocument();
    expect(
      screen.getByText(
        '!!!These identities are claimed by the DRep and are not verified by Daedalus. Open the link and confirm that this DRep ID is published there before you rely on it.'
      )
    ).toBeInTheDocument();

    // The identity entry must sit after the caption, never inside the Links list.
    const identityEntry = screen.getByText('X profile');
    const blogEntry = screen.getByText('Blog');
    expect(identityHeading.compareDocumentPosition(identityEntry)).toBe(
      Node.DOCUMENT_POSITION_FOLLOWING
    );
    expect(linkHeading.compareDocumentPosition(blogEntry)).toBe(
      Node.DOCUMENT_POSITION_FOLLOWING
    );
    expect(identityEntry.closest('ul')).not.toBe(blogEntry.closest('ul'));
  });

  it('buckets an unrecognised reference type under other references', () => {
    renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          references: [
            { type: 'other', label: null, uri: 'https://example.org/misc' },
          ],
        }),
      },
    });

    expect(screen.getByText('!!!Other references')).toBeInTheDocument();
    expect(screen.getByText('https://example.org/misc')).toBeInTheDocument();
    expect(screen.queryByText('!!!Claimed identities')).not.toBeInTheDocument();
  });

  it('opens an https reference uri through the external-link handler', () => {
    const { app } = renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          references: [
            { type: 'link', label: 'Blog', uri: 'https://example.org/blog' },
          ],
        }),
      },
    });

    const link = screen.getByText('Blog').closest('a');
    expect(link).toHaveAttribute('href', 'https://example.org/blog');
    expect(link).toHaveAttribute('rel', 'noopener noreferrer');

    fireEvent.click(link!);

    expect(app.openExternalLink).toHaveBeenCalledWith(
      'https://example.org/blog'
    );
  });

  it('renders a non-https reference uri as inert text', () => {
    const { app } = renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          references: [
            { type: 'link', label: null, uri: 'http://example.org/plain' },
          ],
        }),
      },
    });

    const entry = screen.getByText('http://example.org/plain');
    expect(entry.closest('a')).toBeNull();
    expect(app.openExternalLink).not.toHaveBeenCalled();
  });

  it('renders the stated payment address read-only with a working copy button', async () => {
    const address = 'addr1qxexamplepaymentaddressvalue';
    const writeText = jest.fn(async () => undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText },
    });
    try {
      renderPage({
        governanceOverrides: {
          anchorStateByDRepId: verifiedState({ paymentAddress: address }),
        },
      });

      expect(screen.getByText('!!!Stated payment address')).toBeInTheDocument();
      expect(
        screen.getByText(
          "!!!This address is the DRep's own claim. Delegating your voting power requires no payment to any address."
        )
      ).toBeInTheDocument();
      expect(screen.getByText(address).tagName).toBe('SPAN');
      expect(screen.queryByDisplayValue(address)).not.toBeInTheDocument();

      fireEvent.click(
        screen.getByRole('button', { name: '!!!Copy stated payment address' })
      );

      expect(writeText).toHaveBeenCalledWith(address);
      expect(
        await screen.findByText('!!!Payment address copied')
      ).toBeInTheDocument();
    } finally {
      delete (navigator as any).clipboard;
    }
  });

  it('reaches no logger on either payment-address copy path', async () => {
    const address = 'addr1qxexamplepaymentaddressvalue';
    const spies = (['debug', 'info', 'warn', 'error'] as const).map((level) =>
      jest.spyOn(rendererLogger, level).mockImplementation(() => undefined)
    );
    const writeText = jest.fn(async () => undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText },
    });
    try {
      renderPage({
        governanceOverrides: {
          anchorStateByDRepId: verifiedState({ paymentAddress: address }),
        },
      });
      const copyButton = () =>
        screen.getByRole('button', { name: '!!!Copy stated payment address' });

      fireEvent.click(copyButton());
      await screen.findByText('!!!Payment address copied');

      // The unavailable branch must be as silent as the success branch: no
      // length, no error code, nothing that could carry the address.
      delete (navigator as any).clipboard;
      fireEvent.click(copyButton());

      spies.forEach((spy) => expect(spy).not.toHaveBeenCalled());
    } finally {
      delete (navigator as any).clipboard;
      spies.forEach((spy) => spy.mockRestore());
    }
  });

  it('renders the profile block when references and payment address are absent', () => {
    renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({ objectives: 'Only objectives' }),
      },
    });

    expect(screen.getByText('!!!Off-chain profile')).toBeInTheDocument();
    expect(screen.getByText('Only objectives')).toBeInTheDocument();
    expect(screen.queryByText('!!!References')).not.toBeInTheDocument();
    expect(
      screen.queryByText('!!!Stated payment address')
    ).not.toBeInTheDocument();
  });

  it('renders the profile block with no name when the anchor carries only prose', () => {
    renderPage({
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          givenName: null,
          objectives: 'Objectives only',
        }),
      },
    });

    expect(screen.getByText('Objectives only')).toBeInTheDocument();
    expect(screen.queryByText('!!!Name')).not.toBeInTheDocument();
    // The name caption is name-specific copy and must not appear without a name.
    expect(
      screen.queryByText(
        "!!!This name is the DRep's own claim, hash-matched to the anchor recorded on-chain. Daedalus does not verify identity."
      )
    ).not.toBeInTheDocument();
  });

  it('renders the new profile labels in ja-JP', () => {
    renderPage({
      locale: 'ja-JP',
      governanceOverrides: {
        anchorStateByDRepId: verifiedState({
          objectives: 'Fixture objectives',
          paymentAddress: 'addr1qxexamplepaymentaddressvalue',
          references: [
            {
              type: 'identity',
              label: 'X profile',
              uri: 'https://example.org/id',
            },
          ],
        }),
      },
    });

    expect(screen.getByText('!!!目的')).toBeInTheDocument();
    expect(
      screen.getByText('!!!申告されたアイデンティティ')
    ).toBeInTheDocument();
    expect(screen.getByText('!!!申告された支払いアドレス')).toBeInTheDocument();
  });
```

The existing test at `:358` ("keeps every on-chain row when the anchor is
unavailable") is AC-3's regression proof and must stay green untouched.

**Snapshots.** `source/renderer/app/containers/governance/__snapshots__/DRepDetailPage.spec.tsx.snap`
holds the two category-badge snapshots from tests at `:288` and `:297`; neither
renders anchor content, so they must be **unchanged**. If either moves, you
changed something outside your scope — do not `-u` past it without finding out
why.

---

### Step 12 — Re-assert the sanitization floor

**File:** `tests/jest/security/governance-sanitization.spec.ts`

#### 12a. Add to the `filterLogData` block (`describe` opens at `:78`)

Place beside the existing `:290` case ("removes givenName, verifiedName and a
nested anchorContent object"):

```ts
  it('removes every CIP-119 profile field name added by the anchor pipeline', () => {
    const data = {
      objectives: 'Objectives prose',
      motivations: 'Motivations prose',
      qualifications: 'Qualifications prose',
      references: [{ type: 'identity', uri: 'https://example.org/id' }],
      paymentAddress: 'addr1qxexamplepaymentaddressvalue',
      doNotList: true,
    };

    expect(filterLogData(data)).toEqual({});
  });

  it('removes CIP-119 profile fields nested under a verified anchor content object', () => {
    const address = 'addr1qxexamplepaymentaddressvalue';
    const data = {
      anchorState: {
        state: 'verified',
        content: { paymentAddress: address, objectives: 'Objectives prose' },
      },
    };

    expect(jsonStr(filterLogData(data))).not.toContain(address);
    expect(jsonStr(filterLogData(data))).not.toContain('Objectives prose');
  });
```

#### 12b. Do NOT add a render case to this file

`tests/jest/security/governance-sanitization.spec.ts` is a `.ts` file that
imports no React and mounts no component (its four `describe` blocks are
`filterLogData` units `:78`, call boundaries over store/analytics sinks `:310`,
analytics URL masking `:603`, and the main-process fetch `:641`). Keep it that
way: adding a component render here would need a ThemeProvider/IntlProvider
harness the file does not have, and task-174 Step 12 applies the same rule.

The payment-address copy path's logger spy therefore lives in
`DRepDetailPage.spec.tsx` — Step 11c's case
`reaches no logger on either payment-address copy path`, which spies the same
`source/renderer/app/utils/logging` sink this file uses at `:40` and covers both
the success branch and the clipboard-unavailable branch. That path must reach no
logger at all, not even a length, which is why it differs from `DRepIdDisplay`'s
`{ drepIdLength }` pattern; the reason is recorded in situ as the comment above
`handleCopyPaymentAddress` (Step 6).

Net effect on this file: **+2 tests**, both in the `:78` block from 12a.

#### 12c. The two-anchor rule

The floor is only proved by citing **both** suites together. Citing one is a
false green:

```bash
node_modules/.bin/jest --testPathPattern=security/governance-sanitization --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern=governance/logDRepStateSnapshot --no-coverage --runInBand
```

---

### Step 13 — Story fixtures

**File:** `storybook/stories/governance/DRepDetail.stories.tsx`

Replace the verified branch of `anchorStateFor` (`:43-50`):

```tsx
  if (choice === 'verified') {
    return {
      state: 'verified',
      hash,
      host: 'governance-preview.example.org',
      content: {
        givenName: 'Daedalus Preview DRep',
        objectives:
          'Advocate for treasury discipline and predictable protocol parameter changes.',
        motivations:
          'Long-term stake pool operator with an interest in governance participation.',
        qualifications:
          'Five years operating Cardano infrastructure; contributor to two CIPs.',
        references: [
          {
            type: 'link',
            label: 'Public blog',
            uri: 'https://governance-preview.example.org/blog',
          },
          {
            type: 'identity',
            label: 'Social profile',
            uri: 'https://governance-preview.example.org/profile',
          },
          {
            type: 'other',
            label: null,
            uri: 'https://governance-preview.example.org/misc',
          },
        ],
        paymentAddress: 'addr1qxpreviewstatedpaymentaddressvalue',
        doNotList: false,
      },
    };
  }
```

Add one option to `ANCHOR_STATE_OPTIONS` (`:31-35`) and one branch to
`anchorStateFor` for a verified anchor that carries **no** `givenName` and no
references/paymentAddress, so the AC-8 empty-field path is visually
inspectable:

```tsx
const ANCHOR_STATE_OPTIONS = {
  Verified: 'verified',
  'Verified — prose only': 'verified-minimal',
  Unavailable: 'unavailable',
  'Not requested': 'none',
};
```

Locale is **not** wired locally — the comment at `:85-87` records why: the
global `StoryWrapper` decorator supplies the `IntlProvider` and the
English/Japanese toggle at the top of the preview window drives every label.
**Do not add an `IntlProvider` and do not add `*_ja` story exports.**

---

### Step 14 — Verify the design-doc text; do NOT edit it

AC-1 requires the `image` deferral to be "recorded in that design doc". It
**already is**, at
`.agent/plans/governance/drep-discovery/designs/drep-discovery-design.md:218`:

> "**`image` / `imageObject` is deferred out of this render set** — dropped, not
> staged. Rendering it requires a second hash check over the image bytes on top
> of the anchor digest, and the inline-base64-versus-URL tradeoff against the
> anchor size cap is an open product question; until that decision is taken the
> detail view keeps the default avatar shown in the wireframe above."

Confirm the text is present:

```bash
grep -n "image. / .imageObject. is deferred out of this render set" \
  .agent/plans/governance/drep-discovery/designs/drep-discovery-design.md
```

Expected: one hit at line 218. **Adding a second deferral paragraph is a
defect.** Record "verified present at `:218`, no edit required" in the task's
`statusReason` instead.

Also record in `statusReason`:

- The render list this task implements is at `:216` (the AC cites the stale
  `:215`).
- **AC-2's confirmation half is discharged by task-154**, which lands later in
  the canonical build order. task-157 cannot report AC-2 green from its own
  commit; it makes the verified data available and hands the confirmation-dialog
  gating to task-154.

The tracker file is
`.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`
(task-157 at `:1904`). It is tool-managed JSON: **never run prettier over it.**

---

### Step 15 — Format, verify, commit

#### 15a. Format

`nix fmt` is mandated by the repo but `nix` is unavailable in this devcontainer.
Substitute the exact command below with **explicit changed paths only**. Never
`yarn prettier` — its package.json script embeds a repo-wide `"**/*.*"` glob
(`package.json:47`) and reformats ~250 unrelated files even when handed a path.

```bash
node_modules/.bin/prettier --write \
  source/common/types/governance.types.ts \
  source/common/utils/logging.ts \
  source/main/governance/AnchorVerificationService.ts \
  source/renderer/app/stores/GovernanceStore.ts \
  source/renderer/app/utils/governance/isHttpsUrl.ts \
  source/renderer/app/components/governance/drep-detail/DRepDetailAnchorContent.tsx \
  source/renderer/app/components/governance/drep-detail/DRepDetailAnchorSection.tsx \
  source/renderer/app/components/governance/drep-detail/DRepDetail.scss \
  source/renderer/app/containers/governance/DRepDetailPage.spec.tsx \
  storybook/stories/governance/DRepDetail.stories.tsx \
  tests/jest/governance/AnchorVerificationService.spec.ts \
  tests/jest/governance/GovernanceStore.spec.ts \
  tests/jest/security/governance-sanitization.spec.ts
```

Do **not** include the locale JSON, `defaultMessages.json`, `whitelist_*.json`
or the tasks tracker in that list. Note that prettier 2.1.2 does not stabilise
on some pre-existing files in this repo; if a path you did not edit shows up
dirty afterwards, `git restore` it. Running `nix fmt` before merge remains a
user-owned obligation — say so when you report.

#### 15b. Verify

Run these in order. **Measure the baseline for every suite you touch before you
edit it** and record `baseline -> after`. The delta is the contract, not the
total. Counts below were measured at `55e8985bf`.

```bash
# 1. Typecheck (precompile regenerates the gitignored *.scss.d.ts).
yarn compile
#    expect: exit 0, no errors.
#    Equivalent without the hook:
#      node_modules/.bin/typed-scss-modules source/renderer/app
#      node_modules/.bin/tsc --noEmit

# 2. Main-process parser.
node_modules/.bin/jest --testPathPattern=governance/AnchorVerificationService --no-coverage --runInBand
#    baseline: 1 suite, 13 tests. expect: 1 suite, 13 -> 21 tests
#    (1 rewritten in place, +8 added).

# 3. IPC channel round-trip (widened payload must still pass through unchanged).
node_modules/.bin/jest --testPathPattern=ipc/governanceAnchorChannel --no-coverage --runInBand
#    expect: unchanged suite and test count.

# 4. Store projection.
node_modules/.bin/jest --testPathPattern=governance/GovernanceStore --no-coverage --runInBand
#    expect: +2 tests, every pre-existing test still green.

# 5. Detail view. This is also where the payment-address logger spy runs
#    (Step 11c), so run it before declaring the floor proved.
node_modules/.bin/jest --testPathPattern=containers/governance/DRepDetailPage --no-coverage --runInBand
#    baseline: 1 suite, 21 tests, 2 snapshots. expect: 21 -> 31 tests
#    (+10, Step 11c), 2 snapshots UNCHANGED.

# 6. Sanitization floor — cite BOTH, together.
node_modules/.bin/jest --testPathPattern=security/governance-sanitization --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern=governance/logDRepStateSnapshot --no-coverage --runInBand
#    baseline: 35 and 5. expect: 35 -> 37 in the first (+2, Step 12a only),
#    5 unchanged in the second. Step 12b adds nothing here by design.

# 7. i18n markers and parity.
node_modules/.bin/jest --testPathPattern=i18n/preliminaryCopyMarkers --no-coverage --runInBand
#    expect: 1 suite, 5 tests, unchanged count, all green.
#    Baseline governance.* key counts at 55e8985bf: 97 en-US / 97 ja-JP,
#    all !!!-marked. This task adds 13 per catalog and runs first in the slice,
#    so after it: governance.* 110 / 110, whole-catalog 1644 / 1644.
#    The +13 delta is the contract; the totals are the cumulative baseline the
#    four later key-adding tasks measure from.

# 8. Full suite.
node_modules/.bin/jest --runInBand
#    tests/jest/governance/GovernanceCliArgvSmoke.spec.ts self-skips when
#    cardano-cli is off PATH (1 skipped suite, 12 skipped tests) — expected.

# 9. Lint. Errors are the gate; warnings are not.
yarn lint
#    expect: exit 0, 0 errors. ~5591 pre-existing warnings at HEAD; the count
#    rises when a new file lands under source/ (isHttpsUrl.ts). tests/ is
#    eslint-ignored.

# 10. Storybook. `yarn storybook:build` is RED at HEAD for a pre-existing
#     manager-webpack reason unrelated to any change, so `yarn check:all` is red
#     too. The usable floor is:
yarn storybook
#     Open Governance / DRep Detail, switch the Anchor state knob to
#     "Verified", and flip the global English/Japanese toggle. Check the
#     ja-JP labels do not overflow the field rows (task-172 left a live
#     ja-JP overflow risk and this task adds five field rows to the same view).
```

#### 15c. Commit

One commit, subject line only, no body, no trailers, no `Co-Authored-By`:

```bash
git commit -m "feat(gov): task-157 render the remaining verified cip-119 profile fields"
```

If `yarn i18n:manage` or prettier left an unintended file dirty, revert it with
`git restore <path>` **before** committing. Never `git stash`.

---

### Files this task edits

| file | anchors touched |
|---|---|
| `source/common/types/governance.types.ts` | replace `:93-96` |
| `source/main/governance/AnchorVerificationService.ts` | `:3-7`, `:17`, replace `:29-62` |
| `source/common/utils/logging.ts` | extend the list at `:44-63` (insert after `:63`) |
| `source/renderer/app/stores/GovernanceStore.ts` | type import block, replace `:49-52`, replace `:434-442`, `:532` |
| `source/renderer/app/utils/governance/isHttpsUrl.ts` | **new file** |
| `source/renderer/app/components/governance/drep-detail/DRepDetailAnchorSection.tsx` | add import after `:4`, delete `:44-52`, `:76`, `:111` |
| `source/renderer/app/components/governance/drep-detail/DRepDetailAnchorContent.tsx` | full rewrite (was 96 lines) |
| `source/renderer/app/components/governance/drep-detail/DRepDetail.scss` | append after `:112` |
| `source/renderer/app/i18n/locales/en-US.json` | +13 keys in the `:289-293` block |
| `source/renderer/app/i18n/locales/ja-JP.json` | +13 keys in the `:289-293` block |
| `source/renderer/app/i18n/locales/defaultMessages.json` | regenerated by `yarn i18n:manage` |
| `source/renderer/app/i18n/locales/whitelist_en-US.json`, `whitelist_ja-JP.json` | regenerated by `yarn i18n:manage` |
| `storybook/stories/governance/DRepDetail.stories.tsx` | `:31-35`, `:43-50` |
| `tests/jest/governance/AnchorVerificationService.spec.ts` | `:79-91`, rewrite `:137-149`, +8 tests |
| `tests/jest/governance/GovernanceStore.spec.ts` | `:671-680`, `:995-1000`, +2 tests |
| `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx` | `:16` (+ the `rendererLogger` import), after `:41`, `:342`, `:416`, +10 tests |
| `tests/jest/security/governance-sanitization.spec.ts` | +2 in the `:78` block; the `:310` block is **unchanged** (see Step 12b) |
| `source/main/ipc/governanceAnchorChannel.spec.ts` | `:56-59` — only if `tsc --noEmit` flags the narrowed `content` literal |

**Files this task must NOT edit:**
`VotingPowerDelegationConfirmationDialog.tsx` (+ its spec),
`VotingGovernancePage.tsx`, `DRepCard.tsx`, `DRepIdDisplay.tsx`,
`DRepSourceLabel.tsx`, `DRepDetailOnchainSection.tsx`,
`drep-directory/helpers.ts`, `drep-discovery-design.md`,
`tests/mocks/governance/anchor-cip119-sample.json` and its `.hash` sibling.

---

## task-153 — Honor CIP-119 `doNotList` in default cohort

`estimatedHours: 5.5` · `priority: medium` · `dependencies: ["task-151", "task-118", "task-122", "task-172"]` · `targetPath: source/renderer/app/stores/` (indicative only — the real file set is at the end of this section) · build position **2 of 6**, immediately after task-157 · mode: **autonomous**.

Every path in this section is repo-relative from the worktree root and was re-read at `55e8985bf`. Line
numbers marked **(HEAD)** were measured before task-157 landed; task-157 edits
`source/common/types/governance.types.ts`, `source/main/governance/AnchorVerificationService.ts`,
`source/renderer/app/stores/GovernanceStore.ts`, `source/common/utils/logging.ts` and the detail-view
components, so **re-locate every anchor in `GovernanceStore.ts` by the quoted code, not by the number**.
Anchors in `helpers.ts`, `DRepDirectory.tsx`, `DRepCard.tsx` and `DRepDirectoryList.tsx` are untouched by
task-157 and stay exact.

**The spec and story files are NOT uniformly safe.** task-157 also edits
`source/renderer/app/containers/governance/DRepDetailPage.spec.tsx` (a type import at `:16`, a
`rendererLogger` import beside it, a `verifiedContent`/`verifiedState` helper after `:41`, and +10 tests),
`tests/jest/governance/GovernanceStore.spec.ts` (`:671-680`, `:995-1000`, +2 tests) and
`storybook/stories/governance/DRepDetail.stories.tsx` (`:31-35`, `:43-50` replaced by a larger block). Every
line number this task quotes in those three files is a **HEAD** number that task-157 has already moved:
re-locate by the quoted text, never by the number. The Step 6 table flags each of them inline.

---

### Scope, non-goals and boundaries (read this instead of the PRD)

**What this task does.** A DRep whose Blake2b-256-verified CIP-119 anchor sets `doNotList: true` is dropped
from the default cohort, and stays reachable everywhere else. The flag is projected from the already-verified
anchor state onto the renderer's `AppDRepDirectoryEntry`, consumed by exactly two computeds
(`GovernanceStore.defaultCohort`) and one pure helper (`isStaleFavorite`), and it makes a favorited
opted-out DRep show the existing stale caption.

**What this task explicitly does NOT do — do not add any of it:**

1. **It does not widen `VerifiedDRepAnchorContent`, does not touch `parseVerifiedContent`, and does not
   extend `filterLogData`'s key list.** task-157 owns all three and lands before you. `doNotList` is already
   on the wire and already redacted when you start; Step 0 verifies that and stops you if it is not.
2. **It adds no member to `DRepStatus`.** `DRepStatus` is the closed union `'active' | 'inactive'`
   (`source/common/types/governance.types.ts:35`) and `DRepStatusBadge.tsx:26-29` builds an exhaustive
   `Record<DRepStatus, string>` over exactly those two. There is no `Retired` badge and no
   `Excluded from default cohort` badge in this release.
3. **It adds no new `DRepSourceLabel` variant and no new i18n key.** The favorites caption reuses the
   shipped `governance.drepFavorites.staleCaption` (`DRepCard.tsx:53-57`). Total new strings: **zero**.
4. **It renders nothing new in the DRep detail view.** `doNotList` is a directory/cohort signal, not a
   profile field; `DRepDetailAnchorContent` is untouched.
5. **It does not filter `showAllList`, `drepIndex`, `top35DRepIds`, `displayedDRepList` when the cohort is
   inactive, the search index, `filterDReps` or `sortDReps`.** Filtering any of those makes a legitimately
   registered DRep unreachable in the app, which the design doc forbids at
   `drep-discovery-design.md:239`.
6. **It never purges a favorite.** No code path may remove a `doNotList` DRep from `favoriteDRepIds`.
7. **It adds no bulk anchor prefetch and no per-DRep CLI invocation.**
8. **It does not touch the confirmation dialog, `CurrentVoteSummary`, `DRepIdDisplay` or any source label.**

**Stated limitation you must carry, not engineer around (AC-1 is true only within it).**
`doNotList` reaches `AppDRepDirectoryEntry` only through the same lazy, per-detail-visit anchor fetch that
populates `verifiedName` (`GovernanceStore.fetchAnchorContent`, triggered from `DRepDetailPage`). The v1
model is stated in `shared-design-tokens.md:250`:

> "**Verified-name search (deferred beyond v1).** v1 search matches DRep IDs only. Verified `givenName` is
> populated in `GovernanceStore.drepIndex` lazily per detail visit (anchor-1), so it does not cover unvisited
> DReps; name search is deferred until a bulk cohort anchor-prefetch phase makes names available
> directory-wide."

and reinforced at `drep-discovery-design.md:247`. **User-visible consequence:** a `doNotList: true` DRep stays
in the default cohort until something in the current session has fetched its anchor, and an unvisited
`doNotList: true` favorite shows no stale caption. The exclusion is a best-effort courtesy to the DRep's
stated preference, **not a security or privacy control**, and nothing else in the app depends on it being
complete. **Forbidden workarounds:** do not inject store state into a test to simulate global knowledge; do
not add bulk anchor fetching; do not gate cohort membership on an unresolved fetch (that would empty the
cohort on a cold start).

---

### Locked invariants this change must not break (inlined — do not look them up)

- **Invariant 6 — CLI discipline.** Bulk `--all-dreps` once per refresh; per-DRep CLI invocations are
  forbidden. This task performs no IPC at all.
- **Invariant 7 — Default cohort is binding.** "Exclude top 35 by voting power; up to the next 200 eligible
  (active, remaining `drepActivity` > 6 epochs, completed metadata when available), randomized. The default
  cohort IS the 'Recommended' sort — no Recommended tab, no per-card Recommended badge." The `doNotList`
  exclusion is an **addition** to this rule, never a replacement: the top-35 slice, the 200 cap, the
  6-epoch floor, the seeded shuffle and the canonical drepId sort all stay exactly as they are.
- **Invariant 8 — Badges are informational only.** They "never reorder, filter or override the cohort."
  `helpers.ts:177-180` already records that filter code must never import from the badge module. Nothing in
  this task may import `DRepCategoryBadge` or `DRepStatusBadge` into store or helper code.
- **Invariant 2 — Sanitization floor.** No DRep id, no `abstain`/`no_confidence` literal, no CIP-129/CIP-105
  bech32 string in any logger, analytics or electron-store payload. This task adds **no** logger call, no
  analytics call and no electron-store write. The task-111 spy suite is re-asserted unchanged.
- **Invariant 5 — Lovelace losslessness.** `defaultCohort` sorts through the `BigNumber` comparator
  `compareByVotingPowerDesc`. Your added predicate must sit in the existing `.filter(...)` and must not touch
  the comparator or coerce lovelace to `Number`.
- **Invariant 12 — Favorites are per-device** via the Electron local store, not per-wallet, not synced. This
  task reads the favorites set; it never writes it.
- **Invariant 14 — DRep status grounding.** Canonical on-chain status is `active | inactive`; `expiring` is
  renderer-derived display state; `retired` is deferred until a distinct unregistration signal exists.
  `DRepStatus` gains no new member.
- **Invariant 11 — Preliminary copy.** Every new en-US and ja-JP string keeps the leading `!!!` marker.
  This task adds none, so the gate is "the marker suite stays green and the catalogs stay byte-identical".

---

### Step 0 — Preconditions from task-157 (run before anything else)

```bash
cd /home/node/.claude/jobs/3bad97d1/wt-anchor-2
grep -n "doNotList" source/common/types/governance.types.ts
grep -n "doNotList" source/main/governance/AnchorVerificationService.ts
grep -n "doNotList" source/common/utils/logging.ts
grep -n -A 8 "export type AnchorEnrichEntry" source/renderer/app/stores/GovernanceStore.ts
grep -n -B 2 -A 14 "result.status === 'verified'" source/renderer/app/stores/GovernanceStore.ts
```

**If the first three greps return nothing, STOP.** task-157 has not landed; this task cannot start. Do not
add `doNotList` to `source/common/types/governance.types.ts` or to
`source/main/governance/AnchorVerificationService.ts` yourself — that is task-157's seam and widening it
twice produces a merge conflict and a duplicated parse rule.

**Step 0b — the accessor shape is fixed, not a choice.** task-157's Step 4b lands the verified member of
`AnchorEnrichEntry` as exactly one shape:

```ts
  | {
      state: 'verified';
      hash: string;
      host: string;
      content: VerifiedDRepAnchorContent;
    }
```

so every snippet below reads **`state.content.doNotList`** and **`state.content.givenName`**. The fourth grep
must print that nested member. If it prints a flat member instead (`givenName` / `doNotList` sitting directly
on the `verified` object), task-157 did not land as specified — **STOP and report it**; do not adapt the code
below, because a flat shape also breaks task-157's own `DRepDetailAnchorContent`, whose single source of
truth is `state.content.<field>` (task-157 §5, decision D-D).

**Step 0c — there is no store-side fallback to add.** task-157's Step 4c spreads the whole IPC payload into
the store state:

```ts
              content: {
                ...result.content,
                givenName: clampVerifiedName(result.content.givenName),
              },
```

so `doNotList` is already on `anchorStateByDRepId` when you start. The fifth grep must show that spread. Do
**not** add a per-field mapping line for `doNotList` in `fetchAnchorContent` and do **not** add a
`doNotList` member to `AnchorEnrichEntry` — both would duplicate task-157's seam.

---

### The live seams you are changing (quoted, so you do not have to hunt)

**A. `source/renderer/app/stores/GovernanceStore.ts:23-36` (HEAD) — the renderer entry.** The field you add
sits here, beside task-151's `verifiedName`:

```ts
export interface AppDRepDirectoryEntry {
  /** Bech32-encoded DRep ID. */
  drepId: string;
  /** Voting power in lovelace as BigNumber, or null if ranking unavailable. */
  votingPower: BigNumber | null;
  /** Active / Inactive. */
  status: DRepDirectoryEntry['status'];
  /** Remaining epochs until expiry (null if unknown). */
  drepActivity: DRepDirectoryEntry['drepActivity'];
  /** Anchor presence (URL + hash) from on-chain. No fetch performed in slice-1. */
  anchor: DRepAnchorPresence | null;
  /** Verified CIP-119 givenName, or null. Projection of anchorStateByDRepId. */
  verifiedName: string | null;
}
```

**B. `GovernanceStore.ts:211-225` (HEAD) — the cohort computed and its doc comment:**

```ts
  /**
   * Default cohort: rank by voting power, drop the top 35, keep up to the
   * next 200 eligible entries, then shuffle from the session seed. The
   * shuffle input is drepId-canonicalized so display order is a pure
   * function of (membership, seed) - stable across refreshes that change
   * voting powers without changing membership.
   */
  @computed get defaultCohort(): AppDRepDirectoryEntry[] | null {
    if (!this.isCohortActive) return null;
    const ranked = [...this.drepList].sort(compareByVotingPowerDesc);
    const eligible = ranked
      .slice(COHORT_TOP_EXCLUSION)
      .filter(
        (entry) =>
          entry.status === 'active' &&
          entry.drepActivity != null &&
          entry.drepActivity > COHORT_MIN_REMAINING_EPOCHS
      );
    const selected = eligible.slice(0, COHORT_MAX_SIZE);
    const canonical = [...selected].sort(compareDRepIdAsc);
    return seededShuffle(canonical, this.cohortSeed);
  }
```

**C. `GovernanceStore.ts:279-282` (HEAD) — `showAllList`, which you must NOT filter:**

```ts
  @computed get showAllList(): AppDRepDirectoryEntry[] {
    const canonical = [...this.drepList].sort(compareDRepIdAsc);
    return seededShuffle(canonical, this.cohortSeed);
  }
```

**D. `GovernanceStore.ts:501-514` (HEAD) — the rehydrator:**

```ts
  private _rehydrateDReps(raw: DRepDirectoryEntry[]): AppDRepDirectoryEntry[] {
    return raw.map((entry) => ({
      drepId: entry.drepId,
      votingPower: entry.votingPower ? new BigNumber(entry.votingPower) : null,
      status: entry.status,
      drepActivity: entry.drepActivity,
      anchor: entry.anchor,
      verifiedName: entry.verifiedName,
    }));
  }
```

**E. `GovernanceStore.ts:516-538` (HEAD) — the hash-guarded projection pass you extend and rename:**

```ts
  /**
   * Re-applies verified names onto a freshly rebuilt list. A name is dropped
   * when the entry's on-chain anchor hash no longer matches the hash that was
   * verified, so a re-registered anchor can never keep showing the old name.
   */
  private _applyVerifiedNames(
    entries: AppDRepDirectoryEntry[]
  ): AppDRepDirectoryEntry[] {
    if (this.anchorStateByDRepId.size === 0) return entries;
    return entries.map((entry) => {
      const state = this.anchorStateByDRepId.get(entry.drepId);
      const verifiedName =
        state != null &&
        state.state === 'verified' &&
        entry.anchor != null &&
        entry.anchor.hash === state.hash
          ? state.givenName
          : null;
      return verifiedName === entry.verifiedName
        ? entry
        : { ...entry, verifiedName };
    });
  }
```

Its three call sites (HEAD): `:325-327` in `fetchDRepList`, `:364-372` in `_enrichVotingPower`, `:447` in
`fetchAnchorContent`.

> Seam E is the one exception to this section's "re-locate by the quoted code, not by the number" rule.
> The quote above is HEAD; task-157 Step 4d already rewrote `? state.givenName` to `? state.content.givenName`
> before task-153 starts, so that line will not match verbatim. Locate the method by its name
> `_applyVerifiedNames` and replace it whole. Seams A/B/C/D are genuinely untouched by task-157 and do
> match verbatim.

**F. `source/renderer/app/components/governance/drep-directory/helpers.ts:279-288` — verified exact:**

```ts
/**
 * A favorited entry is stale once its status leaves the default-cohort
 * universe. Only the deferred retired status qualifies and no live entry
 * carries it yet; doNotList joins this check when anchor metadata lands.
 */
const STALE_FAVORITE_STATUSES: ReadonlySet<string> = new Set(['retired']);

export function isStaleFavorite(entry: AppDRepDirectoryEntry): boolean {
  return STALE_FAVORITE_STATUSES.has(entry.status);
}
```

**G. The favorites caption render path — no code change needed, this is why AC-6 works today.**
`DRepDirectoryList.tsx:40` declares `isStaleFavoriteEntry?: (entry: AppDRepDirectoryEntry) => boolean;`,
`:53` defaults it to the real helper (`isStaleFavoriteEntry = isStaleFavorite`), and `:95` applies it as
`isStaleFavorite={isFavoritesView && isStaleFavoriteEntry(entry)}`. `DRepDirectory.tsx:90` declares the prop
optional with **no** default and forwards it at `:298`, and `DRepDirectoryPage.tsx:91-116` never passes it —
so `undefined` falls through to the real helper. `DRepCard.tsx:128-132` renders the caption:

```tsx
      {isStaleFavorite && (
        <p className={styles.staleCaption}>
          {intl.formatMessage(messages.staleCaption)}
        </p>
      )}
```

---

### Step 1 — Add `doNotList` to `AppDRepDirectoryEntry`

In `source/renderer/app/stores/GovernanceStore.ts`, in `interface AppDRepDirectoryEntry` (seam A), add the
field immediately after `verifiedName`:

```ts
  /** Verified CIP-119 givenName, or null. Projection of anchorStateByDRepId. */
  verifiedName: string | null;
  /** Verified CIP-119 doNotList. Projection of anchorStateByDRepId; false until the anchor is fetched. */
  doNotList: boolean;
```

Type is `boolean`, **not** `boolean | null` and **not** optional. Rationale you must preserve: "unknown" and
"explicitly false" behave identically at every consumer, an optional property would let a construction site
omit it silently, and `boolean` keeps `isStaleFavorite` a one-line expression. Do **not** add a twin field to
`DRepDirectoryEntry` in `source/common/types/governance.types.ts` — the CLI never produces this flag, it is a
renderer-side projection only, and adding a wire field that main always sets to a constant repeats the
`verifiedName` mistake recorded as corpus conflict F-11.

### Step 2 — Default it in `_rehydrateDReps`

In seam D, add one line after `verifiedName: entry.verifiedName,`:

```ts
      verifiedName: entry.verifiedName,
      // Not a wire field: doNotList is projected from verified anchor content.
      doNotList: false,
```

That comment stays — the absence of a wire source is the non-obvious part.

### Step 3 — Rename `_applyVerifiedNames` to `_applyVerifiedMetadata` and project the flag

Replace seam E in full with:

```ts
  /**
   * Re-applies verified anchor state onto a freshly rebuilt list. Both the
   * name and the doNotList flag are dropped when the entry's on-chain anchor
   * hash no longer matches the hash that was verified, so a re-registered
   * anchor can never keep showing the old name nor keep the DRep out of the
   * cohort.
   */
  private _applyVerifiedMetadata(
    entries: AppDRepDirectoryEntry[]
  ): AppDRepDirectoryEntry[] {
    if (this.anchorStateByDRepId.size === 0) return entries;
    return entries.map((entry) => {
      const state = this.anchorStateByDRepId.get(entry.drepId);
      const verified =
        state != null &&
        state.state === 'verified' &&
        entry.anchor != null &&
        entry.anchor.hash === state.hash
          ? state
          : null;
      const verifiedName =
        verified === null ? null : verified.content.givenName;
      const doNotList = verified !== null && verified.content.doNotList;
      return verifiedName === entry.verifiedName &&
        doNotList === entry.doNotList
        ? entry
        : { ...entry, verifiedName, doNotList };
    });
  }
```

Notes a small model must not get wrong:

- The `verified` const (rather than a boolean flag) is deliberate: it gives TypeScript the narrowed
  `state: 'verified'` member, which is the only member carrying `content`, so both
  `verified.content.givenName` and `verified.content.doNotList` typecheck without a cast.
- `verified !== null && verified.content.doNotList` evaluates to `boolean`, matching the non-optional
  `doNotList: boolean` you added in Step 1. `content.doNotList` is itself `boolean` on task-157's widened
  `VerifiedDRepAnchorContent` (`source/common/types/governance.types.ts`, its Step 1), so no `=== true`
  coercion is needed — but the `verified !== null` guard is, because an unfetched entry has no content.
- The identity short-circuit (`return entry` when nothing changed) must keep BOTH fields in its comparison,
  otherwise a `doNotList` transition on an entry whose name did not change is silently dropped.

Then rename all three call sites — `_applyVerifiedNames(` → `_applyVerifiedMetadata(` at `:325`, `:364` and
`:447` (HEAD). Verify none remain:

```bash
grep -rn "_applyVerifiedNames" source/ tests/ storybook/    # must print nothing
grep -rn "_applyVerifiedMetadata" source/renderer/app/stores/GovernanceStore.ts   # must print 4 lines
```

The rename is mandatory: a private method named `_applyVerifiedNames` that also applies a cohort-exclusion
flag is a name that lies. It is private, so `tsc --noEmit` finds every call site.

### Step 4 — Exclude `doNotList` from `defaultCohort` only

In seam B, add exactly one clause to the existing `.filter(...)` predicate and extend the doc comment:

```ts
  /**
   * Default cohort: rank by voting power, drop the top 35, keep up to the
   * next 200 eligible entries, then shuffle from the session seed. The
   * shuffle input is drepId-canonicalized so display order is a pure
   * function of (membership, seed) - stable across refreshes that change
   * voting powers without changing membership. A verified doNotList entry
   * is dropped from the eligible pool only - the top-35 slice is taken
   * first so it stays identical to top35DRepIds.
   */
  @computed get defaultCohort(): AppDRepDirectoryEntry[] | null {
    if (!this.isCohortActive) return null;
    const ranked = [...this.drepList].sort(compareByVotingPowerDesc);
    const eligible = ranked
      .slice(COHORT_TOP_EXCLUSION)
      .filter(
        (entry) =>
          entry.status === 'active' &&
          !entry.doNotList &&
          entry.drepActivity != null &&
          entry.drepActivity > COHORT_MIN_REMAINING_EPOCHS
      );
    const selected = eligible.slice(0, COHORT_MAX_SIZE);
    const canonical = [...selected].sort(compareDRepIdAsc);
    return seededShuffle(canonical, this.cohortSeed);
  }
```

**The ordering is load-bearing and there is exactly one correct placement.** The filter goes *after*
`.slice(COHORT_TOP_EXCLUSION)`. Filtering `ranked` *before* the slice would change which entries fall inside
the top 35, desynchronising `defaultCohort` from `top35DRepIds` (`GovernanceStore.ts:264-272`, HEAD), which
is computed from the unfiltered list. Do not touch `COHORT_TOP_EXCLUSION`, `COHORT_MAX_SIZE`,
`COHORT_MIN_REMAINING_EPOCHS`, `compareByVotingPowerDesc`, `compareDRepIdAsc` or `seededShuffle`.

**Change nothing else in the store.** In particular leave `showAllList` (seam C), `top35DRepIds`,
`drepIndex`, `displayedDRepList`, `verifiedMetadataIds`, `cohortMedianVotingPower` and `cohortContext`
exactly as they are. Two consequences are correct and expected, not bugs: excluding an entry also removes it
from `cohortContext.memberIds` (so `DRepCategoryBadge` classifies it as out-of-cohort — informational only,
per invariant 8), and it leaves `cohortMedianVotingPower`'s sample by construction.

### Step 5 — Teach `isStaleFavorite` the flag

In `source/renderer/app/components/governance/drep-directory/helpers.ts`, replace `:279-288` with:

```ts
/**
 * A favorited entry is stale once it leaves the default-cohort universe:
 * a verified anchor asking not to be listed, or the still-deferred retired
 * status that no live entry carries yet.
 */
const STALE_FAVORITE_STATUSES: ReadonlySet<string> = new Set(['retired']);

export function isStaleFavorite(entry: AppDRepDirectoryEntry): boolean {
  return entry.doNotList || STALE_FAVORITE_STATUSES.has(entry.status);
}
```

Keep `STALE_FAVORITE_STATUSES` and the `'retired'` member: `retired` stays deferred under invariant 14 and
removing the set would delete the forward-compatible seam task-122 shipped. Add no import — `helpers.ts:2`
already imports `AppDRepDirectoryEntry`. Add nothing to `filterDReps` (`:189-220`) or `sortDReps`
(`:243-277`).

### Step 6 — Fixture sweep: 14 construction sites

`AppDRepDirectoryEntry` gains a required field, so every object literal that builds one must add
`doNotList: false,`. Place the new line immediately after each `verifiedName: null,` line, and where the
builder ends in `...overrides` put it **before** the spread so overrides still win. `tsc --noEmit` is the
authority; these are the sites measured at HEAD:

| file | site(s) at HEAD | note |
|---|---|---|
| `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` | `:32` (`baseEntries[0]`), `:42` (`buildEntry`), `:80` (`realEntry`, before `...overrides`) | |
| `source/renderer/app/components/governance/drep-directory/helpers.spec.ts` | `:58` (`buildEntry`, before `...overrides`) | |
| `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx` | `:37` (`ACTIVE_ENTRY`) | typed, tsc flags it |
| `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx` | `:37` (`baseEntry`) at HEAD — **task-157 moved it**; re-locate the `verifiedName: null,` line inside `const baseEntry: AppDRepDirectoryEntry = {` | typed, tsc flags it |
| `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx` | `:31` (`drepEntry`) | untyped literal — tsc will **not** flag it; add it anyway, Step 10 needs it |
| `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` | `:134` (`drepEntry`) | untyped literal — add it anyway for fixture honesty |
| `storybook/stories/governance/CurrentVoteSummary.stories.tsx` | `:50` | typed return, tsc flags it |
| `storybook/stories/governance/DRepDetail.stories.tsx` | `:66` (`withAnchorEntry`) at HEAD — **task-157's Step 13 grew `anchorStateFor` above it**; re-locate by the `withAnchorEntry` identifier | typed, tsc flags it |
| `storybook/stories/governance/DRepDirectory.stories.tsx` | `:55`, `:63` (`baseEntries`), `:81` (`buildEntry`) | see Step 11 — `:63` becomes `true` |
| `storybook/stories/governance/_utils/fixtures.ts` | `:159`, `:170` | typed map, tsc flags both |

Do **not** touch the `verifiedName: null` occurrences that build the **wire** type `DRepDirectoryEntry` —
they are a different interface and gain nothing. Those are:
`source/main/governance/GovernanceQueryService.ts:526`,
`tests/jest/governance/GovernanceStore.spec.ts:51,75,336,576,711,973` (HEAD numbers — task-157 edits that
file at `:671-680` and `:995-1000`, so the last two shift; identify them by the enclosing
`DRepDirectoryEntry` literal, not by the line),
`tests/jest/governance/logDRepStateSnapshot.spec.ts:45`.

### Step 7 — Store tests (`tests/jest/governance/GovernanceStore.spec.ts`)

Append one self-contained `describe` block at the end of the file. It does not disturb the existing blocks
and resets all three channel mocks itself (the `default cohort` block resets only two, which is why the tests
do not go there).

```ts
describe('GovernanceStore doNotList cohort exclusion', () => {
  const drepIdAt = (i: number) =>
    `drep1dnl${String(i).padStart(4, '0')}aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa`;

  // Real preprod on-chain anchor pair from the epoch-295 drep-state sample.
  const ANCHOR = {
    hash: '9e8cb2b0f4c2ddbd9dea316b44680d8a989743868aeb40c1e6959982452f38e1',
    url: 'https://raw.githubusercontent.com/cardano-foundation/cardano-academy/refs/heads/main/Cardano%20Academy.jsonld',
  };
  // Below the 35-entry top exclusion, so the entry is cohort-eligible before the fetch.
  const OPTED_OUT = 37;

  beforeEach(() => {
    mockRequest.mockReset();
    mockStakeRequest.mockReset();
    mockAnchorRequest.mockReset();
  });

  const buildDrep = (
    i: number,
    overrides: Partial<DRepDirectoryEntry> = {}
  ): DRepDirectoryEntry => ({
    anchor: i === OPTED_OUT ? ANCHOR : null,
    verifiedName: null,
    drepActivity: 10,
    drepId: drepIdAt(i),
    status: 'active',
    votingPower: null,
    ...overrides,
  });

  const allDreps = (): DRepDirectoryEntry[] =>
    Array.from({ length: 40 }, (_, i) => buildDrep(i));

  const stakeFor = (count: number): Record<string, string> => {
    const map: Record<string, string> = {};
    for (let i = 0; i < count; i++) {
      map[drepIdAt(i)] = String(1_000_000_000_000 - i * 1_000_000);
    }
    return map;
  };

  const loadStore = async (
    dreps: DRepDirectoryEntry[] = allDreps()
  ): Promise<GovernanceStore> => {
    mockRequest.mockResolvedValue({
      dreps,
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockResolvedValue({
      fetchedAt: 1_750_000_000_500,
      stakeByDRepId: stakeFor(dreps.length),
    });
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();
    return store;
  };

  const optedOutResponse = () => ({
    status: 'verified' as const,
    content: { givenName: 'Opted Out DRep', doNotList: true },
    host: 'raw.githubusercontent.com',
    fetchedAt: 1_750_000_001_000,
  });

  it('drops a verified doNotList DRep from the default cohort and the cohort context', async () => {
    const store = await loadStore();
    expect(store.defaultCohort?.map((e) => e.drepId)).toContain(
      drepIdAt(OPTED_OUT)
    );

    mockAnchorRequest.mockResolvedValue(optedOutResponse());
    await store.fetchAnchorContent(drepIdAt(OPTED_OUT), ANCHOR);

    expect(store.drepIndex.get(drepIdAt(OPTED_OUT))?.doNotList).toBe(true);
    expect(store.defaultCohort?.map((e) => e.drepId)).not.toContain(
      drepIdAt(OPTED_OUT)
    );
    expect(store.cohortContext.memberIds?.has(drepIdAt(OPTED_OUT))).toBe(false);
  });

  it('keeps a doNotList DRep in showAllList, drepList and drepIndex', async () => {
    const store = await loadStore();
    mockAnchorRequest.mockResolvedValue(optedOutResponse());
    await store.fetchAnchorContent(drepIdAt(OPTED_OUT), ANCHOR);

    expect(store.showAllList.map((e) => e.drepId)).toContain(
      drepIdAt(OPTED_OUT)
    );
    expect(store.drepList.map((e) => e.drepId)).toContain(drepIdAt(OPTED_OUT));
    expect(store.drepIndex.get(drepIdAt(OPTED_OUT))?.drepId).toBe(
      drepIdAt(OPTED_OUT)
    );
    expect(store.showAllList).toHaveLength(40);
  });

  it('leaves every other entry cohort-eligible and shrinks the cohort by exactly one', async () => {
    const store = await loadStore();
    const before = store.defaultCohort?.length ?? 0;

    mockAnchorRequest.mockResolvedValue(optedOutResponse());
    await store.fetchAnchorContent(drepIdAt(OPTED_OUT), ANCHOR);

    expect(store.defaultCohort).toHaveLength(before - 1);
  });

  it('returns a doNotList DRep to the cohort when the on-chain anchor hash changes', async () => {
    const store = await loadStore();
    mockAnchorRequest.mockResolvedValue(optedOutResponse());
    await store.fetchAnchorContent(drepIdAt(OPTED_OUT), ANCHOR);
    expect(store.defaultCohort?.map((e) => e.drepId)).not.toContain(
      drepIdAt(OPTED_OUT)
    );

    const reRegistered = allDreps().map((entry) =>
      entry.drepId === drepIdAt(OPTED_OUT)
        ? { ...entry, anchor: { ...ANCHOR, hash: 'b'.repeat(64) } }
        : entry
    );
    mockRequest.mockResolvedValue({
      dreps: reRegistered,
      epoch: 513,
      fetchedAt: 1_750_000_002_000,
    });
    await store.fetchDRepList();

    expect(store.drepIndex.get(drepIdAt(OPTED_OUT))?.doNotList).toBe(false);
    expect(store.defaultCohort?.map((e) => e.drepId)).toContain(
      drepIdAt(OPTED_OUT)
    );
  });

  it('leaves the cohort untouched when the anchor verifies without doNotList', async () => {
    const store = await loadStore();
    mockAnchorRequest.mockResolvedValue({
      ...optedOutResponse(),
      content: { givenName: 'Listed DRep', doNotList: false },
    });
    await store.fetchAnchorContent(drepIdAt(OPTED_OUT), ANCHOR);

    expect(store.drepIndex.get(drepIdAt(OPTED_OUT))?.doNotList).toBe(false);
    expect(store.defaultCohort?.map((e) => e.drepId)).toContain(
      drepIdAt(OPTED_OUT)
    );
  });
});
```

Why the fourth test matters: it is the proof of the hash guard in Step 3 — a re-registered anchor must not
keep a DRep out of the cohort on the strength of a stale verification.

`mockAnchorRequest` is an untyped `jest.Mock`, so the two-field `content` literal compiles regardless of how
many fields task-157 put on `VerifiedDRepAnchorContent`. Note that `content` here is the **IPC response**
shape (`DRepAnchorResult`), which happens to match the store's `AnchorEnrichEntry.content` — do not confuse
the two when reading Step 3.

**Do not add a `doNotList` parse test to `tests/jest/governance/AnchorVerificationService.spec.ts`.** The
parser is task-157's seam and its own suite covers it; a duplicate case there is a scope leak.

### Step 8 — Helper test (`source/renderer/app/components/governance/drep-directory/helpers.spec.ts`)

Replace the `describe('isStaleFavorite')` block at `:360-367` with:

```ts
describe('isStaleFavorite', () => {
  it('is false for every current on-chain status', () => {
    expect(isStaleFavorite(buildEntry(1))).toBe(false);
    expect(
      isStaleFavorite(buildEntry(2, { drepActivity: 0, status: 'inactive' }))
    ).toBe(false);
  });

  it('is true for a verified doNotList entry at either status', () => {
    expect(isStaleFavorite(buildEntry(3, { doNotList: true }))).toBe(true);
    expect(
      isStaleFavorite(buildEntry(4, { doNotList: true, status: 'inactive' }))
    ).toBe(true);
  });
});
```

### Step 9 — Directory component tests (`.../drep-directory/DRepDirectory.spec.tsx`)

Four tests. The first two go into the top-level `describe('DRepDirectory')`, immediately after
`it('finds and opens a non-cohort entry by ID with show-all off', …)` at `:612`. The last two go inside
`describe('favorites')` (`:732`), after
`it('never renders the stale caption in the directory view', …)`.

The component receives the cohort as the `drepList` prop, so an excluded entry is modelled by leaving it out
of `drepList` while keeping it in `showAllList` — exactly what the store now produces.

```tsx
  it('surfaces a doNotList entry through show-all', () => {
    // The store drops the opted-out entry from the cohort but never from
    // showAllList, so the escape hatch must still reach it.
    const cohortEntry = realEntry(4);
    const optedOutEntry = realEntry(5, { doNotList: true });
    renderComponent({
      drepList: [cohortEntry],
      showAllList: [cohortEntry, optedOutEntry],
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);

    fireEvent.click(screen.getByText('!!!Show all DReps'));

    expect(screen.getAllByText('!!!View details')).toHaveLength(2);
  });

  it('opens a doNotList entry from an exact DRep ID with show-all off', () => {
    const onViewDetails = jest.fn();
    const cohortEntry = realEntry(4);
    const optedOutEntry = realEntry(5, { doNotList: true });
    renderComponent({
      drepList: [cohortEntry],
      showAllList: [cohortEntry, optedOutEntry],
      onViewDetails,
    });

    const input = screen.getByPlaceholderText('!!!Search by DRep ID');
    fireEvent.change(input, {
      target: { value: realDrepId(5).slice(0, 'drep1'.length + 20) },
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    expect(onViewDetails).not.toHaveBeenCalled();

    fireEvent.change(input, { target: { value: realDrepId(5) } });

    expect(onViewDetails).toHaveBeenCalledTimes(1);
    expect(onViewDetails).toHaveBeenCalledWith(realDrepId(5));
  });
```

```tsx
    it('captions a doNotList favorite through the real predicate and keeps its status badge', () => {
      renderComponent({
        drepList: [realEntry(1), realEntry(2, { doNotList: true })],
        favoriteDRepIds: new Set([realDrepId(1), realDrepId(2)]),
        view: 'favorites',
      });

      expect(
        screen.getAllByText(/no longer in the default cohort/)
      ).toHaveLength(1);
      expect(screen.getAllByLabelText('!!!Active')).toHaveLength(2);
      expect(screen.getAllByText('!!!View details')).toHaveLength(2);
      expect(screen.getByText(truncatedDrepId(2))).toBeInTheDocument();
    });

    it('renders no caption for a doNotList favorite in the directory view', () => {
      renderComponent({
        drepList: [realEntry(2, { doNotList: true })],
        favoriteDRepIds: new Set([realDrepId(2)]),
      });

      expect(
        screen.queryByText(/no longer in the default cohort/)
      ).not.toBeInTheDocument();
      expect(screen.getByText('!!!View details')).toBeInTheDocument();
    });
```

Three things the first favorites test proves at once and must not be weakened: no
`isStaleFavoriteEntry` is passed (so the real `isStaleFavorite` runs), the caption is selective (1 of 2
favorites), and the opted-out favorite keeps its real `Active` badge and its row — no purge, no invented
badge. `getAllByLabelText('!!!Active')` targets the `aria-label` on `DRepStatusBadge`'s wrapper span
(`DRepStatusBadge.tsx:32-34`); do **not** use `getAllByText('!!!Active')`, which matches both the wrapper and
the inner label span and returns twice the card count.

### Step 10 — Container test (`source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx`)

This is the AC-6 proof through the real container, which is the component that must not inject a predicate.

Parameterise the existing fixtures. `buildGovernanceStore` at `:38`:

```ts
const buildGovernanceStore = (
  entry: typeof drepEntry = drepEntry,
  favoriteDRepIds: Set<string> = new Set<string>()
) => ({
  cohortContext: {
    medianVotingPower: null,
    memberIds: null,
    verifiedMetadataIds: new Set<string>(),
  },
  displayedDRepList: [entry],
  drepIndex: new Map([[entry.drepId, entry]]),
  drepList: [entry],
  error: null,
  favoriteDRepIds,
  isCohortActive: true,
  lastFetchedAt: Date.now() - 60_000,
  refresh: jest.fn(),
  refreshState: GovernanceRefreshState.Loaded,
  reshuffleCohort: jest.fn(),
  showAllList: [entry],
  toggleFavorite: jest.fn(),
  top35DRepIds: new Set<string>(),
  votingPowerState: VotingPowerEnrichState.Loaded,
});
```

`renderPage` at `:60` gains two optional inputs and forwards them:

```ts
const renderPage = ({
  isNodeInSync = true,
  syncProgress = 100,
  initialRoute = ROUTES.GOVERNANCE.DREPS,
  entry = drepEntry,
  favoriteDRepIds = new Set<string>(),
}: {
  isNodeInSync?: boolean;
  syncProgress?: number | null;
  initialRoute?: string;
  entry?: typeof drepEntry;
  favoriteDRepIds?: Set<string>;
} = {}) => {
  // Observable so the container's reaction sees the flip like the real store.
  const networkStatus = observable({ isNodeInSync, syncProgress });
  const governance = buildGovernanceStore(entry, favoriteDRepIds);
```

Everything below that line in `renderPage` is unchanged. Then append one test inside
`describe('DRepDirectoryPage')`:

```ts
  it('captions a doNotList favorite on the favorites route with no predicate injected', () => {
    renderPage({
      initialRoute: ROUTES.GOVERNANCE.FAVORITES,
      entry: { ...drepEntry, doNotList: true },
      favoriteDRepIds: new Set([drepEntry.drepId]),
    });

    expect(
      screen.getByText(/no longer in the default cohort/)
    ).toBeInTheDocument();
  });
```

If this test ever fails because someone added an `isStaleFavoriteEntry={…}` prop to
`DRepDirectoryPage.tsx`, the fix is to delete that prop, not to adjust the test.

### Step 11 — Storybook

Two edits in `storybook/stories/governance/DRepDirectory.stories.tsx`, both of which retire an explicitly
recorded placeholder. Add no new story file, no local `IntlProvider`, and no per-locale story variant — the
global `StoryWrapper` decorator in `storybook/preview.tsx` supplies the English/Japanese toggle, and the
existing `Governance / DRep Directory > Connected flow` story (`:290`) already covers the integrated
favorites journey and needs no change beyond inheriting the fixture below.

**11a.** In `baseEntries` (`:49-70`), give the second entry the real flag (it is the entry the stale-favorite
story already targets):

```tsx
  {
    anchor: null,
    verifiedName: null,
    doNotList: true,
    drepActivity: 4,
    drepId: 'drep1xj23tk3y_qyv7c9m2z89w3t8mvk9e2uwc3q8u6j7r2x5y9w0p1',
    status: 'inactive',
    votingPower: new BigNumber('940000000'),
  },
```

The first `baseEntries` entry (`:50-60`) and `buildEntry` (`:72-88`) get `doNotList: false,` per Step 6.
Setting the flag here does not remove the entry from any directory story: the stories pass a fixed
`drepList`, and the cohort exclusion is a store computed the stories do not run. That is correct — the story
demonstrates the favorites treatment, not the cohort maths.

**11b.** Rewrite the `'Favorites view — stale favorite'` story (`:538-556`) to drop the synthetic predicate
and its now-false comment:

```tsx
  // The favorites treatment for a real verified doNotList entry: status badge
  // plus inline caption, never an auto-purge.
  .add('Favorites view — stale favorite', () => (
    <div style={CENTERED_STYLE}>
      {renderDirectory(
        GovernanceRefreshState.Loaded,
        baseEntries,
        null,
        DEFAULT_SYNC_STATE,
        true,
        {
          view: 'favorites',
          favoriteDRepIds: new Set(baseEntries.map((e) => e.drepId)),
        }
      )}
    </div>
  ));
```

Leave `FavoritesOptions.isStaleFavoriteEntry` (`:154`) and its forwarding at `:179` in place — the seam stays
available; this story simply stops needing it.

**Manual check (part of the Verify block):** run `yarn storybook`, open
`Governance / DRep Directory > Favorites view — stale favorite`, confirm exactly one of the two cards shows
the caption under its badge row, then flip the global toggle to Japanese and confirm the caption renders in
ja-JP without overflowing the card. `yarn storybook:build` is red at HEAD for a pre-existing
manager-webpack reason unrelated to this change; do not treat it as a gate.

### Step 12 — Design-doc reconciliation

**12a — the one real edit (AC-7).** In
`.agent/plans/governance/drep-discovery/designs/drep-discovery-design.md`, replace line **112** in full.

Current line 112, verified verbatim in this worktree:

> **Stale favorites.** If a favorited DRep becomes Retired or appears with `doNotList=true` after `anchor-2` lands, it remains in the favorites list with its current `Retired` or `Excluded from default cohort` status badge (shared tokens §1) and an inline caption: `governance.drepFavorites.staleCaption` → *"This DRep is no longer in the default cohort."* No automatic removal. The user unfavorites explicitly.

Replacement line 112:

> **Stale favorites.** If a favorited DRep appears with `doNotList=true` after `anchor-2` lands, it remains in the favorites list with its current status badge and an inline caption: `governance.drepFavorites.staleCaption` → *"This DRep is no longer in the default cohort."* `DRepStatus` is the closed union `active | inactive`, so no `Retired` or `Excluded from default cohort` badge exists to show; the caption alone carries the signal. `Retired` stays deferred until a distinct unregistration signal exists. No automatic removal. The user unfavorites explicitly.

Why: `DRepStatusBadge.tsx:26-29` is an exhaustive `Record<DRepStatus, string>` over `active | inactive`, and
AC-5 plus invariant 14 forbid adding a member, so the badge the old sentence promised is unimplementable in
this release. Record the conflict in the commit's tracker `statusReason`.

**12b — verify-and-record, no edit (AC-9, AC-10, AC-11, AC-12).** The tasks JSON cites stale line numbers;
task-165's inserted "Directory Identity: ID-Only in v1" section (`:249-259`) shifted them, and the required
content is already present at every corrected anchor. **Adding a second paragraph beside the existing one is
a defect.** Confirm each, then record it in `statusReason`:

```bash
sed -n '110p;112p;239p;241,245p' \
  .agent/plans/governance/drep-discovery/designs/drep-discovery-design.md
```

| AC | JSON cites | real anchor | what must be there |
|---|---|---|---|
| AC-9 | `:228` | **`:239`** | "`Show all` replaces the cohort with the **full registration list** — every registered DRep, including the top 35, entries below the 6-epoch floor and inactive entries — in the same seeded session order as the cohort…" — matches `GovernanceStore.showAllList` (seam C) exactly |
| AC-10 | `:228` | **`:239`**, same block | "Once `doNotList` lands in `anchor-2`, a `doNotList=true` DRep is excluded from the default cohort but stays reachable through show-all, search and direct DRep ID entry." |
| AC-11 | `:109` (blank) | **`:110`** | "No illustration ships — the earlier 'prominent illustration' claim is resolved as dropped, not deferred." Matches `DRepEmptyState.tsx:105-123`, which has no asset. Resolved; make no edit and ship no asset |
| AC-12 | `:230-234` | **`:239`** (opt-in sorts, tail of the same paragraph) + **`:241-245`** (popularity guardrail) | do-not-touch; live behaviour is `DRepDirectory.tsx:351-355` |

After 12a, this must show exactly one changed line:

```bash
git diff --stat .agent/plans/governance/drep-discovery/designs/drep-discovery-design.md
git diff .agent/plans/governance/drep-discovery/designs/drep-discovery-design.md
```

`.agent/**` is prettier-ignored (`.prettierignore` ignores `/*` and re-includes only `source/`, `features/`,
`storybook/`, `hardware-wallet-tests/`, `tests/`), so never run prettier over the design doc.

### Step 13 — Tracker: re-verify task-122 AC-5 (AC-8), update task-153, commit

The tasks JSON is **tool-managed**: edit values in place, preserve surrounding formatting, never run prettier
over `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`. Both row edits and
the code land in **one** commit (AC-8 says "in this task's commit").

**13a — re-verify task-122 AC-5 against the real predicate first.** AC-5 reads, verbatim
(`governance-drep-discovery-plan-tasks.json:741`):

> "Favorites page renders Retired and doNotList-excluded DReps with the appropriate status badge and the staleCaption message ID. A Storybook story covers the stale-favorite state. No automatic purge of favorites on status change."

Run the checks and keep the output for the `statusReason`:

```bash
node_modules/.bin/jest \
  --testPathPattern="(drep-directory/helpers.spec|drep-directory/DRepDirectory.spec|containers/governance/DRepDirectoryPage.spec)" \
  --no-coverage --runInBand
grep -rn "isStaleFavoriteEntry" source/renderer/app/containers/governance/DRepDirectoryPage.tsx   # must print nothing
```

Verdict to record: the `doNotList` half is now exercised end-to-end (real predicate, real badge, real
caption, no injected prop, no purge) and a story covers it; the `Retired` half stays deferred under
invariant 14, and no `Retired` / `Excluded from default cohort` badge exists, so the badge clause is
satisfied by the entry's real `active`/`inactive` badge.

**13b — edit the task-122 row.** In `governance-drep-discovery-plan-tasks.json`, on the object with
`"id": "task-122"` (`:699`): leave `"status": "complete"`, set `"updatedAt"` to today's `YYYY-MM-DD`, and
replace the final sentence of `"statusReason"` — the one beginning `Complete, NOT verified: AC-5's real
Retired/doNotList stale-state rendering cannot be exercised…` and ending `…deferred to those owners plus
slice-8's release-verification.` — with prose stating that anchor-2 task-153 landed the real `doNotList`
signal on `AppDRepDirectoryEntry`, that `isStaleFavorite` returns true for it, that the favorites view and
`DRepDirectoryPage` render the `staleCaption` through the real predicate with no `isStaleFavoriteEntry`
injected and with no purge, that the Storybook stale-favorite story now uses the real flag instead of a
synthetic predicate, and that the `Retired` half stays deferred under invariant 14 with no `Retired` /
`Excluded from default cohort` badge in this release. Add
`source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx` to task-122's `"evidence"` array if it
is not already there. Change nothing else on that row.

**13c — edit the task-153 row** (`:1799`). Set `"status": "complete"`, add `"statusReason"`, `"evidence"` and
`"updatedAt"` in the key order the completed sibling rows use — `id, title, description, status,
statusReason, evidence, updatedAt, priority, estimatedHours, dependencies, targetPath, acceptanceCriteria`.
`statusReason` must state, in prose: the measured per-suite test deltas and the `tsc --noEmit` result; that
`doNotList` is consumed from task-157's already-widened `VerifiedDRepAnchorContent` and that this task added
neither a wire field nor a parser rule; that AC-7 was re-scoped because
`drep-discovery-design.md:112`'s `Retired` / `Excluded from default cohort` badge is unimplementable against
the closed `DRepStatus` union, and that `:112` was struck accordingly; that AC-9/AC-10/AC-11 are already
satisfied at the corrected anchors `:239`, `:239` and `:110` and were verified rather than re-edited, and
AC-12's sorts (`:239`) and popularity guardrail (`:241-245`) are unchanged; that AC-8's task-122
re-verification landed in this commit; and — never reported green — the stated limitation that AC-1 holds
only for DReps whose anchor was fetched in the current session, because the anchor fetch is lazy and
per-detail-visit (`shared-design-tokens.md:250`, `drep-discovery-design.md:247`), so an unvisited
`doNotList: true` DRep stays in the cohort and an unvisited `doNotList: true` favorite shows no caption.

**13d — commit.** Exactly one subject line, no body, no trailer, no `Co-Authored-By`:

```
feat(gov): task-153 exclude doNotList DReps from the default cohort
```

---

### Verify

Run everything from `/home/node/.claude/jobs/3bad97d1/wt-anchor-2`. The baselines below were **measured at
`55e8985bf`**, before task-157 landed. task-157 changes the counts of three of the suites named here —
`tests/jest/governance/GovernanceStore.spec.ts` (+2), `DRepDetailPage.spec.tsx` (+10) and
`tests/jest/security/governance-sanitization.spec.ts` (+2) — and leaves the rest untouched. **Re-measure
each suite immediately before Step 1 and treat the delta as the contract, not the total.**

```bash
cd /home/node/.claude/jobs/3bad97d1/wt-anchor-2

# 1. FORMAT — nix fmt is unavailable in this devcontainer and remains a
#    user-owned pre-merge obligation. Explicit paths only; NEVER `yarn prettier`
#    (its package.json script embeds a repo-wide "**/*.*" glob and reformats
#    ~250 unrelated files even when handed a path).
node_modules/.bin/prettier --write \
  source/renderer/app/stores/GovernanceStore.ts \
  source/renderer/app/components/governance/drep-directory/helpers.ts \
  source/renderer/app/components/governance/drep-directory/helpers.spec.ts \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx \
  source/renderer/app/containers/governance/DRepDetailPage.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx \
  storybook/stories/governance/DRepDetail.stories.tsx \
  storybook/stories/governance/CurrentVoteSummary.stories.tsx
```

**Do NOT run prettier on these three files** — they carry pre-existing drift at HEAD (the repo was last
formatted by a newer prettier than the pinned 2.1.2, so `--write` would revert unrelated lines):

- `tests/jest/governance/GovernanceStore.spec.ts` (3 drifted lines at `:956-957`)
- `storybook/stories/governance/DRepDirectory.stories.tsx` (13 drifted lines at `:368-374`)
- `storybook/stories/governance/_utils/fixtures.ts` (6 drifted lines at `:156-159`)

Hand-match the surrounding style in those three (2-space indent, trailing commas, single quotes) and confirm
you introduced no new drift beyond what HEAD already had:

```bash
node_modules/.bin/prettier --check \
  tests/jest/governance/GovernanceStore.spec.ts \
  storybook/stories/governance/DRepDirectory.stories.tsx \
  storybook/stories/governance/_utils/fixtures.ts
# expect: the same three files reported, and no others
```

Never run prettier over `.agent/**` (prettier-ignored) or over the i18n catalogs.

```bash
# 2. TYPECHECK — the authority for the Step 6 fixture sweep.
node_modules/.bin/typed-scss-modules source/renderer/app
node_modules/.bin/tsc --noEmit
# expect: 0 errors. tsconfig.json has no "include", so this covers source/, tests/ AND storybook/.

# 3. FOCUSED SUITES — measured at 55e8985bf; deltas are the contract.
node_modules/.bin/jest --testPathPattern="drep-directory/helpers.spec" --no-coverage --runInBand
# baseline 1 suite / 25 tests  -> expect 26 (+1, Step 8)

node_modules/.bin/jest --testPathPattern="drep-directory/DRepDirectory.spec" --no-coverage --runInBand
# baseline 1 suite / 48 tests / 1 snapshot -> expect 52 (+4, Step 9), snapshot unchanged at 1

node_modules/.bin/jest --testPathPattern="containers/governance/DRepDirectoryPage.spec" --no-coverage --runInBand
# baseline 1 suite / 8 tests -> expect 9 (+1, Step 10)

node_modules/.bin/jest --testPathPattern="jest/governance/GovernanceStore.spec" --no-coverage --runInBand
# baseline 1 suite / 49 tests at 55e8985bf -> expect +5 (Step 7) over whatever task-157 left

# 4. SANITIZATION FLOOR — the cv-2 F-31 two-anchor rule: cite BOTH, together.
#    Citing only one is a false green. Neither may change: this task adds no
#    logger call, no analytics call and no electron-store write.
node_modules/.bin/jest --testPathPattern="security/governance-sanitization" --no-coverage --runInBand
# 35 tests at 55e8985bf, 37 in build order (task-157 adds 2) -> expect that
# number unchanged. Measure it before Step 1; the delta is the contract.
node_modules/.bin/jest --testPathPattern="jest/governance/logDRepStateSnapshot" --no-coverage --runInBand
# baseline 1 suite / 5 tests -> expect 5, unchanged (no slice task touches it)

# 5. No new sink reaches the flag (must print nothing):
grep -rn "doNotList" source/ | grep -Ei "logger|analytics|electron-store|setDRepFavorites"

# 6. i18n — this task adds ZERO keys. Run the gate anyway to prove it.
node_modules/.bin/jest --testPathPattern="i18n/preliminaryCopyMarkers" --no-coverage --runInBand
# baseline 1 suite / 5 tests -> expect 5, unchanged
yarn i18n:manage
# yarn i18n:manage WRITES files. Expect NO change to the two catalogs. Revert any
# incidental write surgically — NEVER `git stash`, the stash stack is shared
# across worktrees and concurrent sessions:
git status --porcelain source/renderer/app/i18n translations
git restore source/renderer/app/i18n/locales/en-US.json \
            source/renderer/app/i18n/locales/ja-JP.json \
            source/renderer/app/i18n/locales/defaultMessages.json \
            translations/messages.json
#   All four catalogs live under i18n/locales/ except translations/messages.json;
#   `git restore` aborts the WHOLE invocation on an unmatched pathspec, so a
#   mistyped path silently leaves the other three files dirty.
#   (restore only the paths that show as modified and that you did not intend to change)

# 7. LINT — errors are the gate; warnings are not. ~5591 pre-existing warnings at HEAD.
yarn lint     # expect exit 0, 0 errors

# 8. FULL JEST — `jest tests/jest` is only ~8% of the suite; this is the real run.
node_modules/.bin/jest --runInBand
# tests/jest/governance/GovernanceCliArgvSmoke.spec.ts self-skips when cardano-cli is
# off PATH (1 skipped suite / 12 skipped tests) — expected, not a regression.

# 9. STORYBOOK — usable floor only; `yarn storybook:build` is red at HEAD for a
#    pre-existing manager-webpack reason unrelated to this change.
yarn storybook

# 10. Design-doc diff is exactly one line (Step 12a) and the tracker is value-only:
git diff --stat .agent/plans/governance/drep-discovery/designs/drep-discovery-design.md
git diff .agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json
```

Regression checks that must hold and are worth reading the diff for:

- `grep -n "showAllList" source/renderer/app/stores/GovernanceStore.ts` — the computed body is byte-identical
  to seam C.
- `grep -rn "doNotList" source/common/types/governance.types.ts source/main/` — only task-157's lines; this
  task adds none.
- `grep -rn "DRepStatus" source/common/types/governance.types.ts` — still `'active' | 'inactive'`.
- `grep -rn "_applyVerifiedNames" source/ tests/ storybook/` — prints nothing.

---

### Files touched by task-153

Source (4):

1. `source/renderer/app/stores/GovernanceStore.ts` — `AppDRepDirectoryEntry.doNotList` (seam A),
   `_rehydrateDReps` default (seam D), `_applyVerifiedNames` → `_applyVerifiedMetadata` + flag projection
   (seam E) and its three call sites, `defaultCohort` predicate + doc comment (seam B). Nothing inside
   `fetchAnchorContent` and nothing in `AnchorEnrichEntry` — both are task-157's (Step 0c).
2. `source/renderer/app/components/governance/drep-directory/helpers.ts` — `isStaleFavorite` and its doc
   comment (`:279-288`).
3. `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx` — fixture + parameterised
   `buildGovernanceStore`/`renderPage` + 1 test.
4. `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` — fixtures + 4 tests.

Fixture-sweep only (7): `source/renderer/app/components/governance/drep-directory/helpers.spec.ts` (+1 test),
`source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx`,
`source/renderer/app/containers/governance/DRepDetailPage.spec.tsx`,
`source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`,
`storybook/stories/governance/CurrentVoteSummary.stories.tsx`,
`storybook/stories/governance/DRepDetail.stories.tsx`,
`storybook/stories/governance/_utils/fixtures.ts`.

Tests (1): `tests/jest/governance/GovernanceStore.spec.ts` — one new `describe` block, 5 tests.

Storybook (1): `storybook/stories/governance/DRepDirectory.stories.tsx` — fixture sweep, `baseEntries[1]`
gains `doNotList: true`, stale-favorite story drops the synthetic predicate.

Plans (2): `.agent/plans/governance/drep-discovery/designs/drep-discovery-design.md` (line 112 only),
`.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json` (task-122 row, task-153
row).

**Not touched, by contract:** `source/common/types/governance.types.ts`,
`source/main/governance/AnchorVerificationService.ts`, `source/common/utils/logging.ts`,
`tests/jest/governance/AnchorVerificationService.spec.ts`,
`tests/jest/security/governance-sanitization.spec.ts`,
`source/renderer/app/components/governance/drep-detail/*`,
`source/renderer/app/components/governance/_shared/DRepIdDisplay.tsx`,
`.../_shared/DRepSourceLabel.tsx`, `.../_shared/DRepStatusBadge.tsx`, `.../_shared/DRepEmptyState.tsx`,
`.../voting-governance/VotingPowerDelegationConfirmationDialog.tsx`, the i18n catalogs.

---

## task-174 — Render the dual CIP-129 / CIP-105 DRep ID display in detail and deduped search rows

**Build position:** 3 of 6 in `anchor-2` (`157 → 153 → 174 → 154 → 155 → 156`).
**Estimated:** 5 h · priority medium · all four dependencies (task-107, task-116, task-121, task-129) are `complete`.
**Interaction mode:** `autonomous`.

This task touches **no** main-process file, **no** IPC channel, **no** store, and
**no** anchor code. It is a renderer-component change plus its prop thread, its
copy, its tests and its stories.

---

### 1. What you are building (read this before touching anything)

`DRepIdDisplay` renders exactly one truncated DRep ID with one copy button. Two
shipped design contracts are unbuilt on top of it:

- `shared-design-tokens.md:78` — "Always show both CIP-129 (new bech32) and
  CIP-105 (legacy `drep1…`) when both are derivable. In cards, show CIP-129
  primary + truncated middle (e.g., `drep1yg7s…aj8ras`) with a copy button. **In
  detail, show both forms fully, monospaced, each with its own copy button.**"
- `shared-design-tokens.md:248` — "**Both ID forms are searched.** … If the same
  underlying DRep credential matches via both forms, the result list
  **deduplicates by underlying DRep credential** (the row shows both ID forms
  stacked)."

You add an **opt-in `variant` prop** to `DRepIdDisplay` with three values:

| `variant` | who passes it | render |
|---|---|---|
| `'single'` (default) | every existing call site, unchanged | today's exact output: one truncated `<code>` in a `Tooltip`, one copy button |
| `'stacked'` | `DRepCard`, only while a search is active | two truncated `<code>` rows (CIP-129 then CIP-105), each in a `Tooltip`, each with its own copy button |
| `'full'` | `DRepDetail` | two untruncated monospaced `<code>` rows, no `Tooltip`, each with its own copy button |

The CIP-105 form is **derived inside the component** from the CIP-129 id via
`normalizeDRepIdentity`, and the second row is simply **not rendered** when that
returns `null`. Nothing throws, nothing is re-encoded by hand.

---

### 2. Scope and non-goals (binding — the implementer reads only this section)

**In scope**

1. `source/renderer/app/components/governance/_shared/DRepIdDisplay.tsx` — the `variant` prop and both new render paths.
2. `source/renderer/app/components/governance/_shared/DRepIdDisplay.scss` — four new class names.
3. `source/renderer/app/components/governance/drep-directory/DRepCard.tsx` — a new optional `isSearchResult` prop that selects `variant="stacked"`.
4. `source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx` — pass `isSearchResult` through to `DRepCard`.
5. `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx` — pass `isSearchResult={isSearchActive}` to the **directory** list only.
6. `source/renderer/app/components/governance/drep-detail/DRepDetail.tsx` — `variant="full"` on the identity block.
7. Five new i18n keys in `en-US.json` and `ja-JP.json`.
8. One new spec file, three updated spec files, two updated story files.
9. The tracker row for task-174 (`status`, `statusReason`, `evidence`, `updatedAt`).

**Explicit non-goals — do NOT do these**

- **No change to `drep-directory/helpers.ts`.** `buildDRepSearchIndex` (`:70-84`),
  `searchDRepsByIdPrefix` (`:106-131`), `filterDReps` (`:189-220`) and
  `sortDReps` (`:243-277`) keep their current signatures and their current
  `AppDRepDirectoryEntry[]` carrier type. The search index's own CIP-105
  derivation (`Cardano.DRepID.toCip105DRepID`, `helpers.ts:77`) stays exactly as
  it is; see §4 conflict C-1.
- **No dedup work.** Deduplication already happens: the index holds one row per
  credential (`helpers.ts:101-104` — "One index row per credential, so a query
  matching an entry via both forms yields exactly one result"). You are rendering
  the already-deduped row differently, nothing more.
- **No change to the delegation confirmation dialog.** `VotingPowerDelegation­ConfirmationDialog.tsx`'s §7 identity block is task-175's and its name line is task-154's. Do not open that file.
- **No change to `CurrentVoteSummary.tsx`.** It calls `<DRepIdDisplay drepId={currentVote.drep.raw} />` at `:106` with no variant, so it keeps today's rendering by construction. Do not add a variant there.
- **No verified anchor content, and no name, anywhere on a card.**
  `drep-discovery-design.md:251-259`: "No name field exists on the card, and no
  card may grow one in v1."
- **No name search.** `drep-discovery-design.md:247`: "Verified `givenName` search
  is deferred until a bulk cohort anchor-prefetch phase populates names for the
  whole directory."
- **No design-doc edit.** AC-6 is verify-and-record only; see §5.
- **No new npm dependency**, no new IPC channel, no `source/main/` change, no store change.
- **No new `DRepSourceLabel` variant** and no new `DRepStatus` member.

---

### 3. Locked invariants this change must not break (inlined verbatim — do not go looking them up)

> **Invariant 2 — Sanitization floor.** No DRep id, no `abstain`/`no_confidence`
> literal, no CIP-129/CIP-105 bech32 string in any logger, analytics or
> electron-store payload — re-asserted via the task-111 spy suite in every slice.
> The task-168 DRep-state snapshot is the one documented exception.

Concretely for you: the renderer logger forwards its payload verbatim to
`electronLog` and applies **no** key-based redaction of its own
(`source/renderer/app/utils/logging.ts:26-37` — `logToLevel` builds
`{ message, data, environmentData }` and calls `electronLog[level](...args)`).
The only defence is that the id never enters the payload. Both warn payloads
therefore keep exactly the shape they have today, for **both** forms:

- unavailable branch → `{ drepIdLength }` only
- failure branch → `{ error, drepIdLength }` only

> **Invariant 4 — No second delegation backend.** Selection supplies a DRep ID to
> the existing `delegateVotes` / `VotingStore` signing paths via React Router
> `location.state` only. `VotingStore` never reads `GovernanceStore` directly.

> **Invariant 10 — Byte-equality.** CIP-129, CIP-105 and the signed payload
> `vote.id` stay byte-equal through every identity-display change; the on-device
> DRep ID equals `vote.chosenOption`.

Concretely for you: the "Select for delegation" handoff still passes
`entry.drepId` unchanged — `DRepCard.tsx:154` (`onSelectForDelegation(entry.drepId)`)
and `DRepDetail.tsx:126` (`drepId={entry.drepId}` into `DRepDetailActions`). You
must not route the derived CIP-105 form into any handoff, form field, route
state or request. It is display-only.

> **Invariant 11 — Preliminary copy.** Every new en-US and ja-JP string keeps the
> leading `!!!` marker. Removing `!!!` is a release-end manual review, never a
> per-slice task.

> **Invariant 8 — Badges are informational only.** They never reorder, filter or
> override the cohort. (Relevant only as a reminder that nothing you add to the
> card row may participate in filtering or ordering.)

---

### 4. Corrected line anchors — use these, not the ones in the task text

The task JSON and the design docs cite anchors that no longer match the repo at
`55e8985bf`. Every anchor below was re-read in the live worktree.

| cited in task-174 | actually at | what is really there |
|---|---|---|
| `DRepDetail.tsx:103` | **`DRepDetail.tsx:113`** (block `:112-115`) | `:103` is a `</div>` inside the not-found branch |
| `DRepCard.tsx:121` | **`DRepCard.tsx:126`** | `:121` is the `>` closing the favorite-toggle button's attribute list |
| `DRepDirectoryList.tsx:84` | **`DRepDirectoryList.tsx:88-100`**, `<DRepCard>` at `:89` | `:84` is blank |
| `helpers.ts:71-83` | **`helpers.ts:70-84`** | signature at `:70`, body through `:84` |
| shared-design-tokens "§4 :76" | **`shared-design-tokens.md:78`** | `:76` is the §4 heading |
| shared-design-tokens "§11 :244" | **`shared-design-tokens.md:248`** | `:244` is blank |
| `DRepIdDisplay.tsx:28-32` (props) | **accurate** | — |
| `DRepIdDisplay.tsx:71-84` (single code + copy) | **accurate** | — |
| `DRepDirectory.tsx:151` (branch point) | **accurate** | `const visibleEntries = useMemo(() => {` |
| `drep-discovery-design.md:84-85` (wireframe) | **accurate** | — |

**Recorded conflicts (write these into `research/anchor-2-findings.md` at slice close; they change nothing you build):**

- **C-1 — the task description's search-row claim is false.** It says "search rows
  can pass through the CIP-105 form `buildDRepSearchIndex` already derives per
  entry". `searchDRepsByIdPrefix` **discards** it at `helpers.ts:129`
  (`.map(({ entry }) => entry)`) and returns `AppDRepDirectoryEntry[]` — the same
  type the cohort path returns. AC-1's own path ("deriving CIP-105 … via
  normalizeDRepIdentity") is the implementable one and wins. The stacked variant
  is selected by a boolean threaded down the existing row path.
- **C-2 — two CIP-105 encodings coexist in the repo and they differ in HRP.**
  `helpers.ts:77` uses `Cardano.DRepID.toCip105DRepID` from `@cardano-sdk/core`,
  which for a key credential emits the legacy `drep1…` form (measured:
  `drep185r8rr6j9evjs984vnr7haf5cn3qw5w220usk23cxffvwuutaz3`).
  `normalizeDRepIdentity.ts:37,41` emits the CIP-105-spec HRP form
  `drep_vkh1…` (measured for the same credential:
  `drep_vkh185r8rr6j9evjs984vnr7haf5cn3qw5w220usk23cxffvw6msqtt`). Both encode
  the identical 28 credential bytes; only the HRP differs. AC-1 mandates
  `normalizeDRepIdentity` for display, so **the rendered CIP-105 form is
  `drep_vkh1…` / `drep_script1…`**, which differs from the `drep1…` sample drawn
  in the `drep-discovery-design.md:84-85` wireframe. Do not "fix" this by
  switching to the SDK helper and do not edit the wireframe — record it.
  The duplication is intentional and scoped: the SDK call stays the **search
  index's** derivation.

---

### 5. AC-6 is verify-and-record. Do not edit any design doc.

AC-6 asks you to correct a card-vs-detail contradiction at
`drep-discovery-design.md:240-241`. **That contradiction no longer exists.**
Verified in the live file:

- `:239` is the Show-all paragraph, `:240` is blank, `:241` is
  "**Popularity-sort guardrail.** When the user activates the `voting power desc`
  sort under Show-all, …" — neither line mentions the card identity.
- The card contract now lives at `:251-259` under the heading
  `## Directory Identity: ID-Only in v1` (`:249`), inserted by task-165:
  > "v1 directory cards and search are **DRep-ID-only**. Card identity is the
  > CIP-129-primary truncated ID with a single copy button (`DRepIdDisplay`,
  > shared tokens §4); the full dual CIP-129 + CIP-105 rendering belongs to the
  > detail view and the deduped search row, not to the card. No name field exists
  > on the card, and no card may grow one in v1."

Action: run the verification command in §12 step 9, then record in the task's
`statusReason` that AC-6 was satisfied on disk at `:251-259` before this task
started. **Do not add a paragraph, do not re-edit `:240-241`, do not touch the
design doc at all.**

---

### 6. The exact seams you are changing (quoted, so you do not have to hunt)

**`_shared/DRepIdDisplay.tsx:28-32` — props today**

```tsx
interface Props {
  drepId: string;
  showCopiedConfirmation?: boolean;
  intl: intlShape.isRequired;
}
```

**`_shared/DRepIdDisplay.tsx:50-67` — the copy handler and its two warn payloads**

```tsx
  const handleCopy = useCallback(() => {
    if (!navigator.clipboard || !navigator.clipboard.writeText) {
      logger.warn('DRepIdDisplay: clipboard API is unavailable', {
        drepIdLength: drepId.length,
      });
      return;
    }

    navigator.clipboard
      .writeText(drepId)
      .then(() => setCopied(true))
      .catch((error) => {
        logger.warn('DRepIdDisplay: failed to copy DRep ID', {
          error,
          drepIdLength: drepId.length,
        });
      });
  }, [drepId]);
```

**`_shared/DRepIdDisplay.tsx:71-84` — the single truncated form**

```tsx
    <span className={styles.container}>
      <Tooltip tip={drepId} skin={TooltipSkin} isAligningRight={false}>
        <code className={styles.id} aria-label={drepId}>
          {truncated}
        </code>
      </Tooltip>
      <Button
        className={styles.copyButton}
        onClick={handleCopy}
        label={intl.formatMessage(messages.copyButton)}
        skin={ButtonSkin}
        aria-label={intl.formatMessage(messages.copyLabel)}
      />
```

**`utils/governance/normalizeDRepIdentity.ts:11-15` — the failure contract you rely on**

```
 * Pure decoder for DRep identifiers: CIP-129 `drep1…` (29-byte payload with a
 * 0x22 key / 0x23 script header) and CIP-105 `drep_vkh1…` / `drep_script1…`
 * (bare 28-byte credential). Unknown HRP, length mismatch, bad checksum, or
 * bad header returns null; never throws, never logs.
```

**`drep-directory/DRepCard.tsx:109-127` — the card top row**

```tsx
      <div className={styles.topRow}>
        <button
          type="button"
          className={styles.favoriteToggle}
          …
        >
          <span aria-hidden="true">{isFavorite ? '★' : '☆'}</span>
        </button>
        <DRepStatusBadge status={entry.status} />
        <DRepCategoryBadge entry={entry} cohort={cohort} />
        <DRepIdDisplay drepId={entry.drepId} />
      </div>
```

**`drep-directory/DRepDirectoryList.tsx:88-100` — the shared row map**

```tsx
        {pageEntries.map((entry) => (
          <DRepCard
            key={entry.drepId}
            entry={entry}
            cohort={cohort}
            isFavorite={favoriteDRepIds.has(entry.drepId)}
            onToggleFavorite={onToggleFavorite}
            isStaleFavorite={isFavoritesView && isStaleFavoriteEntry(entry)}
            onSelectForDelegation={onSelectForDelegation}
            onViewDetails={onViewDetails}
            votingPowerState={votingPowerState}
          />
        ))}
```

**`drep-directory/DRepDirectory.tsx:137-140` — the boolean you thread**

```tsx
  const queryKind = getDRepQueryKind(searchQuery);
  const isSearchActive =
    queryKind === 'prefix' ||
    queryKind === 'exactValid' ||
    queryKind === 'invalidFullForm';
```

**`drep-detail/DRepDetail.tsx:112-115` — the detail identity block**

```tsx
      <div className={styles.header}>
        <DRepIdDisplay drepId={entry.drepId} showCopiedConfirmation />
        <DRepCategoryBadge entry={entry} cohort={cohort} />
      </div>
```

---

### 7. Test vectors (measured with the repo's own `bech32` package — copy these literals verbatim)

| CIP-129 (`drepId`) | CIP-105 that `normalizeDRepIdentity` derives |
|---|---|
| `drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras` | `drep_vkh185r8rr6j9evjs984vnr7haf5cn3qw5w220usk23cxffvw6msqtt` |
| `drep1ygqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgweajrn` | `drep_vkh1qyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszuzsm4q` |
| `drep1ygpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqsahpxyl` | `drep_vkh1qgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqypytfq8` |

Credential hex of the first row: `3d06718f522e592814f564c7ebf534c4e20751ca53f90b2a383252c7`.

**An id that does NOT decode** (already the fixture in
`DRepDetailPage.spec.tsx:27` and `DRepDirectory.spec.tsx:34`):
`drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b` — it contains a
character outside the bech32 charset, so `normalizeDRepIdentity` returns `null`
and the CIP-105 row must be omitted. Use it for the omission tests.

Truncation reference (`first 8 … last 6`):
`drep1yg7…aj8ras` and `drep_vkh…6msqtt` for the first row above.

---

### 8. Ordered implementation steps

#### Step 1 — Measure the baselines before you edit anything

```bash
cd /home/node/.claude/jobs/3bad97d1/wt-anchor-2
node_modules/.bin/jest --testPathPattern="(governance/drep-directory/DRepDirectory.spec|containers/governance/DRepDetailPage.spec|security/governance-sanitization|i18n/preliminaryCopyMarkers)" --no-coverage --runInBand
```

**The delta is the contract, not the total.** The `55e8985bf` column is the
planning measurement; the "expected when you start" column applies task-157's
and task-153's landed deltas, since both run before you (build order §
"Build order (binding)"). Whatever the command prints is your real baseline —
if it matches neither column, stop and find out which predecessor is missing.

| suite | at `55e8985bf` | expected when you start | snapshots | moved by |
|---|---|---|---|---|
| `DRepDirectory.spec.tsx` | 48 | **52** | 1 | task-153 +4 |
| `DRepDetailPage.spec.tsx` | 21 | **31** | 2 | task-157 +10 |
| `tests/jest/security/governance-sanitization.spec.ts` | 35 | **37** | 0 | task-157 +2 |
| `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` | 5 | **5** | 0 | — |
| **total** | **109** | **125** | **3** | |

#### Step 2 — `source/renderer/app/components/governance/_shared/DRepIdDisplay.scss`

Append four classes after the existing `.copiedConfirmation` block (`:32-35`).
Do not modify `.container`, `.id`, `.copyButton` or `.copiedConfirmation` — the
`single` variant still uses them.

```scss
.stack {
  display: inline-flex;
  flex-direction: column;
  align-items: flex-start;
  gap: 2px;
}

.formRow {
  display: inline-flex;
  align-items: center;
  gap: 4px;
}

.formCaption {
  font-size: 12px;
  color: var(--theme-text-secondary, #6b7384);
}

.idFull {
  font-family: var(--font-mono, 'SF Mono', 'Fira Code', monospace);
  font-size: 14px;
  color: var(--theme-text-primary);
  word-break: break-all;
}
```

`*.scss.d.ts` files are gitignored (`.gitignore:141`) and generated. After this
edit, regenerate them before typechecking:

```bash
node_modules/.bin/typed-scss-modules source/renderer/app
```

(or just run `yarn compile`, whose `precompile` hook does it for you).

#### Step 3 — `source/renderer/app/components/governance/_shared/DRepIdDisplay.tsx`

Replace the whole file with the following. It is a superset of today's file: the
`single` branch is byte-for-byte the current render.

```tsx
import React, { useCallback, useMemo, useState } from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import { logger } from '../../../utils/logging';
import { normalizeDRepIdentity } from '../../../utils/governance/normalizeDRepIdentity';
import styles from './DRepIdDisplay.scss';

const messages = defineMessages({
  copyButton: {
    id: 'governance.drepDirectory.copyButton',
    defaultMessage: '!!!Copy',
    description: 'Copy button label for a DRep ID',
  },
  copyLabel: {
    id: 'governance.drepDirectory.copyId',
    defaultMessage: '!!!Copy DRep ID',
    description: 'Accessible label for the DRep ID copy button',
  },
  copiedToast: {
    id: 'governance.drepDetail.copyIdToast',
    defaultMessage: '!!!DRep ID copied',
    description: 'Inline confirmation shown after copying a DRep ID',
  },
  cip105Caption: {
    id: 'governance.drepDirectory.cip105Caption',
    defaultMessage: '!!!(CIP-105)',
    description: 'Caption in front of the CIP-105 form of a DRep ID',
  },
  copyLabelCip129: {
    id: 'governance.drepDirectory.copyIdCip129',
    defaultMessage: '!!!Copy CIP-129 DRep ID',
    description: 'Accessible label for the CIP-129 copy button',
  },
  copyLabelCip105: {
    id: 'governance.drepDirectory.copyIdCip105',
    defaultMessage: '!!!Copy CIP-105 DRep ID',
    description: 'Accessible label for the CIP-105 copy button',
  },
  idAriaCip129: {
    id: 'governance.drepDirectory.idAriaCip129',
    defaultMessage: '!!!CIP-129 DRep ID {value}',
    description: 'Accessible label of the CIP-129 DRep ID value',
  },
  idAriaCip105: {
    id: 'governance.drepDirectory.idAriaCip105',
    defaultMessage: '!!!CIP-105 DRep ID {value}',
    description: 'Accessible label of the CIP-105 DRep ID value',
  },
});

export type DRepIdDisplayVariant = 'single' | 'stacked' | 'full';

interface Props {
  drepId: string;
  variant?: DRepIdDisplayVariant;
  showCopiedConfirmation?: boolean;
  intl: intlShape.isRequired;
}

/** Maximum display length before truncation. */
const MAX_DISPLAY_LENGTH = 18;
const PREFIX_LENGTH = 8;
const SUFFIX_LENGTH = 6;

function truncateId(id: string): string {
  if (id.length <= MAX_DISPLAY_LENGTH) return id;
  return `${id.slice(0, PREFIX_LENGTH)}…${id.slice(-SUFFIX_LENGTH)}`;
}

function DRepIdDisplay({
  drepId,
  variant = 'single',
  showCopiedConfirmation = false,
  intl,
}: Props) {
  const [copied, setCopied] = useState(false);

  const copyValue = useCallback((value: string) => {
    if (!navigator.clipboard || !navigator.clipboard.writeText) {
      logger.warn('DRepIdDisplay: clipboard API is unavailable', {
        drepIdLength: value.length,
      });
      return;
    }

    navigator.clipboard
      .writeText(value)
      .then(() => setCopied(true))
      .catch((error) => {
        logger.warn('DRepIdDisplay: failed to copy DRep ID', {
          error,
          drepIdLength: value.length,
        });
      });
  }, []);

  const handleCopy = useCallback(() => copyValue(drepId), [copyValue, drepId]);

  // The legacy form is derived per render, never stored or handed on: an id
  // the decoder rejects simply has no second row.
  const cip105 = useMemo(
    () =>
      variant === 'single'
        ? null
        : normalizeDRepIdentity(drepId)?.cip105 ?? null,
    [drepId, variant]
  );

  const confirmation = showCopiedConfirmation && copied && (
    <span
      className={styles.copiedConfirmation}
      role="status"
      aria-live="polite"
    >
      {intl.formatMessage(messages.copiedToast)}
    </span>
  );

  if (variant === 'single') {
    const truncated = truncateId(drepId);

    return (
      <span className={styles.container}>
        <Tooltip tip={drepId} skin={TooltipSkin} isAligningRight={false}>
          <code className={styles.id} aria-label={drepId}>
            {truncated}
          </code>
        </Tooltip>
        <Button
          className={styles.copyButton}
          onClick={handleCopy}
          label={intl.formatMessage(messages.copyButton)}
          skin={ButtonSkin}
          aria-label={intl.formatMessage(messages.copyLabel)}
        />
        {confirmation}
      </span>
    );
  }

  const isFull = variant === 'full';

  return (
    <span className={styles.stack}>
      <span className={styles.formRow}>
        {isFull ? (
          <code
            className={styles.idFull}
            aria-label={intl.formatMessage(messages.idAriaCip129, {
              value: drepId,
            })}
          >
            {drepId}
          </code>
        ) : (
          <Tooltip tip={drepId} skin={TooltipSkin} isAligningRight={false}>
            <code
              className={styles.id}
              aria-label={intl.formatMessage(messages.idAriaCip129, {
                value: drepId,
              })}
            >
              {truncateId(drepId)}
            </code>
          </Tooltip>
        )}
        <Button
          className={styles.copyButton}
          onClick={handleCopy}
          label={intl.formatMessage(messages.copyButton)}
          skin={ButtonSkin}
          aria-label={intl.formatMessage(messages.copyLabelCip129)}
        />
      </span>
      {cip105 !== null && (
        <span className={styles.formRow}>
          <span className={styles.formCaption}>
            {intl.formatMessage(messages.cip105Caption)}
          </span>
          {isFull ? (
            <code
              className={styles.idFull}
              aria-label={intl.formatMessage(messages.idAriaCip105, {
                value: cip105,
              })}
            >
              {cip105}
            </code>
          ) : (
            <Tooltip tip={cip105} skin={TooltipSkin} isAligningRight={false}>
              <code
                className={styles.id}
                aria-label={intl.formatMessage(messages.idAriaCip105, {
                  value: cip105,
                })}
              >
                {truncateId(cip105)}
              </code>
            </Tooltip>
          )}
          <Button
            className={styles.copyButton}
            onClick={() => copyValue(cip105)}
            label={intl.formatMessage(messages.copyButton)}
            skin={ButtonSkin}
            aria-label={intl.formatMessage(messages.copyLabelCip105)}
          />
        </span>
      )}
      {confirmation}
    </span>
  );
}

export default injectIntl(DRepIdDisplay);
```

Points a reviewer will check, so get them right:

- `copyValue` has an **empty dependency array** — it closes over nothing but
  `setCopied`. Do not add `drepId` to it.
- The two `logger.warn` message strings and payload key names are **unchanged**.
  `drepIdLength: value.length` is the only edit, and it is still a number.
- `normalizeDRepIdentity` is called only for the non-`single` variants, so the
  card's default path does zero extra work.
- There is **one** `copied` state and therefore **one** confirmation node, even
  with two copy buttons. `DRepDetailPage.spec.tsx:272`'s
  `findByText('!!!DRep ID copied')` keeps matching exactly one element.

#### Step 4 — `source/renderer/app/components/governance/drep-directory/DRepCard.tsx`

4a. Add to `Props` (`:60-70`), directly after `isStaleFavorite?: boolean;` at `:65`:

```tsx
  isSearchResult?: boolean;
```

4b. Add to the destructured parameter list (`:85-95`), directly after
`isStaleFavorite = false,` at `:90`:

```tsx
  isSearchResult = false,
```

4c. Replace `:126`:

```tsx
        <DRepIdDisplay drepId={entry.drepId} />
```

with:

```tsx
        <DRepIdDisplay
          drepId={entry.drepId}
          variant={isSearchResult ? 'stacked' : 'single'}
        />
```

Nothing else in this file changes. `onSelectForDelegation(entry.drepId)` at
`:154` and `onViewDetails(entry.drepId)` at `:149` stay byte-identical.

#### Step 5 — `source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx`

5a. Add to `Props` (`:34-45`), directly after `isStaleFavoriteEntry?: …` at `:40`:

```tsx
  isSearchResult?: boolean;
```

5b. Add to the destructured parameter list (`:47-58`), directly after
`isStaleFavoriteEntry = isStaleFavorite,` at `:53`:

```tsx
  isSearchResult = false,
```

5c. In the map at `:88-100`, add one prop to `<DRepCard>` directly after
`isStaleFavorite={…}` at `:95`:

```tsx
            isSearchResult={isSearchResult}
```

#### Step 6 — `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx`

Add one prop to the **directory** `<DRepDirectoryList>` at `:363-371` only —
after `entries={visibleEntries}` at `:364`:

```tsx
                isSearchResult={isSearchActive}
```

**Do not** add it to the favorites-view `<DRepDirectoryList>` at `:292-302`. The
favorites view hides the search input entirely (`DRepDirectory.spec.tsx:793-804`
pins that), so a favorites row is never a search result.

`isSearchActive` already exists at `:137-140` and needs no change.

#### Step 7 — `source/renderer/app/components/governance/drep-detail/DRepDetail.tsx`

Replace `:113`:

```tsx
        <DRepIdDisplay drepId={entry.drepId} showCopiedConfirmation />
```

with:

```tsx
        <DRepIdDisplay
          drepId={entry.drepId}
          variant="full"
          showCopiedConfirmation
        />
```

Leave `DRepDetail.scss`'s `.header` (`:20-24`) alone.

#### Step 8 — i18n: five new keys in both catalogs

Insert into **`source/renderer/app/i18n/locales/en-US.json`** at the correct
alphabetical positions (the file is key-sorted; line numbers below are the
pre-edit positions):

| after line | insert |
|---|---|
| `:327` (`…category.threshold.tooltip`) | `  "governance.drepDirectory.cip105Caption": "!!!(CIP-105)",` |
| `:334` (`…copyId`) | `  "governance.drepDirectory.copyIdCip105": "!!!Copy CIP-105 DRep ID",`<br>`  "governance.drepDirectory.copyIdCip129": "!!!Copy CIP-129 DRep ID",` |
| `:349` (`…filter.metadata.without`) | `  "governance.drepDirectory.idAriaCip105": "!!!CIP-105 DRep ID {value}",`<br>`  "governance.drepDirectory.idAriaCip129": "!!!CIP-129 DRep ID {value}",` |

Insert into **`source/renderer/app/i18n/locales/ja-JP.json`** at the same
key-sorted positions (the two catalogs are key-for-key parallel):

```json
  "governance.drepDirectory.cip105Caption": "!!!(CIP-105)",
  "governance.drepDirectory.copyIdCip105": "!!!CIP-105 DRep IDをコピー",
  "governance.drepDirectory.copyIdCip129": "!!!CIP-129 DRep IDをコピー",
  "governance.drepDirectory.idAriaCip105": "!!!CIP-105 DRep ID {value}",
  "governance.drepDirectory.idAriaCip129": "!!!CIP-129 DRep ID {value}",
```

Rules:

- Every one of the ten strings keeps the leading `!!!` (invariant 11).
- The `{value}` placeholder name must be identical in both locales.
- `tests/jest/i18n/preliminaryCopyMarkers.spec.ts:55-62` already asserts that
  every `governance.`-prefixed key is `!!!`-marked in both locales, and `:37-44`
  asserts key-set parity. These are `governance.*` keys, so **no edit to
  `PRELIMINARY_CONFIRMATION_KEYS` (`:17-20`) is needed** — that list is only for
  the `voting.governance.confirmationDialog.*` namespace.
- **Never run prettier over the locale catalogs or `translations/messages.json`** — they are tool-managed.

Then run the i18n gate:

```bash
yarn i18n:manage
git status --porcelain
```

`yarn i18n:manage` = `yarn i18n:extract && yarn i18n:check`; it **writes**
`translations/messages.json`, `source/renderer/app/i18n/locales/defaultMessages.json`
and possibly `whitelist_*.json`. Keep the writes that correspond to your five new
keys. Revert anything else surgically:

```bash
git restore <path-that-was-clean-at-HEAD>
```

**Never `git stash`** — the stash stack is shared across worktrees and concurrent
sessions.

#### Step 9 — New spec: `source/renderer/app/components/governance/_shared/DRepIdDisplay.spec.tsx`

This file does not exist today. It is the primary proof for AC-1, AC-3 and the
component half of AC-4. Harness shape mirrors
`DRepDirectory.spec.tsx:138-171` (ThemeProvider is required because
`DRepIdDisplay` renders a react-polymorph `Button`).

```tsx
import React from 'react';
import { bech32 } from 'bech32';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import jaTranslations from '../../../i18n/locales/ja-JP.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import { logger } from '../../../utils/logging';
import DRepIdDisplay from './DRepIdDisplay';
import type { DRepIdDisplayVariant } from './DRepIdDisplay';

const CIP129 =
  'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras';
const CIP105 =
  'drep_vkh185r8rr6j9evjs984vnr7haf5cn3qw5w220usk23cxffvw6msqtt';
const UNDECODABLE = 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b';

const credentialOf = (id: string): string => {
  const bytes = bech32.fromWords(bech32.decode(id).words);
  const credential = bytes.length === 29 ? bytes.slice(1) : bytes;
  return Buffer.from(credential).toString('hex');
};

const renderDisplay = (
  drepId: string,
  variant: DRepIdDisplayVariant,
  locale = 'en-US'
) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider
        locale={locale}
        messages={locale === 'ja-JP' ? jaTranslations : translations}
      >
        <DRepIdDisplay drepId={drepId} variant={variant} />
      </IntlProvider>
    </ThemeProvider>
  );

describe('DRepIdDisplay', () => {
  afterEach(() => {
    cleanup();
    delete (navigator as any).clipboard;
    jest.restoreAllMocks();
  });

  it('keeps the single truncated form and one copy button by default', () => {
    const { container } = render(
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        variables={SimpleDefaults}
        themeOverrides={themeOverrides}
      >
        <IntlProvider locale="en-US" messages={translations}>
          <DRepIdDisplay drepId={CIP129} />
        </IntlProvider>
      </ThemeProvider>
    );

    expect(container.querySelectorAll('code')).toHaveLength(1);
    expect(screen.getByText('drep1yg7…aj8ras')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy DRep ID' })
    ).toBeInTheDocument();
    expect(screen.queryByText('!!!(CIP-105)')).not.toBeInTheDocument();
  });

  it('stacks both truncated forms with a copy button each', () => {
    const { container } = renderDisplay(CIP129, 'stacked');

    expect(container.querySelectorAll('code')).toHaveLength(2);
    expect(screen.getByText('drep1yg7…aj8ras')).toBeInTheDocument();
    expect(screen.getByText('drep_vkh…6msqtt')).toBeInTheDocument();
    expect(screen.getByText('!!!(CIP-105)')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy CIP-129 DRep ID' })
    ).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    ).toBeInTheDocument();
  });

  it('renders both forms in full with distinct per-form aria labels', () => {
    const { container } = renderDisplay(CIP129, 'full');

    const codes = Array.from(container.querySelectorAll('code'));
    expect(codes).toHaveLength(2);
    expect(codes[0]).toHaveTextContent(CIP129);
    expect(codes[1]).toHaveTextContent(CIP105);
    expect(codes[0].getAttribute('aria-label')).toBe(
      `!!!CIP-129 DRep ID ${CIP129}`
    );
    expect(codes[1].getAttribute('aria-label')).toBe(
      `!!!CIP-105 DRep ID ${CIP105}`
    );
  });

  it('renders both forms over the same credential bytes', () => {
    const { container } = renderDisplay(CIP129, 'full');

    const codes = Array.from(container.querySelectorAll('code'));
    expect(credentialOf(codes[0].textContent as string)).toBe(
      credentialOf(CIP129)
    );
    expect(credentialOf(codes[1].textContent as string)).toBe(
      credentialOf(CIP129)
    );
  });

  it('omits the second row when the id does not decode', () => {
    const { container } = renderDisplay(UNDECODABLE, 'full');

    expect(container.querySelectorAll('code')).toHaveLength(1);
    expect(screen.queryByText('!!!(CIP-105)')).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    ).not.toBeInTheDocument();
  });

  it('copies exactly the form each button labels', () => {
    const writeText = jest.fn(async () => undefined);
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText },
    });

    renderDisplay(CIP129, 'full');

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Copy CIP-129 DRep ID' })
    );
    expect(writeText).toHaveBeenLastCalledWith(CIP129);

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    );
    expect(writeText).toHaveBeenLastCalledWith(CIP105);
  });

  it('logs no id when the clipboard API is unavailable, for either form', () => {
    const warn = jest.spyOn(logger, 'warn').mockImplementation(() => undefined);

    renderDisplay(CIP129, 'full');
    fireEvent.click(
      screen.getByRole('button', { name: '!!!Copy CIP-129 DRep ID' })
    );
    fireEvent.click(
      screen.getByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    );

    expect(warn).toHaveBeenCalledTimes(2);
    warn.mock.calls.forEach(([message, payload]) => {
      expect(message).toBe('DRepIdDisplay: clipboard API is unavailable');
      expect(Object.keys(payload as object)).toEqual(['drepIdLength']);
      const serialized = JSON.stringify(payload);
      expect(serialized).not.toContain(CIP129);
      expect(serialized).not.toContain(CIP105);
    });
  });

  it('logs no id when a copy rejects, for either form', async () => {
    const writeText = jest.fn(() => Promise.reject(new Error('denied')));
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText },
    });
    const warn = jest.spyOn(logger, 'warn').mockImplementation(() => undefined);

    renderDisplay(CIP129, 'full');
    fireEvent.click(
      screen.getByRole('button', { name: '!!!Copy CIP-129 DRep ID' })
    );
    fireEvent.click(
      screen.getByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    );
    await Promise.resolve();
    await Promise.resolve();

    expect(warn).toHaveBeenCalledTimes(2);
    warn.mock.calls.forEach(([message, payload]) => {
      expect(message).toBe('DRepIdDisplay: failed to copy DRep ID');
      expect(Object.keys(payload as object).sort()).toEqual([
        'drepIdLength',
        'error',
      ]);
      const serialized = JSON.stringify(payload, (_key, val) =>
        val instanceof Error ? `${val.message} ${val.stack}` : val
      );
      expect(serialized).not.toContain(CIP129);
      expect(serialized).not.toContain(CIP105);
    });
  });

  it('renders the stacked form with the ja-JP copy labels', () => {
    renderDisplay(CIP129, 'stacked', 'ja-JP');

    expect(
      screen.getByRole('button', { name: '!!!CIP-129 DRep IDをコピー' })
    ).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!CIP-105 DRep IDをコピー' })
    ).toBeInTheDocument();
  });
});
```

Expected: **1 new suite, +10 tests, 0 snapshots.**

If the rejected-promise test proves flaky under `@swc/jest`, replace the two
`await Promise.resolve()` lines with
`await new Promise((resolve) => setTimeout(resolve, 0));` — do not add
`waitFor`, and do not weaken the payload-key assertions.

#### Step 10 — `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`

The truncation helper at `:735-737` and its assertions at `:773-774` and
`:788-789` are inside the `favorites` describe block (`:732`). Favorites rows are
**not** search results, so they stay `single` and these assertions hold
unchanged. **Re-run them, do not edit them.**

Add three tests. Put them immediately after the existing
`it('filters by prefix at 8 characters and never auto-selects, even on Enter with one match', …)`
which ends at `:550`.

```tsx
  it('stacks both ID forms on a search-result row', () => {
    const { container } = renderComponent({
      drepList: [realEntry(1), realEntry(2)],
    });

    fireEvent.change(screen.getByPlaceholderText('!!!Search by DRep ID'), {
      target: { value: realDrepId(1).slice(0, 'drep1'.length + 20) },
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    expect(container.querySelectorAll('code')).toHaveLength(2);
    expect(screen.getByText('!!!(CIP-105)')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    ).toBeInTheDocument();
  });

  it('keeps exactly one ID form on a cohort row', () => {
    const { container } = renderComponent({ drepList: [realEntry(1)] });

    expect(container.querySelectorAll('code')).toHaveLength(1);
    expect(screen.queryByText('!!!(CIP-105)')).not.toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy DRep ID' })
    ).toBeInTheDocument();
  });

  it('hands the CIP-129 id to delegation from a search-result row', () => {
    const onSelectForDelegation = jest.fn();
    renderComponent({
      drepList: [realEntry(1), realEntry(2)],
      onSelectForDelegation,
    });

    fireEvent.change(screen.getByPlaceholderText('!!!Search by DRep ID'), {
      target: { value: realDrepId(1).slice(0, 'drep1'.length + 20) },
    });
    fireEvent.click(screen.getByText('!!!Select for delegation'));

    expect(onSelectForDelegation).toHaveBeenCalledWith(realDrepId(1));
  });
```

Notes:

- `realDrepId(n)` (`:59-66`) is built with `Cardano.DRepID.cip129FromCredential`,
  so it is checksum-valid and `normalizeDRepIdentity` decodes it. `baseEntries[0]`
  (`:34`) and `buildEntry` (`:44-46`) ids do **not** decode — never use them for a
  two-`<code>` assertion.
- `renderComponent` (`:88-172`) returns the RTL render result, so `container`
  destructures straight out of it.
- The existing snapshot at `:499-509` targets
  `screen.getByText('!!!Threshold').closest('span[title]')` — the category badge
  only. It must come back **unchanged**. If it changes, you have altered the card
  layout beyond the ID block; revert and re-check.

Expected: **+3 tests, 1 snapshot unchanged** (48 → 51 measured alone at `55e8985bf`; 52 → 55 in build order, because task-153 adds 4 cases to this suite before you).

#### Step 11 — `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx`

11a. **Required rename.** `:270` currently reads:

```tsx
      fireEvent.click(screen.getByRole('button', { name: '!!!Copy DRep ID' }));
```

The detail view now renders `variant="full"`, so the CIP-129 copy button's
accessible name is the per-form label. Change that line to:

```tsx
      fireEvent.click(
        screen.getByRole('button', { name: '!!!Copy CIP-129 DRep ID' })
      );
```

`:273`'s `expect(writeText).toHaveBeenCalledWith(DREP_ID)` stays as-is — that is
the byte-equality assertion for the copy path.

11b. **Harness parameter.** `renderPage` (`:62-99`) hardcodes `DREP_ID` in the
history entry, so a decodable-id case needs one new optional parameter. Add
`drepId = DREP_ID,` to the destructured argument list (after `governanceOverrides = {},`
at `:63`), add `drepId?: string;` to its inline type (after `governanceOverrides?: Record<string, unknown>;`
at `:69`), and change `:82` from

```tsx
        pathname: `${ROUTES.GOVERNANCE.DREPS}/${DREP_ID}`,
```

to

```tsx
        pathname: `${ROUTES.GOVERNANCE.DREPS}/${drepId}`,
```

11c. Add two tests immediately after the copied-confirmation test (which ends at
`:277`), and add the decodable constants beside `DREP_ID` at `:27`:

```tsx
const DECODABLE_DREP_ID =
  'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras';
const DECODABLE_CIP105 =
  'drep_vkh185r8rr6j9evjs984vnr7haf5cn3qw5w220usk23cxffvw6msqtt';
```

```tsx
  it('renders both ID forms in full in the detail header', () => {
    const decodableEntry = { ...baseEntry, drepId: DECODABLE_DREP_ID };
    const { container } = renderPage({
      drepId: DECODABLE_DREP_ID,
      governanceOverrides: {
        drepIndex: new Map([[DECODABLE_DREP_ID, decodableEntry]]),
        drepList: [decodableEntry],
      },
    });

    expect(container.querySelectorAll('code')[0]).toHaveTextContent(
      DECODABLE_DREP_ID
    );
    expect(screen.getByText(DECODABLE_CIP105)).toBeInTheDocument();
    expect(screen.getByText('!!!(CIP-105)')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    ).toBeInTheDocument();
  });

  it('renders only the CIP-129 form when the id does not decode', () => {
    renderPage();

    expect(screen.queryByText('!!!(CIP-105)')).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    ).not.toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy CIP-129 DRep ID' })
    ).toBeInTheDocument();
  });
```

11d. **Do not touch** the two existing byte-equality tests at `:207-229` and
`:231-242` — `expect(pushSpy).toHaveBeenCalledWith(ROUTES.VOTING.GOVERNANCE, expect.objectContaining({ selectedDRepId: DREP_ID, … }))`
is AC-3's "Select for delegation handoff still passes entry.drepId" assertion and
must pass unchanged.

11e. The two snapshots at `:288-295` and `:297-311` target
`.closest('span[title]')` on a category badge and must come back unchanged.

Expected: **+2 tests, 2 snapshots unchanged** (21 → 23 measured alone at `55e8985bf`; 31 → 33 in build order, because task-157 adds 10 cases to this suite before you).

#### Step 12 — `tests/jest/security/governance-sanitization.spec.ts`

This file is a `.ts` file and no spec under `tests/jest/` renders React — keep it
that way. The render/copy spy assertions live in `DRepIdDisplay.spec.tsx`
(Step 9). Here you add two `filterLogData` cases that pin the two payload shapes
the dual-ID copy path emits. Add them at the end of the
`describe('Governance sanitization — filterLogData', …)` block, immediately
before its closing `});` at `:308`:

```ts
  it('carries no bech32 string in the DRepIdDisplay clipboard-unavailable payload', () => {
    const payload = { drepIdLength: CIP129_DREP.length };
    const s = jsonStr(filterLogData(payload));
    expect(s).not.toContain(CIP129_DREP);
    expect(s).not.toContain(CIP105_KEY);
    expect(s).not.toContain(CIP105_SCRIPT);
  });

  it('carries no bech32 string in the DRepIdDisplay copy-failure payload', () => {
    const payload = {
      error: new Error('clipboard write denied'),
      drepIdLength: CIP105_KEY.length,
    };
    const s = jsonStrWithErrors(filterLogData(payload));
    expect(s).not.toContain(CIP129_DREP);
    expect(s).not.toContain(CIP105_KEY);
    expect(s).not.toContain(CIP105_SCRIPT);
  });
```

`CIP129_DREP` (`:56-57`), `CIP105_KEY` (`:59-60`), `CIP105_SCRIPT` (`:62-63`),
`jsonStr` (`:65-67`) and `jsonStrWithErrors` (`:72-76`) already exist in this
file — do not redefine them.

**task-174 adds no new redaction key.** `source/common/utils/logging.ts:44-63`
already lists `drepId`, `dRepId`, `raw`, `cip105`, `cip129`, `credentialHex`,
`drepIdentity`, `chosenOption`, `votingTarget`, `anchorUrl`, `anchorContent`,
`givenName`, `verifiedName`. Do not edit that list — extending it is task-157's
seam and touching it here is a seam violation.

Expected: **+2 tests** (35 → 37 measured alone at `55e8985bf`; 37 → 39 in build order, because task-157 adds 2 cases to this suite before you).

#### Step 13 — Stories (`AC-5`, "the directory and detail stories cover the dual-ID rendering in both locales")

Both locales are covered by the **global English/Japanese toggle** driven by
`storybook/preview.tsx`'s `StoryWrapper` decorator. **Never** wrap a story in its
own `IntlProvider` and **never** add `*_ja` story exports —
`DRepDetail.stories.tsx:85-87` already records this rule in situ.

13a. `storybook/stories/governance/DRepDetail.stories.tsx` — the current fixture
ids do not decode, so the detail story would show only one form. Swap them for
decodable ids. At `:68`, replace

```tsx
  drepId: 'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k4',
```

with

```tsx
  drepId: 'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras',
```

and at `:76`, replace

```tsx
  drepId: 'drep1xj23tk3y_qyv7c9m2z89w3t8mvk9e2uwc3q8u6j7r2x5y9w0p1',
```

with

```tsx
  drepId: 'drep1ygpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqsahpxyl',
```

Nothing else in the file changes: `storyCohort` (`:79-83`) derives its ids from
`withAnchorEntry.drepId`, so it follows automatically. The
`Loaded — with anchor` story now renders both forms in full.

13b. `storybook/stories/governance/DRepDirectory.stories.tsx` — add a
deterministic story for the stacked search-result row. `DRepDirectory` owns
`searchQuery` in internal state, so a story cannot pre-seed a search; render
`DRepDirectoryList` directly with the flag instead.

Add the import beside the existing `DRepDirectory` import at `:9`:

```tsx
import DRepDirectoryList from '../../../source/renderer/app/components/governance/drep-directory/DRepDirectoryList';
```

Add two fixtures after `paginatedEntries` (`:90-93`):

```tsx
const dualIdEntries: AppDRepDirectoryEntry[] = [
  {
    ...baseEntries[0],
    drepId: 'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras',
  },
  {
    ...baseEntries[1],
    drepId: 'drep1ygqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgweajrn',
  },
];
```

Add a story to the end of the `storiesOf('Governance / DRep Directory', module)`
chain. The chain's last story is `Favorites view — stale favorite` (`:541-557`)
and it terminates at `:557` with `  ));`. Change that line to `  ))` and append
the new story, which now carries the terminating `;`:

```tsx
  .add('Search results — stacked dual ID', () => (
    <div style={CENTERED_STYLE}>
      <DRepDirectoryList
        entries={dualIdEntries}
        cohort={storyCohort}
        favoriteDRepIds={new Set<string>()}
        onToggleFavorite={action('onToggleFavorite')}
        isSearchResult
        onSelectForDelegation={action('onSelectForDelegation')}
        onViewDetails={action('onViewDetails')}
        votingPowerState={VotingPowerEnrichState.Loaded}
      />
    </div>
  ));
```

Do not touch the `Connected flow` story (`:289`) — it is the integrated
full-app exemplar and its rows stay single-form because no search is active.

#### Step 14 — Tracker row (value-only edit)

In `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`,
the task-174 object starts at `:1827` (`"id": "task-174"` at `:1828`). Change
`"status": "pending"` to `"status": "complete"` and add `statusReason`,
`evidence` and `updatedAt` in the shape task-172 uses (`:1743-1772`), at the same
ten-space indentation.

`statusReason` must record, in one paragraph: that AC-6 was already satisfied on
disk at `drep-discovery-design.md:251-259` and no doc edit was made; that the
description's search-index pass-through claim is false (`helpers.ts:129`) and the
`normalizeDRepIdentity` path was taken instead; and that the rendered CIP-105 form
uses the `drep_vkh1…` HRP rather than the legacy `drep1…` form the wireframe
draws (conflict C-2).

**Never run prettier over this file.** Edit values in place and preserve the
surrounding formatting.

---

### 9. Formatting

`nix fmt` is mandated but `nix` is unavailable in this devcontainer. Substitute,
with **explicit paths only**:

```bash
node_modules/.bin/prettier --write \
  source/renderer/app/components/governance/_shared/DRepIdDisplay.tsx \
  source/renderer/app/components/governance/_shared/DRepIdDisplay.scss \
  source/renderer/app/components/governance/_shared/DRepIdDisplay.spec.tsx \
  source/renderer/app/components/governance/drep-directory/DRepCard.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/components/governance/drep-detail/DRepDetail.tsx \
  source/renderer/app/containers/governance/DRepDetailPage.spec.tsx \
  tests/jest/security/governance-sanitization.spec.ts \
  storybook/stories/governance/DRepDetail.stories.tsx \
  storybook/stories/governance/DRepDirectory.stories.tsx
```

- **Never `yarn prettier`** — its package.json script embeds a repo-wide
  `"**/*.*"` glob (`package.json:47`) and reformats ~250 unrelated files even when
  handed a path.
- **Never** format the locale catalogs, `translations/messages.json`,
  `defaultMessages.json` or the tasks JSON.
- If prettier reports churn on a file you did not touch, `git restore` that file.
- Running the real `nix fmt` remains a user-owned pre-merge obligation; say so in
  the task's `statusReason`.

---

### 10. Verify block — run every command, record `baseline → measured`

```bash
cd /home/node/.claude/jobs/3bad97d1/wt-anchor-2

# 1. SCSS typings (new class names) then typecheck.
node_modules/.bin/typed-scss-modules source/renderer/app
node_modules/.bin/tsc --noEmit
#   expect: exit 0, no new errors. tsconfig.json has no "include", so this
#   covers source/, tests/ AND storybook/ — the new story props are checked here.

# 2. The four touched suites plus the new one.
node_modules/.bin/jest --testPathPattern="(_shared/DRepIdDisplay.spec|governance/drep-directory/DRepDirectory.spec|containers/governance/DRepDetailPage.spec|security/governance-sanitization|i18n/preliminaryCopyMarkers)" --no-coverage --runInBand
#   expect: 5 suites (4 -> 5), +15 tests, 3 snapshots unchanged.
#   Per-suite deltas — these are the contract:
#     DRepIdDisplay        +10 (new file, 0 -> 10)
#     DRepDirectory        +3
#     DRepDetailPage       +2
#     governance-sanitization +2
#     preliminaryCopyMarkers  +0
#   The absolute totals in the Step 1 table were measured at 55e8985bf, before
#   task-157 and task-153 ran. In build order they raise three of the four:
#   DRepDetailPage 21 -> 31 (task-157 +10), governance-sanitization 35 -> 37
#   (task-157 +2), DRepDirectory 48 -> 52 (task-153 +4). So the row you should
#   see is 125 -> 140, not 109 -> 124. Step 1 is the authority: measure, then
#   assert the deltas above.

# 3. Sanitization floor — cv-2 F-31's two-anchor rule. The two mandatory
#    anchors are the security suite and the sibling logging suite
#    `tests/jest/governance/logDRepStateSnapshot.spec.ts`, the same pair every
#    other task in this slice cites. Citing only one is a false green, and
#    substituting a different second suite is not the rule.
node_modules/.bin/jest --testPathPattern="tests/jest/security/governance-sanitization" --no-coverage --runInBand
#   expect: 1 suite, +2 tests over your Step 1 measurement, all green. In build
#   order the suite already stands at 37 (HEAD 35, task-157 +2 per the Step 1
#   table above), so the row you should see is 37 -> 39. Step 1 is the
#   authority: measure, then assert the delta.
node_modules/.bin/jest --testPathPattern="tests/jest/governance/logDRepStateSnapshot" --no-coverage --runInBand
#   expect: 1 suite, 5 tests, unchanged and green — this task adds no logger
#   call and no new redaction key

# 3b. Third run, additional to the two-anchor pair, not a substitute for it:
#     the render-and-copy logger spy this task actually introduces.
node_modules/.bin/jest --testPathPattern="_shared/DRepIdDisplay.spec" --no-coverage --runInBand
#   expect: 1 suite, 10 tests, all green — this is where the logger spy runs
#   over the dual-ID render and both copy paths

# 4. Full jest run (jest tests/jest alone is only ~8% of the suite).
node_modules/.bin/jest --runInBand
#   expect: no new failures. tests/jest/governance/GovernanceCliArgvSmoke.spec.ts
#   self-skips when cardano-cli is off PATH (1 skipped suite / 12 skipped
#   tests) — expected, not a regression.

# 5. i18n parity — must print equal counts, no missing keys, all !!!-marked.
node -e '
const en=require("./source/renderer/app/i18n/locales/en-US.json");
const ja=require("./source/renderer/app/i18n/locales/ja-JP.json");
const g=o=>Object.keys(o).filter(k=>k.startsWith("governance."));
const ge=g(en), gj=g(ja);
console.log("en",ge.length,"ja",gj.length);
console.log("missingInJa",ge.filter(k=>!(k in ja)));
console.log("missingInEn",gj.filter(k=>!(k in en)));
console.log("unmarked",ge.filter(k=>!en[k].startsWith("!!!")||!ja[k].startsWith("!!!")));
'
#   expect: **+5 `governance.*` keys per catalog** — that delta is the contract.
#   In build order (task-157 lands 13 `governance.*` keys before you and
#   task-153 lands none), the absolute numbers here are en 115, ja 115
#   (97 at 55e8985bf -> 110 after task-157 -> 115 after this task), and the
#   whole-catalog totals are 1631 -> 1644 -> 1649. All three arrays empty.
#   If task-157 is not on the branch, expect 102 / 102 instead and say so.

# 6. Lint.
yarn lint
#   expect: exit 0, 0 errors. The ~5591 pre-existing warning baseline moves
#   upward because a new file lands under source/ — expected, not a regression.
#   Errors are the gate; warnings are not. tests/ is eslint-ignored.

# 7. Storybook — `yarn storybook:build` is RED at HEAD for a pre-existing
#    manager-webpack reason (and therefore so is `yarn check:all`). The usable
#    floor is the dev server; open both new/changed stories and flip the global
#    English/Japanese toggle on each.
yarn storybook
#   check: Governance / DRep Detail > "Loaded — with anchor" shows two full
#   monospaced ids with a copy button each;
#   Governance / DRep Directory > "Search results — stacked dual ID" shows two
#   stacked truncated forms per card;
#   Governance / DRep Directory > "Loaded" still shows one form per card.
#   Watch the ja-JP card top row for overflow (a known risk carried from the
#   anchor-1 findings: the fixed-width card row already runs tight in ja-JP).

# 8. Working tree — nothing unexpected.
git status --porcelain
#   Only the files in §11 may appear. Revert anything else with
#   `git restore <path>` — NEVER `git stash`.

# 9. AC-6 verify-and-record (no edit).
sed -n '240,241p;249,259p' .agent/plans/governance/drep-discovery/designs/drep-discovery-design.md
#   expect: :240 blank, :241 "**Popularity-sort guardrail.** …",
#   :249 "## Directory Identity: ID-Only in v1", :251-259 the ID-only paragraph.
git diff --stat -- .agent/plans/governance/drep-discovery/designs/drep-discovery-design.md
#   expect: EMPTY. Any diff here is a defect — this task edits no design doc.
```

---

### 11. Files touched (the complete list — anything else is out of scope)

**Source**

1. `source/renderer/app/components/governance/_shared/DRepIdDisplay.tsx`
2. `source/renderer/app/components/governance/_shared/DRepIdDisplay.scss`
3. `source/renderer/app/components/governance/drep-directory/DRepCard.tsx`
4. `source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx`
5. `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx`
6. `source/renderer/app/components/governance/drep-detail/DRepDetail.tsx`

**Copy**

7. `source/renderer/app/i18n/locales/en-US.json`
8. `source/renderer/app/i18n/locales/ja-JP.json`
9. `source/renderer/app/i18n/locales/defaultMessages.json` *(regenerated by `yarn i18n:manage`)*
10. `translations/messages.json` *(regenerated by `yarn i18n:manage`)*

**Tests**

11. `source/renderer/app/components/governance/_shared/DRepIdDisplay.spec.tsx` *(new)*
12. `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
13. `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx`
14. `tests/jest/security/governance-sanitization.spec.ts`

**Stories**

15. `storybook/stories/governance/DRepDetail.stories.tsx`
16. `storybook/stories/governance/DRepDirectory.stories.tsx`

**Planning**

17. `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json` *(task-174 row values only)*

---

### 12. Commit

One commit, a **single Conventional Commits subject line** — no body, no
trailers, no `Co-Authored-By`:

```
feat(gov): task-174 render the dual cip-129 and cip-105 drep id display
```

---

### 13. Definition of done for task-174

- [ ] `DRepIdDisplay` has an opt-in `variant` prop; every call site that does not
      pass it (`DRepCard` outside search, `CurrentVoteSummary.tsx:106`) renders
      exactly what it rendered at `55e8985bf`.
- [ ] CIP-105 is derived only through `normalizeDRepIdentity`; no hand-rolled
      re-encode exists anywhere in the diff.
- [ ] An id that does not decode renders one row, one copy button, and throws
      nothing.
- [ ] `DRepDetail` renders both forms full and monospaced with one copy button
      each; search-result rows render both forms stacked and truncated; cohort
      and favorites rows render exactly one `<code>` with one copy button.
- [ ] A unit test decodes both rendered strings and asserts they carry the same
      credential bytes as `entry.drepId`.
- [ ] Each copy button copies exactly the form it labels.
- [ ] The "Select for delegation" handoff still passes `entry.drepId` — asserted
      from a search-result row and from the detail view.
- [ ] Both clipboard warn payloads are id-free for both forms: unavailable keeps
      `drepIdLength` only, failure keeps `{ error, drepIdLength }`; the floor
      suite and the new component suite are cited together.
- [ ] Five new keys land in both catalogs, all ten strings `!!!`-marked,
      `yarn i18n:manage` clean, unintended writes reverted with `git restore`.
- [ ] `DRepDirectory.spec.tsx`'s existing truncation assertions pass unchanged;
      both existing snapshots come back unchanged.
- [ ] Directory and detail stories show the dual-ID rendering, verified through
      the global English/Japanese toggle — no local `IntlProvider`, no per-locale
      story exports.
- [ ] AC-6 recorded as already satisfied at `drep-discovery-design.md:251-259`;
      `git diff` on that file is empty.
- [ ] `tsc --noEmit` clean, `yarn lint` exit 0, full `jest --runInBand` with no
      new failures.
- [ ] Conflicts C-1 and C-2 written into `research/anchor-2-findings.md` and
      summarised in the task's `statusReason`.

---

## task-154 and task-155

These are build positions **4** and **5** of the canonical anchor-2 order
(`157 → 153 → 174 → 154 → 155 → 156`). task-154 lands the confirmation dialog's
verified-name line; task-155 then sweeps every anchor-derived surface for a
provenance label. Do them in that order — task-155's confirmation-dialog
assertions require task-154's edit to already be on disk.

Every `path:line` below was re-read in the worktree at `55e8985bf`. Where the
tasks JSON or a design doc cites a different line, the live file wins and the
correction is stated inline.

---

## task-154 — Migrate confirmation dialog identity to verified name (preserve byte-equality)

### What this task owns

Exactly two visible things, per `shared-design-tokens.md:135` (quoted verbatim):

> "task-175 (cv-2) renders the pre-anchor block — the CIP-129 line, the CIP-105
> line when derivable, the signed-payload line and the `(Source: On-chain)`
> label — over the identity object task-173 supplies from
> `normalizeDRepIdentity`. **task-154 (anchor-2) then owns only the swap to the
> after-`anchor-2` template: the `{verified givenName}` line and the extended
> `On-chain · Name: Verified off-chain content` source label.** Neither task
> changes the block contents above, and task-154 does not own the pre-anchor
> block."

The after-`anchor-2` template it is building (`shared-design-tokens.md:124-131`):

```
You are delegating your voting power to:
{verified givenName}
CIP-129 DRep ID:  drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras
CIP-105 DRep ID:  drep185r8rr6j9evjs…uutaz3        (when derivable)
Signed payload:   { vote: { type: "drep", id: "<hex credential>" } }
                  (Source: On-chain · Name: Verified off-chain content)
```

This task also **discharges task-157 AC-2's confirmation half** ("Delegation
confirmation shows the verified display name only when verified metadata is
available"). task-157 makes the data available and touches no file under
`components/voting/`; the gating logic ships here.

### What this task must NOT do (read this before writing any code)

- **Do not change `chosenOption`.** It is built at
  `source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx:242-245`
  and must reach `VotingStore.delegateVotes` byte-for-byte:
  ```tsx
  const chosenOption =
    state.selectedVoteType === 'drep'
      ? state.drepInputState.value
      : state.selectedVoteType;
  ```
- **Do not change the CIP-129 line, the CIP-105 line, the signed-payload line or
  the on-chain label** already rendered at
  `VotingPowerDelegationConfirmationDialog.tsx:157-199`.
- **Do not change the signed-payload string** at `:190`:
  ```tsx
  {`{"vote":{"type":"drep","id":"${drepIdentity.credentialHex}"}}`}
  ```
- **Do not add a name to the sentinel branch** (`:201-210`). `Abstain` and
  `No Confidence` are form-only sentinels; they carry no identity.
- **Do not add a new `DRepSourceLabel` variant.** The composite label is two
  existing instances joined by a literal `·`.
- **Do not touch `source/common/utils/logging.ts`.** Its key list is task-157's
  single edit for this slice. task-154 needs no addition — `verifiedName` is
  already redacted at `logging.ts:63` and this task adds no logger, analytics or
  electron-store call on any path it touches.
- **Do not touch `source/renderer/app/stores/VotingStore.ts`** or make
  `VotingStore` read `GovernanceStore`. The name is resolved in the container and
  passed down as a prop.
- **Do not derive the name from `AnchorEnrichEntry.givenName`.** The detail view
  owns that channel; the confirmation dialog reads `entry.verifiedName` via
  `governance.drepIndex` (see "Which name source" below).
- **Do not add name search, sort or filter.** Deferred beyond v1.

### Which name source, and why (this is decided — do not re-derive)

`AppDRepDirectoryEntry.verifiedName` (`GovernanceStore.ts:35`) is the confirmation
dialog's source. Live shape at `GovernanceStore.ts:23-36`:

```ts
export interface AppDRepDirectoryEntry {
  drepId: string;
  votingPower: BigNumber | null;
  status: DRepDirectoryEntry['status'];
  drepActivity: DRepDirectoryEntry['drepActivity'];
  anchor: DRepAnchorPresence | null;
  /** Verified CIP-119 givenName, or null. Projection of anchorStateByDRepId. */
  verifiedName: string | null;
}
```

It is written only by the hash-guarded projection at `GovernanceStore.ts:521-538`,
whose docblock (`:516-519`) states the guarantee this surface depends on:

> "Re-applies verified names onto a freshly rebuilt list. A name is dropped when
> the entry's on-chain anchor hash no longer matches the hash that was verified,
> so a re-registered anchor can never keep showing the old name."

and whose body drops the name unless `entry.anchor.hash === state.hash` and
`state.state === 'verified'`. That is exactly the "never show an unverified
anchor name in the confirmation dialog" rule at `shared-design-tokens.md:133`.

At HEAD `verifiedName` is written but **read by no component** — task-154 is its
first consumer. The detail view keeps reading `state.givenName` off
`AnchorEnrichEntry` (`DRepDetailAnchorContent.tsx:66,79`); do not change that.

`drepIndex` is a `Map<string, AppDRepDirectoryEntry>` (`GovernanceStore.ts:131`)
and is **already in scope** in the container that renders the dialog:
`VotingGovernancePage.tsx:74` passes `drepIndex={governance.drepIndex}` and
`governance` is destructured at `:46`.

**Known and accepted limitation.** The anchor fetch is lazy and per-detail-visit
(`GovernanceStore.fetchAnchorContent:408-451`, triggered from `DRepDetailPage`),
so `verifiedName` is populated only for DReps the user opened in this session. A
user who types an ID straight into the delegation form sees no name. That is
precisely what "only when verified metadata is available" anticipates. Do **not**
add bulk prefetching to close it, and do **not** inject store state in a test to
simulate global knowledge.

### Locked invariants this task must not break (inlined — do not look them up)

1. **No second delegation backend.** Selection supplies a DRep ID to the existing
   `delegateVotes` / `VotingStore` signing paths via React Router
   `location.state` only. `VotingStore` never reads `GovernanceStore` directly.
2. **Byte-equality.** CIP-129, CIP-105 and the signed payload `vote.id` stay
   byte-equal through every identity-display change; the on-device DRep ID equals
   `vote.chosenOption`.
3. **Sanitization floor.** No DRep id, no `abstain`/`no_confidence` literal, no
   CIP-129/CIP-105 bech32 string in any logger, analytics or electron-store
   payload — re-asserted via the task-111 spy suite in every slice. The task-168
   DRep-state snapshot is the one documented exception.
4. **Anchor transport-security floor, never thinned.** No anchor-derived content
   renders without verification AND a verified off-chain source label.
5. **Preliminary copy.** Every new en-US and ja-JP string keeps the leading `!!!`
   marker. Removing `!!!` is a release-end manual review, never a per-slice task.
6. **`Abstain` / `No Confidence` are form-only sentinels**, never DRep directory
   entries.
7. **No auto-delegation.** A verified name is display only; nothing here
   initiates or pre-selects a delegation.

---

### Step 1 — Add the two new message descriptors

File: `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.messages.ts`

Insert two entries into the `defineMessages({ … })` object, immediately after the
`signedPayload` entry (which ends at `:31`) and before `fee` (`:32`):

```ts
  verifiedName: {
    id: 'voting.governance.confirmationDialog.verifiedName',
    defaultMessage: '!!!Verified name',
    description:
      'Label above the verified off-chain DRep name in the delegation confirmation dialog',
  },
  verifiedNameSource: {
    id: 'voting.governance.confirmationDialog.verifiedNameSource',
    defaultMessage: '!!!Name:',
    description:
      'Prefix joining the on-chain source label to the verified-name source label',
  },
```

Do not edit any existing descriptor.

---

### Step 2 — Widen the dialog's props and render the name + composite label

File: `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx`

**2a. Export the value type.** Insert immediately after the
`mapVoteToIntlMessage` helper (which ends at `:41`) and before
`export type VotingPowerDelegationConfirmationDialogState` (`:43`):

```ts
/**
 * The verified name plus the host that served the hash-matched bytes. One
 * object, not two props, so `filterLogData`'s existing `verifiedName` key
 * redacts the host with it at any depth.
 */
export type VerifiedDRepNameSource = {
  host: string;
  name: string;
};
```

**2b. Add the prop.** In `VotingPowerDelegationConfirmationDialogProps`
(`:55-71`), add one line after `selectedWallet: Wallet;` (`:70`), keeping the
alphabetical order the type already uses:

```ts
  verifiedName: VerifiedDRepNameSource | null;
```

The prop is **required, not optional** — that is deliberate, so `tsc --noEmit`
enumerates every call site (one container, one spec harness, three stories).

**2c. Destructure it.** In the function signature (`:73-85`), add `verifiedName`
after `selectedWallet,` (`:84`).

**2d. Render the name line.** Replace lines `:157-169`, which currently read:

```tsx
        {!isSentinelVote ? (
          <>
            <p className={styles.paragraphTitle}>
              {intl.formatMessage(messages.drepId)}
            </p>
            <p className={styles.paragraphValue}>
              {/* Rendered untouched: must stay byte-equal to chosenOption and
                  the delegateVotes dRepId. Name slot is reserved for anchor-2;
                  unverified names never render here. */}
              <code className={styles.drepIdValue}>
                {drepIdentity?.raw ?? chosenOption}
              </code>
            </p>
```

with:

```tsx
        {!isSentinelVote ? (
          <>
            {verifiedName && (
              <>
                {/* Only the hash-guarded verified projection reaches here; an
                    unverified anchor name never renders on a signing surface. */}
                <p className={styles.paragraphTitle}>
                  {intl.formatMessage(messages.verifiedName)}
                </p>
                <p className={styles.paragraphValue}>{verifiedName.name}</p>
              </>
            )}
            <p className={styles.paragraphTitle}>
              {intl.formatMessage(messages.drepId)}
            </p>
            <p className={styles.paragraphValue}>
              {/* Rendered untouched: this string must stay byte-equal to
                  chosenOption and the delegateVotes dRepId. */}
              <code className={styles.drepIdValue}>
                {drepIdentity?.raw ?? chosenOption}
              </code>
            </p>
```

Note what changed in the retained comment: the sentence *"Name slot is reserved
for anchor-2; unverified names never render here."* is **deleted**. It is the one
comment in the governance tree that names a slice, it is now discharged, and it
must not survive as change history. The byte-equality half is kept because that
invariant is still non-obvious from the code.

**2e. Extend the source label.** Replace lines `:195-199`, which currently read:

```tsx
            {drepIdentity && (
              <p className={styles.paragraphValue}>
                <DRepSourceLabel source="on-chain" />
              </p>
            )}
```

with:

```tsx
            {(drepIdentity || verifiedName) && (
              <p className={styles.paragraphValue}>
                <DRepSourceLabel source="on-chain" />
                {verifiedName && (
                  <>
                    {' · '}
                    {intl.formatMessage(messages.verifiedNameSource)}{' '}
                    <DRepSourceLabel
                      source="verified-off-chain"
                      host={verifiedName.host}
                    />
                  </>
                )}
              </p>
            )}
          </>
```

The guard widens from `drepIdentity` to `drepIdentity || verifiedName` so a
verified name can never render without a provenance label, even for an id
`normalizeDRepIdentity` rejects. With `verifiedName` null the rendered output is
byte-identical to HEAD, so the existing case at
`VotingPowerDelegationConfirmationDialog.spec.tsx:353-363`
("renders only the verbatim primary line when the decoder rejects the id", which
asserts `queryByText('!!!On-chain')` is absent) stays green unchanged.

`host` is required by `DRepSourceLabel`'s `verified-off-chain` tooltip
(`DRepSourceLabel.tsx:90`: `intl.formatMessage(tooltipMessage, { host: host ?? '' })`)
— passing nothing would render "Fetched from , hash-matched…".

Nothing else in this file changes. No new scss class is needed: the `·`
separator is plain text inside the existing `styles.paragraphValue` paragraph.

---

### Step 3 — Resolve the name in the container

File: `source/renderer/app/containers/voting/VotingGovernancePage.tsx`

**3a. Imports.** After `:13`
(`import type { DRepIdentity } from '../../../../common/types/governance.types';`)
add:

```ts
import type { AppDRepDirectoryEntry } from '../../stores/GovernanceStore';
import type { VerifiedDRepNameSource } from '../../components/voting/voting-governance/VotingPowerDelegationConfirmationDialog';
```

**3b. Module-level helper.** Insert between the import block and
`type Props = InjectedProps & RouteComponentProps;` (`:15`):

```ts
// The verified-off-chain label names the host that served the bytes; redirects
// are off, so the anchor URL's host is that host. A name whose host will not
// parse is dropped rather than labelled with a blank source.
const resolveVerifiedName = (
  entry: AppDRepDirectoryEntry | undefined
): VerifiedDRepNameSource | null => {
  if (entry?.verifiedName == null || entry.anchor == null) return null;
  try {
    return { host: new URL(entry.anchor.url).host, name: entry.verifiedName };
  } catch {
    return null;
  }
};
```

**3c. Resolve it in the render callback.** Replace lines `:81-87`, currently:

```tsx
          // Sentinels carry no identity; a drep target is decoded for display
          // only — the rendered and submitted string stays chosenOption itself,
          // untouched.
          const drepIdentity: DRepIdentity | null =
            chosenOption === 'abstain' || chosenOption === 'no_confidence'
              ? null
              : normalizeDRepIdentity(chosenOption);
```

with:

```tsx
          // Sentinels carry no identity; a drep target is decoded for display
          // only — the rendered and submitted string stays chosenOption itself,
          // untouched.
          const isSentinel =
            chosenOption === 'abstain' || chosenOption === 'no_confidence';
          const drepIdentity: DRepIdentity | null = isSentinel
            ? null
            : normalizeDRepIdentity(chosenOption);
          const verifiedName = isSentinel
            ? null
            : resolveVerifiedName(governance.drepIndex.get(chosenOption));
```

**3d. Pass it.** In the JSX at `:89-115`, add one line after
`selectedWallet={selectedWallet}` (`:114`):

```tsx
              verifiedName={verifiedName}
```

`governance` is already destructured at `:46`; add nothing to the destructure.
`voting.delegateVotes({ chosenOption, passphrase, wallet: selectedWallet })` at
`:99-105` is **unchanged** — that is the byte-equality guarantee.

---

### Step 4 — Locale catalogs

Do **not** hand-edit the catalogs first. Run the extractor after Steps 1–3 are on
disk (this is the anchor-1 precedent, `anchor-1-implementation-guide.md:3767`):

```bash
cd /home/node/.claude/jobs/3bad97d1/wt-anchor-2
yarn i18n:manage
```

It writes four tracked files: `source/renderer/app/i18n/locales/en-US.json`,
`.../ja-JP.json`, `.../defaultMessages.json` and `translations/messages.json`.
All four belong in this commit. Then check the diff and **surgically revert any
file or hunk it touched that this task did not intend to change**:

```bash
git diff --name-only
git restore <path>          # NEVER `git stash` — the stash stack is shared
```

Then fill the ja-JP values by hand. Final state, **two new keys per catalog**.
Keys are stored alphabetically; both go between
`"voting.governance.confirmationDialog.title"` (en-US `:967`, ja-JP `:967`) and
`"voting.governance.confirmationDialog.vote"` (`:968`):

`source/renderer/app/i18n/locales/en-US.json`
```json
  "voting.governance.confirmationDialog.verifiedName": "!!!Verified name",
  "voting.governance.confirmationDialog.verifiedNameSource": "!!!Name:",
```

`source/renderer/app/i18n/locales/ja-JP.json`
```json
  "voting.governance.confirmationDialog.verifiedName": "!!!検証済みの名前",
  "voting.governance.confirmationDialog.verifiedNameSource": "!!!名前:",
```

Both keep the leading `!!!` in both locales. Never run prettier over these files
or over `translations/messages.json` — they are tool-managed.

**This task's contract is the delta: +2 keys per catalog, 0 of them
`governance.*`** (both are in the `voting.` namespace).

The absolute numbers depend on what landed before you. Measured at `55e8985bf`:
1631 keys per catalog, 97 `governance.*`. In build order — task-157 (+13
`governance.*`), task-153 (0), task-174 (+5 `governance.*`), then this task —
the totals when you finish are **1651 / 1651** with `governance.*` at
**115 / 115**. Re-measure with the parity script in Step 11 rather than trusting
the number; if a predecessor is missing from the branch the totals shift and the
delta is still what you assert.

---

### Step 5 — Register the new keys with the marker enforcement spec

File: `tests/jest/i18n/preliminaryCopyMarkers.spec.ts`

The `governance.`-prefixed rule at `:55-62` does **not** cover
`voting.governance.confirmationDialog.*`; the explicit list at `:17-20` does.
Replace lines `:15-20`, currently:

```ts
// Only these two confirmation-dialog keys are preliminary; the rest of that
// namespace predates the feature and is legitimately unmarked.
const PRELIMINARY_CONFIRMATION_KEYS = [
  'voting.governance.confirmationDialog.drepIdCip105',
  'voting.governance.confirmationDialog.signedPayload',
];
```

with:

```ts
// Only these confirmation-dialog keys are preliminary; the rest of that
// namespace predates the feature and is legitimately unmarked.
const PRELIMINARY_CONFIRMATION_KEYS = [
  'voting.governance.confirmationDialog.drepIdCip105',
  'voting.governance.confirmationDialog.signedPayload',
  'voting.governance.confirmationDialog.verifiedName',
  'voting.governance.confirmationDialog.verifiedNameSource',
];
```

Test count is unchanged (5); only the array grows. If you leave this out the two
new markers are unenforced and the task is not done.

---

### Step 6 — Storybook

File: `storybook/stories/voting/Governance.stories.tsx`

`verifiedName` is a required prop, so all three call sites must be updated or
`tsc --noEmit` fails. **Never add a local `IntlProvider` and never author
per-locale story variants** — the global English/Japanese toggle in
`storybook/preview.tsx` is the mechanism.

**6a.** Add a knob-driven helper next to `toStoryDRepIdentity` (`:63-66`), after
`voteOptions` (`:68-72`):

```tsx
const toStoryVerifiedName = (option: string) =>
  option === 'abstain' ||
  option === 'no_confidence' ||
  !boolean('Verified anchor name available', true)
    ? null
    : {
        host: text('Verified name host', 'raw.githubusercontent.com'),
        name: text('Verified name', 'Daedalus Test DRep'),
      };
```

`boolean` and `text` are already imported (`:5-11`).

**6b.** Add `verifiedName={toStoryVerifiedName(chosenOption)}` to the shared
`renderGovernanceConfirmationDialog` helper, after `selectedWallet` in the JSX
that starts at `:254` (`drepIdentity` is at `:256`).

**6c.** Add `verifiedName={toStoryVerifiedName(voteOption)}` to the
`'Confirmation dialog - software wallet'` story's dialog (opens `:414`,
`drepIdentity` at `:416`) and to the `'Confirmation dialog - hardware wallet'`
story's dialog (opens `:452`, `drepIdentity` at `:454`).

The integrated `Voting / Governance > Connected flow` story routes through
`renderGovernanceConfirmationDialog`, so 6b covers it; it must still run.

---

### Step 7 — Dialog component tests

File: `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx`
(measured baseline: **1 suite, 27 tests, 0 snapshots**)

**7a.** Add `verifiedName={null}` to the default props in `renderDialog`
(`:43-55`), after `selectedWallet={softwareWallet}` (`:53`), so every existing
case keeps HEAD behaviour.

**7b.** Rename the existing case at `:89` from
`'never renders a name field, even if extra fields sneak into the identity'` to
`'never renders a name carried on the identity object'`. Keep its body
unchanged — it is still a live guard: the dialog reads only the `verifiedName`
prop, so a name smuggled onto `drepIdentity` must stay invisible.

**7c.** Add a new describe block at the end of the file (after the identity-block
describe closes at `:377`) with **five** cases:

```tsx
describe('VotingPowerDelegationConfirmationDialog — verified name', () => {
  const KEY_CIP129 =
    'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
  const KEY_CIP105 =
    'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
  const KEY_CREDENTIAL_HEX =
    'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
  const verified = { host: 'raw.githubusercontent.com', name: 'Daedalus Test DRep' };

  const renderVerified = (overrides: Record<string, unknown> = {}) =>
    renderDialog({
      chosenOption: KEY_CIP129,
      drepIdentity: normalizeDRepIdentity(KEY_CIP129),
      verifiedName: verified,
      ...overrides,
    });

  afterEach(cleanup);

  it('renders the verified name above the DRep ID', () => { … });
  it('labels the verified name with both source labels and the host tooltip', () => { … });
  it('keeps CIP-129, CIP-105 and the signed payload byte-equal when a name is added', () => { … });
  it('renders no name and only the on-chain label when no verified metadata exists', () => { … });
  it('renders no name for the abstain and no_confidence sentinels', () => { … });
});
```

Case bodies, in full:

1. **name above the ID** — `renderVerified();` then
   `expect(screen.getByText('!!!Verified name')).toBeInTheDocument();`,
   `expect(screen.getByText('Daedalus Test DRep')).toBeInTheDocument();`, and
   assert order by filtering `<p>` text content exactly as the shipped case at
   `:271-281` does:
   ```tsx
   const order = ['!!!Verified name', '!!!DRep ID', '!!!CIP-105 DRep ID', '!!!Signed payload'];
   expect(
     Array.from(document.querySelectorAll('p'))
       .map((node) => node.textContent ?? '')
       .filter((t) => order.includes(t))
   ).toEqual(order);
   ```
2. **both labels + tooltip** — `renderVerified();` then:
   ```tsx
   const label = screen.getByText('!!!Verified off-chain content');
   expect(label.getAttribute('title')).toEqual(
     expect.stringContaining('raw.githubusercontent.com')
   );
   const sourceLine = label.closest('p');
   expect(sourceLine?.textContent).toContain('!!!On-chain');
   expect(sourceLine?.textContent).toContain('!!!Name:');
   ```
   Assert the `·` prefix through the paragraph's `textContent`, **not** through
   `screen.getByText('!!!Name:')`: Testing Library matches a node's direct text
   children only, and the source paragraph's direct text is `" · !!!Name: "`,
   which normalizes to `"· !!!Name:"` and will not match the bare string. The
   `label` and `!!!On-chain` spans each hold a single direct text node, so they
   match by exact string. This case is the §7 composite
   `On-chain · Name: Verified off-chain content` proof.
3. **byte-equality** — `renderVerified();` then assert the three identity
   strings are unchanged by the name:
   ```tsx
   expect(screen.getByText(KEY_CIP129).textContent).toBe(KEY_CIP129);
   expect(screen.getByText(KEY_CIP105).textContent).toBe(KEY_CIP105);
   expect(
     screen.getByText(`{"vote":{"type":"drep","id":"${KEY_CREDENTIAL_HEX}"}}`)
   ).toBeInTheDocument();
   ```
   This is AC-2's renderer half.
4. **no metadata** — `renderVerified({ verifiedName: null });` then
   `expect(screen.queryByText('!!!Verified name')).not.toBeInTheDocument();`,
   `expect(screen.queryByText('!!!Verified off-chain content')).not.toBeInTheDocument();`,
   `expect(screen.getByText('!!!On-chain')).toBeInTheDocument();`. This is
   task-157 AC-2's "only when available".
5. **sentinels** — `it.each(['abstain', 'no_confidence'])`, render with
   `{ chosenOption: option, drepIdentity: null, verifiedName: verified }` and
   assert `queryByText('!!!Verified name')`, `queryByText('Daedalus Test DRep')`
   and `queryByText('!!!Verified off-chain content')` are all absent while
   `getByText('Vote')` is present. This is the invariant-6 guard: even if a
   caller mistakenly supplies a name for a sentinel, none renders.

`normalizeDRepIdentity` and `cleanup` are already imported (`:17`, `:7`).

**Expected: 1 suite, 27 → 32 tests, 0 snapshots.** The delta is the contract, not
the total.

---

### Step 8 — Container tests

File: `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`
(measured baseline: **1 suite, 27 tests, 0 snapshots**)

**8a.** In `describe('Confirmation dialog prop contract')`, add `'verifiedName'`
to the `EXPECTED_DIALOG_PROPS` array (it currently lists exactly the ten shipped
props; the test at `'hands the dialog exactly the current-target prop set'`
compares sorted key sets, so omitting this fails).

**8b.** Add three cases to `describe('Confirmation dialog identity derivation')`
(which already holds `'classifies a CIP-129 script DRep by its header byte'` and
`'passes a null identity for an id the decoder rejects…'`). Use the same
`openConfirmation(...)` harness those cases use, and seed `drepIndex` through the
governance store stub the harness builds:

1. `'passes the hash-guarded verified name and its anchor host to the dialog'` —
   seed `drepIndex` with an entry whose `verifiedName` is `'Daedalus Test DRep'`
   and whose `anchor.url` is
   `'https://raw.githubusercontent.com/example/drep.jsonld'`; assert
   `mockDialogProps[last].verifiedName` equals
   `{ host: 'raw.githubusercontent.com', name: 'Daedalus Test DRep' }`.
2. `'passes a null verified name when the entry carries none'` — seed the entry
   with `verifiedName: null`; assert the prop is `null`.
3. `'passes a null verified name for the abstain sentinel'` — open the
   confirmation with `chosenOption: 'abstain'`; assert the prop is `null` and
   `drepIdentity` is `null`.

Also assert in case 1 that `stores.voting.initializeVPDelegationTx` was called
with `expect.objectContaining({ chosenOption: <the same id string> })` — the
container half of AC-2.

**Expected: 1 suite, 27 → 30 tests, 0 snapshots.**

---

### Step 9 — Hardware-wallet assertions (AC-3, release-blocking)

`shared-design-tokens.md:139`: "A hardware-wallet Jest test must assert that the
identifier surfaced by the device prompt is byte-equal to `vote.chosenOption`…
This is a release-blocking assertion."

Both mappers derive the device credential at the same line:
`source/renderer/app/utils/shelleyLedger.ts:71` and
`source/renderer/app/utils/shelleyTrezor.ts:71`:

```ts
const { type, hash } = Cardano.DRepID.toCredential(Cardano.DRepID(cert.vote));
```

attached as `dRep: parseVoteDelegation(cert)` at `shelleyLedger.ts:97` /
`shelleyTrezor.ts:99`. **Do not change either mapper.** The four shipped
`byte-equal to vote.chosenOption` cases in each spec already pin the happy path;
what AC-3 adds is proof that the credential is a pure function of `cert.vote`
and cannot be perturbed by the display fields this slice introduces.

**9a.** `source/renderer/app/utils/shelleyLedger.spec.ts` (baseline **7 tests**)
— add one case inside the existing describe (which closes at `:87`):

```ts
  it('derives the on-device credential from vote alone, ignoring display-only fields', () => {
    const withDisplayFields = {
      ...castVote(CIP129_KEY),
      verifiedName: 'Daedalus Test DRep',
    } as CoinSelectionCertificate;

    expect(toLedgerCertificate(withDisplayFields).params.dRep).toEqual(
      toLedgerCertificate(castVote(CIP129_KEY)).params.dRep
    );
    expect(toLedgerCertificate(withDisplayFields).params.dRep).toEqual({
      type: DRepParamsType.KEY_HASH,
      keyHashHex: decodedHash(CIP129_KEY),
    });
  });
```

`castVote` (`:19-24`), `decodedHash` (`:28-29`) and `CIP129_KEY` (`:12`) already
exist in that file.

**9b.** `source/renderer/app/utils/shelleyTrezor.spec.ts` (baseline **7 tests**)
— add the mirror case inside the describe that closes at `:102`:

```ts
  it('derives the on-device credential from vote alone, ignoring display-only fields', () => {
    const withDisplayFields = {
      ...castVote(CIP129_KEY),
      verifiedName: 'Daedalus Test DRep',
    } as CoinSelectionCertificate;
    const result = toTrezorCertificate(withDisplayFields) as {
      dRep?: { type: number; keyHash?: string };
    };

    expect(result.dRep).toEqual({
      type: PROTO.CardanoDRepType.KEY_HASH,
      keyHash: decodedHash(CIP129_KEY),
    });
  });
```

`castVote` (`:24-29`), `decodedHash` (`:31-32`) and `CIP129_KEY` (`:17`) already
exist there.

**Expected: 2 suites, 7 → 8 tests each.**

---

### Step 10 — Tracker rows (value-only edits)

File: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`

The task-154 object is at `:1850-1867` (`"id": "task-154"` at `:1851`). Edit only
the `status` and `statusReason` values in place; preserve every surrounding
character of formatting. **Never run prettier over this file — it is
tool-managed.**

`statusReason` must record, in one paragraph:
- AC-1 discharged by the `{verified givenName}` line plus the composite
  `On-chain · Name: Verified off-chain content` label, sourced from
  `entry.verifiedName` via `governance.drepIndex`;
- AC-2 discharged by the dialog-spec byte-equality case and the container's
  unchanged `chosenOption` handoff;
- AC-3 discharged by the two new `shelleyLedger.spec.ts` / `shelleyTrezor.spec.ts`
  cases;
- the inbound hand-off: **this task also discharges task-157 AC-2's confirmation
  half**; task-157's own row records the outbound side;
- the stated limitation: `verifiedName` is populated only for DReps whose anchor
  was fetched in the current session, so the after-`anchor-2` template is the
  exception path in practice.

Do not touch any other task row in this commit.

---

### Step 11 — Format, verify, commit

```bash
cd /home/node/.claude/jobs/3bad97d1/wt-anchor-2

# 1. FORMAT. `nix fmt` is unavailable in this devcontainer and remains a
#    user-owned pre-merge obligation. Explicit paths only.
#    NEVER `yarn prettier` — its package.json script embeds a repo-wide
#    "**/*.*" glob and reformats ~250 unrelated files even when given a path.
node_modules/.bin/prettier --write \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.messages.ts \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx \
  source/renderer/app/utils/shelleyLedger.spec.ts \
  source/renderer/app/utils/shelleyTrezor.spec.ts \
  tests/jest/i18n/preliminaryCopyMarkers.spec.ts \
  storybook/stories/voting/Governance.stories.tsx
#    NOT the locale catalogs, NOT translations/messages.json, NOT the tasks JSON.

# 2. TYPECHECK — exit 0. tsconfig.json has no "include", so this covers
#    source/, tests/ AND storybook/.
node_modules/.bin/typed-scss-modules source/renderer/app
node_modules/.bin/tsc --noEmit

# 3. The suites this task changes. Measured baselines at 55e8985bf on the left.
node_modules/.bin/jest --testPathPattern="VotingPowerDelegationConfirmationDialog" --no-coverage --runInBand
#   expect: 1 suite, 27 -> 32 tests, 0 snapshots
node_modules/.bin/jest --testPathPattern="containers/voting/VotingGovernancePage" --no-coverage --runInBand
#   expect: 1 suite, 27 -> 30 tests, 0 snapshots
node_modules/.bin/jest --testPathPattern="utils/shelleyLedger" --no-coverage --runInBand
#   expect: 1 suite, 7 -> 8 tests
node_modules/.bin/jest --testPathPattern="utils/shelleyTrezor" --no-coverage --runInBand
#   expect: 1 suite, 7 -> 8 tests
node_modules/.bin/jest --testPathPattern="i18n/preliminaryCopyMarkers" --no-coverage --runInBand
#   expect: 1 suite, 5 tests unchanged, green with the two added keys

# 4. SANITIZATION FLOOR — the two-anchor rule. Cite BOTH, together.
#    Citing only one is a false green.
node_modules/.bin/jest --testPathPattern="tests/jest/security/governance-sanitization" --no-coverage --runInBand
#   expect: 1 suite, unchanged and green. Measure the count before you edit:
#   35 at 55e8985bf, but 39 in build order (task-157 +2, task-174 +2). This
#   task adds no new redactable field name: `verifiedName` is already covered
#   at source/common/utils/logging.ts:63 and at the spec case on :290.
node_modules/.bin/jest --testPathPattern="tests/jest/governance/logDRepStateSnapshot" --no-coverage --runInBand
#   expect: 1 suite, 5 tests, unchanged and green

# 5. i18n parity + markers. The contract is the delta: +2 total keys per
#    catalog, +0 `governance.*`, both new keys `!!!`-marked. Run this once
#    before Step 4 and once after, and report both lines.
#    In build order (after task-157 +13 and task-174 +5 governance.* keys) the
#    second run prints `1651 1651 115 115 true` then `[]`.
node -e '
const en=require("./source/renderer/app/i18n/locales/en-US.json");
const ja=require("./source/renderer/app/i18n/locales/ja-JP.json");
const g=o=>Object.keys(o).filter(k=>k.startsWith("governance."));
const c=["voting.governance.confirmationDialog.verifiedName","voting.governance.confirmationDialog.verifiedNameSource"];
console.log(Object.keys(en).length,Object.keys(ja).length,g(en).length,g(ja).length,
  c.every(k=>en[k]?.startsWith("!!!")&&ja[k]?.startsWith("!!!")));
console.log(Object.keys(en).filter(k=>!(k in ja)).concat(Object.keys(ja).filter(k=>!(k in en))));
'

# 6. LINT — exit 0, 0 errors. The ~5591 pre-existing warnings at HEAD are not
#    the gate; errors are.
yarn lint

# 7. STORYBOOK — `yarn storybook:build` is red at HEAD for a pre-existing
#    manager-webpack reason unrelated to any change, which makes
#    `yarn check:all` red too. The usable floor is:
yarn storybook
#    Render `Voting / Governance > Connected flow`, `Confirmation dialog -
#    software wallet` and `Confirmation dialog - hardware wallet`, toggle the
#    global English/Japanese switch, and toggle the
#    `Verified anchor name available` knob both ways.
#    OWED, not green: no browser exists in this environment, so this pass and
#    the ja-JP overflow check cannot execute here.
```

Commit — a single Conventional Commits subject line, **no body, no trailers, no
`Co-Authored-By`**:

```
feat(gov): task-154 render the verified drep name in the delegation confirmation
```

### Files touched by task-154

| file | change |
|---|---|
| `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.messages.ts` | +2 descriptors after `:31` |
| `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx` | new exported type after `:41`; prop at `:70`; destructure at `:84`; name line replacing `:157-169`; label replacing `:195-199`; slice-naming comment at `:163-165` deleted |
| `source/renderer/app/containers/voting/VotingGovernancePage.tsx` | 2 imports after `:13`; helper before `:15`; derivation replacing `:81-87`; prop after `:114` |
| `source/renderer/app/i18n/locales/en-US.json` | +2 keys after `:967` (via `yarn i18n:manage`) |
| `source/renderer/app/i18n/locales/ja-JP.json` | +2 keys after `:967`, values hand-filled |
| `source/renderer/app/i18n/locales/defaultMessages.json` | regenerated |
| `translations/messages.json` | regenerated |
| `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` | `PRELIMINARY_CONFIRMATION_KEYS` `:15-20` grows to four entries, comment de-numbered |
| `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx` | default prop at `:53`; rename at `:89`; +1 describe, +5 cases after `:377` |
| `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` | `EXPECTED_DIALOG_PROPS` +1; +3 cases in the identity-derivation describe |
| `source/renderer/app/utils/shelleyLedger.spec.ts` | +1 case before `:87` |
| `source/renderer/app/utils/shelleyTrezor.spec.ts` | +1 case before `:102` |
| `storybook/stories/voting/Governance.stories.tsx` | helper after `:72`; `verifiedName` at the three dialog call sites (`:254`, `:414`, `:452`) |
| `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json` | task-154 row `:1850-1867`, `status`/`statusReason` values only |

---

## task-155 — Apply source labeling to DRep Discovery content

### What this task is

**A sweep, not a re-label.** It runs after every anchor-derived surface in this
slice exists (task-157's five detail fields, task-153's `doNotList` projection,
task-174's dual ID, task-154's confirmation name). It has exactly **one product
edit**; everything else is audit plus standing regression assertions.

1. **One edit.** `DRepDetailOnchainSection` gains **one section-level**
   `DRepSourceLabel source="on-chain"`. It currently imports no `DRepSourceLabel`
   at all (`DRepDetailOnchainSection.tsx:1-7`); its provenance rides entirely on
   the section heading string `governance.drepDetail.onchain.title` = `"!!!On-chain"`
   (`:9-14`). `shared-design-tokens.md:45` — the charter sentence for this task —
   says: *"Every rendered field gets an explicit provenance label. This is the
   single most important anti-misleading-content control."* Step 2 places that
   label in a `Source` row inside the section's `<dl>`, which costs **one new
   i18n key** (`governance.drepDetail.onchain.source`) for the row's `<dt>`. That
   is the whole i18n delta of this task: **+1**, not zero.
2. **An audit** that every remaining anchor-derived render carries the verified
   off-chain label, recorded in the tracker row.
3. **Standing regression assertions** in three existing specs that close AC-2 as a
   permanent check rather than a one-time inspection.

### What this task must NOT do

- **Do not re-label the fields task-157 already labelled.** task-157 attaches
  `<DRepSourceLabel source="verified-off-chain" host={…} />` to every field it
  adds, following the shipped `givenName` pattern at
  `DRepDetailAnchorContent.tsx:73-87`. If you find an unlabelled task-157 field,
  that is a task-157 defect to report — fix it there, not with a second sweep
  here.
- **Do not add a per-field label to the on-chain section.** One section-level
  label. A per-field sweep would triple the label count on the detail view for
  zero informational gain and fight the §2 "small pill" visual.
- **`CurrentVoteSummary` is explicitly out of scope.** It already renders
  `<DRepSourceLabel source="on-chain" className={styles.sourceLabel} />` at
  `CurrentVoteSummary.tsx:90`, and it renders **no anchor-derived content at
  all** today, so AC-2 is vacuously satisfied there. Its verified-name enrichment
  is cv-track work assigned by `current-vote-display-design.md:59`. Its own
  comment at `:41-42` records that the `DRepSourceLabelVariant` union cannot
  express its other states — touching it would collide with the "no new variant"
  rule. This boundary is deliberate; record it, do not close it.
- **Do not add a `DRepSourceLabelVariant`.** The union at
  `DRepSourceLabel.tsx:52-57` is `'on-chain' | 'on-chain-anchor-reference' |
  'verified-off-chain' | 'unverified-anchor' | 'anchor-unavailable'` and stays
  exactly that. The tooltip map at `:74-80` is keyed per-variant, so a composite
  variant would need a composite tooltip that misdescribes both halves.
- **Do not put verified content on `DRepCard`.** `drep-discovery-design.md:216`:
  *"`DRepCard` does **not** render verified anchor content even after
  anchor-1/anchor-2 (cards stay on-chain-only) — the verified enrichment
  surfaces in detail and favorites only."* Reinforced at `:251-259`: *"No name
  field exists on the card, and no card may grow one in v1."* The sweep must not
  become an excuse to add one.
- **Do not re-render the confirmation dialog's name or label.** task-154 landed
  both. This task only asserts them.
- **Do not touch `source/common/utils/logging.ts`** (task-157's single edit) or
  any store, parser or wire type.

### Locked invariants this task must not break (inlined)

1. **Anchor transport-security floor, never thinned:** TLS on, redirects off,
   ≤10s timeouts, ~1 MB cap, JSON content-type allow-list, SSRF + DNS-rebinding
   mitigation, Blake2b-256 hash-verify before parse/cache/render, immutable
   hash-keyed cache. **No anchor-derived content renders without verification AND
   a verified off-chain source label.** Anchor URLs open only through the
   HTTPS-only-hardened open-external-url path from task-152.
2. **Badges are informational only:** they never reorder, filter or override the
   cohort. Reinforced in live code at `drep-directory/helpers.ts:177-182`
   ("filter code must never import from the badge module").
3. **Preliminary copy.** Every new en-US and ja-JP string keeps the leading `!!!`
   marker. Removing `!!!` is a release-end manual review.
4. **Sanitization floor.** No DRep id, no `abstain`/`no_confidence` literal, no
   CIP-129/CIP-105 bech32 string in any logger, analytics or electron-store
   payload.
5. **Local-first.** Discovery data comes only from the local node via the
   main-process `GovernanceQueryService`.

---

### Step 1 — Run the audit and record the inventory

Do this **first**, before editing anything, and paste the result into the
tracker `statusReason` in Step 5.

```bash
cd /home/node/.claude/jobs/3bad97d1/wt-anchor-2

# a. Every DRepSourceLabel render site.
grep -rn "<DRepSourceLabel" --include=*.tsx source/ storybook/

# b. Every render that reads verified anchor content.
grep -rn "verifiedName\|state.givenName\|state\.objectives\|state\.motivations\|state\.qualifications\|state\.references\|state\.paymentAddress" \
  --include=*.tsx source/renderer/app/components/ source/renderer/app/containers/
```

Measured inventory at `55e8985bf` (before task-157/153/174/154 land) — six render
sites, one gap:

| # | site | variant | anchor-derived? |
|---|---|---|---|
| 1 | `DRepDetailAnchorContent.tsx:58` | `anchor-unavailable` | yes (the failure state) |
| 2 | `DRepDetailAnchorContent.tsx:80` | `verified-off-chain` | yes (`givenName`, task-151) |
| 3 | `DRepDetailAnchorSection.tsx:104` | `on-chain-anchor-reference` | no (the on-chain URL+hash pair) |
| 4 | `DRepCard.tsx:144` | `on-chain` | no |
| 5 | `VotingPowerDelegationConfirmationDialog.tsx:197` | `on-chain` | no |
| 6 | `CurrentVoteSummary.tsx:90` | `on-chain` | no — out of scope |
| — | **`DRepDetailOnchainSection.tsx` — none** | — | **the gap Step 2 closes** |

Re-run both greps after task-157/153/174/154 have landed and confirm that **every
site returned by (b) is inside or adjacent to a site returned by (a) carrying
`verified-off-chain`**. Any anchor-derived render without one is a blocker: fix it
in the owning task's file and say so in the tracker row.

`DRepCard` must appear in (a) with `on-chain` only, and must return **nothing**
from (b). If a card grew a verified field, that is a design-contract breach
(`drep-discovery-design.md:216`, `:251-259`) — revert it.

---

### Step 2 — The one product edit: section-level provenance on the on-chain section

File: `source/renderer/app/components/governance/drep-detail/DRepDetailOnchainSection.tsx`

**Placement decision — already made, do not re-derive.** The label goes in a
`Source` row at the end of the section's `<dl>`, mirroring the shipped sibling
pattern at `DRepDetailAnchorSection.tsx:99-109`, **not** inline beside the `<h2>`.
Reason: the heading string and the label string are byte-identical in both
locales — `governance.drepDetail.onchain.title` is `"!!!On-chain"` / `"!!!オンチェーン"`
(en-US `:299`, ja-JP `:299`) and `governance.drepDirectory.source.onChain` is
`"!!!On-chain"` / `"!!!オンチェーン"` (en-US `:367`, ja-JP `:367`) — so an adjacent
pill would render the same words twice on one line. The `Source` row keeps this
to exactly **one** section-level label (the binding constraint) while reading
correctly.

**2a. Import.** Add after `:4` (`import DRepStatusBadge from '../_shared/DRepStatusBadge';`):

```ts
import DRepSourceLabel from '../_shared/DRepSourceLabel';
```

**2b. New message.** Add to the `defineMessages({ … })` block, immediately after
the `title` entry (which ends at `:14`) and before `statusLabel` (`:15`):

```ts
  sourceRowLabel: {
    id: 'governance.drepDetail.onchain.source',
    defaultMessage: '!!!Source',
    description: 'Label for the on-chain section source-label row',
  },
```

Define a **new** id — do not reuse `governance.drepDetail.anchor.source`, which
is declared in a different module's local `defineMessages`
(`DRepDetailAnchorSection.tsx:25-29`); two `defineMessages` blocks declaring the
same id is a duplicate-id error at extraction time.

**2c. The row.** Insert as the last child of the `<dl>` — after the "Current
votes" row that closes at `:144`, before `</dl>` at `:145`:

```tsx
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.sourceRowLabel)}
          </dt>
          <dd className={styles.fieldValue}>
            <DRepSourceLabel
              source="on-chain"
              className={styles.sourceLabel}
            />
          </dd>
        </div>
```

`styles.sourceLabel` already exists in `DRepDetail.scss:91-94`; `styles.fieldRow`,
`styles.fieldLabel` and `styles.fieldValue` are already used throughout this
file. **No scss change is needed.**

Nothing else in this file changes. In particular the heading at `:90-92` and the
section `aria-label` at `:88` stay exactly as they are.

---

### Step 3 — Locale catalogs

Run the extractor after Step 2 is on disk:

```bash
cd /home/node/.claude/jobs/3bad97d1/wt-anchor-2
yarn i18n:manage
git diff --name-only
git restore <any file this task did not intend to change>   # NEVER `git stash`
```

Then fill the ja-JP value. **One new key per catalog.** Keys are stored
alphabetically, so it goes immediately **before**
`"governance.drepDetail.onchain.title"` (en-US `:299`, ja-JP `:299`) and after
`"governance.drepDetail.notFound"` (`:298`):

`source/renderer/app/i18n/locales/en-US.json`
```json
  "governance.drepDetail.onchain.source": "!!!Source",
```

`source/renderer/app/i18n/locales/ja-JP.json`
```json
  "governance.drepDetail.onchain.source": "!!!ソース",
```

Both keep the leading `!!!`. The key is `governance.`-prefixed, so the existing
rule at `tests/jest/i18n/preliminaryCopyMarkers.spec.ts:55-62` enforces the
marker automatically — **no edit to that spec is needed for this task.**

**This task's contract is the delta: +1 `governance.*` key per catalog.**
Re-measure with the parity script in the Verify block below rather than assuming
a number. In build order the absolute numbers are: 97 `governance.*` per catalog
at `55e8985bf` → 110 after task-157 (+13) → 115 after task-174 (+5; task-153 and
task-154 add none to this namespace) → **116 after this task**, with catalog
totals 1631 → 1644 → 1649 → 1651 (task-154's +2 `voting.*`) → **1652**. Never run
prettier over the catalogs or `translations/messages.json`.

---

### Step 4 — Standing regression assertions (AC-1, AC-2, AC-4)

No new spec file. Three existing suites gain cases.

**4a. `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx`**
(measured baseline at `55e8985bf`: **1 suite, 21 tests, 2 snapshots**. The
grounding brief says 20 — the live file is 21; use the live number and re-measure
after task-157/174 land, since both add cases here.)

Add two cases and extend one:

1. New — `'labels the on-chain section with the on-chain source label'`:
   ```tsx
   renderPage();
   const onChain = screen.getAllByText('!!!On-chain');
   expect(onChain).toHaveLength(2);
   expect(onChain.map((node) => node.tagName)).toEqual(['H2', 'SPAN']);
   expect(screen.getByText('!!!Source')).toBeInTheDocument();
   ```
   The `H2` is the section heading (`DRepDetailOnchainSection.tsx:90-92`); the
   `SPAN` is `DRepSourceLabel`'s untooltipped return (`DRepSourceLabel.tsx:87`).
   The `on-chain` variant carries no tooltip (the tooltip map at
   `DRepSourceLabel.tsx:74-80` covers only `verified-off-chain`,
   `unverified-anchor` and `anchor-unavailable`), so also assert
   `expect(onChain[1]).not.toHaveAttribute('title');` — the untooltipped variants
   must stay untooltipped or the shared `CurrentVoteSummary` snapshot drifts with
   them.
   Use `getAllByText`, never `getByText`, for `'!!!On-chain'` in this suite from
   now on — there are two matches by design.
2. New — `'keeps the on-chain section complete and labelled with no anchor data'`
   (AC-4): render with `governanceOverrides: { drepIndex: new Map([[DREP_ID, { ...baseEntry, anchor: null }]]), anchorStateByDRepId: new Map() }`
   and assert `'!!!Status'`, `'!!!Expires in'`, `'!!!Voting power'`,
   `'!!!Current votes'` and `'!!!Source'` all render, that
   `getAllByText('!!!On-chain')` still has length 2, and that
   `queryByText('!!!Verified off-chain content')` is **null**. This is the
   "local on-chain view remains complete and useful without anchor data" proof.
3. Extend the shipped case `'keeps every on-chain row when the anchor is
   unavailable'` (opens `:358`) with the same two assertions — the on-chain
   source label is present and no `'!!!Verified off-chain content'` renders on
   the `unavailable` branch.

The two snapshot cases (`:288`, `:297`) snapshot only the badge
`span[title]` (see `__snapshots__/DRepDetailPage.spec.tsx.snap`), so they are
unaffected by this change. Do not re-record them.

**Expected: 1 suite, +2 tests, 2 snapshots unchanged.**

**4b. `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx`**
(baseline before task-154: 27 tests; after task-154: 32)

Add one case — assertion only; task-154 owns the render:

```tsx
  it('never renders a verified name without both source labels', () => {
    renderDialog({
      chosenOption: KEY_CIP129,
      drepIdentity: normalizeDRepIdentity(KEY_CIP129),
      verifiedName: { host: 'example.org', name: 'Daedalus Test DRep' },
    });

    expect(screen.getByText('Daedalus Test DRep')).toBeInTheDocument();
    expect(screen.getByText('!!!On-chain')).toBeInTheDocument();
    expect(
      screen.getByText('!!!Verified off-chain content')
    ).toBeInTheDocument();
  });
```

Place it in the `— verified name` describe task-154 added, reusing its
`KEY_CIP129` constant. **Expected: 1 suite, +1 test.**

**4c. `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`**
(measured at `55e8985bf`: **1 suite, 48 tests, 1 snapshot**; **55** in build
order when you start — task-153 adds 4 cases and task-174 adds 3. Re-measure;
the contract is the +1 delta.)

Add one case inside `describe('DRepDirectory')` (opens `:174`) proving the card
never grew verified content:

```tsx
  it('renders no verified off-chain content on directory cards', () => {
    renderComponent({
      drepList: [
        { ...baseEntries[0], verifiedName: 'Daedalus Test DRep' },
      ],
    });

    expect(screen.queryByText('Daedalus Test DRep')).not.toBeInTheDocument();
    expect(
      screen.queryByText('!!!Verified off-chain content')
    ).not.toBeInTheDocument();
    expect(screen.getAllByText('!!!On-chain').length).toBeGreaterThan(0);
  });
```

`baseEntries` is declared at `:29-38` and already carries `verifiedName: null`.
Do **not** re-record `__snapshots__/DRepDirectory.spec.tsx.snap` — this task
changes no directory markup. **Expected: 1 suite, 48 → 49 tests, 1 snapshot
unchanged.**

---

### Step 5 — Tracker row (value-only edit)

File: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`

The task-155 object is at `:1868-1885` (`"id": "task-155"` at `:1869`). Edit only
`status` and `statusReason`; preserve surrounding formatting. **Never run prettier
over this file.**

`statusReason` must record:
- **AC-1** — the audit inventory from Step 1 (the post-slice re-run, not the
  `55e8985bf` snapshot), naming every `DRepSourceLabel` site and its variant;
- **AC-2** — the one gap closed (`DRepDetailOnchainSection`) plus the three
  standing regression cases that keep it closed;
- **AC-3** — detail was already labelled by task-151
  (`DRepDetailAnchorContent.tsx:73-87`) and by task-157 for its new fields; the
  confirmation label shipped in task-154. This task's contribution is audit plus
  assertion, **not** a second edit;
- **AC-4** — the no-anchor-data case in `DRepDetailPage.spec.tsx`;
- the recorded boundary: **`CurrentVoteSummary` is out of scope** — it renders no
  anchor-derived content and already carries `source="on-chain"` at `:90`; its
  enrichment is cv-track work per `current-vote-display-design.md:59`. Record it
  so a later reviewer does not read the boundary as a miss.

---

### Step 6 — Format, verify, commit

```bash
cd /home/node/.claude/jobs/3bad97d1/wt-anchor-2

# 1. FORMAT — explicit paths only. NEVER `yarn prettier`. `nix fmt` is
#    unavailable here and stays a user-owned pre-merge obligation.
node_modules/.bin/prettier --write \
  source/renderer/app/components/governance/drep-detail/DRepDetailOnchainSection.tsx \
  source/renderer/app/containers/governance/DRepDetailPage.spec.tsx \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx
#    NOT the locale catalogs, NOT translations/messages.json, NOT the tasks JSON.

# 2. TYPECHECK — exit 0.
node_modules/.bin/typed-scss-modules source/renderer/app
node_modules/.bin/tsc --noEmit

# 3. The suites this task changes.
node_modules/.bin/jest --testPathPattern="containers/governance/DRepDetailPage" --no-coverage --runInBand
#   expect: 1 suite, +2 tests over whatever this suite reported after task-174,
#   2 snapshots unchanged. The delta is the contract, not the total.
node_modules/.bin/jest --testPathPattern="VotingPowerDelegationConfirmationDialog" --no-coverage --runInBand
#   expect: 1 suite, +1 test over the post-task-154 count (32 -> 33), 0 snapshots
node_modules/.bin/jest --testPathPattern="drep-directory/DRepDirectory.spec" --no-coverage --runInBand
#   expect: 1 suite, +1 test over the post-task-174 count, 1 snapshot unchanged
node_modules/.bin/jest --testPathPattern="i18n/preliminaryCopyMarkers" --no-coverage --runInBand
#   expect: 1 suite, 5 tests, green with the new governance.* key

# 4. SANITIZATION FLOOR — two-anchor rule, both cited together.
node_modules/.bin/jest --testPathPattern="tests/jest/security/governance-sanitization" --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern="tests/jest/governance/logDRepStateSnapshot" --no-coverage --runInBand
#   expect: both green, counts unchanged by this task. It adds no logger,
#   analytics or electron-store call and no new redactable field name.

# 5. i18n parity + markers over the whole governance namespace.
node -e '
const en=require("./source/renderer/app/i18n/locales/en-US.json");
const ja=require("./source/renderer/app/i18n/locales/ja-JP.json");
const g=o=>Object.keys(o).filter(k=>k.startsWith("governance."));
console.log(g(en).length,g(ja).length,
  g(en).every(k=>en[k].startsWith("!!!")&&ja[k]?.startsWith("!!!")));
console.log(Object.keys(en).filter(k=>!(k in ja)).concat(Object.keys(ja).filter(k=>!(k in en))));
'
#   expect: equal counts, `true`, then `[]`. Run it before Step 2 and after
#   Step 3 and report both: the contract is +1 `governance.*` per catalog.
#   In build order that is 115 -> 116 (and 1651 -> 1652 whole-catalog).

# 6. LINT — exit 0, 0 errors.
yarn lint

# 7. STORYBOOK — `yarn storybook:build` is red at HEAD for a pre-existing
#    manager-webpack reason; the usable floor is:
yarn storybook
#    Render `Governance / DRep Detail` with the anchor-state knob on `verified`
#    and on `unavailable`, and toggle the global English/Japanese switch: the
#    on-chain section must show `Source` + the on-chain pill in both, and the
#    verified pill must appear only on the anchor block.
#    OWED, not green: no browser here, so this and the ja-JP overflow check
#    cannot execute in this environment.

# 8. DESIGN-DOC CHECK (no edit) — confirm the card contract still holds after
#    the whole slice, so the sweep did not put verified content on a card.
sed -n '216p;251,259p' .agent/plans/governance/drep-discovery/designs/drep-discovery-design.md
```

Commit — a single Conventional Commits subject line, **no body, no trailers**:

```
feat(gov): task-155 label the on-chain detail section and audit anchor provenance
```

### Files touched by task-155

| file | change |
|---|---|
| `source/renderer/app/components/governance/drep-detail/DRepDetailOnchainSection.tsx` | import after `:4`; `sourceRowLabel` message after `:14`; `Source` field row after `:144` |
| `source/renderer/app/i18n/locales/en-US.json` | +1 key before `:299` (via `yarn i18n:manage`) |
| `source/renderer/app/i18n/locales/ja-JP.json` | +1 key before `:299`, value hand-filled |
| `source/renderer/app/i18n/locales/defaultMessages.json` | regenerated |
| `translations/messages.json` | regenerated |
| `source/renderer/app/containers/governance/DRepDetailPage.spec.tsx` | +2 cases; extend the case opening at `:358` |
| `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx` | +1 case in task-154's `— verified name` describe |
| `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` | +1 case in the `DRepDirectory` describe (`:174`) |
| `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json` | task-155 row `:1868-1885`, `status`/`statusReason` values only |

**No scss file changes. No new component. No new `DRepSourceLabel` variant. No
change to `CurrentVoteSummary.tsx`, `DRepCard.tsx`, `DRepDetailAnchorContent.tsx`,
`DRepDetailAnchorSection.tsx` or `VotingPowerDelegationConfirmationDialog.tsx`.**

---

## task-156 — Define `Abstain` and `No Confidence` treatment in DRep directory surfaces

Tracker row: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json:1887-1902`
(`"status": "pending"`, `"priority": "low"`, `"estimatedHours": 2`, `dependencies: ["task-151"]` — complete).

**Build position: LAST of the six anchor-2 tasks.** Canonical order is
`157 → 153 → 174 → 154 → 155 → 156`. task-156 verifies the *end state* of the directory, its
empty state and the confirmation dialog after five earlier commits have changed all three. Do not
start it before task-155's commit exists on the branch.

**What this task actually is.** It is a verification-and-guard row, not a feature row. Roughly 80% of
it is reading four live seams and recording that the invariant already holds; the remaining 20% is
adding regression assertions so that the *next* change cannot quietly break it, plus one paragraph in
a design doc. **No file under `source/` other than three `.spec` files is edited. No component, no
store, no type, no i18n catalog, no new string.**

### Files touched (five, no more)

1. `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx` — append one
   nested `describe` block (Step 3).
2. `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx`
   — extend the existing sentinel `it.each` and add one submit-path case (Step 4).
3. `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` — add one `describe` block
   proving the sentinel survives the real form path (Step 5).
4. `.agent/plans/governance/drep-discovery/designs/drep-discovery-design.md` — insert one paragraph
   after line `206` (Step 6).
5. `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json` — the task-156
   row at `:1887-1902` (Step 7).

### Files deliberately NOT touched

- `source/renderer/app/components/governance/_shared/DRepEmptyState.tsx` — the copy is already
  conflict-free (Step 2 proves it). **Adding an "Abstain and No Confidence are not DReps" sentence to
  the empty state is forbidden**: it would put the sentinel labels onto a directory surface the
  invariant says they never appear on, and it would add two `!!!` strings that explain a non-entity.
- `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx`
  — the sentinel gate and branch already work; this task asserts them, it does not rewrite them.
- `source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json`, `defaultMessages.json`,
  `translations/messages.json` — **task-156 adds zero i18n keys.** Do **not** run `yarn i18n:manage`;
  it writes files and this task has nothing for it to pick up. If you run it by reflex, revert every
  file it touched with `git restore <path>` (never `git stash` — the stash stack is shared across
  worktrees).
- `tests/jest/security/governance-sanitization.spec.ts` — the sentinel literals are **already**
  covered there (Step 2). Add no cases; only re-run it.
- `source/common/utils/logging.ts`, `source/renderer/app/stores/GovernanceStore.ts`,
  `source/renderer/app/components/governance/drep-directory/helpers.ts`,
  `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx` — read-only here.
- `.agent/plans/governance/drep-discovery/designs/shared-design-tokens.md` — no edit.

### Scope and non-goals (read this instead of the PRD)

**In scope**

- Prove, with anchors, that `abstain` and `no_confidence` are form-only sentinel *strings* and never
  become `AppDRepDirectoryEntry` rows, search results, favorites, cohort members or detail views.
- Prove the confirmation dialog still renders them through the existing form path, with no identity
  block and — after task-154 — no verified name.
- Prove the directory's empty-state copy names neither sentinel, in both locales, and add a catalog
  guard so a future copy edit cannot introduce one silently.
- Record the information-architecture rationale in `drep-discovery-design.md`.

**Out of scope / forbidden**

- No new in-app copy of any kind. The rationale is documentation, not UI (the app must not explain an
  absence to the user).
- No new `DRepStatus`, `DRepSourceLabelVariant` or `DRepEmptyStateVariant` member.
- No sentinel entry in any list, index, store fixture or Storybook fixture — not even as a "negative
  test fixture" inserted into `drepList` / `showAllList` / `drepIndex`. The guard is that the sentinel
  is *never constructed as an entry*; constructing one to prove it renders oddly would defeat the
  point.
- No change to `mapVoteToIntlMessage`, `isSentinelVote`, `chosenOption` derivation, `drepIdentity`
  derivation, `delegateVotes`, or the analytics call.
- No Storybook story is added or changed by this task.

### Locked invariants this task must not break (pasted, not referenced)

> **13. `Abstain` / `No Confidence` are form-only sentinels, never DRep directory entries.**

> **2. Sanitization floor.** No DRep id, no `abstain`/`no_confidence` literal, no CIP-129/CIP-105
> bech32 string in any logger, analytics or electron-store payload — re-asserted via the task-111 spy
> suite in every slice. The task-168 DRep-state snapshot is the one documented exception.

> **4. No second delegation backend.** Selection supplies a DRep ID to the existing `delegateVotes` /
> `VotingStore` signing paths via React Router `location.state` only. `VotingStore` never reads
> `GovernanceStore` directly.

> **10. Byte-equality.** CIP-129, CIP-105 and the signed payload `vote.id` stay byte-equal through
> every identity-display change; the on-device DRep ID equals `vote.chosenOption`.

> **11. Preliminary copy.** Every new en-US and ja-JP string keeps the leading `!!!` marker. Removing
> `!!!` is a release-end manual review, never a per-slice task. *(task-156 adds no strings, so this
> binds only as "do not touch a catalog".)*

**One documented carve-out inside invariant 2 that you must not "fix".** The live floor suite
deliberately asserts that the **derived vote kind** *is* sent as an analytics dimension:
`tests/jest/security/governance-sanitization.spec.ts:582-586` asserts
`analytics.sendEvent(EventCategories.VOTING, 'Casted governance vote', 'abstain')`, and the suite's
own header docblock (`:5-7`) states: *"The derived vote kind is a sanctioned analytics dimension; the
vote target never is."* The ban is on the vote **target**. Do not change that call, and do not add an
assertion that contradicts it.

**Consequence for the tests you write.** Your specs contain the literals `'abstain'` and
`'no_confidence'` as *test input*, which is fine and already the norm in this repo. What they must
never do is route those literals into a logger, an analytics sink or an electron-store write. Step 3
adds a logger spy that asserts exactly that for the directory search path.

---

#### Context — the four live seams, quoted

Read these before editing. Every anchor below was re-verified in the worktree at `55e8985bf`; if an
earlier anchor-2 commit has shifted a line, re-locate by the quoted text, not by the number.

**(a) The sentinel type and the router-boundary filter.**

`source/renderer/app/components/voting/voting-governance/types.ts:1` — the whole file:

```ts
export type VoteType = 'abstain' | 'no_confidence' | 'drep';
```

`source/renderer/app/containers/governance/delegationFormState.ts:15-19` and `:38-40` — the only
place a sentinel can enter the app from outside, and it is a closed allow-list:

```ts
const VOTE_TYPES: ReadonlyArray<VoteType> = [
  'abstain',
  'no_confidence',
  'drep',
];
// …
  if (VOTE_TYPES.includes(candidate.voteType as VoteType)) {
    picked.voteType = candidate.voteType;
  }
```

Note the shape: a sentinel travels as `voteType`, **never** as `selectedDRepId` (`:41-43` picks
`selectedDRepId` only as a `string`, and the directory only ever supplies a real
`entry.drepId` — `DRepDirectoryPage.tsx:59-66`).

**(b) The form derives `chosenOption` from the vote type, not from the directory.**

`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx:242-245`:

```ts
  const chosenOption =
    state.selectedVoteType === 'drep'
      ? state.drepInputState.value
      : state.selectedVoteType;
```

For a sentinel the chosen option **is** the vote-type string. That string is handed to the dialog at
`:431-432` and, through `VotingGovernancePage.tsx:100-104`, to `voting.delegateVotes({ chosenOption, … })`.

**(c) The container refuses to decode a sentinel as an identity.**

`source/renderer/app/containers/voting/VotingGovernancePage.tsx:81-87`:

```tsx
          // Sentinels carry no identity; a drep target is decoded for display
          // only — the rendered and submitted string stays chosenOption itself,
          // untouched.
          const drepIdentity: DRepIdentity | null =
            chosenOption === 'abstain' || chosenOption === 'no_confidence'
              ? null
              : normalizeDRepIdentity(chosenOption);
```

**This is the line task-154 lands beside.** task-154 resolves the verified name from
`governance.drepIndex` keyed by `chosenOption`; for a sentinel that key can never be present, because
`drepIndex` is keyed by CIP-129 DRep IDs (`GovernanceStore.ts:130-131`: *"O(1) DRep lookup by ID.
Populated alongside drepList"*). Step 5 pins that.

**(d) The dialog's sentinel gate and its identity-block-free branch.**

`source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx:117-120`:

```ts
  // Keyed on the vote kind, not on a successful decode: an id the decoder
  // rejects still renders verbatim rather than as a vote label.
  const isSentinelVote =
    chosenOption === 'abstain' || chosenOption === 'no_confidence';
```

`:201-210` — the sentinel branch, which renders a vote label and nothing else:

```tsx
        ) : (
          <>
            <p className={styles.paragraphTitle}>
              {intl.formatMessage(messages.vote)}
            </p>
            <p className={styles.paragraphValue}>
              {intl.formatMessage(mapVoteToIntlMessage(chosenOption))}
            </p>
          </>
        )}
```

`mapVoteToIntlMessage` at `:32-41` maps `'abstain'` → `sharedGovernanceMessages.abstain` and
`'no_confidence'` → `sharedGovernanceMessages.noConfidence`
(`voting-governance/shared-messages.ts`, ids `voting.governance.abstain` /
`voting.governance.noConfidence`). Catalog values measured at `55e8985bf`: en-US `"Abstain"` /
`"No Confidence"`, ja-JP `"棄権"` / `"不信任"`. **These four strings carry no `!!!` marker and must not
gain one** — they are pre-existing, already-reviewed copy, not new copy.

---

#### Step 0: Measure the baselines you are about to move

Four suites are in play. Because five anchor-2 commits land before this one, the HEAD numbers below
are reference points, not the contract. **Re-run these immediately before Step 3 and write the actual
numbers down; the delta is the contract, not the total.**

```bash
# Measured at 55e8985bf (slice start), for reference only:
#   DRepDirectory.spec.tsx                          1 suite,  48 tests, 1 snapshot
#   VotingPowerDelegationConfirmationDialog.spec.tsx 1 suite,  27 tests, 0 snapshots
#   VotingGovernancePage.spec.tsx                   1 suite,  27 tests, 0 snapshots
#   tests/jest/security/governance-sanitization.spec.ts 1 suite, 35 tests, 0 snapshots
node_modules/.bin/jest --testPathPattern="drep-directory/DRepDirectory.spec" --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern="voting-governance/VotingPowerDelegationConfirmationDialog.spec" --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern="containers/voting/VotingGovernancePage.spec" --no-coverage --runInBand
node_modules/.bin/jest --testPathPattern="tests/jest/security/governance-sanitization" --no-coverage --runInBand
```

---

#### Step 1: Verify the form-only sentinel path (read-only — no edit)

Open each file and confirm the quoted text is still present. Record what you found; you will paste a
condensed version into the tracker `statusReason` in Step 7.

| # | file | expected finding |
|---|---|---|
| 1.1 | `source/renderer/app/components/voting/voting-governance/types.ts:1` | `VoteType` is the closed union `'abstain' \| 'no_confidence' \| 'drep'` — a vote *kind*, never an entry type. |
| 1.2 | `source/renderer/app/containers/governance/delegationFormState.ts:15-19`, `:38-43` | `VOTE_TYPES` allow-list gates `voteType`; `selectedDRepId` is picked separately as a plain string. A sentinel can only arrive as `voteType`. |
| 1.3 | `source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx:242-245` | `chosenOption` is the sentinel string itself when `selectedVoteType !== 'drep'`. |
| 1.4 | `source/renderer/app/containers/voting/VotingGovernancePage.tsx:81-87` | Sentinels short-circuit to `drepIdentity = null`; the submitted string is `chosenOption`, untouched (`:100-104`). |
| 1.5 | `VotingPowerDelegationConfirmationDialog.tsx:117-120`, `:201-210`, `:32-41` | `isSentinelVote` gates the whole identity block out; the sentinel branch renders only the vote label. |

If any of 1.1-1.5 is **not** as described, stop and report it — an earlier anchor-2 commit has broken
invariant 13 and that is a blocking defect, not something task-156 patches around.

#### Step 2: Verify the directory cannot source a sentinel, and its copy does not conflict (read-only — no edit)

| # | file / anchor | expected finding |
|---|---|---|
| 2.1 | `source/renderer/app/stores/GovernanceStore.ts:130-134` | `drepIndex` and `drepList` are the only entry sources; both are populated from the ledger `--all-dreps` query. A sentinel is not a registration and has no `drepId`, so it can never be constructed as an `AppDRepDirectoryEntry`. |
| 2.2 | `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx:151-159` | All three `visibleEntries` branches read `searchDRepsByIdPrefix(searchIndex, …)`, `showAllList` or `drepList`. There is no fourth, synthetic source. |
| 2.3 | `helpers.ts:106-131` | `searchDRepsByIdPrefix` filters an index built from `AppDRepDirectoryEntry[]`; a query that matches no entry returns `[]`. `'no_confidence'` (13 chars, no HRP) is `queryKind === 'prefix'` and matches nothing; `'abstain'` (7 chars) is `'belowMinimum'` and never even runs a search (`helpers.ts:48-56`, `MIN_SEARCH_PREFIX_LENGTH = 8` at `:4`). |
| 2.4 | `helpers.ts:139-153` | `resolveExactDRepMatch` returns `null` unless `Cardano.DRepID.isValid(full)`; neither sentinel is a valid bech32 DRep ID, so direct entry can never open a sentinel detail view. |
| 2.5 | `_shared/DRepEmptyState.tsx:12-52` | All seven message defaults; **none mentions Abstain or No Confidence**. `noResults` (`:19-24`) is `'!!!No DReps match your filters. {ClearFilters} or {ShowAll}.'`; `noFavorites` body (`:40-46`) is about favorites and device storage. AC-3 holds by inspection. |
| 2.6 | `tests/jest/security/governance-sanitization.spec.ts` | The sentinel literals are already pinned at `:141-156` (`filterLogData` redaction via the `voting`/`vote` keys), `:200-205`, `:265-281`, `:377-378`, `:458-459`, `:495-496` and `:598`. **Add nothing here.** |

Also verified at `55e8985bf`, and worth re-running as a one-liner (it must print `[]`):

```bash
node -e "const en=require('./source/renderer/app/i18n/locales/en-US.json');const ja=require('./source/renderer/app/i18n/locales/ja-JP.json');const ns=k=>k.startsWith('governance.drepDirectory.')||k.startsWith('governance.drepFavorites.');const hit=Object.keys(en).filter(ns).filter(k=>en[k].includes(en['voting.governance.abstain'])||en[k].includes(en['voting.governance.noConfidence'])||ja[k].includes(ja['voting.governance.abstain'])||ja[k].includes(ja['voting.governance.noConfidence']));console.log(hit);"
```

#### Step 3: Add the directory sentinel-absence regression block

File: `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`.

**3a.** Add one import. The existing import block ends at `:20` with the `../../../stores/GovernanceStore`
type import. Append immediately after it:

```tsx
import { logger } from '../../../utils/logging';
```

(That is the same specifier `DRepIdDisplay.tsx:7` uses, and this suite already loads that module
transitively, so no new mock is required.)

**3b.** Insert the following nested `describe` **inside** `describe('DRepDirectory', …)`, immediately
after the closing `});` of the existing `describe('favorites', …)` block and before the outer
`describe`'s final `});`. At HEAD the file is 864 lines: `:863` closes the `favorites` describe and
`:864` closes `describe('DRepDirectory', …)`, so the block goes between them. Locate it by the last
test, `renders the favorites empty-state copy in ja-JP`.

```tsx
  describe('form-only vote sentinels', () => {
    const SENTINEL_QUERIES = ['abstain', 'no_confidence'];
    const SENTINEL_LABELS = ['Abstain', 'No Confidence'];

    it('renders no row for either sentinel and never resolves one to a detail view', () => {
      const onViewDetails = jest.fn();
      const onSelectForDelegation = jest.fn();
      renderComponent({
        drepList: [realEntry(1), realEntry(2)],
        onViewDetails,
        onSelectForDelegation,
      });

      const input = screen.getByPlaceholderText('!!!Search by DRep ID');
      SENTINEL_QUERIES.forEach((query) => {
        fireEvent.change(input, { target: { value: query } });
        SENTINEL_LABELS.forEach((label) => {
          expect(screen.queryByText(label)).not.toBeInTheDocument();
        });
      });

      expect(onViewDetails).not.toHaveBeenCalled();
      expect(onSelectForDelegation).not.toHaveBeenCalled();
    });

    it('falls back to the no-results empty state whose copy names neither sentinel', () => {
      renderComponent({ drepList: [realEntry(1)] });

      fireEvent.change(screen.getByPlaceholderText('!!!Search by DRep ID'), {
        target: { value: 'no_confidence' },
      });

      const emptyState = document.querySelector('[data-variant="noResults"]');
      expect(emptyState).not.toBeNull();
      SENTINEL_LABELS.forEach((label) => {
        expect(emptyState.textContent).not.toContain(label);
      });
      expect(screen.queryByText('!!!View details')).not.toBeInTheDocument();
    });

    it('keeps every directory and favorites string free of the sentinel labels in both locales', () => {
      const catalogs: Record<string, string>[] = [translations, jaTranslations];
      const namespaces = [
        'governance.drepDirectory.',
        'governance.drepFavorites.',
      ];

      catalogs.forEach((catalog) => {
        const labels = [
          catalog['voting.governance.abstain'],
          catalog['voting.governance.noConfidence'],
        ];
        const conflicting = Object.keys(catalog)
          .filter((key) => namespaces.some((ns) => key.startsWith(ns)))
          .filter((key) => labels.some((label) => catalog[key].includes(label)));
        expect(conflicting).toEqual([]);
      });
    });

    it('routes no sentinel literal into a logger sink while searching', () => {
      const debugSpy = jest
        .spyOn(logger, 'debug')
        .mockImplementation(() => undefined);
      const infoSpy = jest
        .spyOn(logger, 'info')
        .mockImplementation(() => undefined);
      const warnSpy = jest
        .spyOn(logger, 'warn')
        .mockImplementation(() => undefined);
      const errorSpy = jest
        .spyOn(logger, 'error')
        .mockImplementation(() => undefined);

      renderComponent({ drepList: [realEntry(1)] });

      const input = screen.getByPlaceholderText('!!!Search by DRep ID');
      SENTINEL_QUERIES.forEach((query) => {
        fireEvent.change(input, { target: { value: query } });
      });

      const logged = JSON.stringify([
        debugSpy.mock.calls,
        infoSpy.mock.calls,
        warnSpy.mock.calls,
        errorSpy.mock.calls,
      ]);
      SENTINEL_QUERIES.forEach((query) => {
        expect(logged).not.toContain(query);
      });

      jest.restoreAllMocks();
    });
  });
```

Spy on each level with a literal key, as `tests/jest/security/governance-sanitization.spec.ts:556-561`
does. A single `jest.spyOn(logger, level)` over a union-typed `level` variable makes
`.mockImplementation` resolve against a union of `SpyInstance` types and fails `tsc`.

Notes a reviewer will check, so get them right:

- `'abstain'` is 7 characters, below `MIN_SEARCH_PREFIX_LENGTH = 8`, so the list stays **unfiltered**
  and the two real rows still render. The assertion is only that no *sentinel-labelled* row appears —
  do not assert an empty list for that query.
- `'no_confidence'` is 13 characters with no HRP, so it is a real prefix search that matches nothing
  and drops through to `DRepEmptyState variant="noResults"` (`DRepDirectory.tsx:356-360`). The
  `[data-variant="noResults"]` selector matches the container div at `DRepEmptyState.tsx:75`.
- The catalog test derives the sentinel labels **from the catalog itself** rather than hard-coding
  `'Abstain'` / `'棄権'`, so a future retranslation of `voting.governance.abstain` keeps the guard
  honest.
- `+4 tests` on this suite. No snapshot change (the new block renders nothing new).

#### Step 4: Harden the dialog's sentinel assertions against the task-154 name

File: `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx`.

**4a.** Extend the RTL import at `:7` from

```tsx
import { cleanup, render, screen } from '@testing-library/react';
```

to

```tsx
import {
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from '@testing-library/react';
```

(`@testing-library/react` is 12.1.2 here; `waitFor` and `fireEvent` are both exported.)

**4b.** Replace the existing final `it.each` in `describe('VotingPowerDelegationConfirmationDialog — identity block', …)`
— at HEAD `:365-376`, the case named `renders no identity block for the %s sentinel` — with this
version. Only the `renderDialog` call and the last two expectations are new:

```tsx
  it.each(['abstain', 'no_confidence'])(
    'renders no identity block and no verified name for the %s sentinel',
    (option) => {
      renderDialog({
        chosenOption: option,
        drepIdentity: null,
        verifiedName: { host: 'example.org', name: 'Verified Sentinel Name' },
      });

      expect(screen.getByText('Vote')).toBeInTheDocument();
      expect(screen.queryByText('!!!DRep ID')).not.toBeInTheDocument();
      expect(screen.queryByText('!!!CIP-105 DRep ID')).not.toBeInTheDocument();
      expect(screen.queryByText('!!!Signed payload')).not.toBeInTheDocument();
      expect(screen.queryByText('!!!On-chain')).not.toBeInTheDocument();
      expect(
        screen.queryByText('Verified Sentinel Name')
      ).not.toBeInTheDocument();
      expect(
        screen.queryByText('!!!Verified off-chain content')
      ).not.toBeInTheDocument();
    }
  );
```

**Prop shape — this is load-bearing, not a placeholder.** task-154 types the prop
`verifiedName: VerifiedDRepNameSource | null` where `VerifiedDRepNameSource = { host: string; name: string }`
(its Step 2a/2b, exported from `VotingPowerDelegationConfirmationDialog.tsx` after `:41`), and the dialog
renders `verifiedName.name`. **Pass the object, never a bare string.** A bare string would make
`queryByText('Verified Sentinel Name')` pass even with the sentinel guard deleted — the name would never
render either way — so the assertion would prove nothing.

task-156 runs **last** in the slice (build position 6 of 6), so task-154's prop is on the branch before you
start; there is no "prop does not exist yet" case to hedge for. Before writing the test, confirm the name
with `grep -n "verifiedName" source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx`
and, if task-154 named it something else, substitute that name and keep the `{ host, name }` shape. If the
grep prints nothing, **STOP** — task-154 has not landed and this task's premise is false. Do **not** change
`renderDialog`'s signature.

`'!!!Verified off-chain content'` is the en-US value of `governance.drepDetail.sourceLabel.verified`
(measured at `55e8985bf`); `'!!!On-chain'` is `governance.drepDirectory.source.onChain`.

**4c.** Append one new case to the same `describe`, immediately after the block from 4b, proving the
sentinel completes the existing confirm path rather than merely rendering:

```tsx
  it.each([
    ['abstain', 'Abstain'],
    ['no_confidence', 'No Confidence'],
  ])(
    'confirms the %s sentinel through the existing form path',
    async (option, label) => {
      const onSubmit = jest.fn(async () => ({ success: true as const }));
      renderDialog({ chosenOption: option, drepIdentity: null, onSubmit });

      expect(screen.getByText(label)).toBeInTheDocument();

      fireEvent.change(document.querySelector('input[type="password"]'), {
        target: { value: 'test-passphrase' },
      });
      fireEvent.click(screen.getByRole('button', { name: 'Confirm' }));

      await waitFor(() => expect(onSubmit).toHaveBeenCalledTimes(1));
      expect(onSubmit).toHaveBeenCalledWith('test-passphrase');
    }
  );
```

The dialog's Confirm button stays disabled until `state.passphrase` is non-empty for a software wallet
(`VotingPowerDelegationConfirmationDialog.tsx:147-153`), which is why the passphrase change fires
first. `redirectToWallet` is already a `jest.fn()` in `renderDialog`'s defaults (`spec:52`), so the
success path is inert.

`'Abstain'` and `'No Confidence'` are the shipped, already-reviewed en-US values of
`voting.governance.abstain` / `voting.governance.noConfidence` — they carry **no** `!!!` prefix, which
is why the existing cases at `:74-87` match them bare. Do not add a prefix.

`+2 tests` on this suite (the 4b case replaces an existing one, so it is net zero; 4c's `it.each`
contributes two).

#### Step 5: Prove the sentinel survives the real container form path

File: `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`.

This is the guard that matters most, because task-154 adds a `governance.drepIndex` lookup keyed by
`chosenOption` in exactly this container. For a sentinel that lookup must miss and must not put a name
on the dialog.

Insert a new top-level `describe` immediately **after** `describe('Confirmation dialog identity derivation', …)`
closes (at HEAD, after `:726`) and before `describe('Confirmation dialog prop contract', …)` (`:728`):

```tsx
describe('Form-only vote sentinels reaching the confirmation dialog', () => {
  const openSentinelConfirmation = async (voteType: string) => {
    const flow = renderFlow([
      {
        pathname: ROUTES.VOTING.GOVERNANCE,
        state: { selectedWalletId: WALLET_ID, voteType },
      },
    ]);
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));
    await screen.findByText('Confirm Transaction');
    return flow;
  };

  beforeEach(() => {
    mockDialogProps.length = 0;
  });

  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it.each([
    ['abstain', 'Abstain'],
    ['no_confidence', 'No Confidence'],
  ])(
    'passes %s through as chosenOption with a null identity and no name',
    async (voteType, label) => {
      const { stores } = await openSentinelConfirmation(voteType);

      const props = mockDialogProps[mockDialogProps.length - 1];
      expect(props.chosenOption).toBe(voteType);
      expect(props.drepIdentity).toBeNull();
      expect(props.verifiedName ?? null).toBeNull();
      expect(screen.getByText(label)).toBeInTheDocument();
      expect(stores.voting.initializeVPDelegationTx).toHaveBeenCalledWith(
        expect.objectContaining({ chosenOption: voteType })
      );
    }
  );
});
```

Why this compiles and runs as written:

- `renderFlow`, `mockDialogProps`, `WALLET_ID` and `ROUTES` are already in scope in this file
  (`renderFlow` at `:227`, `mockDialogProps` at `:71`, `WALLET_ID` at `:96`). `openSentinelConfirmation`
  mirrors the existing `openConfirmation` helper at `:276-289`, differing only in supplying `voteType`
  instead of `selectedDRepId`.
- `softwareWallet` (`:98-102`, id `WALLET_ID`) has **no** `currentVote`, so the inherited `voteType`
  wins over the derived seed at `VotingPowerDelegation.tsx:173-175` and the form opens on the sentinel
  vote type.
- The dialog is mocked as a *recorder that renders the real component* (`:75-90`), so both the prop
  object and the rendered DOM are assertable in the same test.
- `props.verifiedName ?? null` is deliberately tolerant of the prop not existing yet; once task-154
  lands, an accidental name leak into the sentinel path fails here.
- `tsconfig.json` has `strict: false` / `strictNullChecks` off, so no non-null assertions are needed.
- Do **not** add a case asserting `stores.governance.drepIndex.has('abstain') === false`. `drepIndex`
  is a fixture `Map` built by this file's `buildStores` (`:189`), so such a test would assert a
  property of the fixture rather than of production code.

`+2 tests` on this suite.

#### Step 6: Record the IA rationale in the design doc

File: `.agent/plans/governance/drep-discovery/designs/drep-discovery-design.md`.

Insertion point: the `## State / Empty / Loading / Error Treatments` table (`:191-206`) is where the
directory's empty states are specified; the paragraph belongs directly under it. Insert a blank line
and then the paragraph **after line `206`** (`| DRep detail load failure | Inline error in main pane; "Back to directory" link |`)
and **before** the existing blank line and `## Anchor Source-Labelling Treatment (anchor-1-ready)`
heading at `:208`.

Exact text to insert (content is binding — do not paraphrase, do not add a heading level, do not cite
a task id):

```markdown
**`Abstain` and `No Confidence` are form-only.** Both are delegation-form sentinels, not DReps: they
have no registration, no anchor, no voting power and no detail view, so they never appear as directory
entries, search results, favorites or cohort members. They are chosen in the delegation form and
carried through the existing path — `VotingPowerDelegation` sets `chosenOption` to the sentinel
string, and the confirmation dialog renders a vote label with no identity block. Because they are not
entries, the directory's empty state must never suggest the directory is the place to find them; its
copy stays scoped to registered DReps and filters.
```

Constraints on this edit:

- **One paragraph, inserted once.** Before writing, grep the file for `Abstain` — the only pre-existing
  hit at `55e8985bf` is line `92`, a vote-tally line inside the detail wireframe
  (`│ │ Current votes: 2 Yes · 1 No · 0 Abstain (this epoch)     │  │`). That is an unrelated
  proposal-vote tally, **not** a directory-entry claim. Leave it exactly as it is. If the grep now
  returns a second prose hit, an earlier commit already added this paragraph — verify and skip the
  edit rather than duplicating it.
- Do not reflow, re-wrap or reformat any other line of the file. `node_modules/.bin/prettier --write`
  is **not** run over this file (see Step 7).
- Do not touch `shared-design-tokens.md`.

#### Step 7: Format, update the tracker row, commit

**7a. Format only the three spec files.** `nix` is absent in this devcontainer so the mandated
`nix fmt` cannot run; the substitute takes explicit paths and nothing else. Never `yarn prettier` —
its package.json script embeds a repo-wide `"**/*.*"` glob and reformats ~250 unrelated files even
when handed a path.

```bash
node_modules/.bin/prettier --write \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx
```

**Do not** pass the design doc or the tasks JSON to prettier. The tasks tracker is tool-managed
(value-only edits, surrounding formatting preserved), and reformatting the design doc would produce
a churn diff unrelated to this task.

If prettier reports files it changed that you did not edit, revert them surgically:

```bash
git restore <path>          # never `git stash` — the stash stack is shared across worktrees
```

**7b. Update the task-156 row** at
`.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json:1887-1902`. Set
`"status": "complete"` and add `"statusReason"`, `"evidence"` and `"updatedAt"` in the key order the
completed sibling rows use — `id, title, description, status, statusReason, evidence, updatedAt,
priority, estimatedHours, dependencies, targetPath, acceptanceCriteria` (see the task-172 row at
`:1740-1744` for the shape). `updatedAt` is `YYYY-MM-DD`. Edit values in place; preserve all
surrounding formatting; never run a formatter over this file.

`evidence`, source files first then plan docs:

```json
"evidence": [
  "source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx",
  "source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx",
  "source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx",
  ".agent/plans/governance/drep-discovery/designs/drep-discovery-design.md",
  ".agent/plans/governance/drep-discovery/task-plans/anchor-2-implementation-guide.md"
]
```

`statusReason` must state, in prose, without ALL-CAPS: that AC-1 and AC-3 were discharged by
verification plus new standing assertions rather than by an edit (naming the five read-only anchors
from Steps 1-2); that no in-app string was added and the rationale landed in the design doc instead;
the measured `baseline → after` test deltas for all four suites; that the sanitization floor was
re-proved on both anchors; and the OWED items below, none of them reported green.

**7c. Commit** — one Conventional Commits subject line, no body, no trailers, no `Co-Authored-By`:

```
test(gov): task-156 confirm abstain and no confidence stay form-only in the directory
```

`test` is the right type — three of the five files are specs and no runtime code changes.
`test(` already appears 5 times in the recent log alongside `feat(`, `docs(`, `fix(` and `refactor(`.

---

#### Verify

Run from the worktree root. Substitute your Step 0 baselines for the `N` values; the **delta** is the
contract, not the total.

```bash
# 1. Directory suite: N -> N+4 tests, snapshot count unchanged.
node_modules/.bin/jest --testPathPattern="drep-directory/DRepDirectory.spec" --no-coverage --runInBand

# 2. Confirmation dialog suite: N -> N+2 tests (Step 4b replaces a case; 4c adds two).
node_modules/.bin/jest --testPathPattern="voting-governance/VotingPowerDelegationConfirmationDialog.spec" --no-coverage --runInBand

# 3. Container suite: N -> N+2 tests.
node_modules/.bin/jest --testPathPattern="containers/voting/VotingGovernancePage.spec" --no-coverage --runInBand

# 4. Sanitization floor — cv-2 F-31's two-anchor rule. The pair is the security
#    suite plus the sibling logging suite tests/jest/governance/logDRepStateSnapshot.spec.ts
#    — the same pair the other five tasks in this slice cite. Cite BOTH runs
#    together; citing one, or substituting a non-logging suite, is a false green.
#    Both must be unchanged from baseline: this task adds no logger, analytics
#    or electron-store sink. (Run 3 above is the container suite and is not an
#    anchor for this rule.)
node_modules/.bin/jest --testPathPattern="tests/jest/security/governance-sanitization" --no-coverage --runInBand
#    expect: unchanged from your Step 0 baseline. That baseline is 35 at
#    55e8985bf but 39 in build order (task-157 +2, task-174 +2) — measure it,
#    do not assume 35.
node_modules/.bin/jest --testPathPattern="tests/jest/governance/logDRepStateSnapshot" --no-coverage --runInBand
#    expect: 5 tests, unchanged and green (no task in this slice touches it)

# 5. Governance regression sweep — nothing else moved.
node_modules/.bin/jest --testPathPattern="(governance|voting)" --no-coverage --runInBand
#    GovernanceCliArgvSmoke.spec.ts self-skips when cardano-cli is off PATH
#    (1 skipped suite / 12 skipped tests). Expected, not a regression.

# 6. Typecheck: exit 0.
node_modules/.bin/typed-scss-modules source/renderer/app
node_modules/.bin/tsc --noEmit
#    tsconfig.json has no "include", so this covers source/, tests/ AND storybook/.

# 7. Lint: exit 0 with 0 errors. Errors are the gate; the ~5591 pre-existing
#    warnings at HEAD are not (tests/ is eslint-ignored, so this task should not
#    move the warning count at all).
yarn lint

# 8. Formatting on the three spec files only.
node_modules/.bin/prettier --check \
  source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx

# 9. i18n catalogs untouched — this task adds no keys. Must print nothing.
git status --porcelain source/renderer/app/i18n translations

# 10. Directory copy still names neither sentinel in either locale. Must print [].
node -e "const en=require('./source/renderer/app/i18n/locales/en-US.json');const ja=require('./source/renderer/app/i18n/locales/ja-JP.json');const ns=k=>k.startsWith('governance.drepDirectory.')||k.startsWith('governance.drepFavorites.');const hit=Object.keys(en).filter(ns).filter(k=>en[k].includes(en['voting.governance.abstain'])||en[k].includes(en['voting.governance.noConfidence'])||ja[k].includes(ja['voting.governance.abstain'])||ja[k].includes(ja['voting.governance.noConfidence']));console.log(hit);"

# 11. The design-doc paragraph landed exactly once (expect 2 hits: the :92
#     wireframe tally line and the new paragraph — no more).
grep -n "Abstain" .agent/plans/governance/drep-discovery/designs/drep-discovery-design.md

# 12. Exactly five files changed, no more.
git status --porcelain
```

If run 1 reports `N+3` or fewer, one of the four Step 3 tests is missing. If run 10 prints a key, a
copy edit somewhere in the slice introduced a conflict and AC-3 is red — fix the copy, not the test.

#### Acceptance

| AC (verbatim from the tracker, `:1898-1900`) | How the steps discharge it |
|---|---|
| "Directory never renders Abstain or No Confidence as DRep entries." | Structural: Step 2.1-2.4 shows the only entry sources are `drepList` / `showAllList` / the search index built from them, all keyed by real CIP-129 DRep IDs, with `resolveExactDRepMatch` gated behind `Cardano.DRepID.isValid`. Standing guard: Step 3's first test drives both sentinel strings through the real search input and asserts no sentinel-labelled row and no `onViewDetails` / `onSelectForDelegation` call. **Green — verified, then guarded.** |
| "Confirmation dialog still supports Abstain and No Confidence via the existing form path." | Step 1.3-1.5 records the live path (`chosenOption` = the vote-type string → `drepIdentity` null → `isSentinelVote` branch → vote label). Step 4c asserts the sentinel confirms through to `onSubmit` at the dialog level; Step 5 asserts the whole container path — `location.state.voteType` → form → `chosenOption` byte-equal → `initializeVPDelegationTx({ chosenOption })` — and that no verified name or identity reaches the dialog. **Green.** |
| "Directory empty-state copy does not conflict with these form-only choices." | Step 2.5 verifies all seven `DRepEmptyState` message defaults name neither sentinel. Step 3's second test asserts the rendered `noResults` empty state's text contains neither label; Step 3's third test asserts the same across every `governance.drepDirectory.*` and `governance.drepFavorites.*` key in **both** catalogs, deriving the labels from the catalog so a retranslation cannot slip past. Step 6 records why the copy must stay that way. **Green — no copy edit needed.** |

The description clause "Document the IA rationale in the directory" is discharged by Step 6 in
`drep-discovery-design.md`, **not** by in-app copy: a user-facing sentence explaining why a non-entity
is absent would add two `!!!` strings for zero benefit and would print the sentinel labels onto a
directory surface invariant 13 says they never appear on.

#### OWED — not provable in this environment, never to be reported green

1. `nix fmt` — `nix` is absent in this devcontainer, so `node_modules/.bin/prettier --write` on the
   three explicit spec paths is a substitute, not the mandated formatter. Pre-merge obligation, owned
   by the user.
2. The Storybook / visual and ja-JP overflow pass over the end-state directory and its empty states.
   There is no browser here; `yarn storybook:build` is additionally red at HEAD for a pre-existing
   manager-webpack reason unrelated to any anchor-2 change, which makes `yarn check:all` red too. The
   usable floor is `yarn storybook`, and task-156 adds no story.
3. `yarn check:all` — red at HEAD for the reason in item 2.
4. The release-end `!!!` copy review, out of scope for every slice by invariant 11.

---
