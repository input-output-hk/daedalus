# DRep metadata: which fields we show, and how

CIP-119 documents are JSON-LD, so their vocabulary is extensible by design and
the community is already extending it without waiting for a CIP. This is the
policy for what Daedalus does with a field it was not written to expect.

**Status:** decided, not yet implemented
**Parser:** `source/main/governance/AnchorVerificationService.ts`
**Renderer:** `source/renderer/app/components/governance/drep-detail/DRepDetailAnchorContent.tsx`

---

## What is out there

Measured across the 404 mainnet DReps whose metadata resolved, sampling 1,000
registered DReps through Koios. Fields outside the seven the parser reads today:

CIP-119 defines eight properties and no others: `paymentAddress`, `givenName`,
`image`, `objectives`, `motivations`, `qualifications`, `references` and
`doNotList`. Everything below is outside that set.

| field | DReps | shape | notes |
|---|---|---|---|
| `image` | 239 | object | `ImageObject`, either a URL or a base64 data URI. Canonical, and withheld deliberately |
| `bio` | 119 | string | **Not** CIP-119. The CIP's rationale records replacing a single `bio` with the three structured fields |
| `email` | 118 | string | **Not** CIP-119. Contact address, not a link |
| `dRepName` | 100 | string | **Not** CIP-119, and all 100 duplicate `givenName` exactly |
| `title` | 13 | string | Role or affiliation |
| `abstract` | 10 | string | |
| `rationale` | 8 | string | |
| `comment` | 7 | string | |
| `externalUpdates` | 5 | array | Objects carrying a `uri` |
| `url`, `logo`, `name`, `ticker`, `mission`, `description`, `nationality`, `security`, `disclosures`, `hashAlgorithm` | 1 each | mixed | The long tail |

Nineteen distinct terms, most of them invented by whoever wrote the document.
That is the extensibility working as intended, and a wallet that renders only
what it was coded for freezes the vocabulary at its last release: a new
convention cannot be seen, so it is not adopted, so it never becomes a
convention.

## The policy

### Two named blocks, and the naming carries a real distinction

The verified fields sit under **CIP-119 metadata fields**, and everything else
under **Additional / non-canonical metadata fields**.

Naming the first for its standard is not pedantry. Those labels are ours:
"Objectives", "Motivations", "Stated payment address" are Daedalus's words for
terms the standard defines, and they are translated. A field nobody has
standardised has no label we could write, let alone translate, so it can only
be shown under the key its author chose, in whatever language they chose it.

A reader who knows which block they are looking at knows which words came from
the wallet and which came from the DRep. That is the same distinction the
blocks exist to draw, carried in their titles.

### Single values before collections

Within a block, fields holding one value come before any field holding a list.

A reader scanning down meets a bounded set first and an open-ended one after
it. The alternative puts a field that was always going to be there behind as
many entries as the DRep chose to publish: eight references is the mainnet
maximum today and nothing caps it. The payment address in particular is a
fixed, single value and does not belong on the far side of an arbitrary list.

The rule applies within each block, and each block closes before the next
opens. Interleaving them once put the additional heading between the payment
address and the references, which captured a field CIP-119 defines beneath a
heading saying the DRep invented it.

It matters more in the additional block than in the canonical one: the keys
there are unknown in advance, so their order cannot be curated, and grouping by
shape is the only ordering available.

### Show unknown fields, never in the same register as verified ones

Our own chrome is what lends a field credibility. A field named `verifiedBy`
holding "Cardano Foundation", rendered with the same label styling as
`Objectives`, launders a self-assertion into something that looks checked. So
unknown fields appear in a separate, visually subordinate block, described as
additional data the DRep published, showing the literal key rather than a
prettified one.

The digest check means the content is authentically what the DRep published. It
says nothing about whether the content is true.

### No images, and inlined payloads rejected by value rather than by key

Banning `image` and `logo` bans the keys someone thought of. A data URI under
any key at all is the same payload, so the rejection tests the value: anything
beginning `data:` is refused wherever it appears. It carries nothing a reader
can read, and rendering it as text would put a wall of base64 on the page.

### Why no images at all

Neither the linked nor the inlined form is rendered.

A linked image discloses the user's IP address and browser to a host the DRep
chose, at page load, with no click. That is strictly worse than the external
link problem, which at least requires the user to act, and 239 DReps supply one.

An inlined image avoids the disclosure and costs memory instead: the largest
metadata document in the sample is 274,310 bytes, of which 268,625 is a single
base64 JPEG. Rejecting data URIs at the parser rather than declining to render
them keeps that weight out of the cache and out of the IPC payload.

### Links only from `references`

`references` is the field CIP-119 defines for them, it is the field the
renderer applies `isHttpsUrl` to, and it is the field the external-link marker
is attached to. A URL appearing anywhere else, whether in `url`, `logo`,
`externalUpdates` or a field nobody has invented yet, renders as text.

This is deliberate rather than incidental. Every additional route to a
clickable, DRep-controlled destination widens the surface described in the
external-link finding, and a URL in an unrecognised field cannot be presented
with the context a reference gets.

### Collapse aliases, on evidence

`dRepName` duplicates `givenName` in 100 cases out of 100, so it carries no
information and is dropped rather than shown as additional data. Nothing else in
the sample earns a collapse: `bio` repeats a canonical field 5 times in 119, and
`title` once in 13, so both are distinct content.

### A warning about reading `@context`

A document's `@context` is written by its author, so a term mapping to
`CIP119:bio` proves only that the author asserted that mapping. It is not
evidence the CIP defines the term, and here it is actively misleading: `bio`
and `email` both map to `CIP119:` IRIs across the mainnet sample and neither
appears in CIP-119 at all. The `CIPQQQ:` variant seen alongside them is the
tell, a placeholder from a draft that was copied forward.

Check the specification, not the documents. The eight properties listed at the
top of this page come from the CIP's own headings.

Collapse rules are added when the data shows a term is redundant, not because
the name reads like a synonym. `name` and `ticker` appear once each and have not
been observed duplicating anything.

## Bounds: volume once, validity per field

Volume is bounded in one place, at the transport, by `ANCHOR_MAX_BYTES`. That
single cap limits every field, every reference and their sum, and it is the only
limit enforceable without having read the document. Per-field volume limits were
tried and removed: they clamped rather than rejected, so a DRep who wrote at
length was cut off mid-sentence with nothing on screen to say so, and they
clamped at parse time, which destroys the text before the cache or the renderer
sees it and leaves no way to offer the rest later. How much of a long field to
show at once belongs to the view, which can collapse and expand.

What stays per field is validity, meaning a check on whether a value is the
thing it claims to be: `givenName` has a length in CIP-119, a payment address
cannot exceed the length of a bech32 Cardano address, and a URI beyond the
interoperable URL length is unlikely to survive the browser it is handed to.

Unknown fields follow the same split, with one bound of their own. Structure is
kept rather than flattened or dropped: a multi-sig DRep publishing its members,
each with a name and a title, has written something a reader wants, and turning
it into one string would lose which name went with which title. Every leaf is
text that renders as text and nothing in the block becomes clickable, so a
nested list costs nothing a flat one does not.

The bound is depth, not length. The renderer walks the tree recursively, so a
document nesting far enough would exhaust the stack rather than produce a page.
Six levels is far past anything a profile needs, a members list with fields
inside each member reaching three. Past it the reader stops following, and the
shallow fields around it survive.

Keys are rendered literally, so a key shaped like markup is text like any
other, and they are set at the weight of a canonical label. Being in the quieter
block is carried by its boundary and its caption, and is not a reason to make a
DRep's own words hard to read.

## What is already true

- No `dangerouslySetInnerHTML` anywhere in governance
- `isHttpsUrl` is a whitelist and is applied in both the renderer and main
- `tests/jest/security/governance-metadata-injection.spec.tsx` holds both

## Related

- [Cohort selection](./cohort-selection.md)
- [Directory ordering](./directory-ordering.md)
- `daedalus-mgmt#38` for the external link warning this policy assumes is coming
