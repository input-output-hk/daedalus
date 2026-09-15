Planner: Iteration 1
Timestamp: 2026-09-16T10:15:00Z

Drafted from the task's entry in the graph, read against `checkSmashServerHealth`,
`api/utils/request.ts`, `StakingStore` and `AssetsStore` rather than against the
PRD's description of them.

Critique of Iteration 1:

1. *The first draft said "the same six lines against GET /tip", because the task
   graph and the PRD both say so.* Reading the six lines found they are not a
   probe: `checkSmashServerHealth` passes the candidate URL to **cardano-wallet**
   as a query parameter and asks cardano-wallet to check it. Copying its shape
   would have produced a request to cardano-wallet for an endpoint it does not
   serve, and the failure would have presented as an unreachable Koios instance.
   Findings 1 and 2, and the approach rewritten around them.

2. *The second draft reused `request` from `api/utils/request.ts`.* That module
   reads `isSelfnode` at load and sends over plain HTTP when it is true, and it
   attaches an agent carrying cardano-wallet's client certificate. Sending a
   user-named third-party URL either of those is wrong. The probe is its own
   `global.https.request`, which the validator's `https`-only rule makes safe to
   hard-code.

3. *The acceptance check was a boolean, mirroring `checkSmashServerIsValid`.*
   There are two refusals here and they mean different things: the URL is not an
   instance, and the instance is behind. A boolean forces one message for both.
   Three-valued, two error codes, and the store picks.

4. *Nothing said what happens before the first network status arrives.*
   `localTip` is null then, and a rule that fails closed would make the settings
   page unusable for the length of a first sync. Stated, and given its own
   criterion.

5. *The endpoint-ordering question was going to be answered in a sentence.* It
   is a PRD-level contradiction and it gets its own section with the ordering
   that stands, in a table, plus what happens to the existing test-only override.

6. *A criterion asserted the `direct` option is accepted without saying how that
   is checked.* Returning true is not evidence that no request was issued.
   Criterion 2 now says the assertion is on the call count.

Changes made in response: findings 1 and 2, the three-valued result, the
null-tip rule and its criterion, the settled-ordering section, and criterion 2.

Scope guard: no settings surface, no client, no change to the registry endpoint,
and no second reduction over the preset list.

Outcome: approved
