Planner: Iteration 1
Timestamp: 2026-09-16T09:05:00Z

Drafted from the task's entry in the graph, read against `stakingConfig.ts`,
`launcher-config.nix`, `config.ts`, `preload.ts` and `declaration.d.ts` rather
than against the PRD's description of them.

Critique of Iteration 1:

1. *The first draft copied the `@ts-ignore` along with the structure.*
   `SMASH_SERVERS_LIST` is typed `Record<SmashServerType, ...>` and holds two of
   the four members, so it carries a `ts-migrate(2739)` suppression, and it reads
   `global.smashUrl` behind a second one. Both are avoidable rather than
   inherent: `Partial<Record<...>>` says what is true about a list that holds a
   subset, and declaring `koiosUrl` in `declaration.d.ts` removes the need for
   the other. Six phases of this branch have added no suppressions and this task
   is not the one to start.

2. *"The default URL is present on global for every network the launcher
   configures" cannot be met as written.* Koios runs three public instances.
   There is no `shelley_qa` or `vasil_dev` instance and there cannot be a
   selfnode one, because a selfnode chain exists only on the machine that made
   it. The criterion is restated as the set that can hold, with the reason, and
   `mainnet_flight` is added because it is a mainnet client that `smashServers`
   silently omits.

3. *Nothing said how a spec reaches a value read from `global` at module load.*
   `jest.config.js` sets one global and it is not this one, so the finding and
   the approach both say it: the spec sets the global before the module loads.
   Left unsaid, the first spec written would have asserted `undefined`.

4. *The validator's rejection cases were one line.* An `http://` URL and a query
   string are two different reasons to reject, and a pattern that admitted paths
   too liberally would pass the first and fail the second. They are separate
   criteria and separate cases.

Changes made in response: `Partial<Record<...>>` and the `declaration.d.ts`
line, criterion 3 restated with its reason, finding 10, and the split cases.

Scope guard: no probe, no persistence, no settings surface, no client, and no
edit to the SMASH block.

Outcome: approved
