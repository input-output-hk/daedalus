Implementation: Iteration 1
Timestamp: 2026-05-19T18:36:53Z

Changes made:
- Added dedicated partial-sync IPC channel contracts in `source/common/ipc/api.ts` for start, status, cancel, restart-normal, and wipe-and-full-sync.
- Added `source/main/ipc/mithrilPartialSyncChannel.ts` with cached idle partial-sync status, best-effort status broadcast, idempotent window rebinding, and explicit reject-until-implemented action handlers.
- Registered the new main handler in `source/main/ipc/index.ts`.
- Added thin renderer wrappers in `source/renderer/app/ipc/mithrilPartialSyncChannel.ts`.
- Added focused wrapper tests in `source/main/ipc/mithrilPartialSyncChannel.spec.ts`.
- Updated `.agent/system/api-endpoints.md` to document the Mithril bootstrap and partial-sync IPC inventory.

Files touched:
- `source/common/ipc/api.ts`
- `source/main/ipc/mithrilPartialSyncChannel.ts`
- `source/main/ipc/index.ts`
- `source/renderer/app/ipc/mithrilPartialSyncChannel.ts`
- `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
- `.agent/system/api-endpoints.md`

Verification run:
- `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts` ✅
- `yarn tsc --noEmit source/main/ipc/mithrilPartialSyncChannel.ts source/renderer/app/ipc/mithrilPartialSyncChannel.ts source/common/ipc/api.ts source/common/types/mithril-partial-sync.types.ts` ⚠️ not a truthful project compile path here; it surfaced broad repo/config noise unrelated to task-101, including decorator and module-resolution errors in existing renderer files.

Deviations from approved plan:
- No functional deviation. The implementation stayed at the wrapper seam and did not add coordinator wiring, node-start suppression helpers, or bootstrap-side setup effects.

User interaction required:
- No

Outcome: Implementation ready for code review

Code Review: Iteration 1
Timestamp: 2026-05-19T18:38:56Z

Blocking findings:
None.

Non-blocking observations:
1. `source/main/ipc/mithrilPartialSyncChannel.ts:86-91` exports `setMithrilPartialSyncStatus`, which is a broader write surface than task-101 needed and bypasses the listener / renderer fanout path in `emitMithrilPartialSyncStatus`. Nothing in this diff misuses it, so this is not a correctness blocker, but it does make later task-102/task-200 integration easier to get subtly wrong.
2. `.agent/system/api-endpoints.md:382-393` is updated correctly for the new channels, but `.agent/system/architecture.md:228,260-264` still describes Mithril IPC only in bootstrap terms. I would not block task-101 on that since the documented workflow specifically requires the API inventory update, but it is mild doc drift to keep in mind once partial sync becomes wired into live orchestration.

Approval bar:
1. No blocking changes required for `task-101`.
2. Optional cleanup: remove or narrow `setMithrilPartialSyncStatus` unless a cache-only mutation seam is intentionally needed.

Decision: approved

