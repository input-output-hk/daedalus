# Implementation Plan

Use the existing Rust watchdog pipeline, typed command bridge, MobX backend store and Mithril error view. There is no project Spec Kit constitution or initialized .specify directory; use the skill's portable stage structure and preserve repository conventions.

On conversion failure keep validated staging and await a recovery command in the pipeline. A dedicated replay command is valid only during this wait. Retry cleans staging before restarting the pipeline; cancel and stop use existing outcomes. Explicit node continuation gets a distinct outcome so the supervisor honors it even after a rebuild.

Replay removes staged ledger/LSM/volatile/clean state and uses the same network recheck, cutover record and installation logic. Partial installation merges immutable files as before, replaces the ledger with an empty directory, removes stale LSM and volatile data, and omits the clean sentinel to force node validation. All replay cleanup errors are fatal before node startup. Normal conversion remains unchanged.

Add localized explanatory copy and a primary replay action for PARTIAL_SYNC_CONVERSION_FAILED. Validate Rust integration behavior across bootstrap, partial sync and rebuild; renderer action and state/command handling; format and compile touched code through project Nix shells.
