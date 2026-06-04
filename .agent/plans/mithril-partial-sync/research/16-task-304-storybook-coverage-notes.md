# Task 304 Storybook Coverage Notes

## Summary

- Task `task-304` completed the Storybook review surface for the diagnostics-launched Mithril partial sync flow.
- The work stayed Storybook-scoped, but truthful completion required fixing existing Storybook browser-compatibility problems that blocked the new stories from registering.

## Durable Findings

- `storybook/stories/_support/environment.ts` must not import `source/main/environment.ts`.
  - That main-process module pulls in `child_process`, which breaks Storybook browser execution with `require is not defined`.
  - A Storybook-local `Environment` stub is the safer seam.
- Storybook needs the same browser-oriented transport replacements as the renderer webpack config for Trezor transport modules.
  - Without those replacements, Storybook can bundle native `usb` and `node-gyp-build` paths and fail in the browser before later story groups register.
  - For this repo, the minimal Storybook-safe fix was:
    - `NormalModuleReplacementPlugin` for `@trezor/transport/lib/transports/nodeusb.js`
    - `NormalModuleReplacementPlugin` for `@trezor/transport/lib/transports/udp.js`
    - `resolve.fallback` stubs for `usb` and `node-gyp-build`
- Storybook registration order can hide valid stories behind unrelated import-chain failures.
  - Moving `Nodes` and `Loading` earlier in `storybook/stories/index.ts` made the new diagnostics and loading stories visible sooner during debugging.
  - Keep existing story registration intact after debugging; do not permanently unregister unrelated legacy stories just to get one new story passing.
- Storybook fixtures that need locale-sensitive copy must not precompute translated strings at module-load time.
  - The `MithrilPartialSyncOverlay` active-progress labels initially stayed English in Japanese because the fixture resolved messages once under the default locale.
  - Generating those labels from the live `intl` context at render time fixed the issue.

## Outcome

- Diagnostics Storybook coverage now includes:
  - `Partial Sync CTA Ready`
  - `Partial Sync CTA Blocked`
  - `Partial Sync Confirmation`
- Partial-sync overlay Storybook coverage now includes:
  - `Active Progress`
  - `Cancelled`
  - `Failed With Restart Allowed`
  - `Failed With Wipe-Only Recovery`
  - `Completed`
- Manual Storybook validation passed under theme switching and `en-US` / `ja-JP` locale switching after the render-time i18n fix.

## Notes For Later Tasks

- If later tasks add more Storybook coverage around loading, hardware wallets, or diagnostics, reuse the Storybook browser-compatibility seams from `task-304` instead of importing main-process modules or relying on native transports in the preview bundle.
- No new runtime i18n catalog entries or theme tokens were needed for `task-304`; the task confirmed existing runtime copy and theme variables were sufficient once Storybook fixtures were made truthful.
