# Shared test environment packaging brief

Requested on 2026-10-29 by Sam Leathers.

Keep standalone Mainnet builds as they are. Replace separate Preview, Preprod
and Mainnet Flight installers with one combined package per supported platform
and architecture. Install the large binaries and application resources once,
include configuration for all three environments, and provide a dedicated
shortcut for each environment.

For macOS, the agreed direction is three small launcher `.app` bundles backed
by one shared payload installed by a `.pkg`. Separate launchers must not require
three copies of Electron or the backend executables. macOS application identity,
signing and lifecycle behavior must be validated rather than assumed.

Sam confirmed that persistent data is already namespaced. Preserve and reuse
that behavior; designing another data storage scheme is outside this feature.

The initial request authorized a specification only. On 2026-10-29 Sam then
authorized rebasing the repository publisher onto master, rebasing this worktree
onto that publisher branch, and starting implementation of the combined installers.
Local planning, implementation and validation are now in scope. Release
publication, signing with production credentials and deployment are not requested.

See [spec.md](spec.md) for requirements and acceptance criteria, and
[checklists/requirements.md](checklists/requirements.md) for specification review.
The implementation plan and task graph are in
[shared-test-environment-packaging-prd.md](shared-test-environment-packaging-prd.md)
and [shared-test-environment-packaging-tasks.json](shared-test-environment-packaging-tasks.json).
