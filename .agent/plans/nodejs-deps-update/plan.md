# Implementation plan

Use the project's Nix dev shell and Yarn 1 lockfile. Preserve completed work in
`.agent/plans/dependency-hygiene/`. No Spec Kit constitution or `.specify` scaffold
exists in this repository; use its established `.agent/plans/` destination and
these specification, plan, and task artifacts.

1. Capture the full live alert baseline, including ecosystem and manifest, and
   inspect available registry versions before choosing changes. Separate Rust
   manifests from the Node.js graph without dropping them from the disposition.
2. Resolve compatible package updates in dependency-owner groups: runtime crypto
   and hardware transport, Electron and its platform sources, application utilities,
   then build/test/Storybook tools. Validate each consequential group before moving
   on. Avoid forcing unrelated package majors through global resolutions.
3. Install first-party window security handlers during main-window construction.
   Deny in-app popup creation, constrain external URL schemes, and block unexpected
   main/frame navigation. Add behavioral regression coverage.
4. Render remote Markdown without raw HTML, and test malicious markup against the
   actual renderer. Tighten CSP where supported without breaking WASM or development.
   Inventory Node/preload dependencies before isolation migration.
5. Reconcile dependency declarations, lockfile, Nix sources and runtime-copy lists.
   Validate type checking, Jest, production builds, formatting, audit delta, and
   packaging contracts. Record manual hardware and platform validation boundaries.

Risks: major upgrades to legacy Storybook/React tooling, native-module ABI changes,
hardware compatibility, unpublished patch versions, and advisories without fixes.
These require concrete evidence and explicit remaining-work records.
