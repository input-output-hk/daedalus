# Shared test environment packaging implementation plan

Specification: [spec.md](spec.md). Tasks:
[shared-test-environment-packaging-tasks.json](shared-test-environment-packaging-tasks.json).
Implementation authorized on 2026-10-29 in the existing worktree, after rebasing
onto the repository publisher on master. Preserve the publisher worktree's
unrelated newsfeed changes; do release-cli work there if later stages require it.

## Product and runtime contract

Define two installer products independently of runtime cluster/chain:

| Product key | Profiles | Native Linux package | Shared backend/application build |
|-------------|----------|----------------------|----------------------------------|
| `mainnet` | `mainnet` | existing `daedalus-mainnet` | existing Mainnet |
| `pilot` | `preview`, `preprod`, `mainnet-flight` | `daedalus-pilot` | one CIP-30/Flight-compatible payload |

The technical product key and native package name are stable packaging identities;
visible profile names stay Preview, Pre-Prod and Flight. This plan chooses
`daedalus-pilot` for the combined native product. Flight remains visibly mainnet
with real ADA. Backend source compatibility is checked across the three profiles
before sharing their executable payload.

Add a small pure Nix product contract under `packaging/` and expose it through the
existing common build inputs. Generate a product configuration bundle containing
one manifest and `profiles/<cluster>/` configuration directories. This bundle
contains configuration only; it must not duplicate Electron or backend payloads.
All profiles retain existing data namespaces, sockets, credentials and services.

The watchdog already injects selected cluster, chain name, Flight status and
configuration path into Electron, and the preload already exports the main
environment object to the renderer. Shortcuts choose an explicit watchdog
configuration; that configuration supplies the network. The only production
TypeScript network-selection change is to read existing `DAEDALUS_NETWORK_NAME` before baked
`NETWORK`, preserving the existing development/legacy fallback. Do not add a
TypeScript product/profile selector or a new environment protocol. Sam explicitly
confirmed this minimal approach during implementation. Build-time product
membership and configuration-path validation belong to packaging and launcher
checks; missing configuration continues to fail through the existing watchdog.

Sam separately approved adapting Linux sandbox package-path validation to the
shared Pilot root and its three explicit configuration paths. That validation
requires the Pilot package manifest while retaining ownership, realpath, helper
hash and supported-host checks. It does not select a network.

## First implementation slice

T001-T012 establish the product/profile contract, runtime network resolution and
configuration bundles, with focused tests. They do not represent finished
installer artifacts. Existing per-environment outputs remain until each new
platform installer and migration path pass its acceptance checks. This allows
review of the environment-selection foundation before the larger packaging work.

## Shared payload and installed layout

Linux installs one root `/opt/daedalus/pilot` with one Electron/backend/application
tree and profile configuration under `config/profiles/<cluster>`. Three launchers
select explicit configurations and retain the old per-profile state roots.
Package ownership, AppArmor/SELinux policy, trusted manifest and runtime sandbox
validation must agree on product root versus selected profile; do not bypass the
existing trust checks by claiming the shared root is an old individual install.
Refactor existing DEB/RPM/Arch builders to consume product descriptors rather
than copying their implementations. Mainnet's installed paths stay unchanged.

Windows uses one shared pilot installation directory and three Start Menu
shortcuts selecting profile configs. Add explicit product ownership and check all
three profiles for running processes before upgrade/removal. Change taskbar app
identity per selected profile; installation root and updater ownership are shared.

macOS installs one shared payload in a stable product-owned root with three small
launcher apps and distinct bundle IDs. An early native prototype must demonstrate
separate running Electron Dock identity and Dock relaunch into the same profile
before integrating the final signed `.pkg`. Exact launcher/shared-payload path
mechanics remain conditional on that experiment. macOS signing, notarization and
installed Finder/Dock checks require a macOS host; Linux-only tests cannot certify
them. Do not replace the agreed three-launcher UX or duplicate the large payload.

## Updates, migration and lifecycle

Windows/macOS test slice: Windows packages one Pilot executable/resources/backend
tree and three tiny frontend metadata/bootstrap directories. Watchdog configs
choose the frontend directory and original Electron settings path; the packaging
bootstrap accepts only those three installed directories, applies their literal
app identity and loads the shared application. No TypeScript network selector is
added. Windows package-identity validation must recognize only this explicit
layout. The initial installer rejects legacy installs and uses product ownership
and a fail-closed process check; installed upgrade/race/rollback certification
remains required. Prototype self-updates stay disabled until shared release routing.

macOS first exposes a prototype under `/Applications/Daedalus Pilot`, containing
three small named `.app` shells and one sibling `Shared` payload. Each shell owns
its Info.plist, launcher, small Electron entry executable, config and package.json.
Each shell also owns small helper app bundles named to match its CFBundleName,
with profile-specific bundle IDs and real executables inside its Frameworks
directory. Electron resolves helper names and parent app identity through those
paths; the complete Frameworks directory must not point into Flight's payload.
Large framework binaries, backends and application code point to the shared tree.
Keep the existing executable/config locations inside each shell so direct-launch
recovery and package identity validation retain their existing contracts. CI builds
unsigned installers only; signing and notarization happen at release time.
Demonstrate distinct running Dock identity and relaunch before integrating
production signing/notarization into the release process. CI must supply
the native macOS build/test evidence; Linux layout fixtures cannot certify it.

Consent regression found during host testing: chain/wallet directories alone do
not preserve Electron preferences. Every Pilot config now supplies an explicit
`--user-data-dir` pointing to that profile's existing application settings path;
Linux launchers export XDG_CONFIG_HOME with the normal HOME/.config fallback.
Configuration/launcher checks passed and Sam confirmed the fix on his host.

Pilot owns one version and updater; each included profile's release metadata
points to the same pilot artifact. Mainnet retains its existing channel. Linux
remains system-package-managed. Update-runner success requires every included
environment to stop before shared files are replaced. Direct executable launch
recovery must preserve an explicit selected profile or explain how to repair the
installation; it cannot silently choose Preview or Mainnet.

For migration, exercise every subset of old Preview/Preprod/Flight products.
Refactor package-owned paths and declare conflicts/replacements only when native
package-manager tests prove ownership safe. Document any required uninstall step
before installation. Preserve profile data and customized chain paths, and verify
that removal of a superseded package cannot remove pilot's shared files.

## Validation and constraints

Use existing Nix shells and pinned dependencies. Rust remains the preference for
maintained tooling; no new Python automation is needed. The existing Spec Kit
artifacts are reused without allocating a numbered branch or scaffolding a second
spec system. No `.specify` constitution/hooks are present in this project.

Validate the pure product contract/config layout with Nix evaluations and checks;
validate existing runtime network handoff and build/development fallbacks with Jest.
Run the relevant TypeScript checks, formatting and native package tests when
those implementations change. Preserve the already tested publisher and rerun
its tests after the master rebase. A release-shell blocker from MinIO's insecure
package marking is recorded separately; do not bypass Nix security policy.

The publisher rebase retained master's modern S3 TLS feature flags. Sam approved
removing unused `minio`/`minio-client` shell dependencies; that fix is committed as
`b8c741c49` and this worktree is stacked on it. Publisher validation on master:
42 tests pass, one existing privileged test ignored, Clippy and treefmt pass.

Later acceptance must inspect built artifacts for duplicate large files, install
and launch all three profiles together with Mainnet, exercise upgrade/removal and
interruption, and record exact artifact/host evidence. Config-only outputs,
unit tests and a specification review are not installer certification.

## First-slice evidence — 2026-10-29

The configuration-only `pilot-profile-configs` output builds successfully, with
one product manifest and configurations for Preview, Preprod and Mainnet Flight.
The Nix check verifies identities, Flight's mainnet chain, unchanged legacy state
directories/sockets, profile-specific node paths and standalone Mainnet's original
configuration path. Backend sharing requires one matching wallet source revision.

The first-slice production TypeScript diff was solely the `DAEDALUS_NETWORK_NAME || NETWORK`
fallback in `source/main/environment.ts`. No new TypeScript selector, product
protocol or profile validator remains. Seven focused environment tests pass.
The complete Jest run passes 219 suites (2,892 passed tests, three skipped,
six passed snapshots); TypeScript compilation and changed-file formatting pass.
All checks ran through project Nix outputs using existing cached dependencies.

That foundation was configuration-only. The Linux implementation below extends
it; subsequent implementation is maintained as a local installer test slice.

## Linux implementation — 2026-10-29

Priority update: deliver a shared NixOS Pilot package before further VM/platform
work, so Sam can exercise the product directly. Expose
`nixos-package-pilot -- <preview|preprod|mainnet-flight>`, plus three installed
desktop launchers. Reuse the native launcher generator with store-root paths,
one shared Electron/application/backend payload and profile configurations.
Preserve existing standalone NixOS outputs and profile state paths. Add Pilot's
activation helper and extend the existing NixOS sandbox package-path validation
to its explicit profile configurations and shared manifest/helper. Keep Electron
inside the package root to satisfy the existing executable identity check.
Build the actual package, exercise generated launchers and reject missing/invalid
profiles; run sandbox regression tests, compile and formatting before handoff.
Direct host commands are in [nixos-testing.md](nixos-testing.md).

The existing DEB, RPM and Arch builders now expose `*-installer-pilot` outputs
with one Flight-compatible payload, three configuration trees and three named
desktop shortcuts. Mainnet retains its existing outputs and paths. The launchers
select explicit watchdog configurations, preserve each profile's state namespace
and set distinct icons/window classes. The generic Pilot entrypoint requires an
explicit profile; it never defaults to a network.

Pilot conflicts with the three legacy native packages because it owns their
shortcut paths. Migration currently requires uninstalling those packages before
installing Pilot; user data stays outside the package root. Automatic replacement
and installed migration are not yet certified.

Package maintenance invokes a shared process guard covering all executables
under the Pilot root, including backend processes. This guard does not establish
a race-free lifecycle lock; installed upgrade/removal, concurrent launch, rollback
and interrupted-maintenance acceptance remain T021.

Focused Nix checks exercise the generated launchers with state paths containing
spaces, explicit selection and a live shared executable. Native fixture archives
are built through the actual DEB/RPM/Arch builders and inspected for shared layout,
conflicts, shortcut destinations, ELF interpreter and maintenance hooks. The same
archive inspection is exposed as `pilot-linux-artifacts` for full-payload builds
(fast DEB/Arch compression, production RPM). Fixture success alone is not an
installed-platform result.

All three full-payload builds and `pilot-linux-artifacts` passed. The native
archives are approximately 561 MB (fast DEB), 351 MB (production RPM) and 562 MB
(fast Arch). These compression modes differ; their sizes are not a cross-format
efficiency comparison. Exact paths and hashes are recorded in
[linux-acceptance.md](linux-acceptance.md).

Pilot VM convenience apps now exist for all three formats. They autostart
Preview through its explicit launcher and mount the three-profile state root
from a disposable per-VM cache directory. This session has no accessible KVM
device, so installed acceptance has not run.

TypeScript compilation and all 219 Jest suites pass after the separately
approved sandbox path update: 2,909 tests pass, three skipped, six snapshots pass.
Windows/macOS packaging, updater routing and installed-platform evidence remain
pending. No installer has been signed, deployed or published.
