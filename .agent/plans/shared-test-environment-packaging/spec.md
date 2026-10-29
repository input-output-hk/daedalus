# Feature Specification: Shared test environment packaging

**Feature Branch**: `shared-test-environment-packaging`
**Created**: 2026-10-29
**Status**: Runtime/configuration foundation implemented; native installers pending
**Base**: `sl/linux-repo-publishing` at `b8c741c49`, rebased onto local `master` at `46c131739`
**Input**: [User request and agreed scope](prompt.md)

## Outcome and scope

Distribute Preview, Preprod and Mainnet Flight together, with one installed
application payload and three independently launchable environments. Keep the
standalone Mainnet product's behavior, backend selection, configuration,
installation identity and release channel unchanged.

The combined distribution contains exactly these environment identities:

| Launcher | Cluster | Chain | Persistent data |
|----------|---------|-------|-----------------|
| Daedalus Preview | `preview` | Preview | Existing Preview namespace |
| Daedalus Pre-Prod | `preprod` | Preprod | Existing Preprod namespace |
| Daedalus Flight | `mainnet-flight` | Mainnet, real ADA | Existing Flight namespace |

"Combined package" means one user-installed product for each supported OS and
architecture, delivered through the platform's existing installer/package format.
Linux retains its applicable `.deb`, `.rpm` and Arch package formats; Windows retains its
installer; macOS retains `.pkg` installation and its currently supported CPU
architectures. This feature does not add platform or architecture support.

Sam prioritized the existing NixOS run workflow for direct host testing before
further VM/platform work: provide one shared Pilot Nix package, three desktop
launchers and explicit command-line profile selection. Preserve existing
standalone NixOS outputs and sandbox activation. The shared NixOS package must
use the same profile configurations and state namespaces as native Pilot packages.

Use the working product name **Daedalus Test Environments** in this specification.
Final marketing wording does not change membership or environment identities.
Flight must remain clearly identifiable as using mainnet and real ADA.

## User Scenarios & Testing

### User Story 1 — Install once and launch a chosen environment (Priority: P1)

As a tester, I install one product and can immediately launch Preview, Preprod
or Flight through a dedicated, recognizable launcher.

**Why this priority**: This is the feature's primary installation experience.

**Independent Test**: On a clean supported host, install the combined artifact
and launch each environment using its installed shortcut without a terminal.

**Acceptance Scenarios**:

1. **Given** a clean supported host, **when** installation completes, **then** all
   three environment configurations and launchers are present and use the same
   installed application/backend payload without downloading another copy.
2. **Given** any of the three launchers, **when** it is activated, **then** the
   selected environment starts with its matching chain configuration, branding,
   services and existing data namespace, without an environment chooser.
3. **Given** the Flight launcher or running window, **when** a user identifies
   its environment, **then** its mainnet identity is unambiguous and it is not
   represented as a testnet with test ADA.
4. **Given** a missing or invalid selected configuration, **when** launch is
   attempted, **then** startup reports the problem without selecting another
   environment or opening its databases.

### User Story 2 — Keep environments independent (Priority: P1)

As a tester, I can run different environments together and retain my existing
wallets, preferences and chain data after adopting the combined package.

**Why this priority**: Packaging consolidation must preserve existing isolation
and must not require wallet restoration or a fresh chain download.

**Independent Test**: Reuse existing environment data, start all three
environments and standalone Mainnet on a suitably resourced host, then activate
each launcher again and restart one environment.

**Acceptance Scenarios**:

1. **Given** existing Preview, Preprod and Flight state, **when** each combined
   launcher starts, **then** it reuses that environment's existing state and any
   configured chain location without copying, moving or merging data.
2. **Given** one environment is running, **when** another is launched, **then**
   both run with their own state, credentials, sockets, endpoints and logs.
3. **Given** an environment is running or starting, **when** its launcher is
   activated again, **then** the existing instance is activated using the
   existing single-instance behavior rather than starting duplicate backends.
4. **Given** multiple environments are running, **when** one exits or restarts,
   **then** the others continue and the restarted environment retains its identity.
5. **Given** standalone Mainnet and Flight are installed, **when** either starts,
   **then** they retain distinct product and state identities despite using the
   same chain.

### User Story 3 — Launch each environment naturally on macOS (Priority: P1)

As a macOS tester, I have three small environment launcher apps and can identify
and return to the correct running environment through Finder and the Dock.

**Why this priority**: Three shortcuts are insufficient if macOS activates the
wrong environment or collapses all launches into one application instance.

**Independent Test**: Install the signed and notarized `.pkg` on supported Intel
and Apple Silicon hosts; exercise Finder launch, Dock activation and relaunch
with multiple environments running.

**Acceptance Scenarios**:

1. **Given** the installed package, **when** its apps are viewed in Finder,
   **then** Preview, Preprod and Flight each have a distinct name, icon and stable
   bundle identifier, backed by one shared payload per installed architecture.
2. **Given** two or more environments are running, **when** a user selects or pins
   their Dock entries, **then** each entry identifies and activates its matching
   environment; relaunch after exit starts that same environment.
3. **Given** a freshly installed signed distribution, **when** any launcher starts,
   **then** normal Gatekeeper assessment succeeds without manual bypasses or
   modifying the installed signed payload.
4. **Given** a deleted launcher, **when** the combined product is repaired or
   reinstalled, **then** the launcher is restored without resetting environment
   data or creating another shared payload.

### User Story 4 — Upgrade and remove one combined product (Priority: P2)

As a tester, I maintain one installed version of the shared product without
losing data or disrupting standalone Mainnet.

**Why this priority**: Shared binaries need a shared lifecycle to avoid partial
updates and stale launchers.

**Independent Test**: Install over representative existing individual products,
upgrade the combined product, repair it and remove it using supported platform
procedures; verify data and standalone Mainnet after each operation.

**Acceptance Scenarios**:

1. **Given** any subset of the old individual products, **when** the user adopts
   the combined product, **then** documented platform migration removes or
   replaces superseded package-owned payloads and launchers without touching
   environment data, custom chain storage or standalone Mainnet.
2. **Given** a combined installation, **when** it is upgraded, **then** all three
   launchers and configurations belong to the same installed payload version.
3. **Given** any included environment is running, **when** an upgrade or removal
   is attempted, **then** the supported procedure requires the included instances
   to stop before replacing or removing their shared executable files.
4. **Given** removal of the combined product, **when** removal completes, **then**
   its launchers and package-owned shared files are removed and all user data
   and the separate Mainnet installation remain available.
5. **Given** an interrupted or failed update, **when** the user follows the
   platform recovery procedure, **then** repair restores one coherent version
   without restoring wallets from recovery phrases.

## Requirements

### Functional requirements

- **FR-001**: The combined distribution MUST include `preview`, `preprod` and
  `mainnet-flight`, and MUST exclude the standalone `mainnet` product.
- **FR-002**: Each installed combined product MUST contain one shared copy of
  Electron, the application code/resources and each common backend executable,
  including node, wallet, watchdog and supporting tools. Per-environment copies,
  hard links or filesystem compression MUST NOT substitute for a shared payload
  design. Distinct architecture slices and components required internally by
  Electron are not environment duplication.
- **FR-003**: The installed payload MUST include a complete configuration set
  for every included environment. Tiny launchers, icons, metadata and distinct
  environment configuration may be separate; large executables/resources may not.
- **FR-004**: Each environment MUST have a dedicated platform-native launcher.
  Linux uses desktop entries; Windows uses Start Menu shortcuts and the existing
  desktop-shortcut policy; macOS uses three small launcher `.app` bundles.
- **FR-005**: The selected environment MUST be established before application
  environment initialization, instance acquisition and backend startup. Main and
  renderer MUST agree on chain, cluster, Flight status and service configuration;
  launch MUST NOT retain another environment's build-time network assumptions.
- **FR-006**: Existing namespaced state and user-selected chain locations MUST
  be reused unchanged. Storage redesign, chain-data sharing between Flight and
  Mainnet, wallet migration and namespace renaming are outside scope.
- **FR-007**: Different included environments MUST run concurrently, subject to
  host resources; repeated launch of one environment MUST activate only its own
  instance. Existing isolation of local credentials, databases and process
  lifecycle MUST be preserved, including coexistence with standalone Mainnet.
- **FR-008**: Visible environment branding and OS activation identity MUST remain
  distinct, including Linux desktop/window association, Windows taskbar
  association and macOS Finder/Dock behavior. Flight MUST retain mainnet labeling.
- **FR-009**: The combined product MUST own one payload version and upgrade/removal
  lifecycle. Release metadata and update routing for each included environment
  MUST resolve to the combined product; legacy clients MUST have a documented
  transition that does not invoke an incompatible old installer/update path.
- **FR-010**: Linux MUST retain system-package-managed installation and updates.
  The application MUST NOT reintroduce portable installers, home extraction or
  privileged application-managed updates as part of consolidation.
- **FR-011**: Upgrades and removals MUST account for all included running
  environments, preserve user data, and avoid mixed-version executable/config
  sets being launched. Recovery MUST use supported repair/reinstallation paths.
- **FR-012**: Migration MUST cover any subset of prior individual installations,
  including mixed versions, package ownership conflicts and stale shortcuts.
  Any manual uninstall requirement MUST be documented before installation.
- **FR-013**: Standalone Mainnet MUST retain its existing package identity,
  backend selection, data paths, launchers, update channel and user behavior.
  Sharing implementation code MUST NOT enroll it in the combined lifecycle.
- **FR-014**: Shared installation paths and configurations MUST be reflected in
  package-identity and sandbox validation without weakening their trust checks.
  Existing dApp feature gates and host-support boundaries MUST be preserved.
- **FR-015**: macOS MUST distribute signed launchers and shared executable code
  through one notarized `.pkg` per supported architecture. Runtime selection
  MUST NOT rewrite signed bundles or require user signing/security bypasses.
- **FR-016**: Release production MUST emit one combined artifact per existing
  supported OS/format/architecture target, with corresponding integrity and
  version metadata. Separate per-environment installers cease to be new-release
  outputs after the documented transition. Mainnet outputs remain separate.
- **FR-017**: Missing configuration, missing shared payload or an unsupported
  selection MUST fail with actionable repair guidance, without silently falling
  back to Mainnet, Flight or another environment.

### Key entities

- **Combined product**: Installed package identity, version and owned shared files.
- **Shared payload**: Application and backend executables/resources used by all
  three environments, independently of their selected state namespace.
- **Environment profile**: Cluster/chain identity, configuration, services,
  branding and references to the existing environment's state locations.
- **Environment launcher**: Native OS launch identity bound to exactly one profile.
- **Release artifact**: Platform/architecture-specific combined installer and its
  integrity/version metadata, distinct from standalone Mainnet artifacts.

## Edge cases

- Launches overlap during initial startup, shutdown, restart or package maintenance.
- An old individual installation is still running when adoption is attempted.
- Old and combined shortcuts coexist temporarily; they must not open the same
  state with two backend instances or accidentally select different chains.
- Payload/configuration is missing or partially replaced; repair must not create
  fresh wallet state as a substitute for the existing namespace.
- Paths contain spaces/non-ASCII characters, or existing data/chain paths are
  customized or symlinked under currently supported behavior.
- Multiple OS users share the installed payload but retain separate user state.
- On macOS a launcher is moved or deleted, or its Dock pin survives an upgrade;
  supported relocation boundaries and repair behavior must be documented.
- Flight and standalone Mainnet run together, receive different release updates,
  and retain distinct namespaces despite identical mainnet network magic.
- A user removes an old individual package after adoption; it must not own or
  remove the combined product's shared files.
- A host meets wallet requirements but does not meet existing dApp sandbox
  requirements; package consolidation must preserve that distinction.

## Success Criteria

- **SC-001**: Each supported installed-artifact target provides exactly three
  working environment launchers from one combined installation.
- **SC-002**: Artifact inspection demonstrates zero per-environment duplicates
  of the common large payload. Record compressed and installed sizes against
  the sum of the three equivalent individual packages; do not count user data.
- **SC-003**: On a suitably resourced supported host, all three environments and
  standalone Mainnet can coexist, relaunch independently and activate correctly
  without duplicate same-environment watchdog/backend instances.
- **SC-004**: Existing data fixtures for all three environments are reused at
  their original locations; adoption, repair and removal preserve wallet state,
  chain paths and preferences without a packaging-induced resync or restoration.
- **SC-005**: Signed macOS artifacts pass installed Finder/Dock, signature and
  Gatekeeper acceptance on supported Intel and Apple Silicon targets. Test
  results explicitly distinguish launcher identity from running Electron identity.
- **SC-006**: Platform upgrade/removal tests cover each environment running, all
  stopped, partial prior installations and interruption/recovery; no supported
  path launches mixed-version files or removes user data or standalone Mainnet.
- **SC-007**: Mainnet packaging regression checks preserve its identity, backend
  selection, configuration, state paths and release routing. A representative
  installed Mainnet launch continues to work alongside the combined product.
- **SC-008**: Relevant automated configuration, launch, update-routing and package
  contracts pass through Nix; installed-platform checks validate changed sandbox
  policies/identity and lifecycle behavior. Evidence records exact artifacts,
  versions, architectures and hosts. Specification review is not such evidence.

## Assumptions and next-stage decisions

Existing data namespacing is established behavior, confirmed by the user. This
feature preserves it and adds regression evidence rather than inventing storage
work. The current shared backend choice for these three clusters is retained;
choosing unrelated backend versions per environment is outside scope.

The [implementation plan](shared-test-environment-packaging-prd.md) and task graph
resolve these decisions before their affected production implementation:

1. Exact shared installation roots, package IDs and ownership rules, using the
   existing platform packaging and release tooling rather than a parallel system.
2. macOS shared-payload placement and launcher mechanism, with an early installed,
   signed proof of separate running application identities and Dock relaunch.
   Three tiny launcher bundles alone do not establish that this works. Changing
   to a single chooser or duplicating Electron would change the agreed scope.
3. How runtime profile selection reaches both processes while preserving trusted
   package identity, existing direct-launch recovery and development behavior.
4. Upgrade routing and transition from individual products, including required
   release-tooling work outside this checkout and manual Linux package steps.
5. Supported macOS relocation/removal procedure and update quiescence across all
   profiles. Exact filesystem layouts belong in that plan, not this specification.

These are planning/validation decisions within the stated behavior, not claims
that the feature or a macOS prototype has already been implemented.

## Current source references

Verified against the base checkout; these identify planning inputs, not a final
implementation layout:

- [`perSystem/common.nix`](../../../perSystem/common.nix): all three included
  clusters select the CIP-30 wallet backend; generated watchdog configurations
  already supply cluster, network name, Flight flag and state paths.
- [`source/main/webpack.config.js`](../../../source/main/webpack.config.js) and
  [`source/renderer/webpack.config.js`](../../../source/renderer/webpack.config.js):
  `NETWORK` is currently injected at build time.
- [`watchdog/src/instance.rs`](../../../watchdog/src/instance.rs): existing
  state-directory instance locking and activation.
- [`flake/lib/builders/windows.nix`](../../../flake/lib/builders/windows.nix):
  NSIS shortcuts already pass the watchdog an explicit configuration path.
- [`packaging/darwin-launcher.go`](../../../packaging/darwin-launcher.go) and
  [`flake/lib/builders/darwin.nix`](../../../flake/lib/builders/darwin.nix): current
  per-app launcher, bundled payload, signing and `.pkg` generation.
- [`perSystem/linux.nix`](../../../perSystem/linux.nix) and
  [`packaging/linux/`](../../../packaging/linux/): existing system packages,
  ownership, maintenance scripts and sandbox-policy assets.
- [`source/main/sandbox/dappSandboxAvailability.ts`](../../../source/main/sandbox/dappSandboxAvailability.ts):
  environment-specific package/path identities that the shared layout affects.
- [Architecture and package constraints](../../system/architecture.md) and
  [watchdog entry-point specification](../linux-repackaging/watchdog-entry-point-prd.md):
  preserve existing process ownership and supported packaging boundaries.
