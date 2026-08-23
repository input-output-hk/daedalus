# Linux System Package Decision (.deb / .rpm)

Status: **accepted package, release, migration, and successor support-matrix
contract**. The package strategy was accepted on 2026-08-12; original matrix
revision `task-005-a-matrix-2026-08-14` was approved by the user acting as
release/product authority on 2026-08-14. Successor revision
`task-108-matrix-2026-08-18` was approved on 2026-08-18 after authoritative
Ubuntu policy documentation invalidated the original Ubuntu 22.04 and exact
parser-version assumptions, with this repository record serving as the durable
approval record. No separate reviewer was required by that authority.
Normative packaging and sandbox requirements are mirrored in
[dapp-browser-cip30-prd.md](../dapp-browser-cip30-prd.md) and
[dapp-browser-cip30-tasks.json](../dapp-browser-cip30-tasks.json). Historical
task-005 preserves the cancelled portable spike. Task-005-a froze the package
and validation contract; task-005-b completed installed `.deb`/`.rpm`
exact-renderer certification on 2026-08-23. Tasks 103 and 110 completed the
runtime fail-closed and `.bin` retirement/update-migration boundaries. This
note remains the authoritative Linux strategy and support-matrix record.

## Decision

Daedalus on Linux ships **system packages only**:

| Format | Role |
|--------|------|
| **`.deb`** | Primary package for Debian/Ubuntu-class desktops |
| **`.rpm`** | Package for the approved Fedora 43 row in this revision |

Install layout uses **`/opt/daedalus/<cluster>`**, where `<cluster>` is the
build-time installer cluster slug, not `$HOME/.daedalus/<cluster>`.

Chromium OS sandboxing for production guests relies on the privileged install
model used by Electron desktop apps (electron-builder pattern):

1. Install Electron and `chrome-sandbox` under the fixed
   `/opt/daedalus/<cluster>` tree as root.
2. Support two independently provable routes: a root-owned mode-`4755` SUID
   helper when user namespaces are unavailable, or user namespaces with a
   root-owned mode-`0755` non-SUID helper. Either route may satisfy a supported
   row when its exact-renderer evidence passes. A package must not weaken one
   route or retry unsandboxed when the other fails.
3. Supported Ubuntu 24.04.x/26.04.x rows require a package-owned exact-path
   AppArmor `flags=(default_allow)` profile with `userns,`, selected by reviewed
   semantic ABI/features rather than exact parser patch-version equality. Fedora 43 requires package-owned SELinux policy
   and exact Electron/helper file contexts. Debian rows require neither policy
   asset unless a later reviewed matrix revision and certification add it.
4. Launchers **must not** pass `--no-sandbox` or `--disable-setuid-sandbox`.
5. Runtime dApp availability remains fail-closed when sandbox-disabling
   argv/environment is present or the task-103 local sandbox canary fails.
6. Never auto-retry unsandboxed and never weaken containment for remote content.

## Authoritative Support Matrix

Revision: `task-108-matrix-2026-08-18`, superseding only the contradicted Ubuntu
predicates in `task-005-a-matrix-2026-08-14`.

All rows are x86_64. Version-series rows include vendor point/security updates;
material kernel, Electron/Chromium, or host-policy changes trigger revalidation.

| Distribution/version | Package | Accepted sandbox routes | Required host-policy integration |
|---|---|---|---|
| Ubuntu 22.04.x LTS | `.deb` | wallet-only pending separate proof | no Daedalus AppArmor policy; helper remains `0755` |
| Ubuntu 24.04.x LTS | `.deb` | independently proven SUID or userns | AppArmor profile attached to exact Electron path |
| Ubuntu 26.04.x LTS | `.deb` | independently proven SUID or userns | AppArmor profile attached to exact Electron path |
| Debian 12.x | `.deb` | independently proven SUID or userns | none by default |
| Debian 13.x | `.deb` | independently proven SUID or userns | none by default |
| Fedora 43 | `.rpm` | independently proven SUID or userns | SELinux process and exact-file contexts |

No Ubuntu interim release is in this revision. Product intent is to support
vendor-supported interim releases, but each exact interim version must first be
added by a reviewed matrix revision and pass installed-artifact certification.
Fedora 42 and openSUSE Leap 15.6 are excluded because their vendor maintenance
ends before this certification baseline. Every other omitted or EOL row is
unsupported for dApp launch: installation may remain available in wallet-only
mode, but no remote guest may launch and no unapproved host-policy change is
applied. A listed row whose selected route cannot establish the frozen package
invariants fails package configuration rather than weakening containment.

## Frozen Package Contract

- Install root: `/opt/daedalus/<cluster>`.
- User command: `/usr/bin/daedalus-<cluster>`.
- Package launcher: `/opt/daedalus/<cluster>/bin/daedalus`.
- Frontend: `/opt/daedalus/<cluster>/libexec/daedalus-frontend`.
- Electron wrapper: `/opt/daedalus/<cluster>/libexec/electron`.
- Resolved Electron: `/opt/daedalus/<cluster>/libexec/bundle-electron/lib/electron/electron`.
- Helper: `/opt/daedalus/<cluster>/libexec/bundle-electron/lib/electron/chrome-sandbox`.
- Identity manifest: `/opt/daedalus/<cluster>/share/daedalus-sandbox-identity.json`.
- AppArmor asset: `/etc/apparmor.d/opt.daedalus.<cluster>.electron`.
- SELinux asset: `/usr/share/selinux/packages/daedalus-<cluster>.cil`. Task-109
  installs it at cluster-specific priority 200. The module is deliberately
  label-only: exact Electron uses Fedora `bin_t`; exact `chrome-sandbox` uses
  Fedora `chrome_sandbox_exec_t`, whose stock targeted-policy transition enters
  `chrome_sandbox_t` when the SUID route is selected. The module grants no
  Daedalus permission and defines no permissive or broad application domain.
- Package directories and executable files are root-owned mode `0755`; policy
  assets are root-owned mode `0644`; the regular non-symlink helper is root-owned
  mode `4755` for SUID evidence or `0755` for userns-only evidence.
- The root-owned mode-`0644` identity manifest pins matrix revision, exact row,
  support state/reason, cluster, exact package-file hashes, helper expectation,
  policy kind, reviewed exact process/file contexts, stock Fedora Chrome policy
  identity, and package/source identity. The probe records live parser/policy
  state separately and compares live files plus independently observed
  process/file policy state to this manifest.
- Maintainer scripts are idempotent, perform no network fetch, never inspect or
  mutate `${XDG_DATA_HOME:-$HOME/.local/share}/Daedalus`, and never disable
  AppArmor/SELinux, alter global userns policy, add permissive domains, or retry
  Electron unsandboxed.
- Linux launcher configuration uses system-package-disabled update mode and
  omits `updateRunnerBin`. The application does not execute package bytes,
  invoke `sudo`, or mutate package-manager state.
- Every desktop, launcher, wrapper, and restart path is free of `--no-sandbox`,
  `--disable-setuid-sandbox`, and equivalent bypasses.

## Release and upgrade contract

- CI/Hydra expose only
  `deb-installer.x86_64-linux.<cluster>` and
  `rpm-installer.x86_64-linux.<cluster>` for Linux releases. There is no Linux
  generic installer/signing job and no new portable `.bin`.
- A release manifest carries both packages as separate `linux-deb` and
  `linux-rpm` entries. Both map to target OS `linux`, require one shared release
  version, and deduplicate into one ordinary announcement.
- Neither Linux package appears in `softwareUpdate`. Windows and macOS retain
  app-managed updates; Linux users close Daedalus and install the matching
  newer local package with `apt` or `dnf`.
- Legacy portable clients receive the ordinary announcement and release-notes
  link only. Their existing installation remains wallet-only and usable until
  the user manually installs a system package.

## Explicit rejections

The following Linux distribution options are **rejected** for Daedalus shipping
and for dApp/OS-sandbox release gates:

| Option | Status |
|--------|--------|
| Portable self-extracting **`.bin`** to `$HOME/.daedalus/<cluster>` | **Rejected** as the Linux product package |
| **AppImage** | **Rejected** |
| **Flatpak** | **Rejected** |
| **Snap** | **Rejected** |
| Other portable/user-extract or store-confinement channels as Linux product packages | **Rejected** |

Rationale for rejecting the portable `.bin`:

- Home extraction cannot establish a root-owned mode-`4755` `chrome-sandbox`.
- Ubuntu 24.04+ AppArmor userns restriction expects fixed-path profiles (as with
  Chrome at `/opt/google/chrome/chrome`); home-installed Electron is unprofiled.
- Package-equivalent portable proof without `--no-sandbox` failed sandbox
  bootstrap (`SIGILL`) on Ubuntu 24.04; see
  [05-linux-chromium-sandbox-packaging.md](./05-linux-chromium-sandbox-packaging.md).
- Task-108 later identified the fixed Electron build's required
  `CHROME_DEVEL_SANDBOX` contract and bound it to the exact root-owned helper in
  the system-package launcher. This preserves the portable negative evidence;
  it does not make a user-owned helper or portable package acceptable.
- ADR-001 requires OS-sandboxed hostile guests; portable packaging cannot meet
  that gate without host policy the installer cannot apply.

`.dmg` remains macOS-only and is out of scope for this Linux decision.

## Relationship to prior task-005 work

- Research `05` and the portable probe remain valid **negative evidence** for the
  rejected portable model. They are not release certification for `.deb`/`.rpm`.
- The probe contract (exact renderer PID via `webContents.getOSProcessId()`,
  same-PID `/proc` NoNewPrivs/seccomp/capabilities/namespaces, privacy redaction)
  is retained and must be re-run against **installed `.deb` and `.rpm` artifacts**.
- Assertions that forbade a usable SUID helper applied only to the portable
  candidate. Under system packages, a reviewed root-owned helper is **expected**
  when userns are unavailable and must be validated, not rejected.

## Implementation ownership

| Work | Owner |
|------|--------|
| Preserve portable negative evidence and rejected-strategy outcome | task-005 (cancelled) |
| Freeze deb/rpm sandbox strategy, authoritative matrix, postinst contract, and probe adaptation | task-005-a |
| Implement `.deb` package, approved SUID or userns route, mandatory Ubuntu AppArmor, identity manifest, and Nix outputs | task-108 |
| Implement `.rpm` package and postinst equivalents | task-109 |
| Certify installed `.deb` and `.rpm` artifacts across the authoritative matrix | task-005-b (completed 2026-08-23) |
| Retire `.bin` shipping, migrate Linux release/update behavior and docs | task-110 (completed 2026-08-23) |
| Remove remaining development/legacy sandbox-disabling defaults, add runtime canary, fail-closed dApps | task-103 (completed 2026-08-23) |
| Real guest and release-candidate packaged proof | task-107, task-802, task-807, task-903-a |
| Deferred task-108 package lifecycle rows and destructive/reboot fixtures after full PRD implementation | task-807 release-candidate gate; manual evidence remains required |

## Certification

Task-005-b passed the exact installed renderer probe on Ubuntu 24.04/26.04,
Debian 12/13, and Fedora 43. Ubuntu 22.04 produced its expected wallet-only
refusal. AppArmor, SELinux, helper-mode, and userns denials failed closed and
their restorations returned to passing results. Exact package, source, host
image, probe, and normalized evidence identities are indexed at
`scripts/linux-chromium-sandbox-probe/evidence/task-005-b/index.json`.

The certification proves this package/matrix baseline only. Task-103 completed
runtime argv/environment rejection, installed-package identity checks, and the
local canary; every later guest, audit, and release-candidate gate remains
required.

## Manual migration

The self-extracting producer, home-replace updater, and launcher-restart
semantics tied to `.bin` are retired. Migration is deliberately manual and
non-destructive:

1. Fully stop Daedalus and retain
   `${XDG_DATA_HOME:-$HOME/.local/share}/Daedalus` in place. A custom
   `XDG_DATA_HOME` must be recorded and supplied consistently.
2. Inspect
   `$HOME/.local/share/applications/Daedalus-<cluster>.desktop`. Move aside only
   a verified symlink into the old `$HOME/.daedalus/<cluster>` tree so it
   cannot shadow the package's system desktop entry.
3. Install the matching downloaded `.deb` with `apt` or `.rpm` with `dnf`.
4. First-launch the exact `/usr/bin/daedalus-<cluster>` command with the same
   `XDG_DATA_HOME`, then confirm wallets and state before considering any
   legacy executable cleanup.

No package script removes the old home executable, wallet state, or stale
`XDG_DATA_HOME/Daedalus/<cluster>/namespaceHelper` symlink. The latter may
remain harmlessly until separately verified. No broad removal is part of the
migration contract.

## Non-negotiables preserved

- No automatic `--no-sandbox` fallback for production guests.
- No remote content in the privileged trusted renderer.
- No AppImage/Flatpak/Snap as substitute containment without this decision being
  formally reopened.
- Production guest launch stays disabled until privileged IPC authentication,
  **packaged `.deb`/`.rpm` OS sandbox proof**, and all other PRD gates complete.

## Decision provenance

- Date: 2026-08-12
- Context: task-005 portable feasibility blocked; product chose system packages
  after design research comparing portable userns, system packages, deferred
  Linux dApps, and external-browser connectors.
- Product constraints at decision: Linux dApps required for launch; portable-only
  was reconsidered and **rejected** in favor of `.deb`/`.rpm`.
- Speakers: product/release owner direction captured in session; recorded here as
  durable plan evidence.
