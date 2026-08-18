# Linux System Package Decision (.deb / .rpm)

Status: **accepted package contract and successor support matrix**. The package strategy
was accepted on 2026-08-12; original matrix revision `task-005-a-matrix-2026-08-14` was
approved by the user acting as release/product authority on 2026-08-14. Successor
revision `task-108-matrix-2026-08-18` was approved on 2026-08-18 after
authoritative Ubuntu policy documentation invalidated the original Ubuntu 22.04
and exact parser-version assumptions, with
this repository record serving as the durable approval record. No separate
reviewer was required by that authority. Normative packaging and sandbox requirements are mirrored in
[dapp-browser-cip30-prd.md](../dapp-browser-cip30-prd.md) and
[dapp-browser-cip30-tasks.json](../dapp-browser-cip30-tasks.json). Historical
task-005 preserves the cancelled portable spike. Task-005-a freezes the package
and validation contract; task-005-b remains incomplete until packaged
`.deb`/`.rpm` sandbox proof exists. This note freezes the strategy and contract,
not package implementation or certification.

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
- Launcher: `/opt/daedalus/<cluster>/bin/daedalus`.
- Frontend: `/opt/daedalus/<cluster>/libexec/daedalus-frontend`.
- Electron wrapper: `/opt/daedalus/<cluster>/libexec/electron`.
- Resolved Electron: `/opt/daedalus/<cluster>/libexec/bundle-electron/lib/electron/electron`.
- Helper: `/opt/daedalus/<cluster>/libexec/bundle-electron/lib/electron/chrome-sandbox`.
- Identity manifest: `/opt/daedalus/<cluster>/share/daedalus-sandbox-identity.json`.
- AppArmor asset: `/etc/apparmor.d/opt.daedalus.<cluster>.electron`.
- SELinux asset: `/usr/share/selinux/packages/daedalus-<cluster>.cil`; task-109 records the reviewed module and exact process/file labels in the identity manifest.
- Package directories and executable files are root-owned mode `0755`; policy
  assets are root-owned mode `0644`; the regular non-symlink helper is root-owned
  mode `4755` for SUID evidence or `0755` for userns-only evidence.
- The root-owned mode-`0644` identity manifest pins matrix revision, exact row,
  support state/reason, cluster, exact package-file hashes, helper expectation,
  policy kind, task-108-reviewed AppArmor semantic ABI/features, and
  task-108/109-reviewed exact policy labels/contexts/module. The probe records
  the observed parser version separately and compares live files plus
  independently observed process/file policy state to this manifest; the
  contract does not invent generic SELinux type names.
- Maintainer scripts are idempotent, perform no network fetch, never inspect or
  mutate `XDG_DATA_HOME/Daedalus`, and never disable AppArmor/SELinux, alter
  global userns policy, add permissive domains, or retry Electron unsandboxed.
- Every desktop, launcher, wrapper, restart, and post-update path is free of
  `--no-sandbox`, `--disable-setuid-sandbox`, and equivalent bypasses.

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
| Certify installed `.deb` and `.rpm` artifacts across the authoritative matrix | task-005-b |
| Retire `.bin` shipping, migrate Linux auto-update and docs | task-110 |
| Remove remaining development/legacy sandbox-disabling defaults, add runtime canary, fail-closed dApps | task-103 (depends on task-005-b certification) |
| Real guest and release-candidate packaged proof | task-107, task-802, task-807, task-903-a |
| Deferred task-108 package lifecycle rows and destructive/reboot fixtures after full PRD implementation | task-807 release-candidate gate; manual evidence remains required |

## Auto-update and migration

Rejecting `.bin` invalidates the current Linux self-extract auto-update path
(`linux-self-extracting-archive.sh`, home replace, launcher restart semantics
tied to that artifact). task-110 owns:

- Stop producing and advertising the portable installer for new releases that
  enable dApps.
- Define migration from existing `$HOME/.daedalus/<cluster>` installs to
  `/opt` system packages without deleting wallet data under
  `XDG_DATA_HOME/Daedalus`.
- Replace update-runner / cardano-launcher expectations that assume `.bin`
  extract.
- Document upgrade from legacy portable installs.

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
