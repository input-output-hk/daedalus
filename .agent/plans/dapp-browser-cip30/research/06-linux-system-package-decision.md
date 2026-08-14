# Linux System Package Decision (.deb / .rpm)

Status: **accepted product/release decision** (2026-08-12). Normative packaging
and sandbox requirements are mirrored in
[dapp-browser-cip30-prd.md](../dapp-browser-cip30-prd.md) and
[dapp-browser-cip30-tasks.json](../dapp-browser-cip30-tasks.json). Historical
task-005 preserves the cancelled portable spike. Task-005-a freezes the package
and validation contract; task-005-b remains incomplete until packaged
`.deb`/`.rpm` sandbox proof exists. This note freezes strategy only.

## Decision

Daedalus on Linux ships **system packages only**:

| Format | Role |
|--------|------|
| **`.deb`** | Primary package for Debian/Ubuntu-class desktops |
| **`.rpm`** | Primary package for Fedora/RHEL/openSUSE-class desktops |

Install layout uses **`/opt/daedalus/<cluster>`**, where `<cluster>` is the
build-time installer cluster slug, not `$HOME/.daedalus/<cluster>`.

Chromium OS sandboxing for production guests relies on the privileged install
model used by Electron desktop apps (electron-builder pattern):

1. Install Electron and `chrome-sandbox` under the fixed
   `/opt/daedalus/<cluster>` tree as root.
2. **SUID helper** when unprivileged user namespaces are unavailable:
   root-owned `chrome-sandbox` mode `4755`.
3. **User namespaces** when the host supports them; helper may remain non-SUID.
4. **AppArmor profile** on Ubuntu 24.04+ (and other AppArmor hosts that restrict
   unprivileged userns): package ships a profile with `userns,` for the fixed
   Electron binary path and loads it in `postinst` when `apparmor_parser`
   accepts the profile ABI.
5. Launchers **must not** pass `--no-sandbox` or `--disable-setuid-sandbox`.
6. Runtime dApp availability remains fail-closed when sandbox-disabling
   argv/environment is present or the task-103 local sandbox canary fails.
7. Never auto-retry unsandboxed and never weaken containment for remote content.

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
| Implement `.deb` package, postinst SUID/AppArmor, Nix outputs | task-108 |
| Implement `.rpm` package and postinst equivalents | task-109 |
| Certify installed `.deb` and `.rpm` artifacts across the authoritative matrix | task-005-b |
| Retire `.bin` shipping, migrate Linux auto-update and docs | task-110 |
| Remove remaining development/legacy sandbox-disabling defaults, add runtime canary, fail-closed dApps | task-103 (depends on task-005-b certification) |
| Real guest and release-candidate packaged proof | task-107, task-802, task-807, task-903-a |

## Supported matrix (minimum product intent)

Exact distro/version rows remain release-owner authoritative, but the packaging
decision implies at least:

- One Debian/Ubuntu-class host for `.deb` (include Ubuntu 24.04 AppArmor userns).
- One Fedora/RHEL-class host for `.rpm`.
- One negative host where sandbox prerequisites fail → dApps fail closed, no
  unsandboxed retry; wallet may remain available only via documented
  dApp-disabled modes if any.

No unnamed distribution is claimed supported until its package row has probe
evidence.

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
