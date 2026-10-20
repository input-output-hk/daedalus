# Feature Specification: Ledger Flex pairing support

**Feature Branch**: `ledger-flex-support`
**Created**: 2026-10-20
**Status**: SDK upgrade implemented; automated validation passed; user reports successful device test
**Base**: local `master` at `22440487d`
**Input**: Start a worktree for getting Ledger Flex support working. Pairing reaches public-key extraction and displays an error with no further details; Nano X works.

## Reported behavior

Connecting a Ledger Flex to pair a wallet fails at the apparent public-key extraction step with a generic error. Ledger Nano X pairing succeeds. These are user observations; the failing operation and root cause have not yet been verified.

On 2026-10-21 the user confirmed: build from master, freshly factory-reset Flex,
Cardano app **7.3.0**, no public-key approval prompt, an error
on the device and a hanging Daedalus UI. The device error text and host OS remain
unknown. App 7.3.0 is within the existing SDK's supported version range; the
separately discovered app-8.x compatibility gap does not establish this cause.
The initial 5.8.3 report referred to a bootloader/microcontroller version, not
Secure Element firmware. The user subsequently confirmed the Secure Element
was upgraded to **1.7.0** and explicitly requested the full SDK upgrade.
After the update, the user confirmed Cardano app **8.0.9**. The configuration
for the next physical test is Secure Element **1.7.0** / Cardano app **8.0.9**;
the user subsequently reported that the updated build works. Address verification,
reconnection, Nano X regression and rejection/disconnection checks have not been
individually reported.

## User Scenarios & Testing

### User Story 1 — Pair a Ledger Flex (Priority: P1)

As a Ledger Flex owner, I can pair my device and create a hardware wallet through the existing Daedalus pairing flow.

**Independent Test**: Pair an unlocked Flex with its Cardano app open, approve any device prompts, and verify the resulting wallet and account addresses.

**Acceptance Scenarios**:

1. **Given** a connected, unlocked Flex with the Cardano app open, **when** I pair and approve public-key export, **then** Daedalus retrieves the expected account public key and completes wallet creation.
2. **Given** successful pairing, **when** I reconnect the device, **then** Daedalus can use the paired wallet without requiring duplicate wallet creation.

### User Story 2 — Understand and recover from pairing failures (Priority: P2)

As a user encountering a pairing error, I receive useful error information and can retry after resolving its cause.

**Independent Test**: Reject public-key export or disconnect during pairing, inspect the resulting message and diagnostic logs, then retry.

**Acceptance Scenarios**:

1. **Given** a rejected export or disconnected device, **when** pairing fails, **then** Daedalus reports the available cause and permits a new pairing attempt.
2. **Given** an unexpected device error, **when** pairing fails, **then** diagnostic logs retain the operation and available error name, message, or device status without exposing wallet key material.

### Regression acceptance

Given a Nano X that pairs successfully before the change, the same pairing and public-key extraction flow must succeed after the change.

### Edge Cases

- Device locked or Cardano app closed.
- User rejects public-key export.
- Device disconnects during export, then reconnects for retry.
- Device returns an error without a usable message.
- Different device firmware and Cardano app versions; record tested versions.

## Requirements

- **FR-001**: Flex pairing MUST retrieve the expected account extended public key and complete the existing wallet creation flow.
- **FR-002**: Pairing MUST preserve existing account derivation and on-device approval requirements.
- **FR-003**: Pairing failures MUST preserve available diagnostic error information and provide useful user-facing guidance where the cause is known.
- **FR-004**: Users MUST be able to retry after cancellation or disconnection without restarting Daedalus.
- **FR-005**: Nano X pairing MUST continue to work.
- **FR-006**: Private keys and recovery phrases MUST remain on the device; diagnostics MUST avoid wallet key material.

## Success Criteria

- **SC-001**: Physical Flex completes pairing and produces account addresses verified against the device.
- **SC-002**: Physical Nano X completes the same regression pairing check.
- **SC-003**: Rejection and disconnection each produce understandable outcomes and allow a successful subsequent attempt.
- **SC-004**: Relevant automated checks pass in the project Nix development shell, and tested hardware, firmware, Cardano app, OS, and Daedalus versions are recorded.

## Scope and Assumptions

The first milestone is Flex pairing and public-key extraction over the existing wired connection flow. Bluetooth, other Ledger models, and broader transaction or governance support are not acceptance claims of this milestone. Broader support requires separate address-verification and signing validation.

The current report does not establish an SDK, transport, firmware, device-identification, or error-serialization defect. Select an implementation only after tracing the actual failure.

## Investigation Inputs Still Needed

- Reproduction OS, exact Daedalus commit, and exact on-device error text.
- Main-process and renderer diagnostics for the failed attempt, excluding sensitive material.
- Nano X firmware/app versions for a controlled comparison.

## Next Stage

See [plan.md](plan.md) and [tasks.md](tasks.md) for implementation and validation.
Retest physical Flex pairing after the user's Secure Element firmware upgrade,
record the resulting Cardano app version, and inspect operation diagnostics if
the failure persists. Complete the Nano X and recovery acceptance checks before
declaring Flex support verified.
