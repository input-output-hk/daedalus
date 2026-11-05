# Specification Quality Checklist

**Feature**: [Shared test environment packaging](../spec.md)
**Reviewed**: 2026-10-29

- [x] Capture the user's request, agreed launcher direction and specification-only scope.
- [x] Include exactly Preview, Preprod and Mainnet Flight; keep Mainnet standalone.
- [x] Require one shared large payload, including Electron and application resources.
- [x] Preserve existing namespaced state and custom chain locations without redesign.
- [x] Define prioritized user stories and Given/When/Then acceptance scenarios.
- [x] Cover concurrent environments, repeat launch, activation, restart and Mainnet coexistence.
- [x] Require three native macOS launcher apps and validate actual running Dock identity.
- [x] Cover installed signatures/notarization and preserve current sandbox boundaries.
- [x] Define shared update ownership, legacy adoption, repair and removal behavior.
- [x] Preserve Linux system-package updates and separate Mainnet release routing.
- [x] Include failure, partial installation, stale shortcut and lifecycle edge cases.
- [x] Provide measurable installed-artifact outcomes without inventing size estimates.
- [x] Link verified current source inputs and distinguish assumptions from proven behavior.
- [x] Defer filesystem layout and mechanism choices to planning without weakening requirements.

All checked items concern specification quality. No implementation, platform
prototype, installed-artifact test or release certification is claimed.
