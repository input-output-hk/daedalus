# Claude Code Instructions

> **For Claude Code.** This file points to the centralized documentation in `.agent/`.

---

## Documentation Location

All agent documentation is centralized in the [`.agent/`](./.agent/) directory.

**Start here:** [`.agent/readme.md`](./.agent/readme.md)

---

## Quick Reference

### System Documentation
- [Architecture](./.agent/system/architecture.md) — Electron/React architecture, IPC patterns
- [API Endpoints](./.agent/system/api-endpoints.md) — cardano-wallet REST API
- [State Management](./.agent/system/state-management.md) — MobX stores and patterns

### Workflows
- [/build](./.agent/workflows/build.md) — Nix shells and Yarn builds
- [/test](./.agent/workflows/test.md) — Jest and Cucumber testing
- [/electron](./.agent/workflows/electron.md) — Electron main process development
- [/frontend](./.agent/workflows/frontend.md) — React/MobX development
- [/hardware-wallets](./.agent/workflows/hardware-wallets.md) — Ledger/Trezor development
- [/ipc](./.agent/workflows/ipc.md) — IPC channel development
- [/nix](./.agent/workflows/nix.md) — Nix environment setup
- [/storybook](./.agent/workflows/storybook.md) — Storybook component development
- [/update-doc](./.agent/workflows/update-doc.md) — Update documentation

### Learning Resources
- [Task History](./.agent/plans/) — Past implementation plans
- [SOPs](./.agent/SOPs/) — Standard operating procedures
- [Skills](./.agent/skills/) — Cardano CLI guidance

---

## Common Commands

### Development
```bash
yarn nix:mainnet          # Enter Nix shell (mainnet)
yarn nix:preprod          # Enter Nix shell (preprod testnet)
yarn dev                  # Start development mode
```

### Building
```bash
yarn build                # Production build
yarn build:main           # Build main process only
yarn build:renderer       # Build renderer only
yarn package              # Create installer
```

### Testing
```bash
yarn test:unit            # Cucumber unit tests
yarn test:e2e             # Cucumber E2E tests
yarn test:jest            # Jest tests
yarn storybook            # Component development
```

### Code Quality
```bash
yarn lint                 # ESLint
yarn compile              # TypeScript check
yarn prettier:check       # Prettier check
yarn check:all            # Run all checks
```

---

## Project Structure

```
daedalus/
├── .agent/                 # Agent documentation (READ THIS)
├── source/
│   ├── main/               # Electron main process
│   │   ├── cardano/        # Node/wallet management
│   │   ├── ipc/            # IPC handlers
│   │   ├── menus/          # Native menus
│   │   └── windows/        # Window management
│   ├── renderer/           # React UI
│   │   └── app/
│   │       ├── components/ # React components
│   │       ├── stores/     # MobX stores
│   │       ├── api/        # cardano-wallet client
│   │       ├── containers/ # Container components
│   │       └── i18n/       # Internationalization
│   └── common/             # Shared code
│       ├── ipc/            # IPC API contracts
│       └── types/          # TypeScript types
├── storybook/              # Component stories
├── tests/                  # Cucumber tests
├── installers/             # Platform installers
└── nix/                    # Nix configuration
```

---

## Creating Documentation

When completing features or resolving issues:

1. **Implementation plans** → Save to `.agent/plans/{domain}/`
2. **Resolved issues** → Create SOP in `.agent/SOPs/{category}/`
3. **New workflows** → Add to `.agent/workflows/`

Run `/update-doc` workflow for guidance.
