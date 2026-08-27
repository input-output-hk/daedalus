# 02. Paper wallet creation is broken and retired

Decided on 2026-08-27 during the dependency hygiene branch. Creation is retired,
restore is kept and now has the fixed vector it never had. **The creation code
was not removed**, because removing it deletes an IPC contract and that is not
dependency work.

## The defect

`generateMnemonic(9)` maps a 9-word request to 96 bits of entropy. BIP39's
minimum is 128, so `entropyToMnemonic` rejects it and the call throws
`Invalid entropy`. `generateAdditionalMnemonics` is that call, and
`WalletsStore._generateCertificate` reaches it at line 1433, so paper wallet
certificate creation fails.

The throw happens inside a `Promise` executor in `api.ts`, so it rejects the
promise rather than reaching the surrounding `try`/`catch`. The caller sees a
raw `Error`, not an `ApiError`.

96 bits would sit below the standard's floor even if the call succeeded, so
making it work at that strength is not an acceptable outcome.

## Why nobody noticed

`SidebarStore.ts:122` sets `PAPER_WALLET_CREATE_CERTIFICATE` to `false`, so the
flow has had no entry point for years.

The scenario covering it,
`tests/wallets/unit/features/scrambling-and-unscrambling-mnemonics.feature`, was
retagged from `@unit` to `@unit @skip` on 2021-06-13 in `c6dd7d9fe`, inside a
pull request titled "Fix automated tests setup". `test:unit` runs
`--tags '@unit and not @skip and not @wip'`, so it has not executed since. Five
years of green builds did not mean on that path what they appeared to mean.

## What has been done

The scenario and the seven step definitions it was the only user of are
removed, and their coverage moved to Jest. No scenario under
`tests/wallets/unit` carries `@skip` or `@wip`.

Restore is asserted against a recorded certificate in
`source/renderer/app/utils/__fixtures__/paper-wallet-certificate.json`, captured
while `scramblePaperWalletMnemonic` still existed. Once creation is removed it
cannot be produced again.

## The removal surface, for whoever takes it

Roughly 1,900 lines across 24 renderer files, plus:

- `source/main/ipc/generatePaperWalletChannel.ts` and its renderer counterpart
- `source/common/types/paper-wallet-request.types.ts`
- `source/renderer/app/utils/paperWalletPdfGenerator.ts`
- six PNGs and a TTF under `source/common/assets/pdf/`
- Storybook stories, i18n keys, and store, action, route and sidebar wiring

**Restore must survive.** It needs `getScrambledInput`,
`unscramblePaperWalletMnemonic`, `mnemonicToSeedHex`, `StepMnemonicsContainer`,
and the fixture above. Only `scramblePaperWalletMnemonic` and
`generateAdditionalMnemonics` are creation-only.
