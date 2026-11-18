import { defineMessages } from 'react-intl';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

const messages: Record<string, ReactIntlMessage> = defineMessages({
  title: {
    id: 'loading.walletDbDamage.title',
    defaultMessage:
      '!!!{count, plural, one {A wallet cannot be opened} other {Some wallets cannot be opened}}',
    description:
      'Headline of the screen that offers to repair wallet data files an older version of Daedalus changed',
  },
  body: {
    id: 'loading.walletDbDamage.body',
    defaultMessage:
      '!!!{count, plural, one {# wallet was} other {# wallets were}} changed by an older version of Daedalus, so Daedalus cannot open {count, plural, one {it} other {them}}. Your funds are safe: they are on the blockchain, not in these wallet files.',
    description:
      'Explanation on the wallet repair offer: how many wallets Daedalus cannot open ({count}) and why, and that funds are not affected',
  },
  offer: {
    id: 'loading.walletDbDamage.offer',
    defaultMessage:
      '!!!Daedalus can save a copy of the wallet files, then repair them. Until then, these wallets will not appear in Daedalus.',
    description:
      'What the repair does, and that the wallets stay missing without it, on the wallet repair offer',
  },
  repair: {
    id: 'loading.walletDbDamage.repair',
    defaultMessage: '!!!Back up and repair',
    description:
      'Button label that backs up and repairs the wallet data files an older version of Daedalus changed',
  },
  notNow: {
    id: 'loading.walletDbDamage.notNow',
    defaultMessage: '!!!Not now',
    description:
      'Button label that hides the wallet repair offer until Daedalus restarts',
  },
  repairingTitle: {
    id: 'loading.walletDbDamage.repairing.title',
    defaultMessage: '!!!Repairing wallets',
    description: 'Headline of the wallet repair screen while the repair runs',
  },
  repairingBody: {
    id: 'loading.walletDbDamage.repairing.body',
    defaultMessage:
      '!!!Daedalus is saving a copy of the wallet files and repairing them. Please wait.',
    description: 'What happens while the wallet repair runs',
  },
  doneTitle: {
    id: 'loading.walletDbDamage.done.title',
    defaultMessage: '!!!Wallet repair finished',
    description: 'Headline of the wallet repair screen once the repair ran',
  },
  repaired: {
    id: 'loading.walletDbDamage.done.repaired',
    defaultMessage:
      '!!!{count, plural, one {# wallet was} other {# wallets were}} repaired.',
    description: 'How many wallets ({count}) the repair repaired',
  },
  notRepaired: {
    id: 'loading.walletDbDamage.done.notRepaired',
    defaultMessage:
      '!!!{count, plural, one {# wallet could not be repaired and is unchanged.} other {# wallets could not be repaired and are unchanged.}}',
    description:
      'How many wallets ({count}) the repair could not repair, and that their files were not changed',
  },
  nothingToRepair: {
    id: 'loading.walletDbDamage.done.nothingToRepair',
    defaultMessage: '!!!No wallets needed repair.',
    description:
      'Shown after the repair ran when no wallet data file needed it any more',
  },
  backupLocation: {
    id: 'loading.walletDbDamage.done.backupLocation',
    defaultMessage:
      '!!!A copy of the wallet files from before the repair is saved in this folder: {folder}',
    description:
      'Where the copy of the wallet data files made before the repair is ({folder} is a folder path)',
  },
  reasonRefused: {
    id: 'loading.walletDbDamage.done.reason.refused',
    defaultMessage:
      '!!!{file}: this file differs in ways Daedalus cannot repair. The file is unchanged.',
    description:
      'Why the repair did not repair a wallet data file ({file} is its name): it has changes the repair is not for, so it was left as it was',
  },
  reasonBackupFailed: {
    id: 'loading.walletDbDamage.done.reason.backupFailed',
    defaultMessage:
      '!!!{file}: Daedalus could not save a copy. The file is unchanged.',
    description:
      'Why the repair did not repair a wallet data file ({file} is its name): the copy made before any change failed, so the file was left as it was',
  },
  reasonRepairFailed: {
    id: 'loading.walletDbDamage.done.reason.repairFailed',
    defaultMessage:
      '!!!{file}: the repair did not complete. The file is unchanged.',
    description:
      'Why the repair did not repair a wallet data file ({file} is its name): the change failed part-way and was undone',
  },
  reasonOther: {
    id: 'loading.walletDbDamage.done.reason.other',
    defaultMessage:
      '!!!{file}: Daedalus did not repair this file. The file is unchanged.',
    description:
      'Shown for a wallet data file ({file} is its name) the repair did not repair, for a reason this version of Daedalus does not name',
  },
  continue: {
    id: 'loading.walletDbDamage.done.continue',
    defaultMessage: '!!!Continue',
    description: 'Button label that closes the wallet repair results',
  },
});

export default messages;
