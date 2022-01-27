// @flow
import { defineMessages } from 'react-intl';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

export const messages: {
  [string]: ReactIntlMessage,
} = defineMessages({
  dialogTitle: {
    id: 'wallet.settings.icoPublicKey',
    defaultMessage: '!!!ICO Public Key',
    description: 'Title for the "ICO Public Key QR Code" dialog.',
  },
  copyPublicKeyLabel: {
    id: 'wallet.settings.copyICOPublicKey',
    defaultMessage: '!!!Copy ICO public key',
    description: 'Copy ICO public key label.',
  },
  derivationPathTooltip: {
    id: 'wallet.settings.dialog.derivationPathTooltip',
    defaultMessage: '!!!Derivation path',
    description: 'Tooltip for the derivation path',
  },
});
