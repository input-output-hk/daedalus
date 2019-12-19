// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import RadioSet from '../../widgets/RadioSet';
import WalletRestoreDialog from './WalletRestoreDialog';
import globalMessages from '../../../i18n/global-messages';
import commonStyles from './StepDialogStyles.scss';
import {
  WALLET_KINDS,
  WALLET_DAEDALUS_KINDS,
  WALLET_YOROI_KINDS,
  WALLET_HARDWARE_KINDS,
} from '../../../config/walletRestoreConfig';
import type { WalletKinds } from '../../../types/walletRestoreTypes';

const messages = defineMessages({
  labelWalletKind: {
    id: 'wallet.restore.dialog.step.WalletKind.label.walletKind',
    defaultMessage: '!!!What kind of wallet would you like to restore?',
    description: 'Label for the "labelwalletKind" checkbox.',
  },
  labelWalletKindDaedalus: {
    id: 'wallet.restore.dialog.step.WalletKind.label.walletKindDaedalus',
    defaultMessage: '!!!Daedalus wallet',
    description: 'Label for the "labelWalletKindDaedalus" checkbox.',
  },
  labelWalletKindYoroi: {
    id: 'wallet.restore.dialog.step.WalletKind.label.walletKindYoroi',
    defaultMessage: '!!!Yoroi wallet',
    description: 'Label for the "labelWalletKindYoroi" checkbox.',
  },
  labelWalletKindHardware: {
    id: 'wallet.restore.dialog.step.WalletKind.label.walletKindHardware',
    defaultMessage: '!!!Hardware wallet',
    description: 'Label for the "labelWalletKindHardware" checkbox.',
  },
  labelDaedalusWalletKind: {
    id: 'wallet.restore.dialog.step.WalletKind.label.daedalusWalletKind',
    defaultMessage:
      '!!!What kind of Daedalus wallet would you like to restore?',
    description: 'Label for the "labelDaedalusWalletKind" checkbox.',
  },
  labelDaedalusWalletKindBalance12Word: {
    id:
      'wallet.restore.dialog.step.WalletKind.label.daedalusWalletKindBalance12Word',
    defaultMessage: '!!!12 words <span>(Balance wallet)</span>',
    description:
      'Label for the "labelDaedalusWalletKindBalance12Word" checkbox.',
  },
  labelDaedalusWalletKindReward15Word: {
    id:
      'wallet.restore.dialog.step.WalletKind.label.daedalusWalletKindReward15Word',
    defaultMessage: '!!!15 words <span>(Rewards wallet)</span>',
    description:
      'Label for the "labelDaedalusWalletKindReward15Word" checkbox.',
  },
  labelDaedalusWalletKindBalance27Word: {
    id:
      'wallet.restore.dialog.step.WalletKind.label.daedalusWalletKindBalance27Word',
    defaultMessage: '!!!27 words <span>(Balance wallet)</span>',
    description:
      'Label for the "labelDaedalusWalletKindBalance27Word" checkbox.',
  },
  labelYoroiWalletKind: {
    id: 'wallet.restore.dialog.step.WalletKind.label.yoroiWalletKind',
    defaultMessage: '!!!What kind of Yoroi wallet would you like to restore?',
    description: 'Label for the "labelYoroiWalletKind" checkbox.',
  },
  labelYoroiWalletKindBalance15Word: {
    id:
      'wallet.restore.dialog.step.WalletKind.label.yoroiWalletKindBalance15Word',
    defaultMessage: '!!!15 words <span>(Balance wallet)</span>',
    description:
      'Label for the "labelDaedalusWalletKindBalance15Word" checkbox.',
  },
  labelYoroiWalletKindReward15Word: {
    id:
      'wallet.restore.dialog.step.WalletKind.label.yoroiWalletKindReward15Word',
    defaultMessage: '!!!15 words <span>(Reward wallet)</span>',
    description:
      'Label for the "labelDaedalusWalletKindReward15Word" checkbox.',
  },
  labelHardwareWalletKind: {
    id: 'wallet.restore.dialog.step.WalletKind.label.hardwareWalletKind',
    defaultMessage:
      '!!!What kind of hardware wallet would you like to restore?',
    description: 'Label for the "labelHardwareWalletKind" checkbox.',
  },
  labelHardwareWalletKindNano: {
    id: 'wallet.restore.dialog.step.WalletKind.label.hardwareWalletKindNano',
    defaultMessage: '!!!24 words - Ledger Nano S or Nano X (Balance wallet)',
    description: 'Label for the "labelHardwareWalletKindNano" checkbox.',
  },
  labelHardwareWalletKindTrezor: {
    id: 'wallet.restore.dialog.step.WalletKind.label.hardwareWalletKindTrezor',
    defaultMessage: '!!!24 words - Trezor (Balance wallet)',
    description: 'Label for the "labelHardwareWalletKindTrezor" checkbox.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  onContinue: Function,
  onClose: Function,
  onSetWalletKind: Function,
  walletKind: string,
  walletKindDaedalus: ?string,
  walletKindYoroi: ?string,
  walletKindHardware: ?string,
};

export default class StepWalletTypeDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  getWalletKind = (kinds: Object, value: ?string, kindParam?: string) => (
    <RadioSet
      label={this.context.intl.formatMessage(messages.labelWalletKind)}
      items={Object.keys(kinds).map((key: string) => {
        const kind: WalletKinds = kinds[key];
        return {
          key: kind,
          label: (
            <FormattedHTMLMessage
              {...messages[`label${kindParam || ''}WalletKind${kind}`]}
            />
          ),
          selected: value === kind,
          onChange: () => this.props.onSetWalletKind(kind, kindParam),
        };
      })}
      verticallyAligned
    />
  );

  render() {
    const {
      onClose,
      onContinue,
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware,
    } = this.props;
    return (
      <WalletRestoreDialog
        stepNumber={0}
        actions={[
          {
            primary: true,
            label: 'Continue',
            onClick: onContinue,
          },
        ]}
        onClose={onClose}
      >
        <div className={commonStyles.component}>
          {this.getWalletKind(WALLET_KINDS, walletKind)}
        </div>
        {walletKind === WALLET_KINDS.DAEDALUS &&
          this.getWalletKind(
            WALLET_DAEDALUS_KINDS,
            walletKindDaedalus,
            WALLET_KINDS.DAEDALUS
          )}
        {walletKind === WALLET_KINDS.YOROI &&
          this.getWalletKind(
            WALLET_YOROI_KINDS,
            walletKindYoroi,
            WALLET_KINDS.YOROI
          )}
        {walletKind === WALLET_KINDS.HARDWARE &&
          this.getWalletKind(
            WALLET_HARDWARE_KINDS,
            walletKindHardware,
            WALLET_KINDS.HARDWARE
          )}
      </WalletRestoreDialog>
    );
  }
}
