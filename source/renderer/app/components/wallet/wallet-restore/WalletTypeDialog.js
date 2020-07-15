// @flow
import React, { Component, Fragment } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { set } from 'lodash';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import RadioSet from '../../widgets/RadioSet';
import WalletRestoreDialog from './widgets/WalletRestoreDialog';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletTypeDialog.scss';
import {
  WALLET_KINDS,
  WALLET_DAEDALUS_KINDS,
  WALLET_YOROI_KINDS,
  WALLET_HARDWARE_KINDS,
} from '../../../config/walletRestoreConfig';
import type {
  WalletKinds,
  WalletKind,
  WalletDaedalusKind,
  WalletYoroiKind,
  WalletHardwareKind,
  HardwareWalletAcceptance,
} from '../../../types/walletRestoreTypes';

const messages = defineMessages({
  labelWalletKind: {
    id: 'wallet.restore.dialog.step.walletKind.label.walletKind',
    defaultMessage: '!!!What kind of wallet would you like to restore?',
    description: 'Label for the "labelwalletKind" checkbox.',
  },
  labelWalletKindDaedalus: {
    id: 'wallet.restore.dialog.step.walletKind.label.walletKindDaedalus',
    defaultMessage: '!!!Daedalus wallet',
    description: 'Label for the "labelWalletKindDaedalus" checkbox.',
  },
  labelWalletKindYoroi: {
    id: 'wallet.restore.dialog.step.walletKind.label.walletKindYoroi',
    defaultMessage: '!!!Yoroi wallet',
    description: 'Label for the "labelWalletKindYoroi" checkbox.',
  },
  labelWalletKindHardware: {
    id: 'wallet.restore.dialog.step.walletKind.label.walletKindHardware',
    defaultMessage: '!!!Hardware wallet',
    description: 'Label for the "labelWalletKindHardware" checkbox.',
  },
  labelDaedalusWalletKind: {
    id: 'wallet.restore.dialog.step.walletKind.label.daedalusWalletKind',
    defaultMessage:
      '!!!What kind of Daedalus wallet would you like to restore?',
    description: 'Label for the "labelDaedalusWalletKind" checkbox.',
  },
  labelDaedalusWalletKind12WordByron: {
    id:
      'wallet.restore.dialog.step.walletKind.label.daedalusWalletKind12WordByron',
    defaultMessage: '!!!12 words <em>(Byron legacy wallet)</em>',
    description: 'Label for the "labelDaedalusWalletKind12WordByron" checkbox.',
  },
  labelDaedalusWalletKind15WordShelley: {
    id:
      'wallet.restore.dialog.step.walletKind.label.daedalusWalletKind15WordShelley',
    defaultMessage: '!!!15 words <em>(Shelley wallet)</em>',
    description:
      'Label for the "labelDaedalusWalletKind15WordShelley" checkbox.',
  },
  labelDaedalusWalletKind27WordPaper: {
    id:
      'wallet.restore.dialog.step.walletKind.label.daedalusWalletKind27WordPaper',
    defaultMessage: '!!!27 words - paper wallet (Byron legacy wallet)</em>',
    description: 'Label for the "labelDaedalusWalletKind27WordPaper" checkbox.',
  },
  labelYoroiWalletKind: {
    id: 'wallet.restore.dialog.step.walletKind.label.yoroiWalletKind',
    defaultMessage: '!!!What kind of Yoroi wallet would you like to restore?',
    description: 'Label for the "labelYoroiWalletKind" checkbox.',
  },
  labelYoroiWalletKind15WordByron: {
    id:
      'wallet.restore.dialog.step.walletKind.label.yoroiWalletKindByronLegacy15Word',
    defaultMessage: '!!!15 words <em>(Byron legacy wallet)</em>',
    description: 'Label for the "labelDaedalusWalletKind15WordByron" checkbox.',
  },
  labelYoroiWalletKind15WordShelley: {
    id:
      'wallet.restore.dialog.step.walletKind.label.yoroiWalletKindShelley15Word',
    defaultMessage: '!!!15 words <em>(Shelley wallet)</em>',
    description:
      'Label for the "labelDaedalusWalletKind15WordShelley" checkbox.',
  },
  labelHardwareWalletKind: {
    id: 'wallet.restore.dialog.step.walletKind.label.hardwareWalletKind',
    defaultMessage:
      '!!!What kind of hardware wallet would you like to restore?',
    description: 'Label for the "labelHardwareWalletKind" checkbox.',
  },
  labelHardwareWalletKindLedger: {
    id: 'wallet.restore.dialog.step.walletKind.label.hardwareWalletKindLedger',
    defaultMessage: '!!!24 words - Ledger Nano S or Nano X (Balance wallet)',
    description: 'Label for the "labelHardwareWalletKindLedger" checkbox.',
  },
  labelHardwareWalletKindTrezor: {
    id: 'wallet.restore.dialog.step.walletKind.label.hardwareWalletKindTrezor',
    defaultMessage: '!!!24 words - Trezor (Balance wallet)',
    description: 'Label for the "labelHardwareWalletKindTrezor" checkbox.',
  },
  hardwareWalletDisclaimer1: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletDisclaimer1',
    defaultMessage:
      '!!!Hardware wallets keep your private keys stored securely on a physical device that is immune to common computer threats such as viruses and software bugs. Recovery phrases for hardware wallets should always be kept offline. By entering your hardware wallet recovery phrase in Daedalus, you are exposing your hardware wallet private keys to the security risks associated with computers and software.',
    description: 'Label for the "hardwareWalletDisclaimer1" disclaimer.',
  },
  hardwareWalletDisclaimer2: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletDisclaimer2',
    defaultMessage:
      '!!!We strongly recommend that you delete the Balance wallet which is restored from your hardware wallet once you have moved any funds into a Rewards wallet.',
    description: 'Label for the "hardwareWalletDisclaimer2" disclaimer.',
  },
  hardwareWalletCheckbox1: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletCheckbox1',
    defaultMessage:
      '!!!I understand and accept responsibility for the security concerns of restoring a hardware wallet on a computer.',
    description: 'Label for the "hardwareWalletCheckbox1" disclaimer.',
  },
  hardwareWalletCheckbox2: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletCheckbox2',
    defaultMessage:
      '!!!I understand that I should delete the Balance wallet I am restoring from a hardware wallet after moving funds to a Rewards wallet.',
    description: 'Label for the "hardwareWalletCheckbox2" disclaimer.',
  },
});

type Props = {
  onContinue: Function,
  onClose: Function,
  onSetWalletKind: Function,
  walletKind: ?WalletKind,
  walletKindDaedalus: ?WalletDaedalusKind,
  walletKindYoroi: ?WalletYoroiKind,
  walletKindHardware: ?WalletHardwareKind,
};

type State = {
  [key: HardwareWalletAcceptance]: boolean,
};

const { isIncentivizedTestnet, isShelleyTestnet } = global;

export default class WalletTypeDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    hardwareWalletAcceptance1: false,
    hardwareWalletAcceptance2: false,
  };

  toggleAcceptance = (param: HardwareWalletAcceptance) =>
    this.setState(currentState => set({}, param, !currentState[param]));

  getWalletKind = (
    kinds: Object,
    message: string,
    value: ?string,
    kindParam?: string
  ) => (
    <RadioSet
      label={this.context.intl.formatMessage(message)}
      items={Object.keys(kinds).map((key: string) => {
        const kind: WalletKinds = kinds[key];
        const messageParam = `label${kindParam || ''}WalletKind${kind}`;
        const msg = messages[messageParam];
        if (!msg) {
          throw new Error(`Missing ${messageParam} message`);
        }
        return {
          key: kind,
          disabled: false,
          label: <FormattedHTMLMessage {...msg} />,
          selected: value === kind,
          onChange: () => this.props.onSetWalletKind(kind, kindParam),
        };
      })}
      verticallyAligned
    />
  );

  get isDisabled() {
    const {
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware,
    } = this.props;
    const { hardwareWalletAcceptance1, hardwareWalletAcceptance2 } = this.state;
    if (!walletKind) return true;
    if (walletKind === WALLET_KINDS.DAEDALUS && !walletKindDaedalus)
      return true;
    if (
      walletKind === WALLET_KINDS.YOROI &&
      !walletKindYoroi &&
      isIncentivizedTestnet
    )
      return true;
    if (
      walletKind === WALLET_KINDS.HARDWARE &&
      (!walletKindHardware ||
        !hardwareWalletAcceptance1 ||
        !hardwareWalletAcceptance2)
    ) {
      return true;
    }
    return false;
  }

  render() {
    const { intl } = this.context;
    const {
      onClose,
      onContinue,
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware,
    } = this.props;
    const { hardwareWalletAcceptance1, hardwareWalletAcceptance2 } = this.state;
    return (
      <WalletRestoreDialog
        stepNumber={0}
        actions={[
          {
            primary: true,
            label: intl.formatMessage(globalMessages.dialogButtonContinueLabel),
            onClick: onContinue,
            disabled: this.isDisabled,
          },
        ]}
        onClose={onClose}
      >
        <div className={styles.component}>
          {this.getWalletKind(
            WALLET_KINDS,
            messages.labelWalletKind,
            walletKind
          )}
        </div>
        <div>
          {walletKind === WALLET_KINDS.DAEDALUS &&
            this.getWalletKind(
              WALLET_DAEDALUS_KINDS,
              messages.labelDaedalusWalletKind,
              walletKindDaedalus,
              WALLET_KINDS.DAEDALUS
            )}
          {isIncentivizedTestnet &&
            walletKind === WALLET_KINDS.YOROI &&
            this.getWalletKind(
              WALLET_YOROI_KINDS,
              messages.labelYoroiWalletKind,
              walletKindYoroi,
              WALLET_KINDS.YOROI
            )}
          {isIncentivizedTestnet &&
            !isShelleyTestnet &&
            walletKind === WALLET_KINDS.HARDWARE && (
              <Fragment>
                {this.getWalletKind(
                  WALLET_HARDWARE_KINDS,
                  messages.labelHardwareWalletKind,
                  walletKindHardware,
                  WALLET_KINDS.HARDWARE
                )}
                <p className={styles.hardwareWalletAcceptance}>
                  {intl.formatMessage(messages.hardwareWalletDisclaimer1)}
                </p>
                <p className={styles.hardwareWalletAcceptance}>
                  <b>
                    {intl.formatMessage(messages.hardwareWalletDisclaimer2)}
                  </b>
                </p>
                <Checkbox
                  className="restoreSecurityNote"
                  label={intl.formatMessage(messages.hardwareWalletCheckbox1)}
                  onChange={() =>
                    this.toggleAcceptance('hardwareWalletAcceptance1')
                  }
                  checked={hardwareWalletAcceptance1}
                  skin={CheckboxSkin}
                />
                <Checkbox
                  className="walletDeleteNote"
                  label={intl.formatMessage(messages.hardwareWalletCheckbox2)}
                  onChange={() =>
                    this.toggleAcceptance('hardwareWalletAcceptance2')
                  }
                  checked={hardwareWalletAcceptance2}
                  skin={CheckboxSkin}
                />
              </Fragment>
            )}
        </div>
      </WalletRestoreDialog>
    );
  }
}
