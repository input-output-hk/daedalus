import React, { Component, Fragment } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { set } from 'lodash';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import RadioSet from '../../widgets/RadioSet';
import WalletRestoreDialog from './widgets/WalletRestoreDialog';
import globalMessages from '../../../i18n/global-messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletTypeDialog.scss' or it... Remove this comment to see the full error message
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
    defaultMessage:
      '!!!15 words <em>(Incentivized Testnet Rewards wallet)</em>',
    description:
      'Label for the "labelDaedalusWalletKind15WordShelley" checkbox.',
  },
  labelDaedalusWalletKind24WordShelley: {
    id:
      'wallet.restore.dialog.step.walletKind.label.daedalusWalletKind24WordShelley',
    defaultMessage: '!!!24 words <em>(Shelley wallet)</em>',
    description:
      'Label for the "labelDaedalusWalletKind24WordShelley" checkbox.',
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
    defaultMessage: '!!!24 words - Ledger (Byron legacy wallet)',
    description: 'Label for the "labelHardwareWalletKindLedger" checkbox.',
  },
  labelHardwareWalletKindTrezor: {
    id: 'wallet.restore.dialog.step.walletKind.label.hardwareWalletKindTrezor',
    defaultMessage: '!!!24 words - Trezor (Byron legacy wallet)',
    description: 'Label for the "labelHardwareWalletKindTrezor" checkbox.',
  },
  hardwareWalletDisclaimer1: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletDisclaimer1',
    defaultMessage:
      '!!!Hardware wallets store your private keys securely on a physical device so they are immune to common computer threats such as viruses and software bugs. Recovery phrases for hardware wallets should always be kept offline. By entering your hardware wallet recovery phrase in Daedalus, you expose your hardware wallet private keys to the security risks associated with computers and software.',
    description: 'Label for the "hardwareWalletDisclaimer1" disclaimer.',
  },
  hardwareWalletDisclaimer2: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletDisclaimer2',
    defaultMessage:
      '!!!All of your assets held on your hardware wallet device are associated with the same wallet recovery phrase and its corresponding private key. If you hold assets other than ada on your hardware wallet device, you expose all of those assets to security risks.',
    description: 'Label for the "hardwareWalletDisclaimer2" disclaimer.',
  },
  hardwareWalletDisclaimer3: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletDisclaimer3',
    defaultMessage:
      '!!!We strongly recommend that you delete the Byron legacy wallet that was restored from your hardware wallet once you have moved funds into a Shelley wallet.',
    description: 'Label for the "hardwareWalletDisclaimer3" disclaimer.',
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
      '!!!I understand that I should delete the Byron legacy wallet I am restoring from a hardware wallet after moving funds to a Shelley wallet.',
    description: 'Label for the "hardwareWalletCheckbox2" disclaimer.',
  },
  hardwareWalletCheckbox3: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletCheckbox3',
    defaultMessage:
      '!!!I understand that I am exposing all of the assets that are stored on my hardware wallet device, and not just ada, to security risks.',
    description: 'Label for the "hardwareWalletCheckbox2" disclaimer.',
  },
});
type Props = {
  onContinue: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  onSetWalletKind: (...args: Array<any>) => any;
  walletKind: WalletKind | null | undefined;
  walletKindDaedalus: WalletDaedalusKind | null | undefined;
  walletKindYoroi: WalletYoroiKind | null | undefined;
  walletKindHardware: WalletHardwareKind | null | undefined;
};
type State = Record<HardwareWalletAcceptance, boolean>;
export default class WalletTypeDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    hardwareWalletAcceptance1: false,
    hardwareWalletAcceptance2: false,
    hardwareWalletAcceptance3: false,
  };
  toggleAcceptance = (param: HardwareWalletAcceptance) =>
    this.setState((currentState) => set({}, param, !currentState[param]));
  getWalletKind = (
    kinds: Record<string, any>,
    message: string,
    value: string | null | undefined,
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
    const {
      hardwareWalletAcceptance1,
      hardwareWalletAcceptance2,
      hardwareWalletAcceptance3,
    } = this.state;
    if (!walletKind) return true;
    if (walletKind === WALLET_KINDS.DAEDALUS && !walletKindDaedalus)
      return true;
    if (walletKind === WALLET_KINDS.YOROI && !walletKindYoroi) return true;
    return (
      walletKind === WALLET_KINDS.HARDWARE &&
      (!walletKindHardware ||
        !hardwareWalletAcceptance1 ||
        !hardwareWalletAcceptance2 ||
        !hardwareWalletAcceptance3)
    );
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
    const {
      hardwareWalletAcceptance1,
      hardwareWalletAcceptance2,
      hardwareWalletAcceptance3,
    } = this.state;
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
          {walletKind === WALLET_KINDS.YOROI &&
            this.getWalletKind(
              WALLET_YOROI_KINDS,
              messages.labelYoroiWalletKind,
              walletKindYoroi,
              WALLET_KINDS.YOROI
            )}
          {walletKind === WALLET_KINDS.HARDWARE && (
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
                {intl.formatMessage(messages.hardwareWalletDisclaimer2)}
              </p>
              <p className={styles.hardwareWalletAcceptance}>
                <b>{intl.formatMessage(messages.hardwareWalletDisclaimer3)}</b>
              </p>
              <Checkbox
                className="walletSecurityRisk"
                label={intl.formatMessage(messages.hardwareWalletCheckbox3)}
                onChange={() =>
                  this.toggleAcceptance('hardwareWalletAcceptance3')
                }
                checked={hardwareWalletAcceptance3}
                skin={CheckboxSkin}
              />
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
