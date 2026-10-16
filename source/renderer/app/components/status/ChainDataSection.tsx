import React, { Component } from 'react';
import classNames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';

import globalMessages from '../../i18n/global-messages';
import type { Intl } from '../../types/i18nTypes';
import type { ChainDamage } from '../../../../common/types/watchdog.types';
import chainDamageMessages from '../loading/chain-damage/ChainDamage.messages';
import Dialog from '../widgets/Dialog';
import DialogBackButton from '../widgets/DialogBackButton';
import styles from './DaedalusDiagnostics.scss';

export const messages = defineMessages({
  sectionLabel: {
    id: 'daedalus.diagnostics.dialog.chainDataSectionLabel',
    defaultMessage: '!!!Blockchain data',
    description:
      'Row label for the blockchain data status and rebuild action in the diagnostics dialog',
  },
  statusOk: {
    id: 'daedalus.diagnostics.dialog.chainDataStatusOk',
    defaultMessage: '!!!No damage detected',
    description:
      'Status shown in diagnostics when no gap in the blockchain data was detected in this session',
  },
  statusDamage: {
    id: 'daedalus.diagnostics.dialog.chainDataStatusDamage',
    defaultMessage: '!!!Damage detected at slot {slot} ({time})',
    description:
      'Status shown in diagnostics with the slot and time of the last gap in the blockchain data detected in this session',
  },
  statusDamageNoSlot: {
    id: 'daedalus.diagnostics.dialog.chainDataStatusDamageNoSlot',
    defaultMessage: '!!!Damage detected ({time})',
    description:
      'Status shown in diagnostics with the time of the last gap in the blockchain data detected in this session, when no slot is known',
  },
  rebuildButton: {
    id: 'daedalus.diagnostics.dialog.chainDataRebuildButton',
    defaultMessage: '!!!Rebuild blockchain data',
    description:
      'Button label that opens the confirmation for rebuilding the blockchain data from diagnostics',
  },
  rebuildHintBlocked: {
    id: 'daedalus.diagnostics.dialog.chainDataRebuildHintBlocked',
    defaultMessage: '!!!Unavailable while Mithril work is already active.',
    description:
      'Hint text beneath the disabled rebuild button in diagnostics while Mithril work is active',
  },
});

// The status line of the Blockchain data row, for the dialog and for the
// copied diagnostics text
export const formatChainDataStatus = (
  intl: Intl,
  chainDamage: ChainDamage | null
): string => {
  if (chainDamage == null) {
    return intl.formatMessage(messages.statusOk);
  }
  const time = new Date(chainDamage.detectedAt).toLocaleString(intl.locale);
  return chainDamage.slot == null
    ? intl.formatMessage(messages.statusDamageNoSlot, { time })
    : intl.formatMessage(messages.statusDamage, {
        slot: chainDamage.slot,
        time,
      });
};

type Props = {
  chainDamage: ChainDamage | null;
  isActionBlocked: boolean;
  onRestoreFocus: () => void;
  onRebuildChain: () => void;
};

type State = {
  isShowingConfirmation: boolean;
};

export default class ChainDataSection extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state: State = { isShowingConfirmation: false };

  showConfirmation = () => {
    if (this.props.isActionBlocked) return;
    this.setState({ isShowingConfirmation: true });
    this.props.onRestoreFocus();
  };

  hideConfirmation = () => {
    this.setState({ isShowingConfirmation: false });
    this.props.onRestoreFocus();
  };

  // The only caller of onRebuildChain, so the confirmation always precedes the
  // rebuild.
  rebuild = () => {
    if (this.props.isActionBlocked) return;
    this.props.onRebuildChain();
    this.hideConfirmation();
  };

  render() {
    const { isActionBlocked } = this.props;
    const { isShowingConfirmation } = this.state;
    const { intl } = this.context;

    if (isShowingConfirmation) {
      return (
        <Dialog
          className={styles.mithrilPartialSyncConfirmationDialog}
          title={intl.formatMessage(chainDamageMessages.confirmTitle)}
          closeOnOverlayClick
          primaryButtonAutoFocus
          backButton={<DialogBackButton onBack={this.hideConfirmation} />}
          onClose={this.hideConfirmation}
          actions={[
            {
              label: intl.formatMessage(chainDamageMessages.confirmBack),
              onClick: this.hideConfirmation,
              className: styles.mithrilPartialSyncConfirmationSecondaryButton,
            },
            {
              label: intl.formatMessage(chainDamageMessages.confirmRebuild),
              primary: true,
              disabled: isActionBlocked,
              onClick: this.rebuild,
              className: styles.mithrilPartialSyncConfirmationPrimaryButton,
            },
          ]}
        >
          <div className={styles.mithrilPartialSyncConfirmationBody}>
            <p>{intl.formatMessage(chainDamageMessages.confirmBody)}</p>
          </div>
        </Dialog>
      );
    }

    return (
      <div className={styles.layoutRow}>
        <div className={styles.layoutHeader}>
          {intl.formatMessage(messages.sectionLabel)}
          {intl.formatMessage(globalMessages.punctuationColon)}
        </div>
        <div
          className={classNames(
            styles.layoutData,
            styles.mithrilPartialSyncData
          )}
        >
          <div className={styles.mithrilPartialSyncRecommendation}>
            <div
              className={classNames(
                styles.chainDataStatus,
                this.props.chainDamage != null && styles.chainDataDamaged
              )}
            >
              {formatChainDataStatus(intl, this.props.chainDamage)}
            </div>
            <button
              className={styles.mithrilPartialSyncButton}
              disabled={isActionBlocked}
              onClick={this.showConfirmation}
              type="button"
            >
              {intl.formatMessage(messages.rebuildButton)}
            </button>
            {isActionBlocked && (
              <div className={styles.mithrilPartialSyncHint}>
                {intl.formatMessage(messages.rebuildHintBlocked)}
              </div>
            )}
          </div>
        </div>
      </div>
    );
  }
}
