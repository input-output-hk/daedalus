// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './AdaRedemptionChoices.scss';
import { ADA_REDEMPTION_TYPES } from '../../../types/redemptionTypes';
import type { RedemptionTypeChoices } from '../../../types/redemptionTypes';

const messages = defineMessages({
  regularTabTitle: {
    id: 'wallet.redeem.choices.tab.title.regularVended',
    defaultMessage: '!!!Regular',
    description: 'Tab title "Regular" on Ada redemption page.',
  },
  forceVendedTabTitle: {
    id: 'wallet.redeem.choices.tab.title.forceVended',
    defaultMessage: '!!!Force vended',
    description: 'Tab title "Force vended" on Ada redemption page.',
  },
  paperVendedTabTitle: {
    id: 'wallet.redeem.choices.tab.title.paperVended',
    defaultMessage: '!!!Paper vended',
    description: 'Tab title "Paper vended" on Ada redemption page.',
  },
  recoveryRegularTabTitle: {
    id: 'wallet.redeem.choices.tab.title.recoveryRegular',
    defaultMessage: '!!!Recovery - regular',
    description: 'Tab title "Recovery - regular" on Ada redemption page.',
  },
  recoveryForceVendedTabTitle: {
    id: 'wallet.redeem.choices.tab.title.recoveryForceVended',
    defaultMessage: '!!!Recovery - force vended',
    description: 'Tab title "Recovery - force vended" on Ada redemption page.',
  },
});

type Props = {
  activeChoice: RedemptionTypeChoices,
  onSelectChoice: Function,
};

@observer
export default class AdaRedemptionChoices extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { activeChoice, onSelectChoice } = this.props;
    return (
      <div className={styles.component}>
        <button
          className={
            activeChoice === ADA_REDEMPTION_TYPES.REGULAR
              ? styles.activeButton
              : ''
          }
          onClick={() => onSelectChoice(ADA_REDEMPTION_TYPES.REGULAR)}
        >
          {intl.formatMessage(messages.regularTabTitle)}
        </button>
        <button
          className={
            activeChoice === ADA_REDEMPTION_TYPES.FORCE_VENDED
              ? styles.activeButton
              : ''
          }
          onClick={() => onSelectChoice(ADA_REDEMPTION_TYPES.FORCE_VENDED)}
        >
          {intl.formatMessage(messages.forceVendedTabTitle)}
        </button>
        <button
          className={
            activeChoice === ADA_REDEMPTION_TYPES.PAPER_VENDED
              ? styles.activeButton
              : ''
          }
          onClick={() => onSelectChoice(ADA_REDEMPTION_TYPES.PAPER_VENDED)}
        >
          {intl.formatMessage(messages.paperVendedTabTitle)}
        </button>
        <button
          className={
            activeChoice === ADA_REDEMPTION_TYPES.RECOVERY_REGULAR
              ? styles.activeButton
              : ''
          }
          onClick={() => onSelectChoice(ADA_REDEMPTION_TYPES.RECOVERY_REGULAR)}
        >
          {intl.formatMessage(messages.recoveryRegularTabTitle)}
        </button>
        <button
          className={
            activeChoice === ADA_REDEMPTION_TYPES.RECOVERY_FORCE_VENDED
              ? styles.activeButton
              : ''
          }
          onClick={() =>
            onSelectChoice(ADA_REDEMPTION_TYPES.RECOVERY_FORCE_VENDED)
          }
        >
          {intl.formatMessage(messages.recoveryForceVendedTabTitle)}
        </button>
      </div>
    );
  }
}
