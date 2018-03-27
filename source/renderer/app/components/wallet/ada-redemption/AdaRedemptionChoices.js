// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './AdaRedemptionChoices.scss';

const messages = defineMessages({
  regularTabTitle: {
    id: 'wallet.redeem.choices.tab.title.regularVended',
    defaultMessage: '!!!Regular',
    description: 'Tab title "Regular" on Ada redemption page.'
  },
  forceVendedTabTitle: {
    id: 'wallet.redeem.choices.tab.title.forceVended',
    defaultMessage: '!!!Force vended',
    description: 'Tab title "Force vended" on Ada redemption page.'
  },
  paperVendedTabTitle: {
    id: 'wallet.redeem.choices.tab.title.paperVended',
    defaultMessage: '!!!Paper vended',
    description: 'Tab title "Paper vended" on Ada redemption page.'
  },
});

type Props = {
  activeChoice: string,
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
          className={activeChoice === 'regular' ? styles.activeButton : ''}
          onClick={() => onSelectChoice('regular')}
        >
          {intl.formatMessage(messages.regularTabTitle)}
        </button>
        <button
          className={activeChoice === 'forceVended' ? styles.activeButton : ''}
          onClick={() => onSelectChoice('forceVended')}
        >
          {intl.formatMessage(messages.forceVendedTabTitle)}
        </button>
        <button
          className={activeChoice === 'paperVended' ? styles.activeButton : ''}
          onClick={() => onSelectChoice('paperVended')}
        >
          {intl.formatMessage(messages.paperVendedTabTitle)}
        </button>
      </div>
    );
  }

}
