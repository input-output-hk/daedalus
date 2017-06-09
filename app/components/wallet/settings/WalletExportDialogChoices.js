// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletExportDialogChoices.scss';

const messages = defineMessages({
  fullTabTitle: {
    id: 'wallet.export.choices.tab.title.full',
    defaultMessage: '!!!Full',
    description: 'Tab title "Full" on wallet export dialog.'
  },
  readOnlyTabTitle: {
    id: 'wallet.export.choices.tab.title.readOnly',
    defaultMessage: '!!!Read-only',
    description: 'Tab title "Read-only" on wallet export dialog.'
  },
  paperWalletTabTitle: {
    id: 'wallet.export.choices.tab.title.paperWallet',
    defaultMessage: '!!!Paper wallet',
    description: 'Tab title "Paper Wallet" on wallet export dialog.'
  },
});

@observer
export default class WalletExportDialogChoices extends Component {

  props: {
    activeChoice: string,
    onSelectChoice: Function,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { activeChoice, onSelectChoice } = this.props;

    return (
      <div className={styles.component}>
        <button
          disabled
          className={activeChoice === 'full' ? styles.activeButton : ''}
          onClick={() => onSelectChoice('full')}
        >
          {intl.formatMessage(messages.fullTabTitle)}
        </button>
        <button
          disabled
          className={activeChoice === 'readOnly' ? styles.activeButton : ''}
          onClick={() => onSelectChoice('readOnly')}
        >
          {intl.formatMessage(messages.readOnlyTabTitle)}
        </button>
        <button
          className={activeChoice === 'paperWallet' ? styles.activeButton : ''}
          onClick={() => onSelectChoice('paperWallet')}
        >
          {intl.formatMessage(messages.paperWalletTabTitle)}
        </button>
      </div>
    );
  }

}
