// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletRecoveryPhraseShowDialog.scss';
import closeCross from '../../assets/images/close-cross.svg';

const messages = defineMessages({
  recoveryPhrase: {
    id: 'wallet.recovery.phrase.show.dialog.title',
    defaultMessage: '!!!Recovery phrase',
    description: 'Title for the "Recovery Phrase" dialog.'
  },
  instructions: {
    id: 'wallet.recovery.phrase.show.dialog.instructions',
    defaultMessage: `Please, make sure you have carefully written down your recovery phrase somewhere safe. 
    You will need this phrase later for next use and recover. Phrase is case sensitive.`,
    description: 'Label for the "Currency" dropdown in the wallet create form.'
  },
  buttonLabel: {
    id: 'wallet.recovery.phrase.show.dialog.button.label',
    defaultMessage: 'Yes, I’ve written it down',
    description: 'Label for button "Yes, I’ve written it down" on the dialog that shows wallet recovery phrase.'
  },
});

@observer
export default class WalletRecoveryPhraseShowDialog extends Component {

  static propTypes = {
    recoveryPhrase: PropTypes.string.isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  actions = [
    {
      label: this.context.intl.formatMessage(messages.buttonLabel),
      onClick: this.submit
    }
  ];

  submit = () => {

  };

  render() {
    const { intl } = this.context;
    const { recoveryPhrase } = this.props;
    return (
      <Dialog
        title={intl.formatMessage(messages.recoveryPhrase)}
        actions={this.actions}
        active
        style={styles.component}
      >
        <div className={styles.instructions}>{intl.formatMessage(messages.instructions)}</div>
        <div className={styles.recoveryPhrase}>{recoveryPhrase}</div>
        <div className={styles.closeButton}>
          <img src={closeCross} role="presentation" />
        </div>
      </Dialog>
    );
  }

}
