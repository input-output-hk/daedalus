import React, { Component, PropTypes } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './DeleteWalletButton.scss';

const messages = defineMessages({
  label: {
    id: 'wallet.settings.deleteWalletButtonLabel',
    defaultMessage: '!!!Delete wallet',
    description: 'Label for the delete button on wallet settings',
  },
});

export default class DeleteWalletButton extends Component {

  static propTypes = {
    onClick: PropTypes.func
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { onClick } = this.props;
    return (
      <button onClick={onClick} className={styles.button}>
        {this.context.intl.formatMessage(messages.label)}
      </button>
    );
  }
}
