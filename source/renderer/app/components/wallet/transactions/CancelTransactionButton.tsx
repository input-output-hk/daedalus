import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './CancelTransactionButton.scss... Remove this comment to see the full error message
import styles from './CancelTransactionButton.scss';

const messages = defineMessages({
  cancelLabel: {
    id: 'wallet.transaction.pending.cancelTransactionButton',
    defaultMessage: '!!!Cancel pending transaction',
    description: 'Label for the cancel pending transaction button',
  },
  removeLabel: {
    id: 'wallet.transaction.failed.removeTransactionButton',
    defaultMessage: '!!!Remove failed transaction',
    description: 'Label for the remove failed transaction button',
  },
});
type Props = {
  onClick: (...args: Array<any>) => any;
  state: 'cancel' | 'remove';
};
export default class CancelTransactionButton extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { onClick, state } = this.props;
    const label = this.context.intl.formatMessage(messages[`${state}Label`]);
    return (
      <Button
        className="attention"
        disabled={false}
        label={label}
        onClick={onClick}
        skin={ButtonSkin}
        themeOverrides={styles}
      />
    );
  }
}
