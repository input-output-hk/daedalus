// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './CancelTransactionButton.scss';

const messages = defineMessages({
  label: {
    id: 'wallet.transaction.pending.cancelTransactionButton',
    defaultMessage: '!!!Cancel pending transaction',
    description: 'Label for the cancel pending transaction button',
  },
});

type Props = {
  onClick: Function,
};

export default class CancelTransactionButton extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { onClick } = this.props;
    const label = this.context.intl.formatMessage(messages.label);
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
