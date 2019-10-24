// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './DeleteWalletButton.scss';

const messages = defineMessages({
  label: {
    id: 'wallet.settings.deleteWalletButtonLabel',
    defaultMessage: '!!!Delete wallet',
    description: 'Label for the delete button on wallet settings',
  },
});

type Props = {
  onClick: Function,
};

export default class DeleteWalletButton extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { onClick } = this.props;
    const label = this.context.intl.formatMessage(messages.label);
    return (
      <Button
        className="flat"
        disabled={false}
        label={label}
        onClick={onClick}
        skin={ButtonSkin}
        themeOverrides={styles}
      />
    );
  }
}
