import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegateWalletButton.scss' o... Remove this comment to see the full error message
import styles from './DelegateWalletButton.scss';

const messages = defineMessages({
  label: {
    id: 'wallet.settings.delegateWalletButtonLabel',
    defaultMessage: '!!!Delegate',
    description: 'Label for the delegate button on wallet settings',
  },
});
type Props = {
  disabled?: boolean;
  onDelegate: (...args: Array<any>) => any;
};
export default class DelegateWalletButton extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { disabled, onDelegate } = this.props;
    const label = this.context.intl.formatMessage(messages.label);
    return (
      <Button
        label={label}
        disabled={disabled}
        onClick={onDelegate}
        themeOverrides={styles}
      />
    );
  }
}
