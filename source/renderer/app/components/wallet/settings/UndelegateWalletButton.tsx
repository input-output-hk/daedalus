import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './UndelegateWalletButton.scss'... Remove this comment to see the full error message
import styles from './UndelegateWalletButton.scss';

const messages = defineMessages({
  label: {
    id: 'wallet.settings.undelegateWalletButtonLabel',
    defaultMessage: '!!!Undelegate',
    description: 'Label for the undelegate button on wallet settings',
  },
});
type Props = {
  disabled?: boolean;
  onUndelegate: (...args: Array<any>) => any;
};
export default class UndelegateWalletButton extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { disabled, onUndelegate } = this.props;
    const label = this.context.intl.formatMessage(messages.label);
    return (
      <Button
        className="flat"
        label={label}
        disabled={disabled}
        onClick={onUndelegate}
        themeOverrides={styles}
      />
    );
  }
}
