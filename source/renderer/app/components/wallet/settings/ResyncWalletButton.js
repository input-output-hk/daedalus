// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './ResyncWalletButton.scss';

const messages = defineMessages({
  label: {
    id: 'wallet.settings.resyncWalletButtonLabel',
    defaultMessage: '!!!Resync wallet',
    description: 'Label for the resync button on wallet settings',
  },
});

type Props = {
  isSubmitting: boolean,
  onClick: Function,
};

export default class ResyncWalletButton extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { onClick, isSubmitting } = this.props;
    const label = this.context.intl.formatMessage(messages.label);
    const buttonClasses = classNames([
      'flat',
      isSubmitting ? styles.isSubmitting : null,
    ]);

    return (
      <Button
        className={buttonClasses}
        disabled={false}
        label={label}
        onClick={onClick}
        skin={ButtonSkin}
        themeOverrides={styles}
      />
    );
  }
}
