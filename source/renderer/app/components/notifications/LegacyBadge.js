// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import styles from './LegacyBadge.scss';

const messages = defineMessages({
  labelBalance: {
    id: 'wallet.legacy.badge.labelBalance',
    defaultMessage: '!!!Balance',
    description: 'Label "Balance" on the legacy badge.',
  },
  labelLegacy: {
    id: 'wallet.legacy.badge.labelLegacy',
    defaultMessage: '!!!Legacy',
    description: 'Label "Legacy" on the legacy badge.',
  },
});

export const LEGACY_BADGE_MODES = {
  FLOATING: 'floating',
  NATURAL: 'natural',
};

type Props = {
  mode: string,
  isFriendsAndFamily: boolean,
};

@observer
export default class LegacyBadge extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { mode, isFriendsAndFamily } = this.props;
    const labelLegacy = intl.formatMessage(messages.labelLegacy);
    const labelBalance = intl.formatMessage(messages.labelBalance);
    const stylesClassName = classNames([
      styles.component,
      mode === LEGACY_BADGE_MODES.FLOATING ? styles.floating : styles.natural,
    ]);

    return (
      <div className={stylesClassName}>
        {isFriendsAndFamily ? labelLegacy : labelBalance}
      </div>
    );
  }
}
