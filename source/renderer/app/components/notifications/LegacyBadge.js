// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './LegacyBadge.scss';

const messages = defineMessages({
  label: {
    id: 'wallet.legacy.badge.label',
    defaultMessage: '!!!Legacy',
    description: 'Label "Legacy" on the legacy badge.',
  },
});

@observer
export default class LegacyBadge extends Component<any> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const label = intl.formatMessage(messages.label);

    return <div className={styles.component}>{label}</div>;
  }
}
