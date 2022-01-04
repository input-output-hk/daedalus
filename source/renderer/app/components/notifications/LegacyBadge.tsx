import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './LegacyBadge.scss' or its cor... Remove this comment to see the full error message
import styles from './LegacyBadge.scss';

const messages = defineMessages({
  label: {
    id: 'wallet.byron.badge.label',
    defaultMessage: '!!!Byron',
    description: 'Label "Byron" on the legacy badge.',
  },
});
export const LEGACY_BADGE_MODES = {
  FLOATING: 'floating',
  NATURAL: 'natural',
};
type Props = {
  mode: string;
};

@observer
class LegacyBadge extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { mode } = this.props;
    const label = intl.formatMessage(messages.label);
    const stylesClassName = classNames([
      styles.component,
      mode === LEGACY_BADGE_MODES.FLOATING ? styles.floating : styles.natural,
    ]);
    return <div className={stylesClassName}>{label}</div>;
  }
}

export default LegacyBadge;
