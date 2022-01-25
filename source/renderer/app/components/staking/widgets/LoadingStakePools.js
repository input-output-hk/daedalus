// @flow
import React from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import styles from './LoadingStakePools.scss';

const messages = defineMessages({
  loadingStakePoolsMessage: {
    id: 'staking.stakePools.loadingStakePoolsMessage',
    defaultMessage: '!!!Loading stake pools',
    description:
      'Loading stake pool message for the Delegation center body section.',
  },
});

type Props = {
  intl: intlShape.isRequired,
};

const LoadingStakePools = observer(({ intl }: Props) => (
  <div className={styles.component}>
    <p>{intl.formatMessage(messages.loadingStakePoolsMessage)}</p>
    <LoadingSpinner big />
  </div>
));

export default injectIntl(LoadingStakePools);
