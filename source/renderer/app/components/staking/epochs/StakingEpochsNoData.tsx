import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingEpochs.scss' or its c... Remove this comment to see the full error message
import styles from './StakingEpochs.scss';

const messages = defineMessages({
  noResults: {
    id: 'staking.epochs.no.results',
    defaultMessage: '!!!No results',
    description: '"No results" results label on staking epochs page.',
  },
});

@observer
class StakingEpochsNoData extends Component<any> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    return (
      <div className={styles.noResultsLabel}>
        {intl.formatMessage(messages.noResults)}
      </div>
    );
  }
}

export default StakingEpochsNoData;
