import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './StakingEpochs.scss';

const messages = defineMessages({
  noResults: {
    id: 'staking.epochs.no.results',
    defaultMessage: '!!!No results',
    description: '"No results" results label on staking epochs page.',
  },
});

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

export default observer(StakingEpochsNoData);
