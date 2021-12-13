import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './FilterResultInfo.scss' or it... Remove this comment to see the full error message
import styles from './FilterResultInfo.scss';

const messages = defineMessages({
  resultInfo: {
    id: 'wallet.transaction.filter.resultInfo',
    defaultMessage:
      '!!!<b>{filtered}</b> out of <b>{total}</b> transactions match your filter.',
    description: 'Filter result info.',
  },
});
type Props = {
  filtered: number;
  total: number;
};

@observer
class FilterResultInfo extends Component<Props> {
  render() {
    const { filtered, total } = this.props;
    const resultInfo = (
      <div className={styles.resultInfo}>
        <FormattedHTMLMessage
          {...messages.resultInfo}
          values={{
            filtered,
            total,
          }}
        />
      </div>
    );
    return <div className={styles.component}>{resultInfo}</div>;
  }
}

export default FilterResultInfo;
