// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './AssetToken.scss';
import { ellipsis } from '../../utils/strings';
import type { WalletSummaryAsset } from '../../api/assets/types';

type Props = {
  asset: WalletSummaryAsset,
  className?: string,
};

@observer
export default class Token extends Component<Props> {
  render() {
    const { asset, className } = this.props;
    const { total, metadata } = asset;
    console.log('asset', asset);
    const componentClasses = classnames([styles.component, className]);
    return (
      <div className={componentClasses}>
        <div className={styles.policyId}>{ellipsis(total.policyId, 9, 4)}</div>
        {total.assetName && (
          <div className={styles.assetName}>{total.assetName}</div>
        )}
        {metadata && <div className={styles.metadata}>METADATA CONTENT</div>}
      </div>
    );
  }
}
