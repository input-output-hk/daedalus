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
  // In case it's not possible to calculate the container width
  // this props defines after how many characters the text will cut off
  policyIdEllipsisLeft?: number,
};

@observer
export default class AssetToken extends Component<Props> {
  render() {
    const { asset, className, policyIdEllipsisLeft } = this.props;
    const { fingerprint, policyId, metadata } = asset;
    const componentClasses = classnames([styles.component, className]);
    return (
      <div className={componentClasses}>
        <div className={styles.content}>
          <div className={styles.fingerprint}>
            {ellipsis(fingerprint || '', 9, 4)}
          </div>
          <div className={styles.policyId}>
            {policyIdEllipsisLeft
              ? ellipsis(policyId, policyIdEllipsisLeft)
              : policyId}
          </div>
        </div>
        {metadata && <div className={styles.metadata}>METADATA CONTENT</div>}
      </div>
    );
  }
}
