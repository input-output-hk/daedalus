// @flow
import React, { Component } from 'react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './AssetToken.scss';
import { ellipsis } from '../../utils/strings';
import type { WalletSummaryAsset } from '../../api/assets/types';

type Props = {
  asset: WalletSummaryAsset,
  // In case it's not possible to calculate the container width
  // this props defines after how many characters the text will cut off
  policyIdEllipsisLeft?: number,
};

@observer
export default class AssetToken extends Component<Props> {
  contentRender() {
    const { asset, policyIdEllipsisLeft } = this.props;
    const { fingerprint, policyId } = asset;
    const componentClasses = classnames([styles.component]);
    return (
      <div className={componentClasses}>
        <div className={styles.fingerprint}>
          {ellipsis(fingerprint || '', 9, 4)}
        </div>
        <div className={styles.policyId}>
          {policyIdEllipsisLeft
            ? ellipsis(policyId, policyIdEllipsisLeft)
            : policyId}
        </div>
      </div>
    );
  }

  popOverRender() {
    const { asset } = this.props;
    const { fingerprint, policyId, assetName, metadata } = asset;
    console.log('metadata', metadata);
    return (
      <div className={styles.popOverContent}>
        <div className={styles.fingerprint}>{fingerprint}</div>
        <div className={styles.policyId}>{policyId}</div>
        <div className={styles.assetName}>{assetName}</div>
      </div>
    );
  }

  render() {
    const children = this.contentRender();
    const popOverContent = this.popOverRender();
    return (
      <PopOver
        themeVariables={{}}
        contentClassName={styles.popOver}
        content={popOverContent}
      >
        {children}
      </PopOver>
    );
  }
}

// @TOKEN TODO: Remove it

// const popperOptions = {
//   placement?: Placement;
//   positionFixed?: boolean;
//   eventsEnabled?: boolean;
//   modifiers?: Modifiers;
//   removeOnDestroy?: boolean;
//   onCreate?(data: Data): void;
//   onUpdate?(data: Data): void;
// };

// export type Placement = 'auto-start'
//   | 'auto'
//   | 'auto-end'
//   | 'top-start'
//   | 'top'
//   | 'top-end'
//   | 'right-start'
//   | 'right'
//   | 'right-end'
//   | 'bottom-end'
//   | 'bottom'
//   | 'bottom-start'
//   | 'left-end'
//   | 'left'
//   | 'left-start';

// export interface Modifiers {
//     shift?: BaseModifier;
//     offset?: BaseModifier & {
//       offset?: number | string,
//     };
//     preventOverflow?: BaseModifier & {
//       priority?: Position[],
//       padding?: number | Padding,
//       boundariesElement?: Boundary | Element,
//       escapeWithReference?: boolean
//     };
//     keepTogether?: BaseModifier;
//     arrow?: BaseModifier & {
//       element?: string | Element,
//     };
//     flip?: BaseModifier & {
//       behavior?: Behavior | Position[],
//       padding?: number | Padding,
//       boundariesElement?: Boundary | Element,
//       flipVariations?: boolean,
//       flipVariationsByContent?: boolean,
//     };
//     inner?: BaseModifier;
//     hide?: BaseModifier;
//     applyStyle?: BaseModifier & {
//       onLoad?: Function,
//       gpuAcceleration?: boolean,
//     };
//     computeStyle?: BaseModifier & {
//       gpuAcceleration?: boolean;
//       x?: 'bottom' | 'top',
//       y?: 'left' | 'right'
//     };
//
//     [name: string]: (BaseModifier & Record<string, any>) | undefined;
//   }
