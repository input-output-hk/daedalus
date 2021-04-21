// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import AssetSettingsDialog from '../../components/assets/AssetSettingsDialog';
import type { WalletSummaryAsset } from '../../api/assets/types';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class AssetSettingsDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleSubmit = (asset: WalletSummaryAsset, decimalPrecision: number) => {
    const { onEditAssetSubmit } = this.props.actions.assets;
    onEditAssetSubmit.trigger({ asset, decimalPrecision });
  };

  render() {
    const { actions, stores } = this.props;
    const { assets } = stores;
    const { assets: assetsActions } = actions;
    const { editingsAsset } = assets;
    if (!editingsAsset) return null;
    return (
      <AssetSettingsDialog
        asset={editingsAsset}
        onSubmit={this.handleSubmit}
        onCancel={assetsActions.onEditAssetCancel.trigger}
      />
    );
  }
}
