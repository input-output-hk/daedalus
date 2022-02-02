import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import AssetSettingsDialog from '../../components/assets/AssetSettingsDialog';
import type { AssetToken } from '../../api/assets/types';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class AssetSettingsDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleSubmit = (asset: AssetToken, decimals: number) => {
    const { onAssetSettingsSubmit } = this.props.actions.assets;
    onAssetSettingsSubmit.trigger({
      asset,
      decimals,
    });
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
        onCancel={assetsActions.onAssetSettingsCancel.trigger}
      />
    );
  }
}

export default AssetSettingsDialogContainer;
