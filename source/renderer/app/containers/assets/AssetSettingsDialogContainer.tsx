import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import AssetSettingsDialog from '../../components/assets/AssetSettingsDialog';
import type { AssetToken } from '../../api/assets/types';

type Props = InjectedProps;

class AssetSettingsDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  handleSubmit = (asset: AssetToken, decimals: number) => {
    const { assets, dialogs } = this.props.actions;
    assets.onAssetSettingsSubmit.trigger({ asset, decimals });
    dialogs.closeActiveDialog.trigger();
  };

  handleCancel = () => {
    const { assets, dialogs } = this.props.actions;
    assets.unsetEditedAsset.trigger();
    dialogs.closeActiveDialog.trigger();
  };

  render() {
    const { stores } = this.props;
    const { assets, uiDialogs } = stores;
    const { editedAsset } = assets;
    if (!uiDialogs.isOpen(AssetSettingsDialog) || !editedAsset) return null;

    return (
      <AssetSettingsDialog
        asset={editedAsset}
        onSubmit={this.handleSubmit}
        onCancel={this.handleCancel}
      />
    );
  }
}

export default inject(
  'stores',
  'actions'
)(observer(AssetSettingsDialogContainer));
