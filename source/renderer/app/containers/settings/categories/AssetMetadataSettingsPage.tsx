import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import AssetMetadataSettings from '../../../components/settings/categories/AssetMetadataSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
class AssetMetadataSettingsPage extends Component<InjectedProps> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  handleSelectSourceUrl = (sourceUrl: string) => {
    this.props.actions.assets.selectAssetMetadataSourceUrl.trigger({
      sourceUrl,
    });
  };

  render() {
    const { stores, actions } = this.props;
    const {
      assetMetadataSourceUrl,
      assetMetadataSourceUrlError,
      assetMetadataSourceLoading,
    } = stores.assets;
    const { openExternalLink } = stores.app;
    const { resetAssetMetadataSourceError } = actions.assets;
    return (
      <AssetMetadataSettings
        sourceUrl={assetMetadataSourceUrl}
        sourceUrlError={assetMetadataSourceUrlError}
        onSelectSourceUrl={this.handleSelectSourceUrl}
        onResetSourceError={resetAssetMetadataSourceError.trigger}
        isLoading={assetMetadataSourceLoading}
        onOpenExternalLink={openExternalLink}
      />
    );
  }
}

export default AssetMetadataSettingsPage;
