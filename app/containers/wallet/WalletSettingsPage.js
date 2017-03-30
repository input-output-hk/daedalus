// @flow
import React, { Component, PropTypes } from 'react';
import { inject, observer } from 'mobx-react';
import WalletSettings from '../../components/wallet/WalletSettings';
import UiDialogsStore from '../../stores/UiDialogsStore';

@inject('actions', 'stores') @observer
export default class WalletSettingsPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      uiDialogs: PropTypes.instanceOf(UiDialogsStore).isRequired,
    }).isRequired,
    actions: PropTypes.shape({
      dialogs: PropTypes.shape({
        open: PropTypes.func.isRequired,
      }).isRequired,
    }).isRequired,
  };

  render() {
    const { actions } = this.props;
    const { uiDialogs } = this.props.stores;
    return (
      <WalletSettings
        openDialogAction={actions.dialogs.open}
        isDialogOpen={uiDialogs.isOpen}
      />
    );
  }

}
