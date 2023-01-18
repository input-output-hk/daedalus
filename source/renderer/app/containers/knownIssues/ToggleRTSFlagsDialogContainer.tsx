import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';
import ToggleRTSFlagsDialog from '../../components/knownIssues/ToggleRTSFlagsDialog/ToggleRTSFlagsDialog';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
class ToggleRTSFlagsDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => null,
  };
  onConfirm = () => {
    this.props.actions.networkStatus.toggleRTSFlagsMode.trigger();
  };
  onClose = () => {
    this.props.actions.app.closeToggleRTSFlagsModeDialog.trigger();
  };

  render() {
    return (
      <ToggleRTSFlagsDialog
        isRTSFlagsModeEnabled={
          this.props.stores.networkStatus.isRTSFlagsModeEnabled
        }
        onClose={this.onClose}
        onConfirm={this.onConfirm}
      />
    );
  }
}

export default ToggleRTSFlagsDialogContainer;
