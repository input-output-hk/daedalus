// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';
import RTSFlagsRecommendationOverlay from '../../components/knownIssues/RTSFlagsRecommendationOverlay/RTSFlagsRecommendationOverlay';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class RTSFlagsRecommendationOverlayContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => null,
  };

  onConfirm = () => {
    this.props.actions.networkStatus.toggleRTSFlagsMode.trigger();
    this.props.actions.profile.acknowledgeRTSModeRecommendation.trigger();
  };

  onClose = () => {
    this.props.actions.profile.acknowledgeRTSModeRecommendation.trigger();
  };

  shouldRender = () => {
    if (
      this.props.stores.networkStatus.environment.meetsHardwareRequirements ||
      !this.props.stores.profile.areTermsOfUseAccepted ||
      this.props.stores.networkStatus.rtsFlagsModeEnabled
    ) {
      return false;
    }

    return !this.props.stores.profile.isRTSModeRecommendationAcknowledged;
  };

  render() {
    if (!this.shouldRender()) {
      return null;
    }

    return (
      <RTSFlagsRecommendationOverlay
        rtsFlagsModeEnabled={
          this.props.stores.networkStatus.rtsFlagsModeEnabled
        }
        onClose={this.onClose}
        onConfirm={this.onConfirm}
      />
    );
  }
}
