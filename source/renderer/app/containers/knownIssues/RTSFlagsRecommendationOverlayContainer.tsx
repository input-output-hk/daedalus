import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';
import RTSFlagsRecommendationOverlay from '../../components/knownIssues/RTSFlagsRecommendationOverlay/RTSFlagsRecommendationOverlay';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
class RTSFlagsRecommendationOverlayContainer extends Component<Props> {
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
      // Relax hardware requirements for selfnode/local-cluster (possibly running in VM):
      this.props.stores.networkStatus.environment.isSelfnode ||
      this.props.stores.networkStatus.environment.hasMetHardwareRequirements ||
      !this.props.stores.profile.areTermsOfUseAccepted ||
      this.props.stores.networkStatus.isRTSFlagsModeEnabled
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
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        isRTSFlagsModeEnabled={
          this.props.stores.networkStatus.isRTSFlagsModeEnabled
        }
        onClose={this.onClose}
        onConfirm={this.onConfirm}
      />
    );
  }
}

export default RTSFlagsRecommendationOverlayContainer;
