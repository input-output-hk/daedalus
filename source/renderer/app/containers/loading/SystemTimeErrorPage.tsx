import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import SystemTimeError from '../../components/loading/system-time-error/SystemTimeError';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class SystemTimeErrorPage extends Component<Props> {
  static defaultProps = {
    stores: null,
    actions: null,
  };

  render() {
    const { actions, stores } = this.props;
    const {
      localTimeDifference,
      ignoreSystemTimeChecks,
    } = stores.networkStatus;
    const { forceCheckNetworkClock } = actions.networkStatus;
    const { app, networkStatus, profile } = stores;
    const { openExternalLink } = app;
    const { currentLocale } = profile;
    const { getNetworkClockRequest } = networkStatus;
    return (
      <SystemTimeError
        localTimeDifference={localTimeDifference}
        currentLocale={currentLocale}
        onExternalLinkClick={openExternalLink}
        onCheckTheTimeAgain={() => forceCheckNetworkClock.trigger()}
        onContinueWithoutClockSyncCheck={ignoreSystemTimeChecks}
        isCheckingSystemTime={getNetworkClockRequest.isExecutingWithArgs({
          isForceCheck: true,
        })}
      />
    );
  }
}

export default SystemTimeErrorPage;
