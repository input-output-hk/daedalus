// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedStoresProps } from '../../types/injectedPropsType';
import SystemTimeError from '../../components/loading/system-time-error/SystemTimeError';

type Props = InjectedStoresProps;

@inject('stores')
@observer
export default class SystemTimeErrorPage extends Component<Props> {
  static defaultProps = { stores: null };

  render() {
    const { stores } = this.props;
    const {
      localTimeDifference,
      // forceCheckTimeDifferenceRequest,
      // forceCheckLocalTimeDifference,
      ignoreSystemTimeChecks,
    } = stores.networkStatus;

    const { currentLocale } = stores.profile;

    const { openExternalLink } = stores.app;

    return (
      <SystemTimeError
        localTimeDifference={localTimeDifference}
        currentLocale={currentLocale}
        onExternalLinkClick={openExternalLink}
        onCheckTheTimeAgain={() => {}}
        onContinueWithoutClockSyncCheck={ignoreSystemTimeChecks}
        isCheckingSystemTime={false}
      />
    );
  }
}
