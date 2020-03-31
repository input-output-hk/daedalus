// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import SystemTimeError from '../../components/loading/system-time-error/SystemTimeError';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class SystemTimeErrorPage extends Component<Props> {
  static defaultProps = { stores: null, actions: null };

  render() {
    const { stores, actions } = this.props;
    const {
      localTimeDifference,
      ignoreSystemTimeChecks,
    } = stores.networkStatus;
    const { restartNode } = actions.networkStatus;

    const { currentLocale } = stores.profile;

    const { openExternalLink } = stores.app;

    return (
      <SystemTimeError
        localTimeDifference={localTimeDifference}
        currentLocale={currentLocale}
        onExternalLinkClick={openExternalLink}
        onCheckTheTimeAgain={() => restartNode.trigger()}
        onContinueWithoutClockSyncCheck={ignoreSystemTimeChecks}
        isCheckingSystemTime={false}
      />
    );
  }
}
