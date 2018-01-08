// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import SupportSettings from '../../../components/settings/categories/SupportSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class SupportSettingsPage extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  onSubmit = (values: { sendLogs: boolean }) => {
    this.props.actions.profile.setSendLogsChoice.trigger(values);
  };

  render() {
    const { setSendLogsChoiceRequest, getSendLogsChoiceRequest } = this.props.stores.profile;
    return (
      <SupportSettings
        onSubmit={this.onSubmit}
        error={setSendLogsChoiceRequest.error}
        sendLogs={getSendLogsChoiceRequest.result}
      />
    );
  }

}
