// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import SupportSettings from '../../../components/settings/categories/SupportSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class SupportSettingsPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  onSubmit = (values: { sendLogs: boolean }) => {
    this.props.actions.profile.setSendLogsChoice.trigger(values);
  };

  render() {
    const { setSendLogsChoiceRequest, getSendLogsChoiceRequest } = this.props.stores.app;
    return (
      <SupportSettings
        onSubmit={this.onSubmit}
        error={setSendLogsChoiceRequest.error}
        sendLogs={getSendLogsChoiceRequest.result}
      />
    );
  }

}
