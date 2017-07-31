// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import TopBar from '../../components/layout/TopBar';
import TopBarLayout from '../../components/layout/TopBarLayout';
import SendLogsChoiceForm from '../../components/profile/send-logs-choice-form/SendLogsChoiceForm';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class SendLogsChoicePage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  onSubmit = (values: { sendLogs: boolean }) => {
    this.props.actions.profile.setSendLogsChoice.trigger(values);
  };

  render() {
    const { currentRoute, setSendLogsChoiceRequest } = this.props.stores.app;
    const isSubmitting = setSendLogsChoiceRequest.isExecuting;
    const topbar = <TopBar currentRoute={currentRoute} />;
    return (
      <TopBarLayout
        topbar={topbar}
      >
        <SendLogsChoiceForm
          onSubmit={this.onSubmit}
          isSubmitting={isSubmitting}
          error={setSendLogsChoiceRequest.error}
        />
      </TopBarLayout>
    );
  }
}
