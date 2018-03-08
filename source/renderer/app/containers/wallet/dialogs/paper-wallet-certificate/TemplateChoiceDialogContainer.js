// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import TemplateChoiceDialog from '../../../../components/wallet/paper-wallet-certificate/TemplateChoiceDialog';
import type { ActionsMap } from '../../../../actions/index';

type Props = {
  actions: any | ActionsMap,
};

@inject('actions') @observer
export default class TemplateChoiceDialogContainer extends Component<Props> {
  onPrint = (values: { selectedTemplate: string }) => {
    this.props.actions.ada.wallets.setCertificateTemplate.trigger(values);
  };

  render() {
    return (
      <TemplateChoiceDialog
        onPrint={this.onPrint}
      />
    );
  }
}
