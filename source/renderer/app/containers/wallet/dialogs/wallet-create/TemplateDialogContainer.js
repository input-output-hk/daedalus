// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import TemplateDialog from '../../../../components/wallet/wallet-create/TemplateDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class InstructionsDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onClose, onContinue, onBack } = this.props;
    return (
      <TemplateDialog
        onClose={onClose}
        onContinue={onContinue}
        onBack={onBack}
      />
    );
  }
}
