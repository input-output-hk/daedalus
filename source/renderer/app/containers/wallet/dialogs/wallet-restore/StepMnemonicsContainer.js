// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import MnemonicsDialog from '../../../../components/wallet/wallet-restore/StepMnemonicsDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class MnemonicsDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onClose, onContinue } = this.props;
    return <MnemonicsDialog onClose={onClose} onContinue={onContinue} />;
  }
}
