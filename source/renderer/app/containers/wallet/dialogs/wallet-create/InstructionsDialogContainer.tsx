import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import InstructionsDialog from '../../../../components/wallet/wallet-create/InstructionsDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
class InstructionsDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onClose, onContinue, isVideoWatched } = this.props;
    return (
      <InstructionsDialog
        isVideoWatched={isVideoWatched}
        onClose={onClose}
        onContinue={onContinue}
      />
    );
  }
}

export default InstructionsDialogContainer;
