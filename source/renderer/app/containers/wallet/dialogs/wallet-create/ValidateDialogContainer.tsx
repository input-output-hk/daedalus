import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ValidateDialog from '../../../../components/wallet/wallet-create/ValidateDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
class ValidateDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onClose, onContinue, onBack } = this.props;
    return (
      <ValidateDialog
        onClose={onClose}
        onContinue={onContinue}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        onBack={onBack}
      />
    );
  }
}

export default ValidateDialogContainer;
