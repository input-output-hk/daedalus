import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import HashDialog from '../../../../components/wallet/wallet-create/HashDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
class HashDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onClose, onContinue } = this.props;
    return <HashDialog onClose={onClose} onContinue={onContinue} />;
  }
}

export default HashDialogContainer;
