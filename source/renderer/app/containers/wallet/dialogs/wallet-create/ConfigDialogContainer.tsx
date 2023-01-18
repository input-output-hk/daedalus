import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ConfigDialog from '../../../../components/wallet/wallet-create/ConfigDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
class ConfigDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onClose, onContinue } = this.props;
    return <ConfigDialog onClose={onClose} onContinue={onContinue} />;
  }
}

export default ConfigDialogContainer;
