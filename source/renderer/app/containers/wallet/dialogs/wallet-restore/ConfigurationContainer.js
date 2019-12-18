// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ConfigurationDialog from '../../../../components/wallet/wallet-restore/ConfigurationDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class ConfigurationDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onClose, onContinue } = this.props;
    return <ConfigurationDialog onClose={onClose} onContinue={onContinue} />;
  }
}
