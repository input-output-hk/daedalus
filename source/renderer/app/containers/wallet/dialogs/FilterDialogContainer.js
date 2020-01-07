// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import FilterDialog from '../../../components/wallet/transactions/FilterDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class FilterDialogContainer extends Component<Props> {
  render() {
    const { dialogs } = this.props.actions;
    const { uiDialogs } = this.props.stores;

    return (
      <FilterDialog
        onClose={() => dialogs.closeActiveDialog.trigger()}
        {...uiDialogs.dataForActiveDialog}
      />
    );
  }
}
