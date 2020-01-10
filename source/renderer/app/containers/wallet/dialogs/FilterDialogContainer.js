// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import FilterDialog from '../../../components/wallet/transactions/FilterDialog';
import type { InjectedDialogContainerFilterProps } from '../../../types/injectedPropsType';

type Props = InjectedDialogContainerFilterProps;

@inject('stores', 'actions')
@observer
export default class FilterDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null, children: null };

  render() {
    const { actions, stores, onFilter } = this.props;
    const { closeActiveDialog } = actions.dialogs;
    const { dataForActiveDialog } = stores.uiDialogs;

    return (
      <FilterDialog
        onFilter={onFilter}
        onClose={() => closeActiveDialog.trigger()}
        {...dataForActiveDialog}
      />
    );
  }
}
