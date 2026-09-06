import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import DataLayerMigrationForm from '../../components/profile/data-layer-migration/DataLayerMigrationForm';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
class DataLayerMigrationPage extends Component<InjectedProps> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  onSubmit = () => {
    this.props.actions.profile.acceptDataLayerMigration.trigger();
  };

  render() {
    const {
      setDataLayerMigrationAcceptanceRequest,
    } = this.props.stores.profile;
    return (
      <CenteredLayout>
        <DataLayerMigrationForm
          onSubmit={this.onSubmit}
          error={setDataLayerMigrationAcceptanceRequest.error}
        />
      </CenteredLayout>
    );
  }
}

export default DataLayerMigrationPage;
