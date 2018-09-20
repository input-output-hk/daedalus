// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import DataLayerMigrationForm from '../../components/profile/data-layer-migration/DataLayerMigrationForm';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class DataLayerMigrationPage extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  onSubmit = () => {
    this.props.actions.profile.acceptDataLayerMigration.trigger();
  };

  render() {
    const { setDataLayerMigrationAcceptanceRequest, termsOfUse } = this.props.stores.profile;
    const isSubmitting = setDataLayerMigrationAcceptanceRequest.isExecuting;
    return (
      <CenteredLayout>
        <DataLayerMigrationForm
          localizedDataLayerMigration={termsOfUse}
          onSubmit={this.onSubmit}
          isSubmitting={isSubmitting}
          error={setDataLayerMigrationAcceptanceRequest.error}
        />
      </CenteredLayout>
    );
  }
}
