// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import AdaRedemptionDialog from '../../components/wallet/ada-redemption/AdaRedemptionForm';

@inject('actions') @observer
export default class AdaRedemptionPage extends Component {

  static propTypes = {
    actions: PropTypes.shape({
      createPersonalWallet: PropTypes.func.isRequired,
    }),
  };

  onSubmit = (values) => {
    console.log(values);
  };

  onCertificateSelected = (values) => {
    console.log(values);
  };

  render() {
    return (
      <Layout>
        <AdaRedemptionDialog
          onSubmit={this.onSubmit}
          onCertificateSelected={this.onCertificateSelected}
        />
      </Layout>
    );
  }
}
