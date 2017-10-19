// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import resolver from '../../lib/resolver';

const Layout = resolver('containers/MainLayout');

@inject('stores', 'actions') @observer
export default class NoWalletsPage extends Component {

  render() {
    return (
      <Layout>
        <div />
      </Layout>
    );
  }

}
