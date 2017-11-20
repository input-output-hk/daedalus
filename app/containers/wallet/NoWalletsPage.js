// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import resolver from '../../utils/imports';

const Layout = resolver('containers/MainLayout');

@inject('stores', 'actions') @observer
export default class NoWalletsPage extends Component<any> {

  render() {
    return (
      <Layout>
        <div />
      </Layout>
    );
  }

}
