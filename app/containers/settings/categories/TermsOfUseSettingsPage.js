// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import TermsOfUseSettings from '../../../components/settings/categories/TermsOfUseSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores') @observer
export default class TermsOfUseSettingsPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  render() {
    const { termsOfUse } = this.props.stores.app;
    return (
      <TermsOfUseSettings localizedTermsOfUse={termsOfUse} />
    );
  }

}
