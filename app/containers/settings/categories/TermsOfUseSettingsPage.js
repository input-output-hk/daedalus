// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import TermsOfUseSettings from '../../../components/settings/categories/TermsOfUseSettings';
import LoadingSpinner from '../../../components/widgets/LoadingSpinner';
import CachedRequest from '../../../stores/lib/CachedRequest';

@inject('stores') @observer
export default class TermsOfUseSettingsPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      settings: PropTypes.shape({
        termsOfUseRequest: PropTypes.instanceOf(CachedRequest),
        termsOfUse: PropTypes.string
      }).isRequired,
    }).isRequired
  };

  render() {
    const { settings } = this.props.stores;
    const { termsOfUseRequest } = settings;
    if (!termsOfUseRequest.result || termsOfUseRequest.isExecuting) return <LoadingSpinner />;
    return <TermsOfUseSettings text={settings.termsOfUse} />;
  }

}
