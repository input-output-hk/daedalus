// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import TopBar from '../../components/layout/TopBar';
import TopBarLayout from '../../components/layout/TopBarLayout';
import LanguageSelectionForm from '../../components/profile/language-selection/LanguageSelectionForm';
import { rebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class LanguageSelectionPage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  onSubmit = async (values: { locale: string }) => {
    const { actions, stores } = this.props;
    const { isUpdateAvailable } = stores.nodeUpdate;
    const { updateLocale } = actions.profile;
    updateLocale.trigger(values);
    await rebuildApplicationMenu.send({ isUpdateAvailable });
  };

  render() {
    const { currentRoute } = this.props.stores.app;
    const {
      setProfileLocaleRequest,
      LANGUAGE_OPTIONS,
      systemLocale,
    } = this.props.stores.profile;
    const isSubmitting = setProfileLocaleRequest.isExecuting;
    const preselectedIndex = LANGUAGE_OPTIONS.findIndex(
      ({ value }) => value === systemLocale
    );
    const preselectedLanguage = LANGUAGE_OPTIONS[preselectedIndex].value;
    const topbar = (
      <TopBar currentRoute={currentRoute} showSubMenuToggle={false} />
    );
    return (
      <TopBarLayout topbar={topbar}>
        <LanguageSelectionForm
          onSubmit={this.onSubmit}
          isSubmitting={isSubmitting}
          languages={LANGUAGE_OPTIONS}
          preselectedLanguage={preselectedLanguage}
          error={setProfileLocaleRequest.error}
        />
      </TopBarLayout>
    );
  }
}
