// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
// import startCase from 'lodash/startCase';
import ProfileSettingsForm from '../../../components/widgets/forms/ProfileSettingsForm';
import { rebuildApplicationMenu } from '../../../ipc/rebuild-application-menu.js';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class GeneralSettingsPage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  handleSelectItem = async (param: string, values: string) => {
    const { actions, stores } = this.props;
    const { isUpdateAvailable } = stores.nodeUpdate;
    const { updateLocale } = actions.profile;

    if (param === 'language') {
      updateLocale.trigger({ locale: values });
      await rebuildApplicationMenu.send({ isUpdateAvailable });
    }
  };

  render() {
    const {
      setProfileLocaleRequest,
      currentLocale,
      currentNumberFormat,
      currentDateEnglishFormat,
      currentDateJapaneseFormat,
      currentTimeFormat,
    } = this.props.stores.profile;
    return (
      <ProfileSettingsForm
        onChangeItem={this.handleSelectItem}
        currentLocale={currentLocale}
        currentNumberFormat={currentNumberFormat}
        currentDateEnglishFormat={currentDateEnglishFormat}
        currentDateJapaneseFormat={currentDateJapaneseFormat}
        currentTimeFormat={currentTimeFormat}
        error={setProfileLocaleRequest.error}
      />
    );
  }
}
