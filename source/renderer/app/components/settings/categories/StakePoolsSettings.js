// @flow
import React, { Component } from 'react';
import { map } from 'lodash';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { isValidUrl } from '../../../utils/validations';
import { getSmashServerIdFromUrl } from '../../../utils/staking';
import InlineEditingInput from '../../widgets/forms/InlineEditingInput';
import styles from './StakePoolsSettings.scss';
import {
  SMASH_SERVERS_LIST,
  SMASH_SERVER_TYPES,
} from '../../../config/stakingConfig';
import type { SmashServerType } from '../../../types/stakingTypes';

import LocalizableError from '../../../i18n/LocalizableError';

const messages = defineMessages({
  smashSelectLabel: {
    id: 'settings.stakePools.smash.select.label',
    defaultMessage: '!!!Off-chain metadata server (SMASH)',
    description:
      'smashSelectLabel for the "Smash" selection on the Stake Pools settings page.',
  },
  smashSelectCustomServer: {
    id: 'settings.stakePools.smash.select.placeholder',
    defaultMessage: '!!!Custom server',
    description:
      'smashSelectCustomServer option for the "Smash" selection on the Stake Pools settings page.',
  },
  smashURLInputLabel: {
    id: 'settings.stakePools.smashUrl.input.label',
    defaultMessage: '!!!SMASH server URL',
    description:
      'smashURLInputLabel for the "Smash Custom Server" selection on the Stake Pools settings page.',
  },
  smashUrlInputPlaceholder: {
    id: 'settings.stakePools.smashUrl.input.placeholder',
    defaultMessage: '!!!Enter custom server URL',
    description:
      'smashUrlInputPlaceholder for the "Smash Custom Server" selection on the Stake Pools settings page.',
  },
  smashUrlInputInvalidUrl: {
    id: 'settings.stakePools.smashUrl.input.invalidUrl',
    defaultMessage: '!!!Invalid URL',
    description:
      'smashUrlInputInvalidUrl for the "Smash Custom Server" selection on the Stake Pools settings page.',
  },
});

type Props = {
  smashServerUrl: string,
  smashServerUrlError?: ?LocalizableError,
  onSelectSmashServerUrl: Function,
  onResetSmashServerError: Function,
  isLoading: boolean,
};

type State = {
  editingSmashServerUrl: string,
  successfullyUpdated: boolean,
  wasLoading: boolean,
};

@observer
export default class StakePoolsSettings extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  /* eslint-disable react/no-unused-state */
  // Disabling eslint due to a [known issue](https://github.com/yannickcr/eslint-plugin-react/issues/2061)
  // `wasLoading` is actually used in the `getDerivedStateFromProps` method
  static getDerivedStateFromProps(
    { isLoading, smashServerUrlError }: Props,
    { wasLoading }: State
  ) {
    const successfullyUpdated =
      wasLoading && !isLoading && !smashServerUrlError;
    return {
      successfullyUpdated,
      wasLoading: isLoading,
    };
  }

  state = {
    editingSmashServerUrl: this.props.smashServerUrl,
    successfullyUpdated: false,
    wasLoading: false,
  };

  componentWillUnmount() {
    this.props.onResetSmashServerError();
  }

  handleSubmit = (url: string) => {
    if (isValidUrl(url || '')) {
      this.setState({
        editingSmashServerUrl: url,
      });
      this.props.onSelectSmashServerUrl(url);
    }
  };

  handleOnSelectSmashServerType = (smashServerType: SmashServerType) => {
    const { onSelectSmashServerUrl } = this.props;
    let editingSmashServerUrl = '';
    if (smashServerType !== SMASH_SERVER_TYPES.CUSTOM) {
      editingSmashServerUrl = SMASH_SERVERS_LIST[smashServerType].url;
      onSelectSmashServerUrl(editingSmashServerUrl);
    }
    this.setState({
      editingSmashServerUrl,
    });
  };

  handleIsValid = (url: string) => url === '' || isValidUrl(url);

  render() {
    const { smashServerUrlError, isLoading } = this.props;
    const { intl } = this.context;
    const { editingSmashServerUrl, successfullyUpdated } = this.state;
    const smashServerType = getSmashServerIdFromUrl(editingSmashServerUrl);

    const smashSelectOptions = [
      ...map(SMASH_SERVERS_LIST, ({ name: label }, value) => ({
        label,
        value,
      })),
      {
        label: intl.formatMessage(messages.smashSelectCustomServer),
        value: SMASH_SERVER_TYPES.CUSTOM,
      },
    ];

    const errorMessage = smashServerUrlError
      ? intl.formatMessage(smashServerUrlError)
      : null;

    return (
      <div className={styles.component}>
        <Select
          label={intl.formatMessage(messages.smashSelectLabel)}
          value={smashServerType}
          options={smashSelectOptions}
          onChange={this.handleOnSelectSmashServerType}
          skin={SelectSkin}
          className={styles.select}
          optionHeight={50}
        />
        <InlineEditingInput
          className={styles.smashServerUrl}
          label={intl.formatMessage(messages.smashURLInputLabel)}
          value={editingSmashServerUrl}
          placeholder={intl.formatMessage(messages.smashUrlInputPlaceholder)}
          onSubmit={this.handleSubmit}
          isValid={this.handleIsValid}
          valueErrorMessage={intl.formatMessage(
            messages.smashUrlInputInvalidUrl
          )}
          errorMessage={errorMessage}
          readOnly={isLoading || smashServerType !== SMASH_SERVER_TYPES.CUSTOM}
          isLoading={isLoading}
          successfullyUpdated={successfullyUpdated}
        />
      </div>
    );
  }
}
