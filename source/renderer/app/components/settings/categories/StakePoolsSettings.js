// @flow
import React, { Component } from 'react';
import { map } from 'lodash';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { isValidUrl } from '../../../utils/validations';
import InlineEditingInput from '../../widgets/forms/InlineEditingInput';
import styles from './StakePoolsSettings.scss';
import {
  SMASH_SERVERS_LIST,
  SMASH_SERVER_TYPES,
} from '../../../config/stakingConfig';
import LocalizableError from '../../../i18n/LocalizableError';
import type { SmashServerType } from '../../../types/stakingTypes';

const messages = defineMessages({
  smashSelectLabel: {
    id: 'settings.stakePools.smash.select.label',
    defaultMessage: '!!!Off-chain data server (SMASH)',
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
    defaultMessage: '!!!Enter custom server',
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
  smashServerType: SmashServerType,
  smashServerUrl?: string,
  smashServerUrlError: ?LocalizableError,
  onSelectSmashServerType: Function,
  onSelectSmashServerUrl: Function,
};

type State = {
  isActive: boolean,
  lastValidServerUrl: ?string,
  lastValidServerType: SmashServerType,
  prevValidServerType: SmashServerType,
};

@observer
export default class StakePoolsSettings extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isActive: false,
    // Last valid type and url
    lastValidServerUrl: this.props.smashServerUrl,
    lastValidServerType: this.props.smashServerType,
    prevValidServerType: this.props.smashServerType,
  };

  componentDidUpdate({ smashServerUrl: prevServerUrl }: Props) {
    // The `smashServerUrl` prop only changes when it's a valid server
    // unless it's empty
    // so we update the `lastValidServerType` and `lastValidServerUrl` states
    const {
      smashServerUrl: nextValidServerUrl,
      smashServerType: nextValidServerType,
    } = this.props;
    if (nextValidServerUrl && nextValidServerUrl !== prevServerUrl) {
      this.setState(({ lastValidServerType }) => ({
        lastValidServerUrl: nextValidServerUrl,
        lastValidServerType: nextValidServerType,
        prevValidServerType: lastValidServerType,
      }));
    }
  }

  componentWillUnmount() {
    // In case the `lastValidServerUrl` prop is empty
    // we revert to the last valid state
    const {
      smashServerType,
      smashServerUrl,
      onSelectSmashServerType,
    } = this.props;
    const { lastValidServerType } = this.state;

    if (smashServerType === SMASH_SERVER_TYPES.CUSTOM && !smashServerUrl) {
      onSelectSmashServerType(lastValidServerType);
    }
  }

  handleSubmit = (url: string) => {
    if (isValidUrl(url)) {
      this.props.onSelectSmashServerUrl(url);
    }
  };

  handleUrlStartEditing = () => {
    this.setState({ isActive: true });
  };

  handleUrlStopEditing = () => {
    this.setState({ isActive: false });
  };

  handleUrlCancelEditing = () => {
    const { onSelectSmashServerUrl } = this.props;
    const { lastValidServerUrl } = this.state;
    onSelectSmashServerUrl(lastValidServerUrl);
    this.setState({ isActive: false });
  };

  handleUrlIsValid = (url: string) => isValidUrl(url);

  // @SMASH TODO - Handle the success message
  handleIsSuccessfullyUpdated = () => {
    const { smashServerType, smashServerUrl } = this.props;
    const { isActive, lastValidServerUrl, prevValidServerType } = this.state;
    if (smashServerType === SMASH_SERVER_TYPES.CUSTOM) {
      return smashServerUrl && smashServerUrl !== lastValidServerUrl;
    }
    return smashServerType !== prevValidServerType;
  };

  render() {
    const {
      smashServerType,
      smashServerUrl,
      smashServerUrlError,
      onSelectSmashServerType,
    } = this.props;
    const { isActive, lastValidServerType, lastValidServerUrl } = this.state;
    const { intl } = this.context;

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

    const validationErrorMessage = intl.formatMessage(
      messages.smashUrlInputInvalidUrl
    );

    return (
      <div className={styles.component}>
        <Select
          label={intl.formatMessage(messages.smashSelectLabel)}
          value={smashServerType}
          options={smashSelectOptions}
          onChange={(type: SmashServerType) => {
            onSelectSmashServerType(type);
          }}
          skin={SelectSkin}
          className={styles.select}
          optionHeight={50}
        />
        <InlineEditingInput
          className={styles.smashServerUrl}
          inputFieldLabel={intl.formatMessage(messages.smashURLInputLabel)}
          inputFieldValue={smashServerUrl || ''}
          inputFieldPlaceholder={intl.formatMessage(
            messages.smashUrlInputPlaceholder
          )}
          onStartEditing={this.handleUrlStartEditing}
          onStopEditing={this.handleUrlStopEditing}
          onCancelEditing={this.handleUrlCancelEditing}
          onSubmit={this.handleSubmit}
          isValid={this.handleUrlIsValid}
          validationErrorMessage={validationErrorMessage}
          successfullyUpdated={false}
          successfullyUpdatedToDo={this.handleIsSuccessfullyUpdated}
          isActive={isActive}
          readOnly={smashServerType !== SMASH_SERVER_TYPES.CUSTOM}
        />

        {smashServerUrlError && (
          <p className={styles.smashServerUrlError}>
            {intl.formatMessage(smashServerUrlError)}
          </p>
        )}
      </div>
    );
  }
}
