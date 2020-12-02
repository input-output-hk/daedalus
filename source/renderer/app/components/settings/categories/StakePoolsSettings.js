// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
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
  smashURLSelectLabel: {
    id: 'settings.stakePools.smashUrl.select.label',
    defaultMessage: '!!!SMASH server URL',
    description:
      'smashURLSelectLabel for the "Smash Custom Server" selection on the Stake Pools settings page.',
  },
  smashUrlSelectPlaceholder: {
    id: 'settings.stakePools.smashUrl.select.placeholder',
    defaultMessage: '!!!Enter custom server',
    description:
      'smashUrlSelectPlaceholder for the "Smash Custom Server" selection on the Stake Pools settings page.',
  },
});

type Props = {
  smashServerType: string,
  smashServerUrl?: string,
  onSelectSmashServerType: Function,
  onSelectSmashServerUrl: Function,
};

type State = {
  isActive: boolean,
  smashServerTypeInitial: SmashServerType,
  smashServerUrlInitial: ?string,
};

@observer
export default class StakePoolsSettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isActive: false,
    smashServerTypeInitial: this.props.smashServerType,
    smashServerUrlInitial: this.props.smashServerUrl,
  };

  componentWillUnmount() {
    const {
      smashServerType,
      smashServerUrl,
      onSelectSmashServerType,
    } = this.props;

    if (smashServerType === SMASH_SERVER_TYPES.CUSTOM && !smashServerUrl) {
      onSelectSmashServerType(SMASH_SERVER_TYPES.IOHK);
    }
  }

  handleSubmit = (url: string) => {
    if (!isValidUrl(url)) {
      return false;
    }
    this.props.onSelectSmashServerUrl(url);
  };

  handleUrlStartEditing = () => {
    this.setState({ isActive: true });
  };

  handleUrlStopEditing = () => {
    this.setState({ isActive: false });
  };

  handleUrlCancelEditing = () => {
    const { onSelectSmashServerUrl } = this.props;
    const { smashServerUrlInitial } = this.state;
    onSelectSmashServerUrl(smashServerUrlInitial);
  };

  handleUrlIsValid = (url: string) => isValidUrl(url);

  render() {
    const {
      smashServerType,
      smashServerUrl,
      onSelectSmashServerType,
      onSelectSmashServerUrl,
    } = this.props;
    const { isActive, smashServerUrlInitial } = this.state;
    const { intl } = this.context;

    const smashSelectOptions = [
      ...Object.entries(SMASH_SERVERS_LIST).map(([id, { name: label }]) => ({
        label,
        value,
      })),
      {
        label: intl.formatMessage(messages.smashSelectCustomServer),
        value: SMASH_SERVER_TYPES.CUSTOM,
      },
    ];

    const successfullyUpdated = smashServerUrl !== smashServerUrlInitial;

    // @SMASH TODO
    const validationErrorMessage = 'Something wrong is not right 🤔';

    return (
      <div className={styles.component}>
        <Select
          label={intl.formatMessage(messages.smashSelectLabel)}
          value={smashServerType}
          options={smashSelectOptions}
          onChange={(smashServerType: SmashServerType) => {
            onSelectSmashServerType(smashServerType);
          }}
          skin={SelectSkin}
          className={styles.select}
          optionHeight={50}
        />
        {smashServerType === SMASH_SERVER_TYPES.CUSTOM && (
          <InlineEditingInput
            className={styles.smashServerUrl}
            inputFieldLabel={intl.formatMessage(messages.smashURLSelectLabel)}
            inputFieldValue={smashServerUrl || ''}
            inputFieldPlaceholder={intl.formatMessage(
              messages.smashUrlSelectPlaceholder
            )}
            onStartEditing={this.handleUrlStartEditing}
            onStopEditing={this.handleUrlStopEditing}
            onCancelEditing={this.handleUrlCancelEditing}
            onSubmit={this.handleSubmit}
            isValid={this.handleUrlIsValid}
            validationErrorMessage={validationErrorMessage}
            successfullyUpdated={successfullyUpdated}
            isActive={isActive}
          />
        )}
      </div>
    );
  }
}
