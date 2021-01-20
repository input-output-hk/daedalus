// @flow
import React, { Component } from 'react';
import { map } from 'lodash';
import { Select } from 'react-polymorph/lib/components/Select';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import RadioSet from '../../widgets/RadioSet';
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
  description1: {
    id: 'settings.stakePools.smash.description1',
    defaultMessage:
      '!!!The Stakepool Metadata Aggregation Server (SMASH) is an off-chain metadata server. It enables a faster loading of stake pools and can be used to improve security and the quality of the results. Every server has a different curation policy.',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionLinkLabel: {
    id: 'settings.stakePools.smash.descriptionLinkLabel',
    defaultMessage: '!!!Find out more.',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionLinkUrl: {
    id: 'settings.stakePools.smash.descriptionLinkUrl',
    defaultMessage:
      '!!!https://iohk.io/en/blog/posts/2020/11/17/in-pools-we-trust/',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionIOHKcontent: {
    id: 'settings.stakePools.smash.descriptionIOHKcontent',
    defaultMessage: '!!!...',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionIOHKurl: {
    id: 'settings.stakePools.smash.descriptionIOHKurl',
    defaultMessage:
      '!!!https://iohk.io/en/blog/posts/2020/11/17/in-pools-we-trust/',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionNone: {
    id: 'settings.stakePools.smash.descriptionNone',
    defaultMessage: '!!!...',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionItem1Title: {
    id: 'settings.stakePools.smash.descriptionItem1Title',
    defaultMessage: '!!!Curating the server list.',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionItem1Content1: {
    id: 'settings.stakePools.smash.descriptionItem1Content1',
    defaultMessage:
      '!!!Every server has a different curation policy. For example, the IOHK server ensures that registered stake pools are valid, helps to avoid duplicated ticker names or trademarks, and checks that the pools do not feature potentially offensive or harmful.',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionItem1Content2: {
    id: 'settings.stakePools.smash.descriptionItem1Content2',
    defaultMessage:
      '!!!Scams, trolls, and abusive behavior are unfortunately part of the online ecosystem, we had to find a way to filter potentially problematic actors out of the playing field.',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionItem2Title: {
    id: 'settings.stakePools.smash.descriptionItem2Title',
    defaultMessage: '!!!Enabling faster loading of stake pools.',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionItem2Content: {
    id: 'settings.stakePools.smash.descriptionItem2Content',
    defaultMessage:
      '!!!Your Daedalus client does not fetch the stake pool data individually from every stake pool. Rather, it loads off-chain data for all of the stake pools found in the selected server.',
    description: 'description for the Stake Pools settings page.',
  },
  description2: {
    id: 'settings.stakePools.smash.description2',
    defaultMessage:
      '!!!If you decide not to use an off-chain metadata server, your Daedalus client will be forced to fetch this data by contacting every stake pool individually, which is a very slow and resource-consuming process. The list of stake pools received is not curated, so Daedalus will get legitimate, duplicates, and fake pools. An added risk to this process is that your antivirus or antimalware software could recognize making thousands of network requests as malicious behavior by the Daedalus client.',
    description: 'description for the Stake Pools settings page.',
  },
  smashSelectLabel: {
    id: 'settings.stakePools.smash.select.label',
    defaultMessage: '!!!Off-chain metadata server (SMASH)',
    description:
      'smashSelectLabel for the "Smash" selection on the Stake Pools settings page.',
  },
  smashSelectIOHKServer: {
    id: 'settings.stakePools.smash.select.IOHKServer',
    defaultMessage: '!!!IOHK (Recommended)',
    description:
      'smashSelectCustomServer option for the "Smash" selection on the Stake Pools settings page.',
  },
  smashSelectDirect: {
    id: 'settings.stakePools.smash.select.direct',
    defaultMessage: '!!!None - let my Daedalus client fetch the data',
    description:
      'smashSelectCustomServer option for the "Smash" selection on the Stake Pools settings page.',
  },
  smashSelectCustomServer: {
    id: 'settings.stakePools.smash.select.customServer',
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
    if (this.handleIsValid(url)) {
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

  handleIsValid = (url: string) =>
    url === '' || isValidUrl(url) || url === SMASH_SERVERS_LIST.direct.url;

  smashSelectMessages = {
    iohk: this.context.intl.formatMessage(messages.smashSelectIOHKServer),
    testingKnown: SMASH_SERVERS_LIST.testingKnown.name,
    direct: this.context.intl.formatMessage(messages.smashSelectDirect),
    custom: this.context.intl.formatMessage(messages.smashSelectCustomServer),
  };

  render() {
    const { smashServerUrlError, isLoading } = this.props;
    const { intl } = this.context;
    const { editingSmashServerUrl, successfullyUpdated } = this.state;
    const smashServerType = getSmashServerIdFromUrl(editingSmashServerUrl);

    const smashSelectOptions = map(SMASH_SERVER_TYPES, (value) => ({
      label: this.smashSelectMessages[value] || value,
      value,
    }));

    const errorMessage = smashServerUrlError
      ? intl.formatMessage(smashServerUrlError)
      : null;

    return (
      <div className={styles.component}>
        <div className={styles.description}>
          <p>
            {intl.formatMessage(messages.description1)} <em>Find out more.</em>
          </p>
        </div>
        <Select
          label={intl.formatMessage(messages.smashSelectLabel)}
          value={smashServerType}
          options={smashSelectOptions}
          onChange={this.handleOnSelectSmashServerType}
          className={styles.select}
          optionHeight={50}
        />
        {smashServerType === SMASH_SERVER_TYPES.CUSTOM && (
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
            readOnly={
              isLoading || smashServerType !== SMASH_SERVER_TYPES.CUSTOM
            }
            isLoading={isLoading}
            successfullyUpdated={successfullyUpdated}
          />
        )}
        <div className={styles.optionDescription}>
          {smashServerType === SMASH_SERVER_TYPES.IOHK &&
            intl.formatMessage(messages.descriptionIOHKcontent)}
          {smashServerType === SMASH_SERVER_TYPES.DIRECT &&
            intl.formatMessage(messages.descriptionNone)}
        </div>
        {/*<div className={styles.description}>
          <p>{intl.formatMessage(messages.description1)}</p>
          <ol className={styles.description}>
            <li>
              <p>
                <b>{intl.formatMessage(messages.descriptionItem1Title)}</b>
              </p>
              <p>{intl.formatMessage(messages.descriptionItem1Content1)}</p>
              <p>{intl.formatMessage(messages.descriptionItem1Content2)}</p>
            </li>
            <li>
              <p>
                <b>{intl.formatMessage(messages.descriptionItem2Title)}</b>
              </p>
              <p>{intl.formatMessage(messages.descriptionItem2Content)}</p>
            </li>
          </ol>
          <p>{intl.formatMessage(messages.description2)}</p>
        </div>*/}
      </div>
    );
  }
}
