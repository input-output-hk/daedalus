// @flow
import React, { Component } from 'react';
import { map } from 'lodash';
import { Select } from 'react-polymorph/lib/components/Select';
import { Link } from 'react-polymorph/lib/components/Link';
import { observer } from 'mobx-react';
import {
  defineMessages,
  intlShape,
  FormattedMessage,
  FormattedHTMLMessage,
} from 'react-intl';
import { getSmashServerIdFromUrl } from '../../../utils/staking';
import InlineEditingInput from '../../widgets/forms/InlineEditingInput';
import styles from './StakePoolsSettings.scss';
import {
  SMASH_SERVERS_LIST,
  SMASH_SERVER_TYPES,
  SMASH_URL_VALIDATOR,
} from '../../../config/stakingConfig';
import type { SmashServerType } from '../../../types/stakingTypes';

import LocalizableError from '../../../i18n/LocalizableError';

const messages = defineMessages({
  description: {
    id: 'settings.stakePools.smash.description',
    defaultMessage:
      '!!!The {link} is an off-chain metadata server that enables the fast loading of stake pool details. Stake pools are also curated and each server has a different curation policy.',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionLinkLabel: {
    id: 'settings.stakePools.smash.descriptionLinkLabel',
    defaultMessage: '!!!Stakepool Metadata Aggregation Server (SMASH)',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionLinkUrl: {
    id: 'settings.stakePools.smash.descriptionLinkUrl',
    defaultMessage:
      '!!!https://iohk.io/en/blog/posts/2020/11/17/in-pools-we-trust/',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionIOHKContent1: {
    id: 'settings.stakePools.smash.descriptionIOHKContent1',
    defaultMessage:
      '!!!The IOHK server ensures that registered stake pools are valid, helps to avoid duplicated ticker names or trademarks, and checks that the pools do not feature potentially offensive or harmful information.',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionIOHKContent2: {
    id: 'settings.stakePools.smash.descriptionIOHKContent2',
    defaultMessage:
      '!!!This allows us to deal with any scams, trolls, or abusive behavior by filtering out potentially problematic actors. {link} about the IOHK SMASH server.',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionIOHKLinkLabel: {
    id: 'settings.stakePools.smash.descriptionIOHKLinkLabel',
    defaultMessage: '!!!Read more',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionIOHKLinkUrl: {
    id: 'settings.stakePools.smash.descriptionIOHKLinkUrl',
    defaultMessage:
      '!!!https://iohk.io/en/blog/posts/2020/11/17/in-pools-we-trust/',
    description: 'description for the Stake Pools settings page.',
  },
  descriptionNone: {
    id: 'settings.stakePools.smash.descriptionNone',
    defaultMessage:
      '!!!<b>This option is not recommended!</b> Without the off-chain metadata server your Daedalus client will fetch this data by contacting every stake pool individually, which is a very slow and resource-consuming process. The list of stake pools received is not curated, so Daedalus will receive legitimate pools, duplicates, and fake pools. An added risk to this process is that your antivirus or antimalware software could recognize the thousands of network requests as malicious behavior by the Daedalus client.',
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
  onOpenExternalLink: Function,
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

  handleIsValid = (url: string) => url === '' || SMASH_URL_VALIDATOR.test(url);

  smashSelectMessages = {
    iohk: <FormattedHTMLMessage {...messages.smashSelectIOHKServer} />,
    iohk1: this.context.intl.formatMessage(messages.smashSelectIOHKServer),
    testingKnown: SMASH_SERVERS_LIST.testingKnown.name,
    direct: this.context.intl.formatMessage(messages.smashSelectDirect),
    custom: this.context.intl.formatMessage(messages.smashSelectCustomServer),
  };

  render() {
    const { smashServerUrlError, isLoading, onOpenExternalLink } = this.props;
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
          <FormattedMessage
            {...messages.description}
            values={{
              link: (
                <Link
                  onClick={() =>
                    onOpenExternalLink(
                      intl.formatMessage(messages.descriptionLinkUrl)
                    )
                  }
                  label={intl.formatMessage(messages.descriptionLinkLabel)}
                />
              ),
            }}
          />
        </div>
        <Select
          label={intl.formatMessage(messages.smashSelectLabel)}
          value={smashServerType}
          options={smashSelectOptions}
          onChange={this.handleOnSelectSmashServerType}
          className={styles.select}
          optionHeight={50}
          selectionRenderer={({ label }) => (
            <div className={styles.selectionRenderer}>{label}</div>
          )}
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
        {smashServerType === SMASH_SERVER_TYPES.IOHK && (
          <div className={styles.optionDescription}>
            <p>{intl.formatMessage(messages.descriptionIOHKContent1)}</p>
            <p>
              <FormattedMessage
                {...messages.descriptionIOHKContent2}
                values={{
                  link: (
                    <Link
                      onClick={() =>
                        onOpenExternalLink(
                          intl.formatMessage(messages.descriptionIOHKLinkUrl)
                        )
                      }
                      label={intl.formatMessage(
                        messages.descriptionIOHKLinkLabel
                      )}
                    />
                  ),
                }}
              />
            </p>
          </div>
        )}

        {smashServerType === SMASH_SERVER_TYPES.DIRECT && (
          <div className={styles.optionDescription}>
            <FormattedHTMLMessage {...messages.descriptionNone} />
          </div>
        )}
      </div>
    );
  }
}
