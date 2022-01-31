import React, { Component } from 'react';
import classnames from 'classnames';
import { map, omit } from 'lodash';
import { Select } from 'react-polymorph/lib/components/Select';
import { Link } from 'react-polymorph/lib/components/Link';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import {
  defineMessages,
  intlShape,
  FormattedMessage,
  FormattedHTMLMessage,
} from 'react-intl';
import { getSmashServerIdFromUrl, getUrlParts } from '../../../utils/staking';
import InlineEditingInput from '../../widgets/forms/InlineEditingInput';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakePoolsSettings.scss' or ... Remove this comment to see the full error message
import styles from './StakePoolsSettings.scss';
import {
  SMASH_SERVERS_LIST,
  SMASH_SERVER_TYPES,
  SMASH_URL_VALIDATOR,
} from '../../../config/stakingConfig';
import type { SmashServerType } from '../../../types/stakingTypes';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/spinner... Remove this comment to see the full error message
import spinningIcon from '../../../assets/images/spinner-ic.inline.svg';
import globalMessages from '../../../i18n/global-messages';
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
  changesSaved: {
    id: 'inline.editing.input.changesSaved',
    defaultMessage: '!!!Your changes have been saved',
    description:
      'Message "Your changes have been saved" for inline editing (eg. on Profile Settings page).',
  },
  invalidUrl: {
    id: 'settings.stakePools.smashUrl.input.invalidUrl',
    defaultMessage: '!!!Invalid URL',
    description:
      'invalidUrl for the "Smash Custom Server" selection on the Stake Pools settings page.',
  },
  invalidUrlPrefix: {
    id: 'settings.stakePools.smashUrl.input.invalidUrlPrefix',
    defaultMessage: '!!!The URL should start with "https://"',
    description:
      'invalidUrlPrefix for the "Smash Custom Server" selection on the Stake Pools settings page.',
  },
  invalidUrlParameter: {
    id: 'settings.stakePools.smashUrl.input.invalidUrlParameter',
    defaultMessage:
      '!!!Only "https://" protocol and hostname (e.g. domain.com) are allowed',
    description:
      'invalidUrlParameter for the "Smash Custom Server" selection on the Stake Pools settings page.',
  },
});
type Props = {
  smashServerUrl: string | null | undefined;
  smashServerUrlError?: LocalizableError | null | undefined;
  onSelectSmashServerUrl: (...args: Array<any>) => any;
  onResetSmashServerError: (...args: Array<any>) => any;
  isLoading: boolean;
  onOpenExternalLink: (...args: Array<any>) => any;
  isSyncing: boolean;
  syncPercentage: number;
};
type State = {
  editingSmashServerUrl: string | null | undefined;
  successfullyUpdated: boolean;
  wasLoading: boolean;
};
const { isSelfnode } = global.environment;

@observer
class StakePoolsSettings extends Component<Props, State> {
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
    const { onSelectSmashServerUrl, onResetSmashServerError } = this.props;
    onResetSmashServerError();
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
  handleErrorMessage = (value: string) => {
    const { intl } = this.context;
    let errorMessage = messages.invalidUrl;
    const { pathname, search } = getUrlParts(value);
    if (!/^https:\/\//i.test(value)) errorMessage = messages.invalidUrlPrefix;
    else if (search || (pathname && pathname.slice(1)))
      errorMessage = messages.invalidUrlParameter;
    return intl.formatMessage(errorMessage);
  };
  smashSelectMessages = {
    iohk: <FormattedHTMLMessage {...messages.smashSelectIOHKServer} />,
    direct: this.context.intl.formatMessage(messages.smashSelectDirect),
    custom: this.context.intl.formatMessage(messages.smashSelectCustomServer),
    none: null,
  };
  renderSmashTypeDropdown = () => {
    const { isSyncing } = this.props;
    const { intl } = this.context;
    const { editingSmashServerUrl, successfullyUpdated } = this.state;
    const smashServerType = getSmashServerIdFromUrl(
      editingSmashServerUrl || ''
    );
    const smashServerTypes = isSelfnode
      ? omit(
          SMASH_SERVER_TYPES,
          SMASH_SERVERS_LIST[SMASH_SERVER_TYPES.IOHK].name
        )
      : SMASH_SERVER_TYPES;
    const smashSelectOptions = map(smashServerTypes, (value) => ({
      label: this.smashSelectMessages[value] || value,
      value,
    }));
    const selectedValue =
      !isSyncing && smashServerType
        ? this.smashSelectMessages[smashServerType] || smashServerType
        : '-';

    if (isSyncing) {
      return (
        <div className={styles.disabledSelect}>
          <div className={styles.label}>
            {intl.formatMessage(messages.smashSelectLabel)}
          </div>
          <div className={styles.input}>{selectedValue}</div>
          <SVGInline svg={spinningIcon} className={styles.icon} />
        </div>
      );
    }

    return (
      <Select
        label={
          <div>
            {intl.formatMessage(messages.smashSelectLabel)}
            {successfullyUpdated && (
              <span className={styles.savingResultLabel}>
                {intl.formatMessage(messages.changesSaved)}
              </span>
            )}
          </div>
        }
        value={smashServerType}
        options={smashSelectOptions}
        onChange={this.handleOnSelectSmashServerType}
        className={styles.select}
        optionHeight={50}
        selectionRenderer={({ label }) => (
          <div className={styles.selectionRenderer}>{label}</div>
        )}
      />
    );
  };
  renderSmashCustomServerInput = () => {
    const { smashServerUrlError, isLoading, isSyncing } = this.props;
    const { intl } = this.context;
    const { editingSmashServerUrl } = this.state;
    const smashServerType = getSmashServerIdFromUrl(
      editingSmashServerUrl || ''
    );
    const errorMessage = smashServerUrlError
      ? intl.formatMessage(smashServerUrlError)
      : null;
    const smashServerUrlStyles = classnames([
      styles.smashServerUrl,
      isSyncing ? styles.syncing : null,
    ]);

    if (smashServerType !== SMASH_SERVER_TYPES.CUSTOM) {
      return null;
    }

    return (
      <InlineEditingInput
        className={smashServerUrlStyles}
        label={intl.formatMessage(messages.smashURLInputLabel)}
        value={editingSmashServerUrl || ''}
        placeholder={intl.formatMessage(messages.smashUrlInputPlaceholder)}
        onSubmit={this.handleSubmit}
        isValid={this.handleIsValid}
        valueErrorMessage={this.handleErrorMessage}
        errorMessage={errorMessage}
        readOnly={isLoading}
        isLoading={isLoading}
        successfullyUpdated={false}
      />
    );
  };
  renderBottomContent = () => {
    const { onOpenExternalLink, isSyncing, syncPercentage } = this.props;
    const { intl } = this.context;
    const { editingSmashServerUrl } = this.state;
    const smashServerType = getSmashServerIdFromUrl(
      editingSmashServerUrl || ''
    );

    if (isSyncing) {
      return (
        <div className={styles.optionDescription}>
          <FormattedHTMLMessage
            {...globalMessages.featureUnavailableWhileSyncing}
            values={{
              syncPercentage,
            }}
          />
        </div>
      );
    }

    if (smashServerType === SMASH_SERVER_TYPES.IOHK) {
      return (
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
      );
    }

    if (smashServerType === SMASH_SERVER_TYPES.DIRECT) {
      return (
        <div className={styles.optionDescription}>
          <FormattedHTMLMessage {...messages.descriptionNone} />
        </div>
      );
    }

    return null;
  };

  render() {
    const { onOpenExternalLink } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.component}>
        <div className={styles.description}>
          <FormattedMessage
            {...messages.description}
            values={{
              link: (
                <Link
                  className={styles.link}
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

        {this.renderSmashTypeDropdown()}
        {this.renderSmashCustomServerInput()}
        {this.renderBottomContent()}
      </div>
    );
  }
}

export default StakePoolsSettings;
