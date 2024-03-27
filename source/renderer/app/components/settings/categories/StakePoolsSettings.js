'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const lodash_1 = require('lodash');
const Select_1 = require('@react-polymorph/components/Select');
const Link_1 = require('@react-polymorph/components/Link');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const staking_1 = require('../../../utils/staking');
const InlineEditingInput_1 = __importDefault(
  require('../../widgets/forms/InlineEditingInput')
);
const StakePoolsSettings_scss_1 = __importDefault(
  require('./StakePoolsSettings.scss')
);
const stakingConfig_1 = require('../../../config/stakingConfig');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/spinner... Remove this comment to see the full error message
const spinner_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/spinner-ic.inline.svg')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const messages = (0, react_intl_1.defineMessages)({
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
const { isSelfnode } = global.environment;
let StakePoolsSettings = class StakePoolsSettings extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  /* eslint-disable react/no-unused-state */
  // Disabling eslint due to a [known issue](https://github.com/yannickcr/eslint-plugin-react/issues/2061)
  // `wasLoading` is actually used in the `getDerivedStateFromProps` method
  static getDerivedStateFromProps(
    { isLoading, smashServerUrlError },
    { wasLoading }
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
  handleSubmit = (url) => {
    if (this.handleIsValid(url)) {
      this.setState({
        editingSmashServerUrl: url,
      });
      this.props.onSelectSmashServerUrl(url);
    }
  };
  handleOnSelectSmashServerType = (smashServerType) => {
    const { onSelectSmashServerUrl, onResetSmashServerError } = this.props;
    onResetSmashServerError();
    let editingSmashServerUrl = '';
    if (smashServerType !== stakingConfig_1.SMASH_SERVER_TYPES.CUSTOM) {
      editingSmashServerUrl =
        stakingConfig_1.SMASH_SERVERS_LIST[smashServerType].url;
      onSelectSmashServerUrl(editingSmashServerUrl);
    }
    this.setState({
      editingSmashServerUrl,
    });
  };
  handleIsValid = (url) =>
    url === '' || stakingConfig_1.SMASH_URL_VALIDATOR.test(url);
  handleErrorMessage = (value) => {
    const { intl } = this.context;
    let errorMessage = messages.invalidUrl;
    const { pathname, search } = (0, staking_1.getUrlParts)(value);
    if (!/^https:\/\//i.test(value)) errorMessage = messages.invalidUrlPrefix;
    else if (search || (pathname && pathname.slice(1)))
      errorMessage = messages.invalidUrlParameter;
    return intl.formatMessage(errorMessage);
  };
  smashSelectMessages = {
    iohk: react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
      ...messages.smashSelectIOHKServer,
    }),
    direct: this.context.intl.formatMessage(messages.smashSelectDirect),
    custom: this.context.intl.formatMessage(messages.smashSelectCustomServer),
    none: null,
  };
  renderSmashTypeDropdown = () => {
    const { isSyncing } = this.props;
    const { intl } = this.context;
    const { editingSmashServerUrl, successfullyUpdated } = this.state;
    const smashServerType = (0, staking_1.getSmashServerIdFromUrl)(
      editingSmashServerUrl || ''
    );
    const smashServerTypes = isSelfnode
      ? (0, lodash_1.omit)(
          stakingConfig_1.SMASH_SERVER_TYPES,
          stakingConfig_1.SMASH_SERVERS_LIST[
            stakingConfig_1.SMASH_SERVER_TYPES.IOHK
          ].name
        )
      : stakingConfig_1.SMASH_SERVER_TYPES;
    const smashSelectOptions = (0, lodash_1.map)(smashServerTypes, (value) => ({
      label: this.smashSelectMessages[value] || value,
      value,
    }));
    const selectedValue =
      !isSyncing && smashServerType
        ? this.smashSelectMessages[smashServerType] || smashServerType
        : '-';
    if (isSyncing) {
      return react_1.default.createElement(
        'div',
        { className: StakePoolsSettings_scss_1.default.disabledSelect },
        react_1.default.createElement(
          'div',
          { className: StakePoolsSettings_scss_1.default.label },
          intl.formatMessage(messages.smashSelectLabel)
        ),
        react_1.default.createElement(
          'div',
          { className: StakePoolsSettings_scss_1.default.input },
          selectedValue
        ),
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: spinner_ic_inline_svg_1.default,
          className: StakePoolsSettings_scss_1.default.icon,
        })
      );
    }
    return react_1.default.createElement(Select_1.Select, {
      label: react_1.default.createElement(
        'div',
        null,
        intl.formatMessage(messages.smashSelectLabel),
        successfullyUpdated &&
          react_1.default.createElement(
            'span',
            { className: StakePoolsSettings_scss_1.default.savingResultLabel },
            intl.formatMessage(messages.changesSaved)
          )
      ),
      value: smashServerType,
      options: smashSelectOptions,
      onChange: this.handleOnSelectSmashServerType,
      optionHeight: 50,
      selectionRenderer: ({ label }) =>
        react_1.default.createElement(
          'div',
          { className: StakePoolsSettings_scss_1.default.selectionRenderer },
          label
        ),
    });
  };
  renderSmashCustomServerInput = () => {
    const { smashServerUrlError, isLoading, isSyncing } = this.props;
    const { intl } = this.context;
    const { editingSmashServerUrl } = this.state;
    const smashServerType = (0, staking_1.getSmashServerIdFromUrl)(
      editingSmashServerUrl || ''
    );
    const errorMessage = smashServerUrlError
      ? intl.formatMessage(smashServerUrlError)
      : null;
    const smashServerUrlStyles = (0, classnames_1.default)([
      StakePoolsSettings_scss_1.default.smashServerUrl,
      isSyncing ? StakePoolsSettings_scss_1.default.syncing : null,
    ]);
    if (smashServerType !== stakingConfig_1.SMASH_SERVER_TYPES.CUSTOM) {
      return null;
    }
    return react_1.default.createElement(InlineEditingInput_1.default, {
      className: smashServerUrlStyles,
      label: intl.formatMessage(messages.smashURLInputLabel),
      value: editingSmashServerUrl || '',
      placeholder: intl.formatMessage(messages.smashUrlInputPlaceholder),
      onSubmit: this.handleSubmit,
      isValid: this.handleIsValid,
      valueErrorMessage: this.handleErrorMessage,
      errorMessage: errorMessage,
      readOnly: isLoading,
      isLoading: isLoading,
      successfullyUpdated: false,
    });
  };
  renderBottomContent = () => {
    const { onOpenExternalLink, isSyncing, syncPercentage } = this.props;
    const { intl } = this.context;
    const { editingSmashServerUrl } = this.state;
    const smashServerType = (0, staking_1.getSmashServerIdFromUrl)(
      editingSmashServerUrl || ''
    );
    if (isSyncing) {
      return react_1.default.createElement(
        'div',
        { className: StakePoolsSettings_scss_1.default.optionDescription },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...global_messages_1.default.featureUnavailableWhileSyncing,
          values: {
            syncPercentage,
          },
        })
      );
    }
    if (smashServerType === stakingConfig_1.SMASH_SERVER_TYPES.IOHK) {
      return react_1.default.createElement(
        'div',
        { className: StakePoolsSettings_scss_1.default.optionDescription },
        react_1.default.createElement(
          'p',
          null,
          intl.formatMessage(messages.descriptionIOHKContent1)
        ),
        react_1.default.createElement(
          'p',
          null,
          react_1.default.createElement(react_intl_1.FormattedMessage, {
            ...messages.descriptionIOHKContent2,
            values: {
              link: react_1.default.createElement(Link_1.Link, {
                onClick: () =>
                  onOpenExternalLink(
                    intl.formatMessage(messages.descriptionIOHKLinkUrl)
                  ),
                label: intl.formatMessage(messages.descriptionIOHKLinkLabel),
              }),
            },
          })
        )
      );
    }
    if (smashServerType === stakingConfig_1.SMASH_SERVER_TYPES.DIRECT) {
      return react_1.default.createElement(
        'div',
        { className: StakePoolsSettings_scss_1.default.optionDescription },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...messages.descriptionNone,
        })
      );
    }
    return null;
  };
  render() {
    const { onOpenExternalLink } = this.props;
    const { intl } = this.context;
    return react_1.default.createElement(
      'div',
      { className: StakePoolsSettings_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: StakePoolsSettings_scss_1.default.description },
        react_1.default.createElement(react_intl_1.FormattedMessage, {
          ...messages.description,
          values: {
            link: react_1.default.createElement(Link_1.Link, {
              className: StakePoolsSettings_scss_1.default.link,
              onClick: () =>
                onOpenExternalLink(
                  intl.formatMessage(messages.descriptionLinkUrl)
                ),
              label: intl.formatMessage(messages.descriptionLinkLabel),
            }),
          },
        })
      ),
      this.renderSmashTypeDropdown(),
      this.renderSmashCustomServerInput(),
      this.renderBottomContent()
    );
  }
};
StakePoolsSettings = __decorate([mobx_react_1.observer], StakePoolsSettings);
exports.default = StakePoolsSettings;
//# sourceMappingURL=StakePoolsSettings.js.map
