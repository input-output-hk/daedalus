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
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
const Stepper_1 = require('@react-polymorph/components/Stepper');
const StepperSkin_1 = require('@react-polymorph/skins/simple/StepperSkin');
const lodash_1 = require('lodash');
const mobx_react_1 = require('mobx-react');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const DialogBackButton_1 = __importDefault(
  require('../../widgets/DialogBackButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const StakePoolsList_1 = require('../stake-pools/StakePoolsList');
const StakePoolsSearch_1 = require('../stake-pools/StakePoolsSearch');
const helpers_1 = require('../stake-pools/helpers');
const BackToTopButton_1 = __importDefault(
  require('../../widgets/BackToTopButton')
);
const DelegationSteps_scss_1 = __importDefault(
  require('./DelegationSteps.scss')
);
const DelegationStepsChooseStakePoolDialog_scss_1 = __importDefault(
  require('./DelegationStepsChooseStakePoolDialog.scss')
);
const ThumbSelectedPool_1 = __importDefault(
  require('../widgets/ThumbSelectedPool')
);
const stakingConfig_1 = require('../../../config/stakingConfig');
const DelegationStepsChooseStakePoolDialog_messages_1 = require('./DelegationStepsChooseStakePoolDialog.messages');
const OversaturationText_1 = require('./OversaturationText');
const messages = (0,
DelegationStepsChooseStakePoolDialog_messages_1.getMessages)();
const Footer = (0, react_1.memo)((props) =>
  react_1.default.createElement(
    'div',
    {
      className:
        DelegationStepsChooseStakePoolDialog_scss_1.default.retiringPoolFooter,
    },
    props.footerText
  )
);
const DelegationStepsChooseStakePoolDialog = (0, mobx_react_1.observer)(
  (props) => {
    const {
      stepsList,
      recentStakePools,
      stakePoolsList,
      onOpenExternalLink,
      currentTheme,
      selectedPool: preselectedPool,
      selectedWallet,
      onClose,
      onBack,
      intl,
      oversaturationPercentage,
    } = props;
    const [searchValue, setSearchValue] = (0, react_1.useState)('');
    const [selectedPool, setSelectedPool] = (0, react_1.useState)(
      preselectedPool
    );
    const [filteredStakePoolsList, setFilteredStakePoolsList] = (0,
    react_1.useState)([]);
    const stakePoolsScrollElementRef = (0, react_1.useRef)();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    const handleSearch = (0, react_1.useCallback)((value) => {
      setSearchValue(value);
    });
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    const handleClearSearch = (0, react_1.useCallback)(() => {
      setSearchValue('');
    });
    const handleSelect = (0, react_1.useCallback)(
      (value) => {
        const _selectedPool = (0, lodash_1.find)(
          stakePoolsList,
          (stakePool) => stakePool.id === value
        );
        setSelectedPool(_selectedPool);
        props.onThumbPoolSelect(_selectedPool.id);
      },
      [props.onThumbPoolSelect, stakePoolsList]
    );
    (0, react_1.useEffect)(() => {
      if (preselectedPool && preselectedPool.id)
        handleSelect(preselectedPool.id);
    }, [preselectedPool]);
    const onContinue = (0, react_1.useCallback)(() => {
      props.onContinue(selectedPool);
    }, [props.onContinue, selectedPool]);
    const {
      name: selectedWalletName,
      lastDelegatedStakePoolId,
      delegatedStakePoolId,
      pendingDelegations,
    } = selectedWallet || {};
    const hasPendingDelegations =
      pendingDelegations && pendingDelegations.length > 0;
    let activeStakePoolId = delegatedStakePoolId;
    if (hasPendingDelegations) {
      activeStakePoolId = lastDelegatedStakePoolId;
    }
    const selectedPoolTicker = selectedPool?.ticker;
    const canSubmit =
      !activeStakePoolId || activeStakePoolId !== selectedPool?.id;
    const actions = [
      {
        className: 'continueButton',
        label: intl.formatMessage(messages.continueButtonLabel),
        onClick: onContinue,
        primary: true,
        disabled: !selectedPool?.id || !canSubmit,
      },
    ];
    const dialogClassName = (0, classnames_1.default)([
      DelegationSteps_scss_1.default.delegationSteps,
      DelegationStepsChooseStakePoolDialog_scss_1.default
        .delegationStepsChooseStakePoolDialogWrapper,
    ]);
    const contentClassName = (0, classnames_1.default)([
      DelegationSteps_scss_1.default.content,
      DelegationStepsChooseStakePoolDialog_scss_1.default.content,
    ]);
    const stepsIndicatorLabel = react_1.default.createElement(
      react_intl_1.FormattedMessage,
      {
        ...messages.stepIndicatorLabel,
        values: {
          currentStep: 2,
          totalSteps: stepsList.length,
        },
      }
    );
    (0, react_1.useEffect)(() => {
      setFilteredStakePoolsList(
        (0, helpers_1.getFilteredStakePoolsList)(stakePoolsList, searchValue)
      );
    }, [stakePoolsList, searchValue]);
    const numberOfRankedStakePools = stakePoolsList.filter(
      (stakePool) =>
        stakingConfig_1.IS_RANKING_DATA_AVAILABLE &&
        stakePool.nonMyopicMemberRewards
    ).length;
    const getSelectionPoolLabel = (0, react_1.useCallback)(() => {
      let label;
      // Label when selected wallet already delegating to selected stake pool
      if (
        selectedPool?.id &&
        activeStakePoolId === delegatedStakePoolId &&
        delegatedStakePoolId === selectedPool?.id
      ) {
        label = react_1.default.createElement(
          react_intl_1.FormattedHTMLMessage,
          {
            ...messages.delegatedStakePoolLabel,
            values: {
              selectedWalletName,
              selectedPoolTicker,
            },
          }
        );
      } else if (
        selectedPool?.id &&
        activeStakePoolId === lastDelegatedStakePoolId &&
        lastDelegatedStakePoolId === selectedPool?.id
      ) {
        label = react_1.default.createElement(
          react_intl_1.FormattedHTMLMessage,
          {
            ...messages.delegatedStakePoolNextLabel,
            values: {
              selectedWalletName,
              selectedPoolTicker,
            },
          }
        );
      } else if (selectedPool?.id) {
        // Stake pool selected and selected wallet are not delegated to it
        const message = !selectedPool.retiring
          ? messages.selectedStakePoolLabel
          : messages.selectedStakePoolLabelRetiring;
        label = react_1.default.createElement(
          react_intl_1.FormattedHTMLMessage,
          {
            ...message,
            values: {
              selectedWalletName,
              selectedPoolTicker,
            },
          }
        );
      } else {
        // Stake pool not selected.
        label = react_1.default.createElement(
          react_intl_1.FormattedHTMLMessage,
          {
            ...messages.selectStakePoolLabel,
            values: {
              selectedWalletName,
            },
          }
        );
      }
      return label;
    }, [
      activeStakePoolId,
      delegatedStakePoolId,
      lastDelegatedStakePoolId,
      selectedWalletName,
      selectedPool?.id,
      selectedPoolTicker,
      selectedPool,
    ]);
    const footer = (0, react_1.useMemo)(
      () =>
        react_1.default.createElement(
          react_1.default.Fragment,
          null,
          selectedPool?.retiring &&
            react_1.default.createElement(Footer, {
              footerText: intl.formatMessage(messages.retiringPoolFooter),
            }),
          oversaturationPercentage > 0 &&
            react_1.default.createElement(
              OversaturationText_1.OversaturationText,
              {
                oversaturationPercentage: oversaturationPercentage.toFixed(2),
                centerText: true,
              }
            )
        ),
      [selectedPool?.retiring, oversaturationPercentage]
    );
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.title),
        subtitle: stepsIndicatorLabel,
        actions: actions,
        footer: footer,
        closeOnOverlayClick: true,
        onClose: onClose,
        className: dialogClassName,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onClose }
        ),
        backButton: react_1.default.createElement(DialogBackButton_1.default, {
          onBack: onBack,
        }),
        scrollWrapperRef: stakePoolsScrollElementRef,
      },
      react_1.default.createElement(BackToTopButton_1.default, {
        scrollableElementClassName: 'Dialog_contentWrapper',
        buttonTopPosition: 110,
        scrollTopToActivate: 470,
      }),
      react_1.default.createElement(
        'div',
        {
          className:
            DelegationSteps_scss_1.default.delegationStepsIndicatorWrapper,
        },
        react_1.default.createElement(Stepper_1.Stepper, {
          steps: stepsList,
          activeStep: 2,
          skin: StepperSkin_1.StepperSkin,
          labelDisabled: true,
        })
      ),
      react_1.default.createElement(
        'div',
        { className: contentClassName },
        react_1.default.createElement(
          'p',
          {
            className:
              DelegationStepsChooseStakePoolDialog_scss_1.default.description,
          },
          intl.formatMessage(messages.description)
        ),
        react_1.default.createElement(
          'div',
          {
            className:
              DelegationStepsChooseStakePoolDialog_scss_1.default
                .selectStakePoolWrapper,
          },
          selectedPool &&
            react_1.default.createElement(ThumbSelectedPool_1.default, {
              stakePool: selectedPool,
              numberOfRankedStakePools: numberOfRankedStakePools,
              alreadyDelegated: selectedPool && !canSubmit,
            }),
          react_1.default.createElement(
            'p',
            {
              className:
                DelegationStepsChooseStakePoolDialog_scss_1.default
                  .selectStakePoolLabel,
            },
            getSelectionPoolLabel()
          )
        ),
        react_1.default.createElement(
          'div',
          {
            className:
              DelegationStepsChooseStakePoolDialog_scss_1.default
                .recentStakePoolsWrapper,
          },
          recentStakePools.length > 0 &&
            react_1.default.createElement(
              'p',
              {
                className:
                  DelegationStepsChooseStakePoolDialog_scss_1.default
                    .recentStakePoolsLabel,
              },
              react_1.default.createElement(react_intl_1.FormattedMessage, {
                ...messages.recentPoolsLabel,
                values: {},
              })
            ),
          react_1.default.createElement(
            StakePoolsList_1.StakePoolsList,
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ listName: string; stakePoolsList: StakePoo... Remove this comment to see the full error message
            {
              // @ts-ignore ts-migrate(2322) FIXME: Type '{ listName: string; stakePoolsList: StakePoo... Remove this comment to see the full error message
              listName: 'recentStakePools',
              stakePoolsList: recentStakePools,
              onOpenExternalLink: onOpenExternalLink,
              currentTheme: currentTheme,
              containerClassName: 'Dialog_content',
              onSelect: handleSelect,
              selectedPoolId: selectedPool?.id,
              numberOfRankedStakePools: numberOfRankedStakePools,
              disabledStakePoolId: activeStakePoolId,
              highlightOnHover: true,
              highlightWithDelay: true,
              selectOnClick: true,
              scrollElementRef: stakePoolsScrollElementRef,
            }
          )
        ),
        react_1.default.createElement(
          'div',
          {
            className:
              DelegationStepsChooseStakePoolDialog_scss_1.default
                .searchStakePoolsWrapper,
          },
          react_1.default.createElement(StakePoolsSearch_1.StakePoolsSearch, {
            search: searchValue,
            label: intl.formatMessage(messages.searchInputLabel),
            placeholder: intl.formatMessage(messages.searchInputPlaceholder),
            onSearch: handleSearch,
            onClearSearch: handleClearSearch,
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            scrollableElementClassName: 'Dialog_content',
            disabledStakePoolId: activeStakePoolId,
          })
        ),
        react_1.default.createElement(
          'div',
          {
            className:
              DelegationStepsChooseStakePoolDialog_scss_1.default
                .stakePoolsListWrapper,
          },
          react_1.default.createElement(
            StakePoolsList_1.StakePoolsList,
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ listName: string; stakePoolsList: StakePoo... Remove this comment to see the full error message
            {
              // @ts-ignore ts-migrate(2322) FIXME: Type '{ listName: string; stakePoolsList: StakePoo... Remove this comment to see the full error message
              listName: 'selectedIndexList',
              stakePoolsList: filteredStakePoolsList,
              onOpenExternalLink: onOpenExternalLink,
              currentTheme: currentTheme,
              onSelect: handleSelect,
              selectedPoolId: selectedPool?.id,
              containerClassName: 'Dialog_content',
              numberOfRankedStakePools: numberOfRankedStakePools,
              disabledStakePoolId: activeStakePoolId,
              highlightOnHover: true,
              highlightWithDelay: true,
              selectOnClick: true,
              scrollElementRef: stakePoolsScrollElementRef,
            }
          )
        )
      )
    );
  }
);
exports.default = (0, react_intl_1.injectIntl)(
  DelegationStepsChooseStakePoolDialog
);
//# sourceMappingURL=DelegationStepsChooseStakePoolDialog.js.map
