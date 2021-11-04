// @flow
import React, { useEffect, useRef, useState, useCallback, memo } from 'react';
import {
  injectIntl,
  FormattedMessage,
  FormattedHTMLMessage,
  intlShape,
} from 'react-intl';
import classNames from 'classnames';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import { find } from 'lodash';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import { StakePoolsList } from '../stake-pools/StakePoolsList';
import { StakePoolsSearch } from '../stake-pools/StakePoolsSearch';
import { getFilteredStakePoolsList } from '../stake-pools/helpers';
import BackToTopButton from '../../widgets/BackToTopButton';
import commonStyles from './DelegationSteps.scss';
import styles from './DelegationStepsChooseStakePoolDialog.scss';
import Wallet from '../../../domains/Wallet';
import ThumbSelectedPool from '../widgets/ThumbSelectedPool';
import { IS_RANKING_DATA_AVAILABLE } from '../../../config/stakingConfig';
import StakePool from '../../../domains/StakePool';
import { getMessages } from './DelegationStepsChooseStakePoolDialog.messages';
import OversaturationText from './OversaturationText';

const messages = getMessages();

type Props = {
  maxDelegationFunds: number,
  stepsList: Array<string>,
  recentStakePools: Array<StakePool>,
  stakePoolsList: Array<StakePool>,
  selectedWallet: ?Wallet,
  onOpenExternalLink: Function,
  currentTheme: string,
  selectedPool: ?StakePool,
  onClose: Function,
  onBack: Function,
  onSelectPool: Function,
  intl: intlShape.isRequired,
};

type FooterProps = {
  footerText: string,
};

const Footer = memo((props: FooterProps) => (
  <div className={styles.retiringPoolFooter}>{props.footerText}</div>
));

const getAmountBeforeBeingSaturated = (
  amountToBeStaked: number,
  selectedPool: StakePool,
  maxDelegationFunds: number
): boolean =>
  selectedPool.saturation < 100
    ? ((100 - selectedPool.saturation) * maxDelegationFunds) / 100
    : 0;

const DelegationStepsChooseStakePoolDialog = (props: Props) => {
  const {
    maxDelegationFunds,
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
  } = props;

  const [searchValue, setSearchValue] = useState<string>('');
  const [selectedPool, setSelectedPool] = useState<?StakePool>(preselectedPool);
  const [oversaturationWarning, setOversaturationWarning] = useState<string>(
    ''
  );
  const stakePoolsScrollElementRef = useRef();

  const handleSearch = useCallback((value: string) => {
    setSearchValue(value);
  });

  const handleClearSearch = useCallback(() => {
    setSearchValue('');
  });

  const handleSelect = useCallback((value: string) => {
    setSelectedPool(
      find(stakePoolsList, (stakePool) => stakePool.id === value)
    );
  });

  const onAcceptPool = useCallback(() => {
    props.onSelectPool(selectedPool?.id);
  }, [selectedPool?.id]);

  useEffect(() => {
    if (selectedPool?.id) {
      console.log('TCL: selectedPool ticker => ', selectedPool.ticker);
      console.log('TCL: selectedPool saturation => ', selectedPool.saturation);
      console.log('TCL: selectedPool => ', selectedPool);

      setOversaturationWarning('Oversaturated');
    }
  }, [selectedPool?.id]);

  const {
    name: selectedWalletName,
    lastDelegatedStakePoolId,
    delegatedStakePoolId,
    pendingDelegations,
  } = selectedWallet;

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
      onClick: onAcceptPool,
      primary: true,
      disabled: !selectedPool?.id || !canSubmit,
    },
  ];

  const dialogClassName = classNames([
    commonStyles.delegationSteps,
    styles.delegationStepsChooseStakePoolDialogWrapper,
  ]);

  const contentClassName = classNames([commonStyles.content, styles.content]);

  const stepsIndicatorLabel = (
    <FormattedMessage
      {...messages.stepIndicatorLabel}
      values={{
        currentStep: 2,
        totalSteps: stepsList.length,
      }}
    />
  );

  const filteredStakePoolsList: Array<StakePool> = useCallback(
    getFilteredStakePoolsList(stakePoolsList, searchValue),
    [stakePoolsList, searchValue]
  );

  const numberOfRankedStakePools: number = stakePoolsList.filter(
    (stakePool) => IS_RANKING_DATA_AVAILABLE && stakePool.nonMyopicMemberRewards
  ).length;

  const getSelectionPoolLabel = useCallback(() => {
    let label;
    // Label when selected wallet already delegating to selected stake pool
    if (
      selectedPool?.id &&
      activeStakePoolId === delegatedStakePoolId &&
      delegatedStakePoolId === selectedPool?.id
    ) {
      label = (
        <FormattedHTMLMessage
          {...messages.delegatedStakePoolLabel}
          values={{
            selectedWalletName,
            selectedPoolTicker,
          }}
        />
      );
    } else if (
      selectedPool?.id &&
      activeStakePoolId === lastDelegatedStakePoolId &&
      lastDelegatedStakePoolId === selectedPool?.id
    ) {
      label = (
        <FormattedHTMLMessage
          {...messages.delegatedStakePoolNextLabel}
          values={{
            selectedWalletName,
            selectedPoolTicker,
          }}
        />
      );
    } else if (selectedPool?.id) {
      // Stake pool selected and selected wallet are not delegated to it
      const message = !selectedPool.retiring
        ? messages.selectedStakePoolLabel
        : messages.selectedStakePoolLabelRetiring;
      label = (
        <FormattedHTMLMessage
          {...message}
          values={{
            selectedWalletName,
            selectedPoolTicker,
          }}
        />
      );
    } else {
      // Stake pool not selected.
      label = (
        <FormattedHTMLMessage
          {...messages.selectStakePoolLabel}
          values={{
            selectedWalletName,
          }}
        />
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

  return (
    <Dialog
      title={intl.formatMessage(messages.title)}
      subtitle={stepsIndicatorLabel}
      actions={actions}
      footer={
        selectedPool && selectedPool.retiring ? (
          <Footer
            footerText={intl.formatMessage(messages.retiringPoolFooter)}
          />
        ) : null
      }
      closeOnOverlayClick
      onClose={onClose}
      className={dialogClassName}
      closeButton={<DialogCloseButton onClose={onClose} />}
      backButton={<DialogBackButton onBack={onBack} />}
      scrollWrapperRef={stakePoolsScrollElementRef}
    >
      <BackToTopButton
        scrollableElementClassName="Dialog_contentWrapper"
        buttonTopPosition={100}
        scrollTopToActivate={100}
      />

      <div className={commonStyles.delegationStepsIndicatorWrapper}>
        <Stepper
          steps={stepsList}
          activeStep={2}
          skin={StepperSkin}
          labelDisabled
        />
      </div>

      <div className={contentClassName}>
        <OversaturationText />
        <p className={styles.description}>
          {intl.formatMessage(messages.description)}
        </p>

        <div className={styles.selectStakePoolWrapper}>
          <ThumbSelectedPool
            stakePool={selectedPool}
            numberOfRankedStakePools={numberOfRankedStakePools}
            alreadyDelegated={selectedPool && !canSubmit}
          />

          <p className={styles.selectStakePoolLabel}>
            {getSelectionPoolLabel()}
          </p>
        </div>

        <div className={styles.recentStakePoolsWrapper}>
          {recentStakePools.length > 0 && (
            <p className={styles.recentStakePoolsLabel}>
              <FormattedMessage {...messages.recentPoolsLabel} values={{}} />
            </p>
          )}
          <StakePoolsList
            listName="recentStakePools"
            stakePoolsList={recentStakePools}
            onOpenExternalLink={onOpenExternalLink}
            currentTheme={currentTheme}
            containerClassName="Dialog_content"
            onSelect={handleSelect}
            selectedPoolId={selectedPool?.id}
            numberOfRankedStakePools={numberOfRankedStakePools}
            disabledStakePoolId={activeStakePoolId}
            highlightOnHover
            highlightWithDelay
            selectOnClick
            scrollElementRef={stakePoolsScrollElementRef}
          />
        </div>

        <div className={styles.searchStakePoolsWrapper}>
          <StakePoolsSearch
            search={searchValue}
            label={intl.formatMessage(messages.searchInputLabel)}
            placeholder={intl.formatMessage(messages.searchInputPlaceholder)}
            onSearch={handleSearch}
            onClearSearch={handleClearSearch}
            scrollableElementClassName="Dialog_content"
            disabledStakePoolId={activeStakePoolId}
          />
        </div>

        <div className={styles.stakePoolsListWrapper}>
          <StakePoolsList
            listName="selectedIndexList"
            stakePoolsList={filteredStakePoolsList}
            onOpenExternalLink={onOpenExternalLink}
            currentTheme={currentTheme}
            onSelect={handleSelect}
            selectedPoolId={selectedPool?.id}
            containerClassName="Dialog_content"
            numberOfRankedStakePools={numberOfRankedStakePools}
            disabledStakePoolId={activeStakePoolId}
            highlightOnHover
            highlightWithDelay
            selectOnClick
            scrollElementRef={stakePoolsScrollElementRef}
          />
        </div>
      </div>
    </Dialog>
  );
};

export default injectIntl(DelegationStepsChooseStakePoolDialog);
