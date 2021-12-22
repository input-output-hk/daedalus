import React, {
  useRef,
  useState,
  useCallback,
  memo,
  useMemo,
  useEffect,
} from 'react';
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
import { observer } from 'mobx-react';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import { StakePoolsList } from '../stake-pools/StakePoolsList';
import { StakePoolsSearch } from '../stake-pools/StakePoolsSearch';
import { getFilteredStakePoolsList } from '../stake-pools/helpers';
import BackToTopButton from '../../widgets/BackToTopButton';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationSteps.scss' or its... Remove this comment to see the full error message
import commonStyles from './DelegationSteps.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DelegationStepsChooseStakePo... Remove this comment to see the full error message
import styles from './DelegationStepsChooseStakePoolDialog.scss';
import Wallet from '../../../domains/Wallet';
import ThumbSelectedPool from '../widgets/ThumbSelectedPool';
import { IS_RANKING_DATA_AVAILABLE } from '../../../config/stakingConfig';
import StakePool from '../../../domains/StakePool';
import { getMessages } from './DelegationStepsChooseStakePoolDialog.messages';
import { OversaturationText } from './OversaturationText';

const messages = getMessages();
type Props = {
  stepsList: Array<string>;
  recentStakePools: Array<StakePool>;
  stakePoolsList: Array<StakePool>;
  selectedWallet: Wallet | null | undefined;
  onOpenExternalLink: (...args: Array<any>) => any;
  currentTheme: string;
  selectedPool: StakePool | null | undefined;
  onClose: (...args: Array<any>) => any;
  onBack: (...args: Array<any>) => any;
  onSelectPool: (...args: Array<any>) => any;
  intl: intlShape.isRequired;
  onContinue: (...args: Array<any>) => any;
  oversaturationPercentage: number;
  onThumbPoolSelect: (...args: Array<any>) => any;
};
type FooterProps = {
  footerText: string;
};
const Footer = memo((props: FooterProps) => (
  <div className={styles.retiringPoolFooter}>{props.footerText}</div>
));
const DelegationStepsChooseStakePoolDialog = observer((props: Props) => {
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
  const [searchValue, setSearchValue] = useState<string>('');
  const [selectedPool, setSelectedPool] = useState<
    StakePool | null | undefined
  >(preselectedPool);
  const [filteredStakePoolsList, setFilteredStakePoolsList] = useState<
    Array<StakePool>
  >([]);
  const stakePoolsScrollElementRef = useRef();
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  const handleSearch = useCallback((value: string) => {
    setSearchValue(value);
  });
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  const handleClearSearch = useCallback(() => {
    setSearchValue('');
  });
  const handleSelect = useCallback(
    (value: string) => {
      const _selectedPool = find(
        stakePoolsList,
        (stakePool) => stakePool.id === value
      );

      setSelectedPool(_selectedPool);
      props.onThumbPoolSelect(_selectedPool.id);
    },
    [props.onThumbPoolSelect, stakePoolsList]
  );
  useEffect(() => {
    if (preselectedPool && preselectedPool.id) handleSelect(preselectedPool.id);
  }, [preselectedPool]);
  const onContinue = useCallback(() => {
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
  useEffect(() => {
    setFilteredStakePoolsList(
      getFilteredStakePoolsList(stakePoolsList, searchValue)
    );
  }, [stakePoolsList, searchValue]);
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
  const footer = useMemo(
    () => (
      <>
        {selectedPool?.retiring && (
          <Footer
            footerText={intl.formatMessage(messages.retiringPoolFooter)}
          />
        )}
        {oversaturationPercentage > 0 && (
          <OversaturationText
            oversaturationPercentage={oversaturationPercentage.toFixed(2)}
            centerText
          />
        )}
      </>
    ),
    [selectedPool?.retiring, oversaturationPercentage]
  );
  return (
    <Dialog
      title={intl.formatMessage(messages.title)}
      subtitle={stepsIndicatorLabel}
      actions={actions}
      footer={footer}
      closeOnOverlayClick
      onClose={onClose}
      className={dialogClassName}
      closeButton={<DialogCloseButton onClose={onClose} />}
      backButton={<DialogBackButton onBack={onBack} />}
      scrollWrapperRef={stakePoolsScrollElementRef}
    >
      <BackToTopButton
        scrollableElementClassName="Dialog_contentWrapper"
        buttonTopPosition={110}
        scrollTopToActivate={470}
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
        <p className={styles.description}>
          {intl.formatMessage(messages.description)}
        </p>

        <div className={styles.selectStakePoolWrapper}>
          {selectedPool && (
            <ThumbSelectedPool
              stakePool={selectedPool}
              numberOfRankedStakePools={numberOfRankedStakePools}
              alreadyDelegated={selectedPool && !canSubmit}
            />
          )}

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
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ listName: string; stakePoolsList: StakePoo... Remove this comment to see the full error message
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
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            scrollableElementClassName="Dialog_content"
            disabledStakePoolId={activeStakePoolId}
          />
        </div>

        <div className={styles.stakePoolsListWrapper}>
          <StakePoolsList
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ listName: string; stakePoolsList: StakePoo... Remove this comment to see the full error message
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
});
export default injectIntl(DelegationStepsChooseStakePoolDialog);
