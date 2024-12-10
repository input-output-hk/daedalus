import React, { useEffect, useState } from 'react';
import { observer } from 'mobx-react';
import { injectIntl, FormattedMessage } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { Button } from 'react-polymorph/lib/components/Button';
import { Link } from 'react-polymorph/lib/components/Link';

import BigNumber from 'bignumber.js';
import { Cardano } from '@cardano-sdk/core';
import BorderedBox from '../../widgets/BorderedBox';
import { messages } from './VotingPowerDelegation.messages';
import styles from './VotingPowerDelegation.scss';
import type { Intl } from '../../../types/i18nTypes';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Wallet from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import ItemsDropdown from '../../widgets/forms/ItemsDropdown';
import { Separator } from '../../widgets/separator/Separator';
import { InitializeVPDelegationTxError } from '../../../stores/VotingStore';
import { VoteType } from './types';
import { sharedGovernanceMessages } from './shared-messages';

type Props = {
  getStakePoolById: (...args: Array<any>) => any;
  initiateTransaction: (params: {
    chosenOption: string;
    wallet: Wallet;
  }) => Promise<
    | { success: true; fees: BigNumber }
    | { success: false; errorCode: InitializeVPDelegationTxError }
  >;
  intl: Intl;
  onExternalLinkClick: (...args: Array<any>) => any;
  stakePools: Array<StakePool>;
  wallets: Array<Wallet>;
  renderConfirmationDialog: (params: {
    chosenOption: string;
    fees: BigNumber;
    onClose: () => void;
    selectedWallet: Wallet;
  }) => React.ReactElement;
};

type FormData = {
  selectedWallet: Wallet;
  selectedVoteType: VoteType;
  drepInputState: {
    dirty: boolean;
    value: string;
  };
  fees?: BigNumber;
};

type Form = Omit<FormData, 'selectedWallet'> & {
  selectedWallet: Wallet | null;
  status: 'form';
};

type FormWithError = Omit<FormData, 'status'> & {
  txInitError: InitializeVPDelegationTxError;
  status: 'form-with-error';
};

type StateFormComplete = FormData & {
  status: 'form-submitted' | 'form-initiating-tx';
};

type StateConfirmation = Omit<FormData, 'fee'> & {
  fees: BigNumber;
  status: 'confirmation';
};

type State = Form | FormWithError | StateFormComplete | StateConfirmation;

// TODO discuss if we need to restrict the length
const isDrepIdValid = (drepId: string) => {
  const isDRepId = (value: string): value is Cardano.DRepID =>
    Cardano.DRepID.isValid(value);
  return isDRepId(drepId) && Cardano.DRepID.toCip105DRepID(drepId) === drepId;
};

const mapOfTxErrorCodeToIntl: Record<
  InitializeVPDelegationTxError,
  typeof messages[keyof typeof messages]
> = {
  generic: messages.initializeTxErrorGeneric,
  same_vote: messages.initializeTxErrorSameVote,
  no_utxos_available: messages.initializeNotEnoughMoney,
  not_enough_money: messages.initializeNotEnoughMoney,
};

const initialState: State = {
  status: 'form',
  selectedWallet: null,
  selectedVoteType: 'drep',
  drepInputState: {
    dirty: false,
    value: '',
  },
};

function VotingPowerDelegation({
  getStakePoolById,
  initiateTransaction,
  intl,
  onExternalLinkClick,
  renderConfirmationDialog,
  wallets,
  stakePools,
}: Props) {
  const [state, setState] = useState<State>(initialState);

  const drepInputIsValid = isDrepIdValid(state.drepInputState.value);

  const formIsValid =
    !!state.selectedWallet &&
    (state.selectedVoteType === 'drep' ? drepInputIsValid : true);

  const submitButtonDisabled =
    !formIsValid ||
    state.status === 'form-submitted' ||
    state.status === 'form-with-error' ||
    state.status === 'form-initiating-tx';

  const voteTypes: { value: VoteType; label: string }[] = [
    {
      value: 'abstain',
      label: intl.formatMessage(sharedGovernanceMessages.abstain),
    },
    {
      value: 'no_confidence',
      label: intl.formatMessage(sharedGovernanceMessages.noConfidence),
    },
    {
      value: 'drep',
      label: intl.formatMessage(sharedGovernanceMessages.delegateToDRep),
    },
  ];

  const chosenOption =
    state.selectedVoteType === 'drep'
      ? state.drepInputState.value
      : state.selectedVoteType;

  useEffect(() => {
    (async () => {
      if (state.status !== 'form-submitted') return;
      setState({
        ...state,
        status: 'form-initiating-tx',
      });
      const result = await initiateTransaction({
        chosenOption,
        wallet: state.selectedWallet,
      });

      if (result.success === true) {
        setState({
          ...state,
          fees: result.fees,
          status: 'confirmation',
        });
      } else {
        setState({
          ...state,
          txInitError: result.errorCode,
          status: 'form-with-error',
        });
      }
    })();
  }, [initiateTransaction, intl, state]);

  return (
    <>
      <div className={styles.component}>
        <BorderedBox>
          <h1 className={styles.heading}>
            {intl.formatMessage(messages.heading)}
          </h1>
          <div className={styles.info}>
            <p>
              <FormattedMessage
                {...messages.paragraph1}
                values={{
                  Link: (
                    <Link
                      className={styles.link}
                      href={intl.formatMessage(messages.paragraph1LinkUrl)}
                      label={intl.formatMessage(messages.paragraph1LinkText)}
                      onClick={(event) =>
                        onExternalLinkClick(
                          intl.formatMessage(messages.paragraph1LinkUrl),
                          event
                        )
                      }
                    />
                  ),
                }}
              />
            </p>
          </div>

          <Separator />

          <WalletsDropdown
            className={styles.walletSelect}
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; label: any; numberOfStakeP... Remove this comment to see the full error message
            label={intl.formatMessage(messages.selectWalletLabel)}
            numberOfStakePools={stakePools.length}
            wallets={wallets}
            onChange={(walletId: string) => {
              const selectedWallet = wallets.find((w) => w.id === walletId);
              setState({
                ...initialState,
                selectedWallet,
              });
            }}
            placeholder={intl.formatMessage(messages.selectWalletPlaceholder)}
            value={state.selectedWallet?.id || null}
            getStakePoolById={getStakePoolById}
            disableSyncingWallets
          />

          {state.selectedWallet && (
            <ItemsDropdown
              className={styles.voteTypeSelect}
              label={intl.formatMessage(messages.selectVotingTypeLabel)}
              options={voteTypes}
              handleChange={(option) =>
                setState({
                  ...state,
                  selectedVoteType: option.value,
                  status: 'form',
                })
              }
              value={state.selectedVoteType}
            />
          )}

          {state.selectedWallet && state.selectedVoteType === 'drep' && (
            <Input
              className={styles.drepInput}
              onChange={(value) => {
                setState({
                  ...state,
                  drepInputState: {
                    dirty: true,
                    value,
                  },
                  status: 'form',
                });
              }}
              spellCheck={false}
              value={state.drepInputState.value}
              label={
                <FormattedMessage
                  {...(environment.isPreprod
                    ? messages.drepInputLabelPreprod
                    : messages.drepInputLabel)}
                  values={{
                    drepDirectoryLink: (
                      <Link
                        className={styles.link}
                        label={intl.formatMessage(
                          messages.drepInputLabelLinkText
                        )}
                        href="#"
                        onClick={(event) =>
                          onExternalLinkClick(
                            intl.formatMessage(
                              environment.isMainnet
                                ? messages.drepInputLabelLinkUrl
                                : messages.drepInputLabelLinkUrlPreview
                            ),
                            event
                          )
                        }
                      />
                    ),
                  }}
                />
              }
              placeholder={intl.formatMessage(messages.drepInputPlaceholder)}
              error={
                state.drepInputState.dirty && !drepInputIsValid
                  ? intl.formatMessage(messages.drepInputError)
                  : undefined
              }
            />
          )}

          {state.status === 'form-with-error' && (
            <p className={styles.generalError}>
              {intl.formatMessage(mapOfTxErrorCodeToIntl[state.txInitError])}
            </p>
          )}

          <Button
            label={intl.formatMessage(messages.submitLabel)}
            className={styles.voteSubmit}
            disabled={submitButtonDisabled}
            onClick={() => {
              setState({
                ...state,
                status: 'form-submitted',
              });
            }}
          />
        </BorderedBox>
      </div>
      {state.status === 'confirmation' &&
        renderConfirmationDialog({
          chosenOption,
          fees: state.fees,
          onClose: () => {
            setState({
              ...state,
              status: 'form',
            });
          },
          selectedWallet: state.selectedWallet,
        })}
    </>
  );
}

export default injectIntl(observer(VotingPowerDelegation));
