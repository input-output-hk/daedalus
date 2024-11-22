import React, { useEffect, useMemo, useState } from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { Button } from 'react-polymorph/lib/components/Button';

import BigNumber from 'bignumber.js';
import BorderedBox from '../../widgets/BorderedBox';
import { messages } from './VotingPowerDelegation.messages';
import styles from './VotingPowerDelegation.scss';
import type { Intl } from '../../../types/i18nTypes';
import { FormattedHTMLMessageWithLink } from '../../widgets/FormattedHTMLMessageWithLink';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Wallet from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import ItemsDropdown from '../../widgets/forms/ItemsDropdown';
import { assertIsBech32WithPrefix } from '../../../../../common/utils/assertIsBech32WithPrefix';
import { Separator } from '../../widgets/separator/Separator';

type Props = {
  getFee: (params: {
    walletId: string;
  }) => Promise<
    { success: true; fees: BigNumber } | { success: false; error: string }
  >;
  getStakePoolById: (...args: Array<any>) => any;
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
  feeError?: string;
  selectedWallet: Wallet | null;
  status: 'form';
};

type StateFormComplete = FormData & {
  status: 'form-submitted' | 'form-awaiting-fee';
};

type StateConfirmation = Omit<FormData, 'fee'> & {
  fees: BigNumber;
  status: 'confirmation';
};

type State = Form | StateFormComplete | StateConfirmation;

type VoteType = 'abstain' | 'noConfidence' | 'drep';

// TODO discuss if we need to restrict the length
const isDrepIdValid = (drepId: string) => {
  try {
    assertIsBech32WithPrefix(drepId, ['drep', 'drep_script']);
  } catch (e) {
    return false;
  }

  return true;
};

function VotingPowerDelegation({
  getFee,
  getStakePoolById,
  intl,
  onExternalLinkClick,
  renderConfirmationDialog,
  wallets,
  stakePools,
}: Props) {
  const [state, setState] = useState<State>({
    status: 'form',
    selectedWallet: null,
    selectedVoteType: 'drep',
    drepInputState: {
      dirty: false,
      value: '',
    },
  });

  const drepInputIsValid = useMemo<boolean>(
    () =>
      state.drepInputState.dirty
        ? isDrepIdValid(state.drepInputState.value)
        : true,
    [state.drepInputState.dirty, state.drepInputState.value]
  );

  const formIsValid =
    !!state.selectedWallet &&
    (state.selectedVoteType === 'drep'
      ? state.drepInputState.dirty &&
        state.drepInputState.value &&
        drepInputIsValid
      : true);

  const submitButtonDisabled =
    !formIsValid &&
    (state.status === 'form-submitted' || state.status === 'form-awaiting-fee');

  const voteTypes: { value: VoteType; label: string }[] = [
    {
      value: 'abstain',
      label: intl.formatMessage(messages.abstain),
    },
    {
      value: 'noConfidence',
      label: intl.formatMessage(messages.noConfidence),
    },
    {
      value: 'drep',
      label: intl.formatMessage(messages.delegateToDRep),
    },
  ];

  useEffect(() => {
    (async () => {
      if (state.status !== 'form-submitted') return;
      setState({
        ...state,
        status: 'form-awaiting-fee',
      });
      const result = await getFee({ walletId: state.selectedWallet.id });

      if (result.success === true) {
        setState({
          ...state,
          fees: result.fees,
          status: 'confirmation',
        });
      } else {
        setState({
          ...state,
          feeError: result.error,
          status: 'form',
        });
      }
    })();
  }, [getFee, state]);

  return (
    <>
      <div className={styles.component}>
        <BorderedBox>
          <h1 className={styles.heading}>
            {intl.formatMessage(messages.heading)}
          </h1>
          <div className={styles.info}>
            <p>
              <FormattedHTMLMessageWithLink
                message={{
                  ...messages.paragraph1,
                  values: {
                    linkLabel: messages.learnMoreLinkLabel,
                    linkURL: messages.paragraph1LinkUrl,
                  },
                }}
                onExternalLinkClick={onExternalLinkClick}
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
              setState({ ...state, selectedWallet });
            }}
            placeholder={intl.formatMessage(messages.selectWalletPlaceholder)}
            value={state.selectedWallet?.id || null}
            getStakePoolById={getStakePoolById}
          />

          {state.selectedWallet && (
            <ItemsDropdown
              className={styles.voteTypeSelect}
              label={intl.formatMessage(messages.selectVotingTypeLabel)}
              options={voteTypes}
              handleChange={(option) =>
                setState({ ...state, selectedVoteType: option.value })
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
                });
              }}
              spellCheck={false}
              value={state.drepInputState.value}
              label={
                <div>
                  {intl.formatMessage(messages.drepInputLabel)}{' '}
                  <a
                    href="#"
                    onClick={(event) => {
                      event.preventDefault();
                      onExternalLinkClick('https://www.1694.io/en/dreps/list');
                    }}
                  >
                    {intl.formatMessage(messages.drepInputLabelLink)}
                  </a>
                </div>
              }
              placeholder={intl.formatMessage(messages.drepInputPlaceholder)}
              error={
                drepInputIsValid
                  ? undefined
                  : intl.formatMessage(messages.drepInputError)
              }
            />
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

          {state.status === 'form' && state.feeError && (
            <h5 className={styles.heading} style={{ color: 'red' }}>
              {state.feeError}
            </h5>
          )}
        </BorderedBox>
      </div>
      {state.status === 'confirmation' &&
        renderConfirmationDialog({
          chosenOption: state.drepInputState.value || state.selectedVoteType,
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
