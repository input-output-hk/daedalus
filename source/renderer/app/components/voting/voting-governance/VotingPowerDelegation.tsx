import React, { useState } from 'react';
import { observer } from 'mobx-react';
import { injectIntl, FormattedHTMLMessage } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';

import BorderedBox from '../../widgets/BorderedBox';
import { messages } from './VotingPowerDelegation.messages';
import styles from './VotingPowerDelegation.scss';
import type { Intl } from '../../../types/i18nTypes';
import { FormattedHTMLMessageWithLink } from '../../widgets/FormattedHTMLMessageWithLink';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Wallet from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import ItemsDropdown from '../../widgets/forms/ItemsDropdown';

type Props = {
  getStakePoolById: (...args: Array<any>) => any;
  intl: Intl;
  onExternalLinkClick: (...args: Array<any>) => any;
  stakePools: Array<StakePool>;
  wallets: Array<Wallet>;
};

function VotingPowerDelegation({
  getStakePoolById,
  intl,
  onExternalLinkClick,
  wallets,
  stakePools,
}: Props) {
  const [selectedWalletId, setSelectedWalletId] = useState<string | null>(null);
  const [selectedVoteType, setSelectedVoteType] = useState<string | null>(null);
  const [drepInputValue, setDrepInputValue] = useState<string>('');
  const voteTypes = [
    {
      value: 'abstain',
      label: 'Abstain',
    },
    {
      value: 'noConfidence',
      label: 'No confidence',
    },
    {
      value: 'drep',
      label: 'Delegate to dRep',
    },
  ];
  return (
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
          <p>
            <FormattedHTMLMessage {...messages.paragraph2} />
          </p>
          <p>
            <FormattedHTMLMessageWithLink
              message={{
                ...messages.paragraph3,
                values: {
                  linkLabel: messages.learnMoreLinkLabel,
                  linkURL: messages.paragraph3LinkUrl,
                },
              }}
              onExternalLinkClick={onExternalLinkClick}
            />
          </p>
        </div>

        <WalletsDropdown
          className={styles.walletSelect}
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; label: any; numberOfStakeP... Remove this comment to see the full error message
          label={'Select a wallet to delegate from'}
          numberOfStakePools={stakePools.length}
          wallets={wallets}
          onChange={(walletId: string) => setSelectedWalletId(walletId)}
          placeholder={'Select a wallet …'}
          value={selectedWalletId}
          getStakePoolById={getStakePoolById}
        />

        {selectedWalletId && (
          <ItemsDropdown
            className={styles.voteTypeSelect}
            label={'Select voting registration type'}
            placeholder={'Select delegation option'}
            options={voteTypes}
            handleChange={(option) => setSelectedVoteType(option.value)}
            value={selectedVoteType}
          />
        )}

        {selectedVoteType && (
          <Input
            className={styles.drepInput}
            onChange={setDrepInputValue}
            spellCheck={false}
            value={drepInputValue}
            label={
              <div>
                Please type or paste a valid DRep ID here. Look up{' '}
                <a onClick={() => onExternalLinkClick('https://google.com')}>
                  DRep directory
                </a>
              </div>
            }
            placeholder={'Paste DRep ID here.'}
          />
        )}
      </BorderedBox>
    </div>
  );
}

export default injectIntl(observer(VotingPowerDelegation));
