// @flow
import React from 'react';
import { observer } from 'mobx-react';
import BorderedBox from '../../widgets/BorderedBox';
import type { Locale } from '../../../../../common/types/locales.types';
import styles from './VotingInfo.scss';
import UpcomingFund from './UpcomingFund';
import VotingOpen from './VotingOpen';
import Headline from './Headline';
import AppStore from './AppStore';
import RegisterToVote from './RegisterToVote';
import {
  VOTING_SNAPSHOT_DATE,
  VOTING_CAST_START_DATE,
  VOTING_CAST_END_DATE,
} from '../../../config/votingConfig';

type Props = {
  currentLocale: Locale,
  currentDateFormat: string,
  currentTimeFormat: string,
  fundPhase: FundPhase,
  onRegisterToVoteClick: Function,
  onExternalLinkClick: Function,
};

const phaseToComponentMap = {
  [FundPhases.SNAPSHOT]: SnapshotPhase,
  [FundPhases.VOTING]: VotingPhase,
  [FundPhases.TALLYING]: TallyingPhase,
  [FundPhases.RESULTS]: ResultsPhase,
};

const VotingInfo = ({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  fundPhase,
  onRegisterToVoteClick,
  onExternalLinkClick,
}: Props) => {
  const PhaseComponent = phaseToComponentMap[fundPhase || FundPhases.SNAPSHOT];

    return (
      <div className={styles.component}>
        <BorderedBox>
          <Headline onExternalLinkClick={onExternalLinkClick} />
          <hr className={styles.separator} />
          <div className={styles.bottomContent}>
            <div className={styles.leftContent}>
              <VotingOpen
                currentLocale={currentLocale}
                currentDateFormat={currentDateFormat}
                currentTimeFormat={currentTimeFormat}
              />
              <div className={styles.appStoreSpacing}>
                <AppStore
                  onAppleStoreLinkClick={onExternalLinkClick}
                  onAndroidStoreLinkClick={onExternalLinkClick}
                />
              </div>
            </div>
            <div className={styles.rightContent}>
              <RegisterToVote
                currentLocale={currentLocale}
                currentDateFormat={currentDateFormat}
                currentTimeFormat={currentTimeFormat}
                onRegisterToVoteClick={onRegisterToVoteClick}
              />
            </div>
          </div>
          <div className={styles.rightContent}>
            <RegisterToVote
              currentLocale={currentLocale}
              currentDateFormat={currentDateFormat}
              currentTimeFormat={currentTimeFormat}
              onRegisterToVoteClick={onRegisterToVoteClick}
            />
          </div>
        </div>
      </BorderedBox>
    </div>
  );
};

export default observer(VotingInfo);
