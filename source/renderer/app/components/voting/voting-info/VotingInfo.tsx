import React from 'react';
import { observer } from 'mobx-react';
import BorderedBox from '../../widgets/BorderedBox';
import type { Locale } from '../../../../../common/types/locales.types';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingInfo.scss' or its corr... Remove this comment to see the full error message
import styles from './VotingInfo.scss';
import ResultsPhase from './ResultsPhase';
import SnapshotPhase from './SnapshotPhase';
import VotingPhase from './VotingPhase';
import TallyingPhase from './TallyingPhase';
import Headline from './Headline';
import AppStore from './AppStore';
import RegisterToVote from './RegisterToVote';
import { FundPhases } from '../../../stores/VotingStore';
import type { FundPhase } from '../../../stores/VotingStore';
import type { CatalystFund } from '../../../api/voting/types';

type Props = {
  currentLocale: Locale;
  currentDateFormat: string;
  currentTimeFormat: string;
  fundPhase: FundPhase;
  fundInfo: CatalystFund;
  onRegisterToVoteClick: (...args: Array<any>) => any;
  onExternalLinkClick: (...args: Array<any>) => any;
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
  fundInfo,
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
            <PhaseComponent
              fundInfo={fundInfo}
              currentLocale={currentLocale}
              currentDateFormat={currentDateFormat}
              currentTimeFormat={currentTimeFormat}
              onExternalLinkClick={onExternalLinkClick}
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
              fundInfo={fundInfo}
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
