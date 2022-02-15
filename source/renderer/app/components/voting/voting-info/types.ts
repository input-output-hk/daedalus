import { FundPhase } from '../../../stores/VotingStore';
import type { Intl } from '../../../types/i18nTypes';
import type { CatalystFund } from '../../../api/voting/types';
import type { Locale } from '../../../../../common/types/locales.types';

export type VotingProps = {
  currentLocale: Locale;
  currentDateFormat: string;
  currentTimeFormat: string;
  fundPhase: FundPhase;
  fundInfo: CatalystFund;
  onRegisterToVoteClick: (...args: Array<any>) => any;
  onExternalLinkClick: (...args: Array<any>) => any;
};

export type PhaseProps = {} & Omit<
  VotingProps,
  'fundPhase' | 'onRegisterToVoteClick'
>;

export type PhaseIntlProps = { intl: Intl } & PhaseProps;
