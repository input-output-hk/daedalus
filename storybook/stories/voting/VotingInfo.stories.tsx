import React from 'react';
import { action } from '@storybook/addon-actions';
import { withKnobs } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import VotingInfo from '../../../source/renderer/app/components/voting/voting-info/VotingInfo';
import { FundPhase } from '../../../source/renderer/app/stores/VotingStore';
import { CatalystFund } from '../../../source/renderer/app/api/voting/types';
import { VotingFooterLinks } from '../../../source/renderer/app/components/voting/VotingFooterLinks';
import {
  DATE_ENGLISH_OPTIONS,
  LANGUAGE_OPTIONS,
  TIME_OPTIONS,
} from '../../../source/renderer/app/config/profileConfig';
import VerticalFlexContainer from '../../../source/renderer/app/components/layout/VerticalFlexContainer';
import { Locale } from '../../../source/common/types/locales.types';
import { mockFundInfo } from './_support/fundInfo';

const votingInfo = {
  fundInfo: mockFundInfo,
  currentLocale: LANGUAGE_OPTIONS[0].value as Locale,
  currentDateFormat: DATE_ENGLISH_OPTIONS[0].value,
  currentTimeFormat: TIME_OPTIONS[0].value,
  onRegisterToVoteClick: action('onRegisterToVoteClick'),
  onExternalLinkClick: action('onExternalLinkClick'),
};

export default {
  title: 'Voting / Voting Info',

  decorators: [
    (story) => (
      <StoryDecorator>
        <VerticalFlexContainer>
          {story()}
          <VotingFooterLinks />
        </VerticalFlexContainer>
      </StoryDecorator>
    ),
    withKnobs,
  ],
};

export const SnapshotPhase = {
  render: () => <VotingInfo {...votingInfo} fundPhase={FundPhase.SNAPSHOT} />,

  name: 'Snapshot phase',
};

export const VotingPhase = {
  render: () => <VotingInfo {...votingInfo} fundPhase={FundPhase.VOTING} />,

  name: 'Voting phase',
};

export const TallyingPhase = {
  render: () => <VotingInfo {...votingInfo} fundPhase={FundPhase.TALLYING} />,

  name: 'Tallying phase',
};

export const ResultsPhase = {
  render: () => <VotingInfo {...votingInfo} fundPhase={FundPhase.RESULTS} />,

  name: 'Results phase',
};

export const ApiError = {
  render: () => <VotingInfo {...votingInfo} fundPhase={null} />,
  name: 'API error',
};
