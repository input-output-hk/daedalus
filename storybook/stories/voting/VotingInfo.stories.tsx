import React from 'react';
import { storiesOf } from '@storybook/react';
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

storiesOf('Voting / Voting Info', module)
  .addDecorator((story) => (
    <StoryDecorator>
      <VerticalFlexContainer>
        {story()}
        <VotingFooterLinks />
      </VerticalFlexContainer>
    </StoryDecorator>
  ))
  .addDecorator(withKnobs) // ====== Stories ======
  .add('Snapshot phase', () => (
    <VotingInfo {...votingInfo} fundPhase={FundPhase.SNAPSHOT} />
  ))
  .add('Voting phase', () => (
    <VotingInfo {...votingInfo} fundPhase={FundPhase.VOTING} />
  ))
  .add('Tallying phase', () => (
    <VotingInfo {...votingInfo} fundPhase={FundPhase.TALLYING} />
  ))
  .add('Results phase', () => (
    <VotingInfo {...votingInfo} fundPhase={FundPhase.RESULTS} />
  ))
  .add('API error', () => <VotingInfo {...votingInfo} fundPhase={null} />);
