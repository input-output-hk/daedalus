import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryLayout from '../_support/StoryLayout';
import StoryProvider from '../_support/StoryProvider';
import StoryDecorator from '../_support/StoryDecorator';
import { CATEGORIES_BY_NAME } from '../../../source/renderer/app/config/sidebarConfig';
import StakingWithNavigation from '../../../source/renderer/app/components/staking/layouts/StakingWithNavigation';
import {
  Step1ConfigurationDialogStory,
  Step2ConfirmationDialogStory,
  Step3SuccessDialogStory,
  Step3FailureDialogStory,
  NoWalletsDialogDialogStory,
  RedemptionUnavailableDialogDialogStory,
} from './_support/RedeemItnWallets';
import { stakingDecorator } from './_support/decorator';

export default {
  title: 'Decentralization / Redeem ITN Rewards',
  decorators: [stakingDecorator],
};

export const Step1 = Step1ConfigurationDialogStory;

Step1.parameters = {
  id: 'redeem-itn-wallets-story',
};

export const Step2 = Step2ConfirmationDialogStory;

Step2.parameters = {
  id: 'redeem-itn-wallets-story',
};

export const Step3Success = Step3SuccessDialogStory;

Step3Success.storyName = 'Step 3 - Success';

Step3Success.parameters = {
  id: 'redeem-itn-wallets-story',
};

export const Step3Failure = Step3FailureDialogStory;

Step3Failure.storyName = 'Step 3 - Failure';

Step3Failure.parameters = {
  id: 'redeem-itn-wallets-story',
};

export const NoWallets = NoWalletsDialogDialogStory;

NoWallets.parameters = {
  id: 'redeem-itn-wallets-story',
};

export const RedemptionUnavailable = RedemptionUnavailableDialogDialogStory;

RedemptionUnavailable.parameters = {
  id: 'redeem-itn-wallets-story',
};
