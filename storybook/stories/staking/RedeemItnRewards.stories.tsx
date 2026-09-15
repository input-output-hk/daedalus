import React from 'react';
import { storiesOf } from '@storybook/react';
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

storiesOf('Decentralization / Redeem ITN Rewards', module)
  .addDecorator(stakingDecorator) // ====== Stories ======
  .add('Step 1', Step1ConfigurationDialogStory, {
    id: 'redeem-itn-wallets-story',
  })
  .add('Step 2', Step2ConfirmationDialogStory, {
    id: 'redeem-itn-wallets-story',
  })
  .add('Step 3 - Success', Step3SuccessDialogStory, {
    id: 'redeem-itn-wallets-story',
  })
  .add('Step 3 - Failure', Step3FailureDialogStory, {
    id: 'redeem-itn-wallets-story',
  })
  .add('No Wallets', NoWalletsDialogDialogStory, {
    id: 'redeem-itn-wallets-story',
  })
  .add('Redemption Unavailable', RedemptionUnavailableDialogDialogStory, {
    id: 'redeem-itn-wallets-story',
  });
