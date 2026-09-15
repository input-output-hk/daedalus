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

export const Step1 = {
  render: Step1ConfigurationDialogStory,

  parameters: {
    id: 'redeem-itn-wallets-story',
  },
};

export const Step2 = {
  render: Step2ConfirmationDialogStory,

  parameters: {
    id: 'redeem-itn-wallets-story',
  },
};

export const Step3Success = {
  render: Step3SuccessDialogStory,
  name: 'Step 3 - Success',

  parameters: {
    id: 'redeem-itn-wallets-story',
  },
};

export const Step3Failure = {
  render: Step3FailureDialogStory,
  name: 'Step 3 - Failure',

  parameters: {
    id: 'redeem-itn-wallets-story',
  },
};

export const NoWallets = {
  render: NoWalletsDialogDialogStory,

  parameters: {
    id: 'redeem-itn-wallets-story',
  },
};

export const RedemptionUnavailable = {
  render: RedemptionUnavailableDialogDialogStory,

  parameters: {
    id: 'redeem-itn-wallets-story',
  },
};
