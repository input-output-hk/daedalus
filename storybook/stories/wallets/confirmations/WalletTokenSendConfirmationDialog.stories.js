// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Helpers
import BigNumber from 'bignumber.js';
import StoryDecorator from '../../_support/StoryDecorator';

// Screens
import WalletTokenSendConfirmationDialog
  from '../../../../source/renderer/app/components/wallet/WalletTokenSendConfirmationDialog';
import { DECIMAL_PLACES_IN_ADA } from '../../../../source/renderer/app/config/numbersConfig';
import { HwDeviceStatuses} from '../../../../source/renderer/app/domains/Wallet';
import { generateHash, generateWallet } from '../../_support/utils';
import { formattedAmountToNaturalUnits } from '../../../../source/renderer/app/utils/formatters';

storiesOf('Wallet Token Dialog|Send Confirmation', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Wallet Token Send Confirmation Dialog', () => (
    <div>
      <WalletTokenSendConfirmationDialog
        amount={new BigNumber(100100).toFormat(DECIMAL_PLACES_IN_ADA)}
        receiver={generateHash()}
        totalAmount={new BigNumber(100100).add(new BigNumber(0.10100)).toFormat(DECIMAL_PLACES_IN_ADA)}
        transactionFee={new BigNumber(0.10100).toFormat(DECIMAL_PLACES_IN_ADA)}
        amountToNaturalUnits={formattedAmountToNaturalUnits(new BigNumber(100100).toString())}
        onSubmit={() => null}
        isSubmitting={false}
        isFlight={false}
        onCancel={() => null}
        currencyUnit="USDC"
        onExternalLinkClick={() => null}
        hwDeviceStatus={HwDeviceStatuses.CONNECTING}
        isHardwareWallet={false}
        onInitiateTransaction={() => null}
        walletName={generateWallet('TrueUSD', '15119903750165').name}
        nativeTokens={[
          generateWallet('ADA', '55119903750165'),
          generateWallet('Tether', '25119903750165'),
          generateWallet('TrueUSD', '15119903750165'),
          generateWallet('USD Coin', '0'),
        ]}
      />
    </div>
  ));
