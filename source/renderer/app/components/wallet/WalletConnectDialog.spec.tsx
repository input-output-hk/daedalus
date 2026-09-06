import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import StoryDecorator from '../../../../../storybook/stories/_support/StoryDecorator';
import {
  DeviceModels,
  DeviceTypes,
} from '../../../../common/types/hardware-wallets.types';
import translations from '../../i18n/locales/en-US.json';
import WalletConnectDialog from './WalletConnectDialog';

const transportDevice = {
  deviceId: 'flex',
  deviceType: DeviceTypes.LEDGER,
  deviceModel: DeviceModels.LEDGER_FLEX,
  deviceName: 'Ledger Flex',
  path: '/dev/hidraw9',
} as const;

const renderDialog = (hwDeviceStatus: 'connecting' | 'ready') => {
  const onPairWallet = jest.fn();
  render(
    <StoryDecorator>
      <IntlProvider locale="en-US" messages={translations}>
        <WalletConnectDialog
          onClose={jest.fn()}
          isSubmitting={false}
          hwDeviceStatus={hwDeviceStatus}
          transportDevice={transportDevice}
          error={null}
          onExternalLinkClick={jest.fn()}
          onPairWallet={onPairWallet}
        />
      </IntlProvider>
    </StoryDecorator>
  );
  return onPairWallet;
};

describe('WalletConnectDialog', () => {
  afterEach(cleanup);

  it('requires a valid wallet name before pairing a new device', () => {
    const onPairWallet = renderDialog('ready');
    const pairButton = screen.getByRole('button', { name: 'Pair wallet' });

    expect(screen.getByLabelText('Wallet name')).toBeVisible();
    expect(pairButton).toBeDisabled();

    fireEvent.change(screen.getByLabelText('Wallet name'), {
      target: { value: 'Ledger Flex wallet' },
    });
    fireEvent.click(pairButton);

    expect(onPairWallet).toHaveBeenCalledWith('Ledger Flex wallet');
  });

  it('does not offer pairing before the device is ready', () => {
    renderDialog('connecting');

    expect(screen.queryByLabelText('Wallet name')).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: 'Pair wallet' })
    ).not.toBeInTheDocument();
  });
});
