import React from 'react';
import BigNumber from 'bignumber.js';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import {
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import Wallet, { HwDeviceStatuses } from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import DelegationStepsConfirmationDialog from './DelegationStepsConfirmationDialog';

// Keep the actual confirmation form and its device/password/fee predicates;
// replace only the common modal shell and device-status presentation.
jest.mock('../../widgets/Dialog', () => ({
  __esModule: true,
  default: ({
    children,
    actions,
  }: {
    children: React.ReactNode;
    actions: Array<{
      label: React.ReactNode;
      onClick: () => void;
      disabled?: boolean;
    }>;
  }) => (
    <div>
      {children}
      {actions.map((action, index) => (
        <button key={index} onClick={action.onClick} disabled={action.disabled}>
          {action.label}
        </button>
      ))}
    </div>
  ),
}));
jest.mock(
  '../../hardware-wallet/HardwareWalletStatus',
  () =>
    function () {
      return null;
    }
);

const baseProps = (): React.ComponentProps<
  typeof DelegationStepsConfirmationDialog
> => ({
  selectedWallet: {
    id: 'wallet',
    name: 'Wallet',
    isHardwareWallet: false,
  } as unknown as Wallet,
  selectedPool: { id: 'pool', ticker: 'POOL' } as unknown as StakePool,
  stepsList: ['Wallet', 'Stake pool', 'Confirmation'],
  transactionFee: {
    fee: new BigNumber('0.17'),
    deposits: new BigNumber(2),
    depositsReclaimed: new BigNumber(0),
  },
  isSubmitting: false,
  error: null,
  hwDeviceStatus: HwDeviceStatuses.CONNECTING,
  isTrezor: false,
  oversaturationPercentage: 0,
  onConfirm: jest.fn(),
  onClose: jest.fn(),
  onBack: jest.fn(),
  onExternalLinkClick: jest.fn(),
});
const form = (
  props: React.ComponentProps<typeof DelegationStepsConfirmationDialog>
) => (
  <ThemeProvider
    theme={daedalusTheme}
    skins={SimpleSkins}
    variables={SimpleDefaults}
    themeOverrides={themeOverrides}
  >
    <IntlProvider locale="en-US" messages={translations}>
      <DelegationStepsConfirmationDialog {...props} />
    </IntlProvider>
  </ThemeProvider>
);

describe('restored delegation confirmation', () => {
  afterEach(cleanup);

  it('requires the spending password and forwards it only after explicit confirmation', async () => {
    const props = baseProps();
    const { container } = render(form(props));
    expect(screen.getByRole('button', { name: 'Confirm' })).toBeDisabled();
    expect(props.onConfirm).not.toHaveBeenCalled();
    const password = container.querySelector<HTMLInputElement>(
      'input[type="password"]'
    );
    if (!password) throw new Error('Missing spending password field');
    fireEvent.change(password, { target: { value: 'wallet-password' } });
    await waitFor(() =>
      expect(screen.getByRole('button', { name: 'Confirm' })).toBeEnabled()
    );
    fireEvent.click(screen.getByRole('button', { name: 'Confirm' }));
    await waitFor(() =>
      expect(props.onConfirm).toHaveBeenCalledWith('wallet-password', false)
    );
  });

  it('cancels without submitting', () => {
    const props = baseProps();
    render(form(props));
    fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(props.onClose).toHaveBeenCalledTimes(1);
    expect(props.onConfirm).not.toHaveBeenCalled();
  });

  it('requires successful device verification and a calculated fee for hardware approval', () => {
    const props = baseProps();
    props.selectedWallet = {
      ...props.selectedWallet,
      isHardwareWallet: true,
    } as Wallet;
    const view = render(form(props));
    expect(screen.getByRole('button', { name: 'Confirm' })).toBeDisabled();
    view.rerender(
      form({
        ...props,
        hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
        transactionFee: null,
      })
    );
    expect(screen.getByRole('button', { name: 'Confirm' })).toBeDisabled();
    view.rerender(
      form({
        ...props,
        hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
      })
    );
    expect(screen.getByRole('button', { name: 'Confirm' })).toBeEnabled();
    expect(props.onConfirm).not.toHaveBeenCalled();
  });
});
