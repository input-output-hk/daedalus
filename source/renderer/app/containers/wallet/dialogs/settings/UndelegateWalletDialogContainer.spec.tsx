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
import translations from '../../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../../themes/daedalus';
import { themeOverrides } from '../../../../themes/overrides';
import { formattedWalletAmount } from '../../../../utils/formatters';
import UndelegateWalletDialogContainer from './UndelegateWalletDialogContainer';

jest.mock('../../../../components/widgets/Dialog', () => ({
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

const createFixture = () => {
  const wallet = {
    id: 'wallet',
    name: 'Wallet',
    isHardwareWallet: false,
    delegatedStakePoolId: 'pool',
  };
  const request = { isExecuting: false, error: null, reset: jest.fn() };
  const calculateDelegationFee = jest.fn().mockResolvedValue({
    fee: new BigNumber('0.17'),
    deposits: new BigNumber(0),
    depositsReclaimed: new BigNumber(0),
  });
  const trigger = jest.fn();
  const close = jest.fn();
  const resetHardware = jest.fn();
  const props = {
    onExternalLinkClick: jest.fn(),
    stores: {
      wallets: {
        allWallets: [wallet],
        getWalletById: () => wallet,
        undelegateWalletSubmissionSuccess: false,
        _undelegateWallet: jest.fn(),
      },
      staking: {
        calculateDelegationFee,
        quitStakePoolRequest: request,
        isDelegationTransactionPending: false,
        getStakePoolById: () => ({ name: 'Pool', ticker: 'POOL' }),
      },
      networkStatus: { futureEpoch: {} },
      profile: { currentLocale: 'en-US' },
      uiDialogs: { dataForActiveDialog: { walletId: 'wallet' } },
      hardwareWallets: {
        sendMoneyRequest: request,
        selectCoinsRequest: request,
        checkIsTrezorByWalletId: () => false,
        _resetTransaction: resetHardware,
      },
    },
    actions: {
      wallets: {
        undelegateWallet: { trigger },
        setUndelegateWalletSubmissionSuccess: { trigger: jest.fn() },
      },
      dialogs: { closeActiveDialog: { trigger: close } },
    },
  };
  const view = render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <UndelegateWalletDialogContainer
          {...(props as unknown as React.ComponentProps<
            typeof UndelegateWalletDialogContainer
          >)}
        />
      </IntlProvider>
    </ThemeProvider>
  );
  return { ...view, trigger, close, resetHardware, calculateDelegationFee };
};

describe('upstream undelegation confirmation', () => {
  beforeEach(() => {
    global.cip30Enabled = false;
  });
  afterEach(cleanup);

  it('shows the fixed upstream deposit refund and fee before any submission', async () => {
    const fixture = createFixture();
    await waitFor(() =>
      expect(
        screen.getByText(formattedWalletAmount(new BigNumber(2), false))
      ).toBeInTheDocument()
    );
    expect(
      screen.getByText(formattedWalletAmount(new BigNumber('0.17'), false))
    ).toBeInTheDocument();
    expect(fixture.trigger).not.toHaveBeenCalled();
    expect(fixture.calculateDelegationFee).toHaveBeenCalledWith({
      walletId: 'wallet',
    });
  });

  it('cancels software confirmation without resetting unrelated hardware work', async () => {
    const fixture = createFixture();
    await waitFor(() =>
      expect(fixture.calculateDelegationFee).toHaveBeenCalled()
    );
    fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(fixture.close).toHaveBeenCalledTimes(1);
    expect(fixture.resetHardware).not.toHaveBeenCalled();
    expect(fixture.trigger).not.toHaveBeenCalled();
  });
});
