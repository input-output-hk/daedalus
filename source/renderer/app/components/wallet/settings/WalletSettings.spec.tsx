import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import StoryDecorator from '../../../../../../storybook/stories/_support/StoryDecorator';
import translations from '../../../i18n/locales/en-US.json';
import WalletSettings from './WalletSettings';

const renderSettings = (
  overrides: Partial<React.ComponentProps<typeof WalletSettings>> = {}
) => {
  const onWithdrawRewardsChange = jest.fn();
  const noop = jest.fn();
  const props = {
    walletId: 'wallet-1',
    walletName: 'Wallet',
    isRestoring: false,
    isSyncing: false,
    isDelegating: false,
    isSubmitting: false,
    isInvalid: false,
    showExportLink: false,
    lastUpdatedField: null,
    delegationStakePoolStatus: null,
    lastDelegationStakePoolStatus: null,
    walletPublicKey: null,
    icoPublicKey: null,
    creationDate: new Date('2026-01-01T00:00:00Z'),
    spendingPasswordUpdateDate: null,
    openDialogAction: noop,
    isDialogOpen: () => false,
    onFieldValueChange: noop,
    onStartEditing: noop,
    onStopEditing: noop,
    onCancel: noop,
    onVerifyRecoveryPhrase: noop,
    onCopyWalletPublicKey: noop,
    onCopyICOPublicKey: noop,
    updateDataForActiveDialogAction: noop,
    onDelegateClick: noop,
    nameValidator: () => true,
    isLegacy: false,
    changeSpendingPasswordDialog: null,
    walletPublicKeyDialogContainer: null,
    icoPublicKeyDialogContainer: null,
    walletPublicKeyQRCodeDialogContainer: null,
    icoPublicKeyQRCodeDialogContainer: null,
    undelegateWalletDialogContainer: null,
    deleteWalletDialogContainer: null,
    unpairWalletDialogContainer: null,
    shouldDisplayRecoveryPhrase: false,
    recoveryPhraseVerificationDate: null,
    recoveryPhraseVerificationStatus: 'ok',
    recoveryPhraseVerificationStatusType: 'ok',
    wordCount: 24,
    locale: 'en-US',
    isSpendingPasswordSet: true,
    isHardwareWallet: false,
    singleAddressMode: false,
    isSingleAddressModeDisabled: false,
    onSingleAddressModeChange: noop,
    withdrawRewardsOverride: null,
    withdrawRewardsDefault: true,
    onWithdrawRewardsChange,
    ...overrides,
  };
  render(
    <StoryDecorator>
      <IntlProvider locale="en-US" messages={translations}>
        {/* @ts-expect-error the props above fill only what this page draws */}
        <WalletSettings {...props} />
      </IntlProvider>
    </StoryDecorator>
  );
  return { onWithdrawRewardsChange };
};

const chooseOption = (current: RegExp, option: RegExp) => {
  fireEvent.click(screen.getByDisplayValue(current));
  fireEvent.click(screen.getByText(option));
};

describe('WalletSettings withdraw rewards', () => {
  afterEach(cleanup);

  it.each([
    ['software', false],
    ['hardware', true],
  ])('draws the choice for a %s wallet', (_name, isHardwareWallet) => {
    renderSettings({ isHardwareWallet });
    expect(
      screen.getByText('Withdraw staking rewards with transactions')
    ).toBeVisible();
    expect(screen.getByDisplayValue(/As in Settings > Wallets/)).toBeVisible();
  });

  it('draws no choice for a legacy wallet', () => {
    renderSettings({ isLegacy: true });
    expect(
      screen.queryByText('Withdraw staking rewards with transactions')
    ).toBeNull();
  });

  it.each([
    [true, 'As in Settings > Wallets (Withdraw)'],
    [false, 'As in Settings > Wallets (Do not withdraw)'],
  ])(
    'names the global value, %s, in the follow option',
    (withdrawRewardsDefault, label) => {
      renderSettings({ withdrawRewardsDefault });
      expect(screen.getByDisplayValue(label)).toBeVisible();
    }
  );

  it('shows the wallet choice when there is one', () => {
    renderSettings({ withdrawRewardsOverride: false });
    expect(screen.getByDisplayValue('Do not withdraw')).toBeVisible();
  });

  it('stores true, false or null from the three options', () => {
    const { onWithdrawRewardsChange } = renderSettings({
      withdrawRewardsOverride: false,
    });
    chooseOption(/Do not withdraw/, /^Withdraw$/);
    expect(onWithdrawRewardsChange).toHaveBeenLastCalledWith(true);
    chooseOption(/Do not withdraw|Withdraw/, /^As in Settings > Wallets/);
    expect(onWithdrawRewardsChange).toHaveBeenLastCalledWith(null);
  });
});
