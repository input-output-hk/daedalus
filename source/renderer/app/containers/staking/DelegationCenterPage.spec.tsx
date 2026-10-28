import React from 'react';
import { Provider } from 'mobx-react';
import { cleanup, render } from '@testing-library/react';
import '@testing-library/jest-dom';
import DelegationCenterPage from './DelegationCenterPage';

// The injected props come from the Provider.
const Page = DelegationCenterPage as unknown as React.ComponentType;

jest.mock(
  '../../components/staking/delegation-center/DelegationCenter',
  () => ({
    __esModule: true,
    default: () => null,
  })
);
jest.mock(
  '../../components/staking/delegation-center/DelegationCenterNoWallets',
  () => ({
    __esModule: true,
    default: () => null,
  })
);
jest.mock(
  '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog',
  () => ({ __esModule: true, default: () => null })
);
jest.mock('./dialogs/DelegationSetupWizardDialogContainer', () => ({
  __esModule: true,
  default: () => null,
}));
jest.mock('../wallet/dialogs/settings/UndelegateWalletDialogContainer', () => ({
  __esModule: true,
  default: () => null,
}));

const renderPage = () => {
  const release = jest.fn();
  const acquireStakePools = jest.fn(() => release);
  const stores = {
    app: { openExternalLink: jest.fn() },
    uiDialogs: { isOpen: () => false },
    staking: {
      stakePools: [],
      getStakePoolById: jest.fn(),
      fetchingStakePoolsFailed: false,
      acquireStakePools,
    },
    wallets: { allWallets: [] },
    networkStatus: {},
    profile: {},
  };
  const view = render(
    <Provider stores={stores as any} actions={{ dialogs: {} } as any}>
      <Page />
    </Provider>
  );
  return { acquireStakePools, release, ...view };
};

describe('DelegationCenterPage', () => {
  afterEach(cleanup);

  it('asks for the stake pool list while it is open and withdraws the ask when it closes', () => {
    const { acquireStakePools, release, unmount } = renderPage();
    expect(acquireStakePools).toHaveBeenCalledTimes(1);
    expect(release).not.toHaveBeenCalled();
    unmount();
    expect(release).toHaveBeenCalledTimes(1);
  });
});
