import React from 'react';
import { IntlProvider } from 'react-intl';
import { render, screen, cleanup } from '@testing-library/react';
import '@testing-library/jest-dom';
import { Provider as MobxProvider } from 'mobx-react';
import translations from '../../../i18n/locales/en-US.json';
import StoryDecorator from '../../../../../../storybook/stories/_support/StoryDecorator';
import {
  DiscreetModeFeatureProvider,
  BrowserLocalStorageBridge,
} from '../../../features';
import type { SidebarWalletType } from '../../../types/sidebarTypes';
import SidebarWalletsMenu from './SidebarWalletsMenu';

describe('Sidebar Wallets Menu', () => {
  afterEach(cleanup);

  function TestDecorator({
    wallets,
    searchValue,
  }: {
    wallets: Array<SidebarWalletType>;
    searchValue: string;
  }) {
    return (
      <StoryDecorator>
        <MobxProvider>
          <IntlProvider locale="en-US" messages={translations}>
            <BrowserLocalStorageBridge>
              <DiscreetModeFeatureProvider>
                <SidebarWalletsMenu
                  wallets={wallets}
                  onAddWallet={jest.fn()}
                  onWalletItemClick={() => {}}
                  isActiveWallet={() => false}
                  isAddWalletButtonActive={false}
                  isShelleyActivated
                  visible={false}
                  searchValue={searchValue}
                />
              </DiscreetModeFeatureProvider>
            </BrowserLocalStorageBridge>
          </IntlProvider>
        </MobxProvider>
      </StoryDecorator>
    );
  }

  function createWallet() {
    let id = 0;
    return (title: string): SidebarWalletType => {
      id += 1;
      return {
        id: id.toString(),
        title,
        // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'BigNumber... Remove this comment to see the full error message
        amount: 0,
        isConnected: false,
        isRestoreActive: false,
        restoreProgress: 0,
        isNotResponding: false,
        isLegacy: false,
        createdAt: new Date(),
        recoveryPhraseVerificationDate: new Date(),
        hasNotification: false,
      };
    };
  }

  const wallet = createWallet();
  const wallets = [
    wallet('Loki'),
    wallet('Ledger Nano X'),
    wallet('Ledger Nano S'),
    wallet('Odin'),
    wallet('byron'),
    wallet('byron new new1'),
  ];
  test.each([
    [' ', ['Ledger Nano X', 'Ledger Nano S', 'byron new new1']],
    ['OKI', ['Loki', 'Odin']],
    ['byron1111', ['byron', 'byron new new1']],
    ['brom', ['byron', 'byron new new1']],
    ['asdf', []],
    ['legasdf', []],
  ])(
    '<SidebarWalletsMenu /> should filter by %s and display %s',
    (searchValue, results) => {
      const assertVisibleItems = 1;
      const assertSearchResult = results.length;
      expect.assertions(assertSearchResult + assertVisibleItems);
      render(<TestDecorator wallets={wallets} searchValue={searchValue} />);
      results.forEach((r) => expect(screen.queryByTestId(r)).toBeVisible());
      expect(screen.queryAllByTestId('walletMenu')).toHaveLength(
        assertSearchResult
      );
    }
  );
});
