// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observable, runInAction } from 'mobx';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import BigNumber from 'bignumber.js';
import moment from 'moment';

// Assets and helpers
import StoryDecorator from './support/StoryDecorator';
import walletsIcon from '../../source/renderer/app/assets/images/sidebar/wallet-ic.inline.svg';
import settingsIcon from '../../source/renderer/app/assets/images/sidebar/settings-ic.inline.svg';
import adaIcon from '../../source/renderer/app/assets/images/sidebar/ada-redemption-ic.inline.svg';
import paperCertificateIcon from '../../source/renderer/app/assets/images/sidebar/paper-certificate-ic.inline.svg';
import { formattedWalletAmount } from '../../source/renderer/app/utils/ada/formatters';
import NodeSyncStatusIcon from '../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import WalletAddress from '../../source/renderer/app/domains/WalletAddress';
import { generateTransaction } from './WalletTransactionsList.stories.js';
import { transactionStates, transactionTypes } from '../../source/renderer/app/domains/WalletTransaction';

// Empty screen elements
import TopBar from '../../source/renderer/app/components/layout/TopBar';
import Sidebar from '../../source/renderer/app/components/sidebar/Sidebar';
import SidebarLayout from '../../source/renderer/app/components/layout/SidebarLayout';
import SidebarWalletsMenu from '../../source/renderer/app/components/sidebar/wallets/SidebarWalletsMenu';
import WalletWithNavigation from '../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';

// Screens
import WalletSummary from '../../source/renderer/app/components/wallet/summary/WalletSummary';
import WalletSendForm from '../../source/renderer/app/components/wallet/WalletSendForm';
import WalletReceive from '../../source/renderer/app/components/wallet/WalletReceive';
import WalletTransactionsList from '../../source/renderer/app/components/wallet/transactions/WalletTransactionsList';
import WalletSettings from '../../source/renderer/app/components/wallet/WalletSettings';

type Props = {
  activeNavItem?: string,
  children?: any | Node
};

const sidebarCategories = [
  {
    name: 'WALLETS',
    route: '/wallets',
    icon: walletsIcon,
  },
  {
    name: 'SETTINGS',
    route: '/settings',
    icon: settingsIcon,
  },
  {
    name: 'ADA_REDEMPTION',
    route: '/ada-redemption',
    icon: adaIcon,
  },
  {
    name: 'PAPER_WALLET',
    route: '/paper-wallet-create-certificate',
    icon: paperCertificateIcon,
  }
];

const sidebarMenus = observable({
  wallets: {
    items: [
      { id: '1', title: 'Mining', info: '0.000000 ADA', isConnected: true },
      { id: '2', title: 'Shopping', info: '66.998.623133 ADA', isConnected: true },
    ],
    activeWalletId: '2',
    actions: {
      onAddWallet: action('toggleAddWallet'),
      onWalletItemClick: (walletId: string) => {
        runInAction(() => sidebarMenus.wallets.activeWalletId = walletId);
      }
    }
  }
});

class WalletScreen extends Component<Props> {

  render() {

    const { children, activeNavItem } = this.props;

    return (
      <div>
        <SidebarLayout
          sidebar={this.sidebar(!!children)}
          topbar={this.topbar}
        >
          {
            children &&
              (
                <WalletWithNavigation
                  isActiveScreen={item => item === activeNavItem}
                  onWalletNavItemClick={action('onWalletNavItemClick')}
                >
                  {children}
                </WalletWithNavigation>
              )
          }
        </SidebarLayout>
      </div>
    );
  }

  sidebar = isShowingSubMenus => (
    <Sidebar
      categories={sidebarCategories}
      activeSidebarCategory={sidebarCategories[0].route}
      menus={sidebarMenus}
      isShowingSubMenus={isShowingSubMenus}
      onCategoryClicked={action('onCategoryClicked')}
      isDialogOpen={() => false}
      onAddWallet={action('onAddWallet')}
      openDialogAction={action('openDialog')}
      onSubmitSupportRequest={()=>{}}
    />
  )

  // TODO: Find out how to activave the `topBarTitle` on TopBar
  get topbar() {
    return (
      <TopBar
        formattedWalletAmount={formattedWalletAmount}
        currentRoute="summary"
        showSubMenuToggle
        showSubMenus
      >
        <NodeSyncStatusIcon
          networkStatus={{
            isSynced: true,
            syncPercentage: 100,
          }}
          isProduction
        />
      </TopBar>
    );
  }
}

storiesOf('WalletScreens', module)

  .addDecorator((story, { story:storyName }) => (
    <StoryDecorator>
      <WalletScreen
        activeNavItem={storyName.toLowerCase()}
      >
      {story()}
      </WalletScreen>
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('Empty', () => false)

  .add('Summary', () => (
    <WalletSummary
      walletName="Shopping wallet"
      amount="45119903750165.23"
      pendingAmount={{
        incoming: new BigNumber(1),
        outgoing: new BigNumber(2),
        total: new BigNumber(3)
      }}
      numberOfTransactions={20303585}
      isLoadingTransactions={false}
    />
  ))

  .add('Send', () => (
    <WalletSendForm
      currencyUnit="Ada"
      currencyMaxFractionalDigits={ 6}
      currencyMaxIntegerDigits={11}
      validateAmount={() => true}
      calculateTransactionFee={()=>{}}
      addressValidator={()=>{}}
      openDialogAction={()=>{}}
      isDialogOpen={()=>{}}
      isRestoreActive={false}
    />
  ))

  .add('Receive', () => (
    <WalletReceive
      walletAddress="5628aab8ac98c963e4a2e8cfce5aa1cbd4384fe2f9a0f3c5f791bfb83a5e02ds"
      isWalletAddressUsed={false}
      walletAddresses={[
        {
          id: '5628aab8ac98c963e4a2e8cfce5aa1cbd4384fe2f9a0f3c5f791bfb83a5e02ds',
          amount: new BigNumber(1),
          isUsed:false,
        },
        {
          id: 'f465aca571de909091967349e1dc39fc9ce9755ff8ffdb7cc3771b503904e49a',
          amount: new BigNumber(1),
          isUsed:false,
        },
        {
          id: '5628aab8ac98c963e4a2e8cfce5aa1cbd4384fe2f9a0f3c5f791bfb83a5e02ds',
          amount: new BigNumber(1),
          isUsed:false,
        },
        {
          id: 'f465aca571de909091967349e1dc39fc9ce9755ff8ffdb7cc3771b503904e49a',
          amount: new BigNumber(1),
          isUsed:false,
        },
        {
          id: '5628aab8ac98c963e4a2e8cfce5aa1cbd4384fe2f9a0f3c5f791bfb83a5e02ds',
          amount: new BigNumber(1),
          isUsed:false,
        },
        {
          id: 'f465aca571de909091967349e1dc39fc9ce9755ff8ffdb7cc3771b503904e49a',
          amount: new BigNumber(1),
          isUsed:false,
        },
        {
          id: '68a1107193a8183bcd7758c9b3d9f3e8b0437206cc432b4e01a49d948157f51e',
          amount: new BigNumber(1),
          isUsed:true,
        },
        {
          id: '68a1107193a8183bcd7758c9b3d9f3e8b0437206cc432b4e01a49d948157f51e',
          amount: new BigNumber(1),
          isUsed:true,
        },
        {
          id: '68a1107193a8183bcd7758c9b3d9f3e8b0437206cc432b4e01a49d948157f51e',
          amount: new BigNumber(1),
          isUsed:true,
        },
        {
          id: '68a1107193a8183bcd7758c9b3d9f3e8b0437206cc432b4e01a49d948157f51e',
          amount: new BigNumber(1),
          isUsed:true,
        },
        {
          id: '68a1107193a8183bcd7758c9b3d9f3e8b0437206cc432b4e01a49d948157f51e',
          amount: new BigNumber(1),
          isUsed:true,
        },
        {
          id: '68a1107193a8183bcd7758c9b3d9f3e8b0437206cc432b4e01a49d948157f51e',
          amount: new BigNumber(1),
          isUsed:true,
        }
      ]}
      onGenerateAddress={()=>{}}
      onCopyAddress={()=>{}}
      isSidebarExpanded
      walletHasPassword={false}
      isSubmitting={false}
    />
  ))

  .add('Transactions', () => (
    <WalletTransactionsList
      transactions={[
        generateTransaction(transactionTypes.INCOME, new Date(), new BigNumber(1), 1),
        generateTransaction(transactionTypes.EXCHANGE, new Date(), new BigNumber(1)),
        generateTransaction(transactionTypes.EXPEND, moment().subtract(1, 'days').toDate(), new BigNumber(2), 0, transactionStates.PENDING),
        generateTransaction(transactionTypes.INCOME, moment().subtract(1, 'days').toDate(), new BigNumber(1), 0, transactionStates.FAILED),
        generateTransaction(transactionTypes.EXPEND, moment().subtract(2, 'days').toDate(), new BigNumber(3)),
        generateTransaction(transactionTypes.EXPEND, moment().subtract(3, 'days').toDate(), new BigNumber(5)),
        generateTransaction(transactionTypes.INCOME, moment().subtract(4, 'days').toDate(), new BigNumber(6)),
      ]}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      assuranceMode={{ low: 1, medium: 2 }}
      walletId="test-wallet"
      formattedWalletAmount={formattedWalletAmount}
    />
  ))

  .add('Settings', () => (
    <WalletSettings
      activeField={null}
      assuranceLevels={[
        {
          "value": "CWANormal",
          "label": {
            id: 'global.assuranceLevel.normal',
            defaultMessage: '!!!Normal',
            description: ''
          }
        },
        {
          "value": "CWAStrict",
          "label": {
            id: 'global.assuranceLevel.strict',
            defaultMessage: '!!!Strict',
            description: ''
          }
        }
      ]}
      isDialogOpen={()=>false}
      isInvalid={false}
      isSubmitting={false}
      isWalletPasswordSet={false}
      lastUpdatedField={null}
      nameValidator={()=>true}
      onCancelEditing={()=>{}}
      onFieldValueChange={()=>{}}
      onStartEditing={()=>{}}
      onStopEditing={()=>{}}
      openDialogAction={()=>{}}
      walletAssurance="CWANormal"
      walletName="Test wallet"
      walletPasswordUpdateDate={moment().subtract(1, 'month').toDate()}
    />
  ));


