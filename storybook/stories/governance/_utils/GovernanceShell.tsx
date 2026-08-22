import React, { useState } from 'react';
import { action } from '@storybook/addon-actions';
import GovernanceWithNavigation from '../../../../source/renderer/app/components/governance/layouts/GovernanceWithNavigation';
import Sidebar from '../../../../source/renderer/app/components/sidebar/Sidebar';
import SidebarLayout from '../../../../source/renderer/app/components/layout/SidebarLayout';
import TopBar from '../../../../source/renderer/app/components/layout/TopBar';
import type { SidebarMenus } from '../../../../source/renderer/app/components/sidebar/types';
import {
  CATEGORIES_BY_NAME,
  SidebarCategoryInfo,
} from '../../../../source/renderer/app/config/sidebarConfig';
import { ROUTES } from '../../../../source/renderer/app/routes-config';
import { TESTNET } from '../../../../source/common/types/environment.types';
import { renderGovernancePage } from './governancePages';
import type { GovernancePageState } from './governancePages';

/**
 * The application chrome a governance screen actually ships inside.
 *
 * Every governance story used to build its own approximation: one wrapped a
 * generic story layout with a sidebar that did not show the governance
 * category, another built a sidebar by hand with a tab bar missing the first
 * tab, and a third rendered on a bare page. Reviewing a screen for release is
 * not possible when each story frames it differently, so they all come through
 * here instead.
 *
 * The tabs are the same three the Governance container renders, in the same
 * order, so a story cannot drift from the running application by listing them
 * differently.
 */

export const GOVERNANCE_TABS = [
  { id: ROUTES.GOVERNANCE.DASHBOARD, label: 'Voting Center' },
  { id: ROUTES.GOVERNANCE.DREPS, label: 'DRep Directory' },
  { id: ROUTES.GOVERNANCE.FAVORITES, label: 'Favorites' },
];

const SIDEBAR_CATEGORIES: Array<SidebarCategoryInfo> = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING,
  CATEGORIES_BY_NAME.GOVERNANCE,
  CATEGORIES_BY_NAME.SETTINGS,
  CATEGORIES_BY_NAME.NETWORK_INFO,
];

const EMPTY_SIDEBAR_MENUS: SidebarMenus = { wallets: null };

// Mirrors the connected-flow story, which renders correctly: a fixed-height
// frame for the sidebar layout to fill, then ordinary flow inside it. Giving
// the section a full height and the page flex:1 made the navigation stretch to
// the height of the window and squeezed the page out of view.
// The same frame StoryLayout gives the staking stories.
const SHELL_STYLE = {
  minHeight: '100%',
  height: '100vh',
};

type Props = {
  /** The tab the story opens on, and the tab its children belong to. */
  activeTab: string;
  /**
   * Fixture state for the tabs the story is not about, so moving between them
   * lands on a real screen instead of a blank one.
   */
  state?: GovernancePageState;
  /** The page under review. Rendered on `activeTab` only. */
  children?: React.ReactNode;
};

/**
 * Tabs navigate rather than merely highlighting: clicking one has to take you
 * to that screen, or the chrome is claiming to be the application while
 * behaving like a picture of it. The story's own page is shown on the tab it
 * belongs to; the others render from shared fixtures.
 */
export default function GovernanceShell({ activeTab, state, children }: Props) {
  const [currentTab, setCurrentTab] = useState(activeTab);
  // The story's page is what the story is about, so it is what opens. Once a
  // tab is clicked the shell shows real pages from then on, including for the
  // tab the story started on: from a detail view, clicking DRep Directory has
  // to reach the directory rather than the detail it was already showing.
  const [hasNavigated, setHasNavigated] = useState(false);

  const handleTabChange = (route: string) => {
    action('onNavItemClick')(route);
    setHasNavigated(true);
    setCurrentTab(route);
  };

  const page =
    !hasNavigated && children != null
      ? children
      : renderGovernancePage(currentTab, state);

  return (
    <div style={SHELL_STYLE}>
      <SidebarLayout
        sidebar={
          <Sidebar
            menus={EMPTY_SIDEBAR_MENUS}
            categories={SIDEBAR_CATEGORIES}
            activeSidebarCategory={ROUTES.GOVERNANCE.ROOT}
            isShowingSubMenus={false}
            pathname={currentTab}
            network={TESTNET}
            onActivateCategory={action('onActivateCategory')}
            onAddWallet={action('onAddWallet')}
            isShelleyActivated
          />
        }
        topbar={<TopBar isShelleyActivated />}
      >
        <GovernanceWithNavigation
          items={GOVERNANCE_TABS}
          activeItem={currentTab}
          isActiveNavItem={(navItemId: string) => navItemId === currentTab}
          onNavItemClick={handleTabChange}
        >
          {page}
        </GovernanceWithNavigation>
      </SidebarLayout>
    </div>
  );
}
