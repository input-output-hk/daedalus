// @flow
import '../../source/renderer/app/themes/index.global.scss';
import './_support/environment';

// Wallets
import './wallets';

// Nodes
import './nodes';

// Staking
import './staking/Staking.stories';
import './staking/CountdownParty.stories';

// dApps
import './dapps/TransactionRequest.stories';

// Voting
import './voting/Voting.stories';

// Settings
import './settings';

// Assets
import './assets/Asset.stories';
import './assets/AssetSettingsDialog.stories';

// News
import './news/NewsFeed.stories';
import './news/IncidentOverlay.stories';
import './news/AlertsOverlay.stories';
import './news/AppUpdateOverlay.stories';

// Navigation
import './navigation/Sidebar.stories';
import './navigation/SidebarCategory.stories';
import './navigation/SidebarWalletsMenu.stories';

// Notifications
import './notifications/Notifications.stories';

// Common
import './common/Widgets.stories';
import './common/ItemsDropdown.stories';

// Discreet Mode
import '../../source/renderer/app/features/discreet-mode/ui/discreet-toggle/DiscreetToggle.story';
import '../../source/renderer/app/features/discreet-mode/ui/DiscreetValue.story';
