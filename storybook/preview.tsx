import React from 'react';
import timemachine from 'timemachine';
import StoryWrapper from './stories/_support/StoryWrapper';
import { themeNames, localeNames, osNames } from './stories/_support/config';
import '!style-loader!css-loader!sass-loader!../source/renderer/app/themes/index.global.scss'; // eslint-disable-line

import './stories/_support/environment';

const decorators = [
  (story, context) => (
    <StoryWrapper
      themeName={context.globals.themeName}
      localeName={context.globals.localeName}
      osName={context.globals.osName}
    >
      {story}
    </StoryWrapper>
  ),
];

/*
 * The theme, locale and OS switches. These were a hand-written toolbar addon
 * that pushed selections over an addon channel; Storybook renders the toolbar
 * from this declaration instead, and persists and URL-encodes the selection
 * itself.
 */
const globalTypes = {
  themeName: {
    name: 'Theme',
    description: 'Daedalus theme',
    defaultValue: themeNames[0],
    toolbar: { icon: 'paintbrush', items: themeNames, dynamicTitle: true },
  },
  localeName: {
    name: 'Locale',
    description: 'Interface language',
    defaultValue: localeNames[0],
    toolbar: { icon: 'globe', items: localeNames, dynamicTitle: true },
  },
  osName: {
    name: 'OS',
    description: 'Operating system profile',
    defaultValue: osNames[0],
    toolbar: { icon: 'browser', items: osNames, dynamicTitle: true },
  },
};

// Sidebar order. storybook/main.ts indexes stories by glob, and without this the
// tree renders in the order require.context happens to return files. The sequence
// below is the one the hand-maintained barrel produced, so the grouping users know
// is preserved now that the barrel that encoded it is gone.
//
// Order is applied per title segment: a nested array orders the level below the
// name it follows. Story order inside a panel is not set here, so a panel built
// from several files lists its stories in file order.
const parameters = {
  options: {
    storySort: {
      order: [
        'Nodes',
        [
          'Connecting and Loading',
          'Splash Network Info',
          'Diagnostic',
          ['Mithril Partial Sync Confirmation'],
          'Updates',
          'Errors',
          'Environment',
          'About',
        ],
        'Loading',
        [
          'Mithril',
          [
            'Bootstrap',
            'Snapshot Picker',
            'Progress',
            'Error',
            'Partial Sync Overlay',
            'Mithril Partial Sync Dialogue',
          ],
          'Chain Storage',
        ],
        'Wallets',
        [
          'Summary',
          'Send',
          'Receive',
          'Transactions',
          'Tokens',
          'Settings',
          'Add Wallet',
          'Import File',
          'Export to File',
          'Hardware Wallets',
          'Set Password',
        ],
        'Decentralization',
        ['Staking', 'Redeem ITN Rewards'],
        'dApps',
        ['TransactionRequest'],
        'Voting',
        ['Voting Registration Wizard', 'Voting Info'],
        'Governance',
        ['DRep Directory', 'DRep Detail', 'Delegation', 'Governance Center'],
        'Settings',
        ['General', 'Language'],
        'Assets',
        ['Asset pill', 'AssetSettingsDialog'],
        'News',
        ['NewsFeed', 'Overlays'],
        'Navigation',
        ['Sidebar', 'Wallets Menu'],
        'Common',
        ['Notifications', 'Widgets', 'ItemsDropdown'],
        'Discreet Mode',
        ['Discreet Mode Toggle', 'Discreet Asset Amount'],
        'Analytics',
      ],
    },
  },
};

timemachine.config({
  dateString: 'Sat, 01 Jan 2022 10:00:00 GMT',
});

// Storybook 8 takes a single default-export Preview object rather than named
// decorators and parameters exports.
export default { decorators, parameters, globalTypes };
