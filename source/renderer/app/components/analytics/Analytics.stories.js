// @flow
import React from 'react';
import { defineMessages, IntlProvider } from 'react-intl';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';
import enMessages from '../../i18n/locales/en-US.json';
import jpMessages from '../../i18n/locales/ja-JP.json';
import AnalyticsDialog from './AnalyticsDialog';
import { action } from '@storybook/addon-actions';
import { observable } from 'mobx';
import StoryDecorator from '../../../../../storybook/stories/_support/StoryDecorator';
import StoryProvider from '../../../../../storybook/stories/_support/StoryProvider';
import StoryLayout from '../../../../../storybook/stories/_support/StoryLayout';

const { intl: enIntl } = new IntlProvider({
  locale: 'en-US',
  messages: enMessages,
}).getChildContext();
const { intl: jpIntl } = new IntlProvider({
  locale: 'ja-JP',
  messages: jpMessages,
}).getChildContext();
const intl = { 'en-US': enIntl, 'ja-JP': jpIntl };

const messages = defineMessages({
  title: {
    id: 'analytics.dialog.title',
    defaultMessage: '!!!Anonymous data collection',
    description: 'Analytics dialog title',
  },
  description: {
    id: 'analytics.dialog.description',
    defaultMessage:
      '!!!All data is anonymous and is used only for product development purposes. Read more in Terms and Conditions.',
    description: 'Analytics data collection description',
  },
  dataCollectionDetailsTitle: {
    id: 'analytics.dialog.dataCollectionDetailsTitle',
    defaultMessage: '!!!What data do we collect?',
    description: 'Data collection details title',
  },
  dataCollectionDetailsUserBehaviour: {
    id: 'analytics.dialog.dataCollectionDetailsUserBehaviour',
    defaultMessage: '!!!User behavior (where the user clicks)',
    description: 'Description for the user behaviour data collection',
  },
  dataCollectionDetailsDeviceInfo: {
    id: 'analytics.dialog.dataCollectionDetailsDeviceInfo',
    defaultMessage: '!!!Device info (OS, RAM, disk space, etc)',
    description: 'Description for the device info data collection',
  },
  dataCollectionSwitchButton: {
    id: 'analytics.dialog.dataCollectionSwitchText',
    defaultMessage: '!!!Allow anonymous data collection',
    description: 'Data collection agreement switch button label',
  },
  confirmButton: {
    id: 'analytics.dialog.confirmButton',
    defaultMessage: '!!!Confirm',
    description: 'Analytics data collection confirmation button text',
  },
});

storiesOf('Analytics', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  .add('Analytics Dialog', () => <AnalyticsDialog />);
