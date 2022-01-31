import React from 'react';
import { defineMessages, IntlProvider } from 'react-intl';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { select, withKnobs } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import enMessages from '../../../source/renderer/app/i18n/locales/en-US.json';
import jpMessages from '../../../source/renderer/app/i18n/locales/ja-JP.json';
import News from '../../../source/renderer/app/domains/News';
import {
  DATE_ENGLISH_OPTIONS,
  DATE_JAPANESE_OPTIONS,
} from '../../../source/renderer/app/config/profileConfig';
import AlertsOverlay from '../../../source/renderer/app/components/news/AlertsOverlay';

const { intl: enIntl } = new IntlProvider({
  locale: 'en-US',
  messages: enMessages,
}).getChildContext();
const { intl: jpIntl } = new IntlProvider({
  locale: 'ja-JP',
  messages: jpMessages,
}).getChildContext();
const intl = {
  'en-US': enIntl,
  'ja-JP': jpIntl,
};
const dateOptionsIntl = {
  'en-US': DATE_ENGLISH_OPTIONS,
  'ja-JP': DATE_JAPANESE_OPTIONS,
};
const messages = defineMessages({
  readMore: {
    id: 'global.labels.readMore',
    defaultMessage: '!!!Read More',
    description: 'Read More button label.',
  },
  failureAlert: {
    id: 'global.errors.failureAlert',
    defaultMessage: '!!!Failure Alert',
    description: 'Failure Alert title.',
  },
  content: {
    id: 'static.dummy.markdown',
    defaultMessage: '!!!Content',
    description: 'Content.',
  },
});

const getAlerts = (locale: string) => [
  new News.News({
    action: {
      label: intl[locale].formatMessage(messages.readMore),
      url: 'https://www.daedalus.io',
    },
    content: intl[locale].formatMessage(messages.content),
    date: Date.now(),
    id: 123,
    target: {
      daedalusVersion: 'v0.13',
      platform: 'macOS',
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ daedalusVersion: string; platform: string;... Remove this comment to see the full error message
      platformVersion: '10.14.6',
    },
    title: intl[locale].formatMessage(messages.failureAlert),
    type: 'alert',
    read: false,
  }),
  new News.News({
    action: {
      label: intl[locale].formatMessage(messages.readMore),
      url: 'https://www.daedalus.io',
    },
    content: intl[locale].formatMessage(messages.content),
    date: Date.now(),
    id: 1234,
    target: {
      daedalusVersion: 'v0.13',
      platform: 'macOS',
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ daedalusVersion: string; platform: string;... Remove this comment to see the full error message
      platformVersion: '10.14.6',
    },
    title: intl[locale].formatMessage(messages.failureAlert),
    type: 'alert',
    read: false,
  }),
];

storiesOf('News|Overlays', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    locale: string;}) =... Remove this comment to see the full error message
  .add('Alerts', (props: { locale: string }) => (
    <AlertsOverlay
      allAlertsCount={getAlerts(props.locale).length}
      alerts={getAlerts(props.locale)}
      onCloseOpenAlert={() => null}
      onMarkNewsAsRead={action('onMarkNewsAsRead')}
      onOpenExternalLink={action('onOpenExternalLink')}
      onProceedNewsAction={action('onProceedNewsAction')}
      currentDateFormat={select(
        'currentDateFormat',
        dateOptionsIntl[props.locale].reduce((obj, { label, value }) => {
          obj[label] = value;
          return obj;
        }, {}),
        dateOptionsIntl[props.locale][0].value
      )}
    />
  ));
