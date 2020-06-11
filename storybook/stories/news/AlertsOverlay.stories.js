// @flow
import React from 'react';
import { defineMessages, IntlProvider } from 'react-intl';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { select, withKnobs } from '@storybook/addon-knobs';

import StoryDecorator from '../_support/StoryDecorator';
import News from '../../../source/renderer/app/domains/News';
import {
  DATE_ENGLISH_OPTIONS,
  DATE_JAPANESE_OPTIONS,
} from '../../../source/renderer/app/config/profileConfig';
import AlertsOverlay from '../../../source/renderer/app/components/news/AlertsOverlay';

const { intl: enIntl } = new IntlProvider({
  locale: 'en-US',
  messages: {
    readMore: 'Read More',
    failureAlert: 'Failure Alert',
    content:
      '# h1 Heading\nUt consequat semper viverra nam libero justo laoreet sit. Sagittis vitae et leo duis. Eget nullam non nisi est sit amet facilisis magna etiam. Nisl tincidunt eget nullam non nisi est sit amet facilisis. Auctor neque vitae tempus quam pellentesque. Vel facilisis volutpat est velit egestas dui id ornare arcu.\n\n## h2 Heading\n\nConsequat mauris nunc congue nisi vitae suscipit. Dictum non consectetur a erat nam. Laoreet non curabitur gravida arcu ac tortor dignissim. Eu augue ut lectus arcu bibendum at. Facilisis gravida neque convallis a cras semper. Ut consequat semper viverra nam libero justo laoreet sit. Sagittis vitae et leo duis. Eget nullam non nisi est sit amet facilisis magna etiam. Nisl tincidunt eget nullam non nisi est sit amet facilisis. Auctor neque vitae tempus quam pellentesque. Vel facilisis volutpat est velit egestas dui id ornare arcu. Nam aliquam sem et tortor consequat id porta nibh venenatis.\n\nViverra nam libero justo laoreet sit amet. Pharetra diam sit amet nisl. Quam viverra orci sagittis eu. Rhoncus dolor purus non enim. Posuere urna nec tincidunt praesent semper feugiat. Suspendisse in est ante in nibh mauris cursus. Sit amet consectetur adipiscing elit duis. Tortor id aliquet lectus proin nibh nisl condimentum id. At in tellus integer feugiat scelerisque. Maecenas sed enim ut sem viverra aliquet. Pellentesque pulvinar pellentesque habitant morbi. Ultrices neque ornare aenean euismod elementum nisi quis eleifend. Praesent tristique magna sit amet purus gravida. Diam volutpat commodo sed egestas egestas. Ut placerat orci nulla pellentesque dignissim enim. Ultrices in iaculis nunc sed augue lacus viverra. Etiam sit amet nisl purus.\n\n## Typographic replacements\n\nEnable typographer option to see result.\n\n(c) (C) (r) (R) (tm) (TM) (p) (P) +-\n\ntest.. test... test..... test?..... test!....\n\n!!!!!! ???? ,,  -- ---\n\n"Smartypants, double quotes" and \'single quotes\'\n\n## Emphasis\n\n**This is bold text**\n\n__This is bold text__\n\n*This is italic text*\n\n_This is italic text_\n\n## Lists\n\nUnordered\n\n+ Create a list by starting a line with +, -, or *\n+ Sub-lists are made by indenting 2 spaces:\n+ Very easy!\n\nOrdered\n\n1. Lorem ipsum dolor sit amet\n2. Consectetur adipiscing elit\n3. Integer molestie lorem at massa\n\n\n1. You can use sequential numbers...\n1. ...or keep all the numbers as `1.`\n\n## Links\n\n[link text](http://dev.nodeca.com)\n\n[link with title](http://nodeca.github.io/pica/demo/ "title text!")\n\nAutoconverted link https://github.com/nodeca/pica (enable linkify to see)\n\n### [Subscript](https://github.com/markdown-it/markdown-it-sub) / [Superscript](https://github.com/markdown-it/markdown-it-sup)\n\n- 19^th^\n- H~2~O\n',
  },
}).getChildContext();
const { intl: jpIntl } = new IntlProvider({
  locale: 'ja-JP',
  messages: {
    readMore: '続きを読む',
    failureAlert: '障害警告',
    content:
      '# 見出し 1\nある初期状態が与えられればその後の全ての状態量の変化が決定される系を力学系と呼ぶ。特に、決定論に従う力学系を扱うことを強調して決定論的力学系とも呼ばれる。カオス理論において研究されるカオスと呼ばれる複雑で確率的なランダムにも見える振る舞いは、この決定論的力学系に従って生み出されるものである。この点を強調するためカオス理論が取り扱うカオスを決定論的カオス(deterministic chaos)とも呼ぶ[3]。複雑で高次元の系ではなくとも、1次元離散方程式や3次元連続方程式のような非常に簡単な低次元の系からでも、確率的ランダムに相当する振る舞いが生起される点が決定論的カオスの特徴といえる。この用語は、カオス理論以前から存在するボルツマンにより導入された分子カオスと呼び分ける意味合いもある。ボルツマンによるカオスは確率論的乱雑さを表しており、カオス理論におけるカオスとは概念が異なる。\n## 見出し 2\nある初期状態が与えられればその後の全ての状態量の変化が決定される系を力学系と呼ぶ。特に、決定論に従う力学系を扱うことを強調して決定論的力学系とも呼ばれる。カオス理論において研究されるカオスと呼ばれる複雑で確率的なランダムにも見える振る舞いは、この決定論的力学系に従って生み出されるものである。この点を強調するためカオス理論が取り扱うカオスを決定論的カオス(deterministic chaos)とも呼ぶ[3]。複雑で高次元の系ではなくとも、1次元離散方程式や3次元連続方程式のような非常に簡単な低次元の系からでも、確率的ランダムに相当する振る舞いが生起される点が決定論的カオスの特徴といえる。この用語は、カオス理論以前から存在するボルツマンにより導入された分子カオスと呼び分ける意味合いもある。ボルツマンによるカオスは確率論的乱雑さを表しており、カオス理論におけるカオスとは概念が異なる。',
  },
}).getChildContext();
const intl = { 'en-US': enIntl, 'ja-JP': jpIntl };
const dateOptionsIntl = {
  'en-US': DATE_ENGLISH_OPTIONS,
  'ja-JP': DATE_JAPANESE_OPTIONS,
};

const messages = defineMessages({
  readMore: {
    id: 'readMore',
    defaultMessage: '!!!Read More',
    description: 'Read More button label.',
  },
  failureAlert: {
    id: 'failureAlert',
    defaultMessage: '!!!Failure Alert',
    description: 'Failure Alert title.',
  },
  content: {
    id: 'content',
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
      platformVersion: '10.14.6',
    },
    title: intl[locale].formatMessage(messages.failureAlert),
    type: 'alert',
    read: false,
  }),
];

storiesOf('News|Alerts', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  .add('Alerts Overlay', (props: { locale: string }) => (
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
