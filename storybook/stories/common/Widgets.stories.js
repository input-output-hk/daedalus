// @flow
import React from 'react';
import { defineMessages, IntlProvider } from 'react-intl';
import { storiesOf } from '@storybook/react';
import { observable, action as mobxAction } from 'mobx';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import StoryLayout from '../_support/StoryLayout';
import enMessages from '../../../source/renderer/app/i18n/locales/en-US.json';
import jpMessages from '../../../source/renderer/app/i18n/locales/ja-JP.json';
import BigButtonForDialogs from '../../../source/renderer/app/components/widgets/BigButtonForDialogs';
import MnemonicInputWidget from '../../../source/renderer/app/components/widgets/forms/MnemonicInputWidget';
import createIcon from '../../../source/renderer/app/assets/images/create-ic.inline.svg';
import importIcon from '../../../source/renderer/app/assets/images/import-ic.inline.svg';
import joinSharedIcon from '../../../source/renderer/app/assets/images/join-shared-ic.inline.svg';
import TinySwitch from '../../../source/renderer/app/components/widgets/forms/TinySwitch';
import ButtonLink from '../../../source/renderer/app/components/widgets/ButtonLink';
import NormalSwitch from '../../../source/renderer/app/components/widgets/forms/NormalSwitch';

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
  create: {
    id: 'global.labels.create',
    defaultMessage: '!!!Create',
    description: 'Create label.',
  },
  createNewWallet: {
    id: 'wallet.add.dialog.create.description',
    defaultMessage: '!!!Create a new wallet',
    description: 'Create a new wallet description.',
  },
  join: {
    id: 'wallet.add.dialog.join.label',
    defaultMessage: '!!!Join',
    description: 'Join label.',
  },
  joinSharedWallet: {
    id: 'wallet.add.dialog.join.description',
    defaultMessage: '!!!Join a shared wallet',
    description: 'Join a shared wallet description.',
  },
  import: {
    id: 'wallet.add.dialog.import.label',
    defaultMessage: '!!!Import',
    description: 'Import label.',
  },
  importExistingWallet: {
    id: 'wallet.add.dialog.import.description',
    defaultMessage:
      '!!!Import wallets from an earlier version of Daedalus or the Daedalus state directory',
    description:
      'Import wallets from an earlier version of Daedalus or the Daedalus state directory description.',
  },
  recoveryPhrase: {
    id: 'wallet.restore.dialog.mnemonicsStep',
    defaultMessage: '!!!Recovery Phrase',
    description: 'Recovery Phrase description.',
  },
  save: {
    id: 'global.labels.save',
    defaultMessage: '!!!Save',
    description: 'Save description.',
  },
});

storiesOf('Common|Widgets', module)
  .addDecorator((story: any, context: any) => {
    const onChangeAction = action('onChange');
    const state = observable({
      checked: false,
      onChange: mobxAction((value, event) => {
        state.checked = value;
        onChangeAction(value, event);
      }),
    });

    return (
      <StoryDecorator propsForChildren={state}>
        <StoryProvider>
          <StoryLayout activeSidebarCategory={null} {...context}>
            {story()}
          </StoryLayout>
        </StoryProvider>
      </StoryDecorator>
    );
  })

  // ====== Stories ======

  .add('BigButtonForDialogs', (props: { locale: string }) => (
    <div>
      <div style={{ width: '300px', height: '200px', display: 'flex' }}>
        <BigButtonForDialogs
          description={intl[props.locale].formatMessage(
            messages.createNewWallet
          )}
          label={intl[props.locale].formatMessage(messages.create)}
          icon={createIcon}
          onClick={() => {}}
        />
      </div>
      <div style={{ width: '300px', height: '200px', display: 'flex' }}>
        <BigButtonForDialogs
          description={intl[props.locale].formatMessage(
            messages.joinSharedWallet
          )}
          label={intl[props.locale].formatMessage(messages.join)}
          icon={joinSharedIcon}
          onClick={() => {}}
          isDisabled
        />
      </div>
      <div style={{ width: '300px', height: '200px', display: 'flex' }}>
        <BigButtonForDialogs
          description={intl[props.locale].formatMessage(
            messages.importExistingWallet
          )}
          label={intl[props.locale].formatMessage(messages.import)}
          icon={importIcon}
          onClick={() => {}}
        />
      </div>
    </div>
  ))

  .add('MnemonicInputWidget - 9 words', (props: { locale: string }) => {
    const tokens = observable(['', '', '', '', '', '', '', '', '']);
    return (
      <div style={{ padding: 20 }}>
        <MnemonicInputWidget
          label={intl[props.locale].formatMessage(messages.recoveryPhrase)}
          tokens={tokens}
          onTokenChanged={(index, token) => {
            tokens[index] = token;
          }}
        />
      </div>
    );
  })

  .add('TinySwitch', () => <TinySwitch />)

  .add('TinySwitch - short label', (props: { locale: string }) => (
    <TinySwitch label={intl[props.locale].formatMessage(messages.save)} />
  ))

  .add('ButtonLink', (props: { locale: string }) => (
    <ButtonLink
      label={intl[props.locale].formatMessage(messages.save)}
      onClick={action('onClick')}
    />
  ))

  .add('NormalSwitch', () => (
    <div>
      <NormalSwitch onChange={action('onChange')} />
      <NormalSwitch onChange={action('onChange')} checked />
    </div>
  ));
