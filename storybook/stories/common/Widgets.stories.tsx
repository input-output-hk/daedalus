import React from 'react';
import { defineMessages, IntlProvider } from 'react-intl';
import { storiesOf } from '@storybook/react';
import { observable, action as mobxAction } from 'mobx';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean, number, text, date } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import StoryLayout from '../_support/StoryLayout';
import enMessages from '../../../source/renderer/app/i18n/locales/en-US.json';
import jpMessages from '../../../source/renderer/app/i18n/locales/ja-JP.json';
import BigButtonForDialogs from '../../../source/renderer/app/components/widgets/BigButtonForDialogs';
import InlineEditingInput from '../../../source/renderer/app/components/widgets/forms/InlineEditingInput';
import CountdownWidget from '../../../source/renderer/app/components/widgets/CountdownWidget';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../source/renderer/app/a... Remove this comment to see the full error message
import createIcon from '../../../source/renderer/app/assets/images/create-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../source/renderer/app/a... Remove this comment to see the full error message
import importIcon from '../../../source/renderer/app/assets/images/import-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../source/renderer/app/a... Remove this comment to see the full error message
import joinSharedIcon from '../../../source/renderer/app/assets/images/join-shared-ic.inline.svg';
import TinySwitch from '../../../source/renderer/app/components/widgets/forms/TinySwitch';
import ButtonLink from '../../../source/renderer/app/components/widgets/ButtonLink';
import NormalSwitch from '../../../source/renderer/app/components/widgets/forms/NormalSwitch';
import { Separator } from '../../../source/renderer/app/components/widgets/separator/Separator';
import { CollapsibleSection } from '../../../source/renderer/app/components/widgets/collapsible-section/CollapsibleSection';
import { VerticalSeparator } from '../../../source/renderer/app/components/wallet/widgets/VerticalSeparator';

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

function WidgetPresentationBox({ children }) {
  return <div style={{ padding: 20 }}>{children}</div>;
}

storiesOf('Common / Widgets', module)
  .addDecorator((story: any, context: any) => {
    if (context.name === 'CountdownWidget') {
      return story();
    }

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
  .addDecorator(withKnobs) // ====== Stories ======
  .add('CountdownWidget', () => (
    <CountdownWidget
      startDateTime={new Date(date('startDateTime')).toISOString()}
      format="DD-HH-mm-ss"
    />
  ))
  .add('InlineEditingInput', () => (
    <div>
      <div
        style={{
          width: '700px',
          height: '200px',
          display: 'flex',
        }}
      >
        <InlineEditingInput
          label={text('inputFieldLabel', 'Input label')}
          value=""
          placeholder={text('inputFieldPlaceholder', 'Enter you text here')}
          onSubmit={action('onSubmit')}
          isValid={(value) => value && value.length > 3 && value !== 'error'}
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          validationErrorMessage={text('validationErrorMessage', 'Error!')}
          successfullyUpdated={boolean('successfullyUpdated', true)}
          isActive={boolean('isActive', true)}
          isSubmitting={boolean('isSubmitting', false)}
          inputBlocked={boolean('inputBlocked', false)}
          disabled={boolean('disabled', false)}
          readOnly={boolean('readOnly', false)}
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2-4 arguments, but got 1.
          maxLength={number('maxLength')}
        />
      </div>
    </div>
  ))
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    locale: string;}) =... Remove this comment to see the full error message
  .add('BigButtonForDialogs', (props: { locale: string }) => (
    <div>
      <div
        style={{
          width: '300px',
          height: '200px',
          display: 'flex',
        }}
      >
        <BigButtonForDialogs
          description={intl[props.locale].formatMessage(
            messages.createNewWallet
          )}
          label={intl[props.locale].formatMessage(messages.create)}
          icon={createIcon}
          onClick={() => {}}
        />
      </div>
      <div
        style={{
          width: '300px',
          height: '200px',
          display: 'flex',
        }}
      >
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
      <div
        style={{
          width: '300px',
          height: '200px',
          display: 'flex',
        }}
      >
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
  .add('TinySwitch', () => <TinySwitch />)
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    locale: string;}) =... Remove this comment to see the full error message
  .add('TinySwitch - short label', (props: { locale: string }) => (
    <TinySwitch label={intl[props.locale].formatMessage(messages.save)} />
  ))
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    locale: string;}) =... Remove this comment to see the full error message
  .add('ButtonLink', (props: { locale: string }) => (
    <ButtonLink
      label={intl[props.locale].formatMessage(messages.save)}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      onClick={action('onClick')}
    />
  ))
  .add('NormalSwitch', () => (
    <div>
      <NormalSwitch onChange={action('onChange')} />
      <NormalSwitch onChange={action('onChange')} checked />
    </div>
  ))
  .add('CollapsibleTextBlock', () => (
    <WidgetPresentationBox>
      <CollapsibleSection header="Lorem ipsum dolor sit amet">
        <p
          style={{
            font: '14px var(--font-light)',
            color: 'var(--theme-main-body-messages-color)',
          }}
        >
          Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc
          tincidunt fringilla tellus quis tempus. Phasellus sed magna in metus
          blandit luctus sit amet in velit. Maecenas magna erat, vestibulum eget
          commodo vel, placerat et arcu. Nam pellentesque tortor a nunc finibus
          bibendum. Suspendisse gravida ut arcu eget ornare. In turpis est,
          molestie et nisl sit amet, luctus elementum dui. Lorem ipsum dolor sit
          amet, consectetur adipiscing elit. Praesent elementum cursus nunc, in
          ultricies purus scelerisque et.Phasellus metus mi, varius interdum
          sollicitudin nec, vulputate id purus. Suspendisse potenti. Donec
          porta, eros ut pulvinar mattis, nunc libero venenatis erat, sed
          ultrices ex ligula quis nibh. Praesent vehicula sapien nec cursus
          vulputate. Donec ullamcorper vulputate enim et mattis. Suspendisse
          potenti. Quisque quis venenatis elit, in fermentum mauris. Ut laoreet,
          nibh a rhoncus rhoncus, mi est pretium neque, sit amet pellentesque
          odio eros et augue.
        </p>
      </CollapsibleSection>
    </WidgetPresentationBox>
  ))
  .add('Separator', () => (
    <WidgetPresentationBox>
      <Separator />
    </WidgetPresentationBox>
  ))
  .add('VerticalSeparator', () => (
    <WidgetPresentationBox>
      <VerticalSeparator />
    </WidgetPresentationBox>
  ));
