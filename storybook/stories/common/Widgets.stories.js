// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { observable, action as mobxAction } from 'mobx';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../_support/StoryDecorator';
import BigButtonForDialogs from '../../../source/renderer/app/components/widgets/BigButtonForDialogs';
import MnemonicInputWidget from '../../../source/renderer/app/components/widgets/forms/MnemonicInputWidget';
import createIcon from '../../../source/renderer/app/assets/images/create-ic.inline.svg';
import importIcon from '../../../source/renderer/app/assets/images/import-ic.inline.svg';
import joinSharedIcon from '../../../source/renderer/app/assets/images/join-shared-ic.inline.svg';
import TinySwitch from '../../../source/renderer/app/components/widgets/forms/TinySwitch';
import ButtonLink from '../../../source/renderer/app/components/widgets/ButtonLink';

storiesOf('Common|Widgets', module)
  .addDecorator(story => {
    const onChangeAction = action('onChange');
    const state = observable({
      checked: false,
      onChange: mobxAction((value, event) => {
        state.checked = value;
        onChangeAction(value, event);
      }),
    });

    return <StoryDecorator propsForChildren={state}>{story()}</StoryDecorator>;
  })

  // ====== Stories ======

  .add('BigButtonForDialogs', () => (
    <div>
      <div style={{ width: '300px', height: '200px', display: 'flex' }}>
        <BigButtonForDialogs
          description="Create new wallet"
          label="Create"
          icon={createIcon}
          onClick={() => {}}
        />
      </div>
      <div style={{ width: '300px', height: '200px', display: 'flex' }}>
        <BigButtonForDialogs
          description="Join shared wallet up to 5 people"
          label="Join"
          icon={joinSharedIcon}
          onClick={() => {}}
          isDisabled
        />
      </div>
      <div style={{ width: '300px', height: '200px', display: 'flex' }}>
        <BigButtonForDialogs
          description="Import existing wallet"
          label="Import"
          icon={importIcon}
          onClick={() => {}}
        />
      </div>
    </div>
  ))

  .add('MnemonicInputWidget - 9 words', () => {
    const tokens = observable(['', '', '', '', '', '', '', '', '']);
    return (
      <MnemonicInputWidget
        label="Your Passphrase"
        tokens={tokens}
        onTokenChanged={(index, token) => {
          tokens[index] = token;
        }}
      />
    );
  })

  .add('TinySwitch', () => <TinySwitch />)

  .add('TinySwitch - short label', () => <TinySwitch label="My switch" />)

  .add('ButtonLink', () => (
    <ButtonLink
      label="Follow instructions and manually update"
      onClick={action('onClick')}
    />
  ));
