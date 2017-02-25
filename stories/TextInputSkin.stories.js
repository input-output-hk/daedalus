import React, { Component, PropTypes } from 'react';
import { storiesOf, action } from '@kadira/storybook';
import { observable, action as mobxAction } from 'mobx';
import StoryDecorator from './support/StoryDecorator';
import PropsObserver from './support/PropsObserver';
import TextInput from '../app/components/forms/TextInput';
import TextInputSkin from '../app/components/forms/TextInputSkin';
import CurrencyTextInputSkin from '../app/components/forms/CurrencyTextInputSkin';

storiesOf('TextInput', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('plain', () => <TextInput skin={<TextInputSkin />} />)

  .add('label', () => <TextInput label="Some label" skin={<TextInputSkin />} />)

  .add('placeholder', () => <TextInput placeholder="Username" skin={<TextInputSkin />} />)

  .add('disabled', () => (
    <TextInput
      label="Disabled Input"
      placeholder="disabled"
      disabled
      skin={<TextInputSkin />}
    />
  ))

  .add('error', () => (
    <TextInput
      value="Franz"
      error="Requires at least 8 characters"
      label="Username"
      skin={<TextInputSkin />}
    />
  ))

  .add('type=password', () => {
    return (
      <TextInput value="secret" type="password" skin={<TextInputSkin />} />
    );
  })

  .add('focus / blur', () => {
    let input;
    return (
      <div>
        <TextInput
          ref={(ref) => input = ref}
          skin={<TextInputSkin />}
          onFocus={action('onFocus')}
          onBlur={action('onBlur')}
        />
        <button onClick={() => input.focus()}>focus</button> | <button onClick={() => input.blur()}>blur</button>
      </div>
    );
  })

  .add('maxLength(5)', () => {
    const state = observable({ value: '' });
    return (
      <PropsObserver propsForChildren={state}>
        <TextInput
          label="Input with max. 5 Characters"
          onChange={mobxAction((value) => { state.value = value; })}
          maxLength={5}
          skin={<TextInputSkin />}
        />
      </PropsObserver>
    );
  })

  .add('onChange / onKeyPress', () => {
    const state = observable({ value: '' });
    const onChangeAction = action('onChange');
    return (
      <PropsObserver propsForChildren={state}>
        <TextInput
          label="Type to see events logged"
          onChange={mobxAction((value, event) => {
            onChangeAction(value, event);
            state.value = value;
          })}
          onKeyPress={action('onKeyPress')}
          maxLength={5}
          skin={<TextInputSkin />}
        />
      </PropsObserver>
    );
  })

  .add('CurrencyTextInputSkin', () => {
    return <TextInput
      label="Amount"
      placeholder="Amount in"
      currency="ADA"
      skin={<CurrencyTextInputSkin />}
    />
  });
