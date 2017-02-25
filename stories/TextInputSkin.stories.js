import React, { Component, PropTypes } from 'react';
import { storiesOf, action } from '@kadira/storybook';
import { observable, action as mobxAction } from 'mobx';
import StoryDecorator from './support/StoryDecorator';
import PropsObserver from './support/PropsObserver';
import TextInput from '../app/components/forms/TextInput';
import TextInputSkin from '../app/components/forms/TextInputSkin';

storiesOf('TextInput', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('plain', () => <TextInput skin={<TextInputSkin />} />)

  .add('label', () => <TextInput skin={<TextInputSkin label="Some label" />} />)

  .add('placeholder', () => <TextInput skin={<TextInputSkin placeholder="Username" />} />)

  .add('disabled', () => (
    <TextInput
      disabled
      skin={<TextInputSkin placeholder="disabled" label="Disabled Input" />}
    />
  ))

  .add('error', () => (
    <TextInput
      value="Franz"
      error="Usernames must be at least 8 characters long"
      skin={<TextInputSkin label="Username" />}
    />
  ))

  .add('type=password', () => {
    const state = observable({value: ''});
    return (
      <PropsObserver propsForChildren={state}>
        <TextInput
          skin={<TextInputSkin type="password" />}
          onChange={mobxAction((value) => { state.value = value; })}
        />
      </PropsObserver>
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
          onChange={mobxAction((value) => { state.value = value; })}
          maxLength={5}
          skin={<TextInputSkin label="Input with max. 5 Characters" />}
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
          onChange={mobxAction((value, event) => {
            onChangeAction(value, event);
            state.value = value;
          })}
          onKeyPress={action('onKeyPress')}
          maxLength={5}
          skin={<TextInputSkin label="Type to see events logged" />}
        />
      </PropsObserver>
    );
  });
