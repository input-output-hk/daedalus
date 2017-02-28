import React, { Component, PropTypes } from 'react';
import { storiesOf, action } from '@kadira/storybook';
import { observable, action as mobxAction } from 'mobx';
import StoryDecorator from './support/StoryDecorator';
import PropsObserver from './support/PropsObserver';
import Input from 'react-polymorph/lib/components/Input';
import CurrencyInputSkin from '../app/components/forms/CurrencyInputSkin';

storiesOf('CurrencyInputSkin', module)

  .addDecorator((story) => {
    const onChangeAction = action('onChange');
    const state = observable({
      value: '',
      onChange: mobxAction((value, event) => {
        state.value = value;
        onChangeAction(value, event);
      })
    });
    return (
      <StoryDecorator>
        <PropsObserver propsForChildren={state}>
          {story()}
        </PropsObserver>
      </StoryDecorator>
    );
  })

  // ====== Stories ======

  .add('default', () => {
    return <Input
      label="Amount"
      placeholder="Amount in"
      currency="ADA"
      skin={<CurrencyInputSkin />}
    />
  });
