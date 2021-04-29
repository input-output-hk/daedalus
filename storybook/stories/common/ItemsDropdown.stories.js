// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { observable, action as mobxAction } from 'mobx';
import { action } from '@storybook/addon-actions';
import {
  withKnobs /* , boolean, number, text, date */,
} from '@storybook/addon-knobs';
import { escapeRegExp } from 'lodash';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import StoryLayout from '../_support/StoryLayout';
import ItemsDropdown from '../../../source/renderer/app/components/widgets/forms/ItemsDropdown';

storiesOf('Common|ItemsDropdown', module)
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

  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('Generic', () => {
    const options = [
      {
        topLabel: 'Item 1 - top',
        bottomLabel: 'Item 1 - bottom',
        value: 'item1',
        label: 'LABEL',
      },
      {
        topLabel: 'Item 2 - top',
        bottomLabel: 'Item 2 - bottom',
        value: 'item2',
      },
      {
        topLabel: 'Item 3 - top',
        bottomLabel: 'Item 3 - bottom',
        value: 'item3',
      },
    ];
    return (
      <ItemsDropdown
        options={options}
        onChange={action('onChange')}
        value={'item2'}
        onSearch={(searchValue) => {
          return options.filter((option) => {
            const { topLabel, bottomLabel, value } = option;
            const regex = new RegExp(escapeRegExp(searchValue), 'i');
            return (
              regex.test(topLabel) ||
              regex.test(bottomLabel) ||
              regex.test(value)
            );
          });
        }}
      />
    );
  });
