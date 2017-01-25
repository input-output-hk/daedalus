import React from 'react';
import { storiesOf } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import CheckboxWithLongLabel from '../app/components/widgets/forms/CheckboxWithLongLabel';

storiesOf('Widgets', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('CheckboxWithLongLabel - checked', () => (
    <div>
      <CheckboxWithLongLabel
        label={`I understand that if this application is moved to another device or deleted,
        my money can be only recovered with the backup phrase which were written down in a secure place`}
        checked={true}
        onChange={() => {}}
      />
    </div>
  ))

  .add('CheckboxWithLongLabel - unchecked', () => (
    <div>
      <CheckboxWithLongLabel
        label={`I understand that if this application is moved to another device or deleted,
        my money can be only recovered with the backup phrase which were written down in a secure place`}
        checked={false}
        onChange={() => {}}
      />
    </div>
  ));
