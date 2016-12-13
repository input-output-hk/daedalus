import React from 'react';
import { storiesOf } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import Login from '../app/components/auth/Login';

storiesOf('Login', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('Login', () => (
    <div>
      <Login />
    </div>
  ))

  .add('Login submitting', () => (
    <div>
      <Login isSubmitting />
    </div>
  ));
