import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
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
      <Login
        onSubmit={action('submit')}
        onCreateAccount={action('createAccount')}
      />
    </div>
  ))

  .add('Login submitting', () => (
    <div>
      <Login
        isSubmitting
        onSubmit={action('submit')}
        onCreateAccount={action('createAccount')}
      />
    </div>
  ))

  .add('Login invalid', () => (
    <div>
      <Login
        isInvalid
        onSubmit={action('submit')}
        onCreateAccount={action('createAccount')}
      />
    </div>
  ));
