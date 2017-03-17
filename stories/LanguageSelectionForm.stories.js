import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import LanguageSelectionForm from '../app/components/profile/language-selection/LanguageSelectionForm';

storiesOf('LanguageSelectionForm', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('Form - initial', () => (
    <div>
      <LanguageSelectionForm
        onSubmit={action('submit')}
        isSubmitting={false}
        languages={[
          { value: 'en-US', label: 'English' },
          { value: 'ja_JP', label: 'Japanese' },
          { value: 'zh-CN', label: 'Chinese' },
          { value: 'ko_KR', label: 'Korean' },
          { value: 'de-DE', label: 'German' },
          { value: 'hr-HR', label: 'Croatian' },
        ]}
      />
    </div>
  ))

  .add('Form - submitting', () => (
    <div>
      <LanguageSelectionForm
        onSubmit={action('submit')}
        isSubmitting={true}
        languages={[
          { value: 'en-US', label: 'English' },
          { value: 'ja_JP', label: 'Japanese' },
          { value: 'zh-CN', label: 'Chinese' },
          { value: 'ko_KR', label: 'Korean' },
          { value: 'de-DE', label: 'German' },
          { value: 'hr-HR', label: 'Croatian' },
        ]}
      />
    </div>
  ));
