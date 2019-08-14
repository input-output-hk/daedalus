// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from './support/StoryDecorator';
import LanguageSelectionForm from '../../source/renderer/app/components/profile/language-selection/LanguageSelectionForm';
import globalMessages from '../../source/renderer/app/i18n/global-messages';

const LANGUAGES = [
  { value: 'en-US', label: globalMessages.languageEnglish },
  { value: 'ja-JP', label: globalMessages.languageJapanese },
  { value: 'zh-CN', label: globalMessages.languageChinese },
  { value: 'ko-KR', label: globalMessages.languageKorean },
  { value: 'de-DE', label: globalMessages.languageGerman },
  { value: 'hr-HR', label: globalMessages.languageCroatian },
];

storiesOf('LanguageSelectionForm', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('Form - initial', () => (
    <div>
      <LanguageSelectionForm
        onSubmit={action('submit')}
        languages={LANGUAGES}
        preselectedLanguage={LANGUAGES[0].value}
        isSubmitting={false}
      />
    </div>
  ))

  .add('Form - submitting', () => (
    <div>
      <LanguageSelectionForm
        onSubmit={action('submit')}
        languages={LANGUAGES}
        preselectedLanguage={LANGUAGES[0].value}
        isSubmitting
      />
    </div>
  ));
