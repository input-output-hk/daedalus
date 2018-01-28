import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import LanguageSelectionForm from '../app/components/profile/language-selection/LanguageSelectionForm';
import globalMessages from '../app/i18n/global-messages';

const LANGUAGES = [
  { value: 'en-US', label: globalMessages.languageEnglish },
  { value: 'ja-JP', label: globalMessages.languageJapanese },
  { value: 'zh-CN', label: globalMessages.languageChinese },
  { value: 'ko-KR', label: globalMessages.languageKorean },
  { value: 'de-DE', label: globalMessages.languageGerman },
  { value: 'hr-HR', label: globalMessages.languageCroatian },
  { value: 'ru-RU', label: globalMessages.languageRussian },
];

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
        languages={LANGUAGES}
        isSubmitting={false}
      />
    </div>
  ))

  .add('Form - submitting', () => (
    <div>
      <LanguageSelectionForm
        onSubmit={action('submit')}
        languages={LANGUAGES}
        isSubmitting
      />
    </div>
  ));
