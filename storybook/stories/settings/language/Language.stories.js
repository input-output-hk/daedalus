// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../../_support/StoryDecorator';
import LanguageSelectionForm from '../../../../source/renderer/app/components/profile/language-selection/LanguageSelectionForm';
import globalMessages from '../../../../source/renderer/app/i18n/global-messages';

const LANGUAGES = [
  { value: 'en-US', label: globalMessages.languageEnglish },
  { value: 'ja-JP', label: globalMessages.languageJapanese },
  { value: 'zh-CN', label: globalMessages.languageChinese },
  { value: 'ko-KR', label: globalMessages.languageKorean },
  { value: 'de-DE', label: globalMessages.languageGerman },
  { value: 'hr-HR', label: globalMessages.languageCroatian },
];

storiesOf('Settings|Language', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('Select Language - initial', () => (
    <div>
      <LanguageSelectionForm
        onSubmit={action('submit')}
        languages={LANGUAGES}
        preselectedLanguage={LANGUAGES[0].value}
        isSubmitting={false}
      />
    </div>
  ))

  .add('Select Language - submitting', () => (
    <div>
      <LanguageSelectionForm
        onSubmit={action('submit')}
        languages={LANGUAGES}
        preselectedLanguage={LANGUAGES[0].value}
        isSubmitting
      />
    </div>
  ));
