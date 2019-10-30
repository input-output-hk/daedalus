// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from './support/StoryDecorator';
import LanguageSelectionForm from '../../source/renderer/app/components/profile/language-selection/LanguageSelectionForm';
import SettingsMenu from '../../source/renderer/app/components/settings/menu/SettingsMenu';
import globalMessages from '../../source/renderer/app/i18n/global-messages';
import { ROUTES } from '../../source/renderer/app/routes-config';

const LANGUAGES = [
  { value: 'en-US', label: globalMessages.languageEnglish },
  { value: 'ja-JP', label: globalMessages.languageJapanese },
  { value: 'zh-CN', label: globalMessages.languageChinese },
  { value: 'ko-KR', label: globalMessages.languageKorean },
  { value: 'de-DE', label: globalMessages.languageGerman },
  { value: 'hr-HR', label: globalMessages.languageCroatian },
];

const isActiveItem = item => {
  if (item === ROUTES.SETTINGS.GENERAL) {
    return true;
  }
  return false;
};

storiesOf('Settings', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('SettingsMenu', () => (
    <div>
      <SettingsMenu
        onItemClick={action('onItemClick')}
        isActiveItem={isActiveItem}
        isIncentivizedTestnet={false}
      />
    </div>
  ))

  .add('LanguageSelectionForm - initial', () => (
    <div>
      <LanguageSelectionForm
        onSubmit={action('submit')}
        languages={LANGUAGES}
        preselectedLanguage={LANGUAGES[0].value}
        isSubmitting={false}
      />
    </div>
  ))

  .add('LanguageSelectionForm - submitting', () => (
    <div>
      <LanguageSelectionForm
        onSubmit={action('submit')}
        languages={LANGUAGES}
        preselectedLanguage={LANGUAGES[0].value}
        isSubmitting
      />
    </div>
  ));
