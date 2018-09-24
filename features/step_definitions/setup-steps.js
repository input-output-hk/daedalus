import { Given } from 'cucumber';
import termsOfUse from '../support/helpers/terms-of-use-helpers';
import languageSelection from '../support/helpers/language-selection-helpers';

Given(/^I have completed the basic setup$/, async function () {
  await languageSelection.ensureLanguageIsSelected(this.client, { language: 'en-US' });
  await termsOfUse.acceptTerms(this.client);
});
