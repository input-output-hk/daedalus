import { Given } from 'cucumber';
import termsOfUse from '../helpers/terms-of-use-helpers';
import languageSelection from '../helpers/language-selection-helpers';
import dataLayerMigration from '../helpers/data-layer-migration-helpers';

Given(/^I have completed the basic setup$/, async function() {
  await languageSelection.ensureLanguageIsSelected(this.client, {
    language: 'en-US',
  });
  await termsOfUse.acceptTerms(this.client);
  await dataLayerMigration.acceptMigration(this.client);
});
