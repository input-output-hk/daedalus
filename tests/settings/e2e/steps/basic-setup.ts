import { Given } from "cucumber";
import { initialSettingsHelpers, migrationHelpers, termsOfUseHelpers } from "./helpers";

const {
  acceptMigration
} = migrationHelpers;
const {
  acceptTerms
} = termsOfUseHelpers;
const {
  ensureLanguageIsSelected
} = initialSettingsHelpers;
Given(/^I have completed the basic setup$/, async function () {
  await ensureLanguageIsSelected(this.client, {
    language: 'en-US'
  });
  await acceptTerms(this.client);
  await acceptMigration(this.client);
  return true;
});