const TERMS_OF_USE_FORM = '.TermsOfUseForm_component';

const termsOfUse = {
  waitForVisible: async (client, { isHidden } = {}) =>
    client.waitForVisible(TERMS_OF_USE_FORM, null, isHidden),
  acceptTerms: async client => {
    await client.execute(() => {
      daedalus.actions.profile.acceptTermsOfUse.trigger();
    });
    await termsOfUse.waitForVisible(client, { isHidden: true });
  },
};

export default termsOfUse;
