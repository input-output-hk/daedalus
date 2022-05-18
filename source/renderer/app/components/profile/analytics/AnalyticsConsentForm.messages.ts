import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  title: {
    id: 'analytics.form.title',
    defaultMessage: '!!!Anonymous data collection',
    description: 'Analytics form title',
  },
  description: {
    id: 'analytics.form.description',
    defaultMessage:
      '!!!Analytic data is used for product development purposes only.',
    description: 'Analytics data collection description',
  },
  tocDetailsTitle: {
    id: 'analytics.form.tocDetailsTitle',
    defaultMessage:
      '!!!We collect data on (1) User click behavior and (2) Device information.',
    description: 'TOC details title',
  },
  tocDetails: {
    id: 'analytics.form.tocDetails',
    defaultMessage:
      '!!!1.6 Privacy. No one will use your data. Ed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto sunt explicabo. ',
    description: 'TOC details',
  },
  dataCollectionSwitchButton: {
    id: 'analytics.form.dataCollectionSwitchText',
    defaultMessage: '!!!Allow anonymous data collection',
    description: 'Data collection agreement switch button label',
  },
  allowButton: {
    id: 'analytics.form.allowButton',
    defaultMessage: '!!!Allow',
    description: 'Analytics data collection allow button text',
  },
  skipButton: {
    id: 'analytics.dialog.skipButton',
    defaultMessage: '!!!Skip',
    description: 'Analytics data collection skip button text',
  },
  privacyPolicyLink: {
    id: 'analytics.form.privacyPolicyLink',
    defaultMessage: '!!!Daedalus Privacy Policy',
    description: 'Daedalus Privacy Policy link text',
  },
  analyticsSectionPrivacyPolicy: {
    id: 'analytics.form.analyticsSectionPrivacyPolicy',
    defaultMessage:
      '!!!Read more about our privacy practices in the {privacyPolicyLink}.',
    description:
      'Analytics data collection description, under collapsible details',
  },
});
