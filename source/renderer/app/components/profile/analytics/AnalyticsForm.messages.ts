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
      '!!!All data is anonymous and is only used for product development purposes.',
    description: 'Analytics data collection description',
  },
  descriptionWithTOCLink: {
    id: 'analytics.form.descriptionWithTOCLink',
    defaultMessage:
      '!!!All data is anonymous and is only used for product development purposes. Read more in the {termsAndConditionsLink}.',
    description: 'Analytics data collection description, including TOC link',
  },
  tocLink: {
    id: 'analytics.form.tocLink',
    defaultMessage: '!!!Terms and Conditions',
    description: 'Terms and Conditions link text',
  },
  tocDetailsTitle: {
    id: 'analytics.form.tocDetailsTitle',
    defaultMessage: '!!!What ToC says about this?',
    description: 'TOC details title',
  },
  tocDetails: {
    id: 'analytics.form.tocDetails',
    defaultMessage:
      '!!!1.6 Privacy. No one will use your data. Ed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto sunt explicabo. ',
    description: 'TOC details',
  },
  dataCollectionDetailsTitle: {
    id: 'analytics.form.dataCollectionDetailsTitle',
    defaultMessage: '!!!What data do we collect?',
    description: 'Data collection details title',
  },
  dataCollectionDetailsUserBehaviour: {
    id: 'analytics.form.dataCollectionDetailsUserBehaviour',
    defaultMessage: '!!!User behavior (where the user clicks)',
    description: 'Description for the user behaviour data collection',
  },
  dataCollectionDetailsDeviceInfo: {
    id: 'analytics.form.dataCollectionDetailsDeviceInfo',
    defaultMessage: '!!!Device information (OS, RAM, disk space, etc)',
    description: 'Description for the device info data collection',
  },
  dataCollectionSwitchButton: {
    id: 'analytics.form.dataCollectionSwitchText',
    defaultMessage: '!!!Allow anonymous data collection',
    description: 'Data collection agreement switch button label',
  },
});
