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
  dataCollectionSwitchButton: {
    id: 'analytics.form.dataCollectionSwitchText',
    defaultMessage: '!!!Allow anonymous data collection',
    description: 'Data collection agreement switch button label',
  },
  confirmButton: {
    id: 'analytics.dialog.confirmButton',
    defaultMessage: '!!!Confirm',
    description: 'Analytics data collection confirmation button text',
  },
});
