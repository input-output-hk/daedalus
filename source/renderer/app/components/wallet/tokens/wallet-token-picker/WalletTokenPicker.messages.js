import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  title: {
    id: 'wallet.token.picker.title',
    defaultMessage: '!!!Add tokens',
    description: 'Token picker title',
  },
  select30label: {
    id: 'wallet.token.picker.select30label',
    defaultMessage: '!!!Select first 30',
    description: 'Label for select first 30 button label',
  },
  checkedCountLabel: {
    id: 'wallet.token.picker.checkedCountLabel',
    defaultMessage: '!!!{checkedCount} of {maxTokens} max tokens',
    description: 'Label of selected tokens count',
  },
  cancelButtonLabel: {
    id: 'wallet.token.picker.cancelButtonLabel',
    defaultMessage: '!!!Cancel',
    description: 'Label of cancel button',
  },
  addButtonLabel: {
    id: 'wallet.token.picker.addButtonLabel',
    defaultMessage: '!!!Add',
    description: 'Label of add button',
  },
});
