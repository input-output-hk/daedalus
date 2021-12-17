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
});
