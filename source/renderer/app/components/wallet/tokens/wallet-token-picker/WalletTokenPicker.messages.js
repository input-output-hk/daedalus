'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.messages = void 0;
const react_intl_1 = require('react-intl');
exports.messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'wallet.token.picker.title',
    defaultMessage: '!!!Add tokens',
    description: 'Token picker title',
  },
  allTokensLabel: {
    id: 'wallet.token.picker.allTokensLabel',
    defaultMessage: '!!!All tokens',
    description: 'Label for all tokens option',
  },
  favoriteTokensLabel: {
    id: 'wallet.token.picker.favoriteTokensLabel',
    defaultMessage: '!!!Favorites',
    description: 'Label for favorite tokens option',
  },
  checkAllLabel: {
    id: 'wallet.token.picker.checkAllLabel',
    defaultMessage: '!!!Select all',
    description: 'Label for select all button label',
  },
  checkedCountLabel: {
    id: 'wallet.token.picker.checkedCountLabel',
    defaultMessage: '!!!{checkedCount} out of {maxTokens} tokens.',
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
  clearAll: {
    id: 'wallet.token.picker.clearAll',
    defaultMessage: '!!!Clear selection',
    description: 'Label of clear selection button',
  },
  noResults: {
    id: 'wallet.token.picker.noResults',
    defaultMessage: '!!!Results do not match search query',
    description: 'Text for no results',
  },
  maxTokensWarning: {
    id: 'wallet.token.picker.maxTokensWarning',
    defaultMessage:
      '!!!You have already reached a maximum of {maxTokens} tokens for your transaction. To add another token you need to remove one from a list.',
    description: 'Max tokens warning',
  },
});
//# sourceMappingURL=WalletTokenPicker.messages.js.map
