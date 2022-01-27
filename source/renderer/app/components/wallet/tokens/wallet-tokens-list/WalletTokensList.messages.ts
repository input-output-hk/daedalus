// @flow
import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  noResults: {
    id: 'wallet.tokens.list.search.noResults',
    defaultMessage: '!!!No results matching your query',
    description: 'No results on the WalletTokensList',
  },
  searchResults: {
    id: 'wallet.tokens.list.search.searchResults',
    defaultMessage: '!!!Search Results',
    description: 'Search Results on the WalletTokensList',
  },
  columnAmount: {
    id: 'wallet.tokens.list.column.amount',
    defaultMessage: '!!!Amount',
    description: 'Amount header on the WalletTokensList',
  },
  columnToken: {
    id: 'wallet.tokens.list.column.token',
    defaultMessage: '!!!Token',
    description: 'Token header on the WalletTokensList',
  },
  viewAllButtonLabel: {
    id: 'wallet.tokens.list.viewAllButton.label',
    defaultMessage: '!!!View all tokens',
    description: 'View all button label on the WalletTokensList',
  },
});
