// @flow
import data from '../data';

export const loadWalletTransactions = (request: {
  address: string,
  searchTerm: string,
  limit: number
}) => new Promise((resolve) => {
  setTimeout(() => {
    const { address, searchTerm } = request;
    const regexp = new RegExp(searchTerm, 'i');
    let transactions = [];
    if (data.transactions[address]) {
      transactions = (
        data.transactions[address]
        .filter((t) => regexp.test(t.title)) // Filter by title search
        .sort((a, b) => { // Sort by date
          const aIsSmallerOrEqual = a.date < b.date ? 1 : 0;
          return a.date > b.date ? -1 : aIsSmallerOrEqual;
        })
      );
    }
    resolve({
      total: transactions.length,
      transactions: transactions.slice(0, request.limit) // Limit number of results,
    });
  }, 1000);
});
