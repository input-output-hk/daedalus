import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  filterTransactions: {
    searchTerm: PropTypes.string.isRequired,
  },
  loadMoreTransactions: {},
});
