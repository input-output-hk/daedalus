import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  login: {
    email: PropTypes.string.isRequired,
    passwordHash: PropTypes.string.isRequired,
  },
  updateProfileField: {
    field: PropTypes.string.isRequired,
    value: PropTypes.string.isRequired,
  },
  goToRoute: {
    route: PropTypes.string.isRequired,
  },
  createPersonalWallet: {
    name: PropTypes.string.isRequired,
    currency: PropTypes.string.isRequired,
  },
  filterTransactions: {
    searchTerm: PropTypes.string.isRequired,
  },
  loadMoreTransactions: {},
  changeSidebarRoute: {
    route: PropTypes.string.isRequired,
  },
  toggleSidebar: {},
  toggleCreateWalletDialog: {},
  sendMoney: {
    receiver: PropTypes.string.isRequired,
    amount: PropTypes.string.isRequired,
    description: PropTypes.string,
  }
}, PropTypes.validateWithErrors);
