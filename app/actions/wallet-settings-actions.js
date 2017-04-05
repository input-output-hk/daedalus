import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  changeWalletPassword: {
    walletId: PropTypes.string.isRequired,
    oldPassword: PropTypes.string.isRequired,
    newPassword: PropTypes.string.isRequired,
  },
  setWalletPassword: {
    walletId: PropTypes.string.isRequired,
    password: PropTypes.string.isRequired,
  },
  updateWalletAssuranceLevel: {
    assurance: PropTypes.string.isRequired,
  },
});
