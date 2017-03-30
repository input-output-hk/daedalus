import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  updateWalletAssuranceLevel: {
    assurance: PropTypes.string.isRequired,
  },
  updateWalletUnit: {
    unit: PropTypes.number.isRequired,
  },
});
