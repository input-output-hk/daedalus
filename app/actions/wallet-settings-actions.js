import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  updateWalletUnit: {
    unit: PropTypes.number.isRequired,
  },
});
