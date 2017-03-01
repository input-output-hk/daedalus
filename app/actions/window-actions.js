import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  resizeWindow: {
    width: PropTypes.number.isRequired,
    height: PropTypes.number.isRequired,
  },
});
