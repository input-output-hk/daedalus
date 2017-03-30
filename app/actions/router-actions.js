import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  goToRoute: {
    route: PropTypes.string.isRequired,
    params: PropTypes.object,
  },
});
