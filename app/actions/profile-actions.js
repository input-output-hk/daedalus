import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  updateLocale: {
    locale: PropTypes.string.isRequired,
  },
});
