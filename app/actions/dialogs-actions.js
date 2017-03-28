import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  open: {
    dialog: PropTypes.func.isRequired,
  },
  close: {},
  updateDataForActiveDialog: {
    data: PropTypes.object.isRequired,
  }
});
