import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  toggleSidebar: {},
  toggleMaximized: {},
  sidebarCategorySelected: {
    category: PropTypes.string.isRequired,
  },
});
