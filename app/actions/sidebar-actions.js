import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  toggleSidebar: {},
  toggleSubMenus: {},
  sidebarCategorySelected: {
    category: PropTypes.string.isRequired,
  },
  walletSelected: {
    walletId: PropTypes.string.isRequired,
  }
});
