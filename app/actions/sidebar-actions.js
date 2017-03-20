import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  toggleSubMenus: {},
  activateSidebarCategory: {
    category: PropTypes.string.isRequired,
    showSubMenus: PropTypes.bool,
  },
  walletSelected: {
    walletId: PropTypes.string.isRequired,
  }
});
