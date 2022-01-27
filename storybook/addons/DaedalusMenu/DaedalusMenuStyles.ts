import { themes } from '@storybook/theming';

const { light: styles } = themes;
export default {
  component: {
    fontFamily: styles.fontBase,
  },
  menuSlot: {
    display: 'inline-block',
    marginRight: 10,
  },
  button: {
    background: 'transparent',
    border: 'none',
    borderBottom: '3px solid',
    borderBottomColor: 'transparent',
    color: 'rgba(0, 0, 0, 0.4)',
    cursor: 'pointer',
    fontSize: 13,
    height: 40,
    marginRight: '5px',
    padding: '3px',
    verticalAlign: 'top',
  },
  selected: {
    borderBottomColor: styles.barSelectedColor,
    color: styles.colorSecondary,
  },
  separator: {
    background: 'rgba(0,0,0,.1)',
    display: 'inline-block',
    height: 24,
    margin: '8px 15px 0 0',
    verticalAlign: 'top',
    width: 1,
  },
};
