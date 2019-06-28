// import { themes } from '@storybook/theming';

export default {
  component: {
    fontFamily:
      '".SFNSText-Regular", "San Francisco", BlinkMacSystemFont, "Segoe UI", "Roboto", "Oxygen", "Ubuntu", "Cantarell", "Fira Sans", "Droid Sans", "Helvetica Neue", "Lucida Grande", "Arial", sans-serif',
  },
  title: {
    color: 'rgb(68, 68, 68)',
    display: 'inline-block',
    fontSize: '16px',
    fontWeight: 'bold',
    marginRight: '5px',
  },
  content: {
    // background: '#f2f2f2',
    // boxShadow: '0 0 10px 5px black',
    // padding: '10px',
  },
  menuSlot: {
    display: 'inline-block',
    marginRight: '10px',
  },
  button: {
    borderRadius: '3px',
    color: 'rgba(0, 0, 0, 0.4)',
    cursor: 'pointer',
    fontSize: '14px',
    marginRight: '5px',
    padding: '3px',
  },
  selected: {
    backgroundColor: 'rgba(0, 0, 0, 0.07)',
    color: 'rgb(68, 68, 68)',
  },
};
