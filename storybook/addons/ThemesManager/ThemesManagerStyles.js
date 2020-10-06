// @flow
import { themes } from '@storybook/theming';

const { light: styles } = themes;

export default {
  component: {
    fontFamily: styles.fontBase,
    padding: '5px 10px',
  },
  input: {
    background: '#eaeaea',
    border: '1px solid lightgray',
    color: '#5d5d5d',
    fontSize: '14px',
    margin: '10px auto',
    padding: '5px',
    width: '100%',
  },
  list: {
    marginLeft: '0',
    paddingLeft: '30px',
    position: 'relative',
  },
  listItem: {
    color: '#011623',
    display: 'block',
    fontSize: 20,
    textDecoration: 'underline',
  },
  itemColor: {
    background: 'black',
    borderRadius: 2,
    display: 'block',
    height: 20,
    left: '0',
    position: 'absolute',
    width: 20,
  },
};
