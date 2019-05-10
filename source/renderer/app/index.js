// @flow
import React from 'react';
import { render } from 'react-dom';

const App = () => <div>Hello World</div>;

const initializeDaedalus = () => {
  render(<App />, document.getElementById('root'));
};

window.addEventListener('load', initializeDaedalus);
