// @flow
import addons from '@storybook/addons';
import type { DaedalusMenuState } from './DaedalusMenu';

const channel = addons.getChannel();

export const setInitialProps = (props: DaedalusMenuState) => {
  channel.emit('daedalusMenu/init', props);
};

export const updateParam = (query: Object) =>
  channel.emit('daedalusMenu/updateParam', query);

export const onReceiveParam = (cb: Function) =>
  channel.on('daedalusMenu/receiveParam', ({ param, value }) =>
    cb(param, value)
  );
