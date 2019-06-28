import addons from '@storybook/addons';

const channel = addons.getChannel();

export const setInitialProps = props => {
  channel.emit('daedalusMenu/init', props);
};

export const updateParam = query =>
  channel.emit('daedalusMenu/updateParam', query);

export const onReceiveParam = cb =>
  channel.on('daedalusMenu/receiveParam', ({ param, value }) =>
    cb(param, value)
  );
