import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryDecorator from '../../_support/StoryDecorator';
import AboutDialog from '../../../../source/renderer/app/containers/static/AboutDialog';
import { aboutDialogProps } from '../_utils/props';

export default {
  title: 'Nodes / About',

  decorators: [
    (story, context) => (
      <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
    ),
  ],
};

export const _AboutDialog = {
  render: (props) => {
    aboutDialogProps.stores.app.environment.os = props.osName;
    return <AboutDialog {...aboutDialogProps} />;
  },
};
