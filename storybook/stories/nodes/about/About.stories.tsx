import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryDecorator from '../../_support/StoryDecorator';
import AboutDialog from '../../../../source/renderer/app/containers/static/AboutDialog';
import { aboutDialogProps } from '../_utils/props';
import { osNameOf } from '../../_support/globals';

export default {
  title: 'Nodes / About',

  decorators: [
    (story, context) => (
      <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
    ),
  ],
};

export const _AboutDialog = {
  render: (_args, context) => {
    aboutDialogProps.stores.app.environment.os = osNameOf(context);
    return <AboutDialog {...aboutDialogProps} />;
  },
};
