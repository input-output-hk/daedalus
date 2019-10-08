// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';
import StoryDecorator from '../../_support/StoryDecorator';
import AboutDialog from '../../../../source/renderer/app/containers/static/AboutDialog';
import { aboutDialogProps } from '../utils/props';

storiesOf('Nodes|About', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  .add('About Dialog', () => <AboutDialog {...aboutDialogProps} />);
