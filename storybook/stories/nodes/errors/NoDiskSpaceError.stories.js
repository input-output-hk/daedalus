// @flow
import React from 'react';
import { text } from '@storybook/addon-knobs';

import NoDiskSpaceError from '../../../../source/renderer/app/components/loading/no-disk-space-error/NoDiskSpaceError';

export const NoDiskSpaceErrorStory = () => (
  <NoDiskSpaceError
    diskSpaceRequired={`${text('diskSpaceRequired (GB)', 4)} GB`}
    diskSpaceMissing={`${text('diskSpaceRequired (GB)', 1)} GB`}
    diskSpaceRecommended={`${text('diskSpaceRequired (GB)', 8)} GB`}
  />
);
