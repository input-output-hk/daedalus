import React from 'react';
import { action } from '@storybook/addon-actions';
import DataLayerMigrationForm from '../../../../source/renderer/app/components/profile/data-layer-migration/DataLayerMigrationForm';

export const DataLayerMigrationStory = () => (
  <DataLayerMigrationForm onSubmit={action('onSubmit')} />
);
