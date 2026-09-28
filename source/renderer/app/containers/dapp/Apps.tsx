import React from 'react';
import type { ReactNode } from 'react';
import MainLayout from '../MainLayout';

type Props = { children: ReactNode };

function Apps({ children }: Props) {
  return <MainLayout>{children}</MainLayout>;
}

export default Apps;
