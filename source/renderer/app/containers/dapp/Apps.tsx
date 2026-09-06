import React from 'react';
import type { ReactNode } from 'react';
import MainLayout from '../MainLayout';

type Props = { children: ReactNode };

const Apps = ({ children }: Props) => <MainLayout>{children}</MainLayout>;

export default Apps;
