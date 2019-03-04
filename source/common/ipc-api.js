// @flow
import {
  GET_GPU_STATUS_CHANNEL,
} from './ipc/api';

export const GET_GPU_STATUS = {
  REQUEST: GET_GPU_STATUS_CHANNEL,
  SUCCESS: `${GET_GPU_STATUS_CHANNEL}-success`,
  ERROR: `${GET_GPU_STATUS_CHANNEL}-error`,
};
