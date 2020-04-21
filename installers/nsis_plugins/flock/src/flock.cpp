#define UNICODE

#include <windows.h>
#include "exdll.h"
#include <stdio.h>

extern "C" void __declspec(dllexport) flock(HWND hwndParent
    , int string_size
    , TCHAR *variables
    , stack_t **stacktop) {
  HANDLE fh;
  OVERLAPPED overlapped;

  EXDLL_INIT();

  TCHAR lockFileName[MAX_PATH];
  popstring(lockFileName);

  fh = CreateFile(lockFileName, GENERIC_READ, 0, NULL, CREATE_NEW, 0, NULL);

  if (fh == INVALID_HANDLE_VALUE) {
    printf("CreateFile failed (%d)\n", GetLastError());
    pushstring(L"failure");
  }

  memset(&overlapped, 0, sizeof(OVERLAPPED));
  bool result = LockFileEx(fh
      , LOCKFILE_EXCLUSIVE_LOCK | LOCKFILE_FAIL_IMMEDIATELY
      , 0
      , 0xffffffff
      , 0xffffffff
      , &overlapped);

  if (result) {
    pushstring(L"true");
  } else {
    pushstring(L"false");
  }
}
