unit GalaUtils;
{$mode Delphi}
{ Библиотека параллельного программирования Gala.
  Различные дополнительные утилиты.
}

interface

uses Windows;

  function GalaExec(const aCommandLine: String; aDirectory: PChar = nil;
    aShow: Word = SW_SHOW; aRect: PRect = nil): THandle;

implementation

function GalaExec(const aCommandLine: String; aDirectory: PChar;
  aShow: Word; aRect: PRect): THandle;
var
  si:  TStartupInfo;
  pi:  TProcessInformation;
  Ok:  Boolean;
begin
  result := 0;
  ZeroMemory(@si, SizeOf(si));
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := aShow;
  if Assigned(aRect) then begin
    with aRect^ do begin
      si.dwX     := Left;
      si.dwY     := Top;
      si.dwFlags := si.dwFlags or STARTF_USEPOSITION;
      si.dwXSize := Right  - Left;
      si.dwYSize := Bottom - Top;
      if (si.dwXSize > 10) and (si.dwYSize > 10) then
        si.dwFlags := si.dwFlags or STARTF_USESIZE;
    end;
  end;
  Ok := CreateProcess(nil, PChar(aCommandLine), nil, nil, False,
        0, nil, aDirectory, si, pi);
  if Ok then
    result := pi.hProcess;
end;

end.
