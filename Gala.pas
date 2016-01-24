unit Gala;

{$MODE Delphi}

{ Библиотека параллельного программирования Gala.
  Версия 1.11
  Свободно распространяется с исходными кодами
  http://gurin.tomsknet.ru/gala.htm

  Сергей Гурин. Россия, Томск
  E-mail:   gurin@mail.tomsknet.ru
  HomePage: http://gurin.tomsknet.ru/gala.htm
}

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, FileUtil;

type
  TGalaProcess       = class;
  TGalaEntry         = procedure(aData: Pointer) of object;
  TGalaGuard         = function: Boolean of object;
  TGalaTheaterAction = procedure(p: TGalaProcess) of object;

  TGalaLatch = class
  private
    FLatch: TRTLCriticalSection;

  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Lock;
    procedure   Unlock;
  end;

  TGalaLog = class(TGalaLatch)
  private
    FFile:    TextFile;
    FIsOpen:  Boolean;
    FIsError: Boolean;

  public
    destructor  Destroy; override;
    procedure   Open(const aFileName: String; aAppend: Boolean = False);
    procedure   Close;
    procedure   Write(const S: String); virtual;
    property    IsOpen: Boolean read FIsOpen;
    property    IsError: Boolean read FIsError;
  end;

  TGalaSignal = class
  protected
    FHandle: THandle;

    procedure AfterWaiting(p: TGalaProcess); virtual;
  public

    constructor Create(aHandle: THandle);
    procedure   Wait(p: TGalaProcess; aTimeout: Cardinal = INFINITE); virtual;
    property    Handle: THandle read FHandle;
  end;

  TGalaProcessChannel = class
  private
    FOwner:            TGalaProcess;
    FEntry:            TGalaEntry;
    FGuard:            TGalaGuard;
    FPrev:             TGalaProcessChannel;
    FListOfProcesses:  TThreadList;
    FEventOfReadiness: THandle;

  protected
    function  Enabled: Boolean;
    function  IsReady: Boolean;
    procedure Resignal;
    function  TryOfAccept: Boolean;
    procedure Accept(aTimeout: Cardinal);
    procedure Terminate;

  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Send(aSender: TGalaProcess; aData: Pointer = nil;
                aTimeout: Cardinal = INFINITE);
    property    Entry: TGalaEntry read FEntry write FEntry;
    property    Guard: TGalaGuard read FGuard write FGuard;
  end;

  TGalaProcess = class
  private
    FHandle:           THandle;
    FThreadId:         THandle;
    FTerminationEvent: THandle;
    FGroup:            Integer;
    FParentForm:       TForm;
    FStarted:          Boolean;
    FSuspended:        Boolean;
    FTerminated:       Boolean;
    FFinished:         Boolean;
    FRendezvousEvent:  THandle;
    FRendezvousData:   Pointer;
    FRendezvousLatch:  TGalaLatch;
    FRendezvousOk:     Boolean;
    FListOfChannels:   TGalaProcessChannel;

    function  ThreadProc: Integer;
    function  GetPriority: Integer;
    procedure SetPriority(Value: Integer);

  protected
    FStackSize:        Cardinal;
    FSuspendedOnStart: Boolean;
    FFreeOnTerminate:  Boolean;

    procedure SendNotificationGalaMessage(aMessageId: Cardinal);
    procedure Execute; virtual; abstract;
    procedure OnNormalTermination; virtual;
    procedure OnPrematureTermination; virtual;
    procedure OnUnhandledException(E: Exception); virtual;

  protected  
    procedure Trace(const S: String);
    procedure Log(const S: String);
    procedure Pause(aTime: Cardinal);
    procedure Send(aMessageId: Cardinal; aData: Pointer = nil;
              aForm: TForm = nil; aTimeout: Cardinal = INFINITE);
    function  CreateChannel(aEntry: TGalaEntry; aGuard: TGalaGuard = nil):
              TGalaProcessChannel;
    procedure Accept(aChannel: TGalaProcessChannel;
              aTimeout: Cardinal = INFINITE);
    procedure AlternativeAccept(const aChannels: array of TGalaProcessChannel;
              aTimeout: Cardinal = INFINITE);
    procedure Wait(aSignal: TGalaSignal;
              aTimeout: Cardinal = INFINITE);
    function  AlternativeWait(const aSignals: array of TGalaSignal;
              aTimeout: Cardinal = INFINITE): Integer;
    procedure WaitCompletion(const aProcesses: array of TGalaProcess;
              aTimeout: Cardinal = INFINITE);

  public
    ProcessName: String;
    TraceString: String;
    ReturnValue: Integer;

    constructor Create(aGroup: Integer; aParentForm: TForm = nil);
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
    procedure   Terminate;
    procedure   Suspend;
    procedure   Resume;

    property    Handle:     THandle read  FHandle;
    property    ThreadId:   THandle read  FThreadId;
    property    Group:      Integer read  FGroup;
    property    ParentForm: TForm   read  FParentForm;
    property    Suspended:  Boolean read  FSuspended;
    property    Terminated: Boolean read  FTerminated;
    property    Finished:   Boolean read  FFinished;
    property    Priority:   Integer read  GetPriority
                                    write SetPriority;
    property    TerminationEvent: THandle read FTerminationEvent;
    property    FreeOnTerminate: Boolean read FFreeOnTerminate;
  end;

  TGalaTheater = class(TGalaLatch)
  private
    FListOfActive:       TList;
    FListOfTerminated:   TList;
    FIndexOfGroup:       Integer;
    FNotificationWindow: HWND;
    FLog:                TGalaLog;
    FCount:              Integer;

    function  GalaGetPriorityClass: Integer;
    procedure GalaSetPriorityClass(Value: Integer);

  protected
    procedure ForGroup(aAction: TGalaTheaterAction; aGroup: Integer);
    procedure ActResume(p: TGalaProcess);
    procedure ActSuspend(p: TGalaProcess);
    procedure ActTerminate(p: TGalaProcess);
    procedure ActCounting(p: TGalaProcess);

  public
    LogFileName: String;

    constructor Create;
    destructor  Destroy; override;
    function    ExeName: String;
    function    ExePath: String;
    function    GetNewGroup: Integer;
    procedure   ResumeGroup(aGroup: Integer);
    procedure   SuspendGroup(aGroup: Integer);
    procedure   TerminateAllProcesses;
    procedure   TerminateGroup(aGroup: Integer);
    function    TryToDestroyAllProcesses: Boolean;
    function    TryToDestroyGroup(aGroup: Integer): Boolean;
    procedure   DestroyAllProcesses;
    procedure   DestroyGroup(aGroup: Integer);
    procedure   Log(const S: String);
    function    ActiveCount(aGroup: Integer): Integer;
    function    AllActiveCount: Integer;
    function    AllTerminatedCount: Integer;

    property    PriorityClass: Integer read  GalaGetPriorityClass
                                       write GalaSetPriorityClass;
    property    NotificationWindow: HWND read  FNotificationWindow
                                         write FNotificationWindow;
  end;

  EGalaObjectCreationFail = class(Exception)
  public
    constructor Create;
  end;

  EGalaPrematureTermination = class(Exception)
  public
    constructor Create;
  end;

  EGalaProcessWasTerminated = class(Exception)
  public
    constructor Create;
  end;

  EGalaTimeout = class(Exception)
  public
    constructor Create;
  end;

  EGalaWaitFailed = class(Exception)
  public
    constructor Create;
  end;

  EGalaInvalidArgument = class(Exception)
  public
    constructor Create(const aArgument: String; const aClassName: String;
                const aMethod: String);
  end;

const
  GM_PROCESS_START     = WM_USER + 2000;
  GM_PROCESS_TRACE     = WM_USER + 2001;
  GM_PROCESS_TERMINATE = WM_USER + 2002;
  GM_USER              = WM_USER + 2003;

var
  GalaTheater: TGalaTheater;

implementation

resourcestring
{$IFDEF RUSSIAN_VERSION}
  SGalaObjectCreationFail        = 'Отказ создания ГалаОбъекта';
  SGalaPrematureTermination      = 'Неожиданное завершение';
  SGalaProcessWasTerminated      = 'ГалаПроцесс уже завершен';
  SGalaProcessUnhandledException = 'Необработанное исключение ГалаПроцесса';
  SGalaTimeout                   = 'Таймаут';
  SGalaWaitFailed                = 'Отказ при ожидании.';
  SGalaInvalidArgument           = 'Некорректный аргумент';
  SDebugLogError                 = 'Дисковая ошибка при формировании отладочного протокола';
{$ELSE}
  SGalaObjectCreationFail        = 'Fail of creation of the GalaObject.'#13#10;
  SGalaPrematureTermination      = 'Premature termination';
  SGalaProcessWasTerminated      = 'The GalaProcess already was terminated';
  SGalaProcessUnhandledException = 'Unhandled GalaProcess exception';
  SGalaTimeout                   = 'Timeout';
  SGalaWaitFailed                = 'Wait failed';
  SGalaInvalidArgument           = 'Invalid argument';
  SDebugLogError                 = 'Disk error on log';
{$ENDIF}

{ TGalaLatch }

constructor TGalaLatch.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLatch);
end;

destructor TGalaLatch.Destroy;
begin
  DeleteCriticalSection(FLatch);
  inherited Destroy;
end;

procedure TGalaLatch.Lock;
begin
  EnterCriticalSection(FLatch);
end;

procedure TGalaLatch.Unlock;
begin
  LeaveCriticalSection(FLatch);
end;


{ TGalaLog }

destructor TGalaLog.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TGalaLog.Open(const aFileName: String; aAppend: Boolean);
begin
  Close;
  FIsError := True;
  try
    {$I-}
    AssignFile(FFile, aFileName);
    FIsOpen := True;
    if FileExistsUTF8(aFileName) { *Преобразовано из FileExists* } and aAppend then
      Append(FFile)
    else
      Rewrite(FFile);
    {$I+}
    FIsError := IOResult <> 0;
  except
  end;
end;

procedure TGalaLog.Close;
begin
  if FIsOpen then begin
    try
      {$I-}
      CloseFile(FFile);
      {$I+}
      FIsError := IOResult <> 0;
    except
      FIsError := True;
    end;
    FIsOpen := False;
  end;
end;

procedure TGalaLog.Write(const S: String);
begin
  if FIsOpen then begin
    Lock;
    try
      System.WriteLn(FFile, S);
    except
      FIsError := True;
    end;
    Unlock;
  end;
end;


{ TGalaSignal }

constructor TGalaSignal.Create(aHandle: THandle);
begin
  inherited Create;
  FHandle := aHandle;
end;

procedure TGalaSignal.AfterWaiting(p: TGalaProcess);
begin
end;

procedure TGalaSignal.Wait(p: TGalaProcess; aTimeout: Cardinal);
var
  ResWait: DWORD;
  Obj:     array[0..1] of THandle;
begin
  Obj[0] := FHandle;
  Obj[1] := p.FTerminationEvent;
  { ожидание:
    a) сигнализирующего состояния объекта
    б) или внешнего завершения процесса
    в) или таймаута
  }
  ResWait := WaitForMultipleObjects(2, @Obj, False, aTimeout);
  if ResWait = WAIT_FAILED then
    raise EGalaWaitFailed.Create;
  if ResWait = WAIT_TIMEOUT then
    raise EGalaTimeout.Create;
  if ResWait = (WAIT_OBJECT_0 + 1) then
    raise EGalaPrematureTermination.Create;
  AfterWaiting(p);
end;


{ TGalaProcessChannel }

constructor TGalaProcessChannel.Create;
begin
  inherited Create;
  FListOfProcesses  := TThreadList.Create;
  FEventOfReadiness := CreateEvent(
    nil,
    True,  // ручная переустановка. Используется чтобы активизировать
           // одновременно все ожидающие процессы (а не один)
    False, // начальное состояние - несигнализирующее
    nil
  );
  if FEventOfReadiness = 0 then
    raise EGalaObjectCreationFail.Create;
end;

destructor TGalaProcessChannel.Destroy;
begin
  if FEventOfReadiness <> 0 then
    FileClose(FEventOfReadiness); { *Преобразовано из CloseHandle* }
  FListOfProcesses.Destroy;
  inherited Destroy;
end;

procedure TGalaProcessChannel.Send(aSender: TGalaProcess; aData: Pointer;
  aTimeout: Cardinal);
var
  Objs:    array[0..1] of THandle;
  ResWait: DWORD;
begin
  if not Assigned(aSender) then
    raise EGalaInvalidArgument.Create('aSender', ClassName, 'Send');
  if FOwner.Terminated then
    raise EGalaProcessWasTerminated.Create;
  if not Assigned(aData) then
    aData := aSender;
  aSender.FRendezvousData := aData;
  aSender.FRendezvousOk   := False;
  ResetEvent(aSender.FRendezvousEvent);
  // ставим процесс в очередь запросов на рандеву
  FListOfProcesses.Add(aSender);
  { устанавливаем сигнал готовности. Если процесс-сервер ожидает, то этот сигнал
   активизирует его }
  if Enabled then
    SetEvent(FEventOfReadiness);
  { ожидание:
    a) завершения рандеву
    б) или внешнего завершения
    в) или таймаута
  }
  Objs[0] := aSender.FRendezvousEvent;
  Objs[1] := aSender.FTerminationEvent;
  ResWait := WaitForMultipleObjects(2, @Objs, False, aTimeout);
  // убираем процесс из очереди запросов на рандеву
  FListOfProcesses.Remove(aSender);
  // если рандеву находится в процессе выполнения, то дожидаемся его завершения
  aSender.FRendezvousLatch.Lock;
  aSender.FRendezvousLatch.Unlock;
  if not aSender.FRendezvousOk then begin
    if ResWait = WAIT_FAILED then
      raise EGalaWaitFailed.Create;
    if ResWait = WAIT_TIMEOUT then
      raise EGalaTimeout.Create;
    if ResWait = (WAIT_OBJECT_0 + 1) then
      raise EGalaPrematureTermination.Create;
    if FOwner.Terminated then
      raise EGalaProcessWasTerminated.Create;
  end;
end;

// Канал разрешен, если охраняющая функция есть и она возвращает True
function TGalaProcessChannel.Enabled: Boolean;
begin
  if Assigned(Guard) then
    result := Guard
  else
    result := True;
end;

// Канал готов, если он разрешен и очередь процессов непуста
function TGalaProcessChannel.IsReady: Boolean;
begin
  result := Enabled;
  if result then begin
    result := FListOfProcesses.LockList.Count <> 0;
    FListOfProcesses.UnlockList;
  end;
end;

// Установка сигнала готовности в соответствии с действительной готовностью канала
procedure TGalaProcessChannel.Resignal;
begin
  if IsReady then
    SetEvent(FEventOfReadiness)
  else
    ResetEvent(FEventOfReadiness);
end;

function TGalaProcessChannel.TryOfAccept: Boolean;
var
  p:    TGalaProcess;
  List: TList;
begin
  result := False;
  if Enabled then begin
    p    := nil;
    List := FListOfProcesses.LockList;
    try
      // берем первый ожидающий процесс
      if List.Count <> 0 then
        p := TGalaProcess(List.Items[0]);
      { если процесс найден, то, не выходя из критической секции списка
        процессов, входим в критическую секцию рандеву. Это является гарантией,
        что процесс не выйдет из списка раньше, чем начнется рандеву
      }
      if Assigned(p) then
        p.FRendezvousLatch.Lock;
    finally
      FListOfProcesses.UnlockList;
    end;
    if Assigned(p) then
    begin
      try
        // выполняем вход
        Entry(p.FRendezvousData);
        // убираем процесс из очереди запросов на рандеву
        FListOfProcesses.Remove(p);
        p.FRendezvousOk := True;
        result          := True;
      finally
        p.FRendezvousLatch.Unlock;
      end;
      // даем сигнал продолжения ожидающему процессу
      SetEvent(p.FRendezvousEvent);
    end;
  end;
end;

procedure TGalaProcessChannel.Accept(aTimeout: Cardinal);
var
  Objs:      array[0..1] of THandle;
  ResWait:   DWORD;
  StartTime: Cardinal;

  function Timeout: Cardinal;
  var
    dt: Cardinal;
  begin
    if aTimeOut = INFINITE then
      result := INFINITE
    else begin
      dt := GetTickCount - StartTime;
      if aTimeOut > dt then
        result := aTimeout - dt
      else
        result := 1;
    end;
  end;

begin
  StartTime := GetTickCount;
  Objs[0]   := FEventOfReadiness;
  Objs[1]   := FOwner.FTerminationEvent;
  while not TryOfAccept do begin
    { ожидание:
      a) возможности захвата контейнера
      б) или внешнего завершения процесса
      в) или таймаута
    }
    Resignal;
    ResWait := WaitForMultipleObjects(2, @Objs, False, Timeout);
    if ResWait = WAIT_FAILED then
      raise EGalaWaitFailed.Create;
    if ResWait = WAIT_TIMEOUT then
      raise EGalaTimeout.Create;
    if ResWait = (WAIT_OBJECT_0 + 1) then
      raise EGalaPrematureTermination.Create;
  end;
end;

{ Эта процедура вызывается при завершении процесса, владеющего каналом,
  чтобы предотвратить последующий доступ к каналу
}
procedure TGalaProcessChannel.Terminate;
var
  i:    Integer;
  List: TList;
  p:    TGalaProcess;
begin
  List := FListOfProcesses.LockList;
  try
    if List.Count <> 0 then begin
      for i := 0 to Pred(List.Count) do begin
        p := TGalaProcess(List.Items[i]);
        if Assigned(p) then
          SetEvent(p.FRendezvousEvent);
      end;
    end;
  finally
    FListOfProcesses.UnlockList;
  end;
end;


{ TGalaProcess }

// Тело всех ГалаПроцессов (потоков)
function GalaThreadProc(p: TGalaProcess): Integer;
var
  FFreeOnTerminate: Boolean;
begin
  result := p.ThreadProc;
  p.FTerminated := True;
  // закрытие описателей
  FileClose(p.FTerminationEvent); { *Преобразовано из CloseHandle* }
  p.FTerminationEvent := 0;
  FileClose(p.FRendezvousEvent); { *Преобразовано из CloseHandle* }
  p.FRendezvousEvent := 0;
  FileClose(p.FHandle); { *Преобразовано из CloseHandle* }
  p.FHandle := 0;
  FFreeOnTerminate := p.FreeOnTerminate;
  { перемещение процесса из списка активных в список завершенных.
    Блокируются одновременно две очереди, чтобы предотвратить состояние, при
    котором процес не находится ни в одной из очередей
  }
  GalaTheater.Lock;
  try
    GalaTheater.FListOfActive.Remove(p);
    if not FFreeOnTerminate then begin
      GalaTheater.FListOfTerminated.Add(p);
      { конец выполнения потока, после операции установки FFinished можно безопасно
        удалять объект, так как поток его больше не использует
      }
      p.FFinished := True;
    end
    else begin
      // самоуничтожение процесса
      p.FFinished := True;
      p.Free;
    end;
  except
  end;
  GalaTheater.Unlock;
  EndThread(result);
end;

{ Потоковая процедура ГалаПроцесса вызывается из GalaThreadProc только в том
  случае, если ГалаПроцесс был полностью сконструирован, т.е. не
  произошло исключительной ситуации в конструкторах
}
function TGalaProcess.ThreadProc: Integer;
begin
  if FStarted and (not FFinished) then begin
    try
      SendNotificationGalaMessage(GM_PROCESS_START);
      Execute;
      Terminate;
      OnNormalTermination;
    except
      on E: EGalaPrematureTermination do begin
        Terminate;
        try
          OnPrematureTermination;
        except
          on E: Exception do
            OnUnhandledException(E);
        end;
      end;
      on E: Exception do begin
        Terminate;
        OnUnhandledException(E);
      end;
    end;
    try
      SendNotificationGalaMessage(GM_PROCESS_TERMINATE);
    except
      on E: Exception do
        OnUnhandledException(E);
    end;
  end;
  result := ReturnValue;
end;

function TGalaProcess.GetPriority: Integer;
begin
  result := THREAD_PRIORITY_NORMAL;
  if FHandle <> 0 then begin
    case GetThreadPriority(FHandle) of
      THREAD_PRIORITY_IDLE:          result := -3;
      THREAD_PRIORITY_LOWEST:        result := -2;
      THREAD_PRIORITY_BELOW_NORMAL:  result := -1;
      THREAD_PRIORITY_NORMAL:        result := 0;
      THREAD_PRIORITY_ABOVE_NORMAL:  result := 1;
      THREAD_PRIORITY_HIGHEST:       result := 2;
      THREAD_PRIORITY_TIME_CRITICAL: result := 3;
    end;
  end;
end;

procedure TGalaProcess.SetPriority(Value: Integer);
const
  pri: array[-2..2] of Integer =
  ( THREAD_PRIORITY_LOWEST,
    THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL,
    THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST
  );
begin
  if FHandle <> 0 then begin
    if Value <= -3 then
      SetThreadPriority(FHandle, THREAD_PRIORITY_IDLE)
    else if Value >= 3 then
      SetThreadPriority(FHandle, THREAD_PRIORITY_TIME_CRITICAL)
    else
      SetThreadPriority(FHandle, pri[Value]);
  end;
end;

procedure TGalaProcess.SendNotificationGalaMessage(aMessageId: Cardinal);
begin
  if GalaTheater.NotificationWindow <> 0 then
    Windows.SendMessage(GalaTheater.NotificationWindow, aMessageId, 0,
                        Integer(Self));
end;

function TGalaProcess.CreateChannel(aEntry: TGalaEntry; aGuard: TGalaGuard = nil):
  TGalaProcessChannel;
begin
  if not Assigned(aEntry) then
    raise EGalaInvalidArgument.Create('aEntry', ClassName, 'CreateChannel');
  inherited Create;
  result        := TGalaProcessChannel.Create;
  result.FOwner := self;
  result.FEntry := aEntry;
  result.FGuard := aGuard;
  // добавление канала в односвязный список каналов процесса
  result.FPrev  := FListOfChannels;
  FListOfChannels := result;
end;

procedure TGalaProcess.OnNormalTermination;
begin
end;

procedure TGalaProcess.OnPrematureTermination;
begin
end;

procedure TGalaProcess.OnUnhandledException(E: Exception);
var
  s: String;
  h: HWND;
begin
  s := GalaTheater.ExeName + '. ' + SGalaProcessUnhandledException;
  h := GalaTheater.NotificationWindow;
  if Assigned(FParentForm) then begin
    try
      h := FParentForm.Handle;
    except
    end;
  end;
  MessageBox(h, PChar(E.Message), PChar(s), MB_ICONHAND or MB_OK);
end;

procedure TGalaProcess.Trace(const S: String);
begin
  if TraceString <> S then begin
    TraceString := S;
    SendNotificationGalaMessage(GM_PROCESS_TRACE);
  end;
end;

procedure TGalaProcess.Log(const S: String);
begin
  GalaTheater.Log(S);
end;

procedure TGalaProcess.Pause(aTime: Cardinal);
begin
  if not FFinished then begin
    if aTime = 0 then
      Sleep(0)
    else begin
      // пауза с контролем внешнего завершения процесса
      WaitForSingleObject(FTerminationEvent, aTime);
      if FTerminated then
        raise EGalaPrematureTermination.Create;
    end;
  end;
end;

procedure TGalaProcess.Send(aMessageId: Cardinal; aData: Pointer;
  aForm: TForm; aTimeout: Cardinal);
var
  PRes: DWORD;
  FRes: LResult;
begin
  if not Assigned(aData) then
    aData := self;
  if not Assigned(aForm) then begin
    aForm := FParentForm;
    if not Assigned(aForm) then
      EGalaInvalidArgument.Create('aForm', ClassName, 'Send');
  end;
  if aTimeout = INFINITE then
    Windows.SendMessage(aForm.Handle, aMessageId,
      0, Integer(aData))
  else begin
    FRes := Windows.SendMessageTimeout(aForm.Handle, aMessageId,
        0, Integer(aData), SMTO_BLOCK, aTimeout, PRes);
    if FRes = 0 then
      raise EGalaTimeout.Create;
  end;
end;

procedure TGalaProcess.Accept(aChannel: TGalaProcessChannel;
  aTimeout: Cardinal = INFINITE);
begin
  aChannel.Accept(aTimeout);
end;

procedure TGalaProcess.AlternativeAccept
 (const aChannels: array of TGalaProcessChannel; aTimeout: Cardinal);
var
  h:         array[0..MAXIMUM_WAIT_OBJECTS] of THandle;
  i:         Integer;
  len:       Cardinal;
  ResWait:   Cardinal;
  StartTime: Cardinal;

  function Timeout: Cardinal;
  var
    dt: Cardinal;
  begin
    if aTimeOut = INFINITE then
      result := INFINITE
    else begin
      dt := GetTickCount - StartTime;
      if aTimeOut > dt then
        result := aTimeout - dt
      else
        result := 1;
    end;
  end;

begin
  len := Length(aChannels);
  if (len >= (MAXIMUM_WAIT_OBJECTS - 2)) or (len = 0) then
    raise EGalaInvalidArgument.Create('aChannels', ClassName, 'AlternativeAccept');
  for i := 0 to Pred(len) do
    if aChannels[i].TryOfAccept then
      Exit;
  StartTime := GetTickCount;
  for i := 0 to Pred(len) do
    h[i] := aChannels[i].FEventOfReadiness;
  h[len] := FTerminationEvent;

  repeat
    // обновление сигналов готовности для всех каналов
    for i := 0 to Pred(len) do
      aChannels[i].Resignal;
    { ожидание:
      а) готовности любого канала
      б) или внешнего завершения процесса
      в) или таймаута
    }
    ResWait := WaitForMultipleObjects(len + 1, @h, False, Timeout);
    if ResWait = WAIT_TIMEOUT then
      raise EGalaTimeout.Create;
    if ResWait = (WAIT_OBJECT_0 + len) then
      raise EGalaPrematureTermination.Create;
    if ResWait < (WAIT_OBJECT_0 + len) then
      i := ResWait - WAIT_OBJECT_0
    else if (ResWait >= WAIT_ABANDONED_0) and (ResWait < (WAIT_ABANDONED_0 + len)) then
      i := ResWait - WAIT_ABANDONED_0
    else
      raise EGalaWaitFailed.Create;
    // выполнение цикла до тех пор, пока не будет успешной попытки приема
  until aChannels[i].TryOfAccept;
end;

procedure TGalaProcess.Wait(aSignal: TGalaSignal; aTimeout: Cardinal);
begin
  aSignal.Wait(Self, aTimeout);
end;

function TGalaProcess.AlternativeWait(const aSignals: array of TGalaSignal;
  aTimeout: Cardinal): Integer;
var
  h:       array[0..MAXIMUM_WAIT_OBJECTS] of THandle;
  i:       Integer;
  len:     Cardinal;
  ResWait: Cardinal;
begin
  len := Length(aSignals);
  if (len >= (MAXIMUM_WAIT_OBJECTS - 2)) or (len = 0) then
    raise EGalaInvalidArgument.Create('aSignals', ClassName, 'AlternativeWait');
  for i := 0 to Pred(len) do
    h[i] := aSignals[i].Handle;
  h[len] := FTerminationEvent;
  { ожидание:
    а) любого сигнала
    б) или внешнего завершения процесса
    в) или таймаута
  }
  ResWait := WaitForMultipleObjects(len + 1, @h, False, aTimeout);
  if ResWait = WAIT_FAILED then
      raise EGalaWaitFailed.Create;
  if ResWait = WAIT_TIMEOUT then
    raise EGalaTimeout.Create;
  if ResWait = (WAIT_OBJECT_0 + len) then
    raise EGalaPrematureTermination.Create;
  if ResWait < (WAIT_OBJECT_0 + len) then
    result := ResWait - WAIT_OBJECT_0
  else if (ResWait >= WAIT_ABANDONED_0) and (ResWait < (WAIT_ABANDONED_0 + len)) then
    result := ResWait - WAIT_ABANDONED_0
  else
    raise EGalaWaitFailed.Create;
  aSignals[result].AfterWaiting(Self);
end;

procedure TGalaProcess.WaitCompletion(const aProcesses: array of TGalaProcess;
  aTimeout: Cardinal = INFINITE);
var
  ResWait:   DWORD;
  Obj:       array[0..1] of THandle;
  i:         Integer;
  StartTime: Cardinal;

  function Timeout: Cardinal;
  var
    dt: Cardinal;
  begin
    if aTimeOut = INFINITE then
      result := INFINITE
    else begin
      dt := GetTickCount - StartTime;
      if aTimeOut > dt then
        result := aTimeout - dt
      else
        result := 1;
    end;
  end;

begin
  StartTime := GetTickCount;
  Obj[1] := FTerminationEvent;
  for i := Low(aProcesses) to High(aProcesses) do begin
    if Assigned(aProcesses[i]) then begin
      Obj[0] := aProcesses[i].FHandle;
      if not aProcesses[i].FFinished then begin
        { ожидание:
          a) завершения очередного процесса
          б) или внешнего завершения текущего процесса
          в) или таймаута
        }
        ResWait := WaitForMultipleObjects(2, @Obj, False, Timeout);
        if ResWait = WAIT_TIMEOUT then
          raise EGalaTimeout.Create;
        if ResWait = (WAIT_OBJECT_0 + 1) then
          raise EGalaPrematureTermination.Create;
       end;
     end;
  end;
end;

constructor TGalaProcess.Create(aGroup: Integer; aParentForm: TForm);
begin
  inherited Create;
  ProcessName       := ClassName;
  FStackSize        := 0;
  FSuspendedOnStart := False;
  FSuspended        := True;
  FGroup            := aGroup;
  FParentForm       := aParentForm;
  // Событие для вывода потока из состояния ожидания при внешнем завершении
  FTerminationEvent := CreateEvent(
    nil,
    True,  // ручная переустановка
    False, // начальное состояние - несигнализирующее
    nil
  );
  if FTerminationEvent = 0 then
    raise EGalaObjectCreationFail.Create;
  // событие для ожидания рандеву
  FRendezvousEvent := CreateEvent(
    nil,
    False, // автоматическая переустановка
    False, // начальное состояние - несигнализирующее
    nil
  );
  if FRendezvousEvent = 0 then
    raise EGalaObjectCreationFail.Create;
  FRendezvousLatch := TGalaLatch.Create;
end;

{ Этот метод вызывается автоматически после выполнения всех конструкторов.
  До этого момента объект не считается полностью сконструированным. Процедура
  добавляет ГалаПроцесс в список процессов ГалаТеатра и стартует поток
}
procedure TGalaProcess.AfterConstruction;
begin
  FHandle := BeginThread(nil, FStackSize, @GalaThreadProc, Self,
             CREATE_SUSPENDED, FThreadId);
  if FHandle = 0 then
    raise EGalaObjectCreationFail.Create;
  // добавление ГалаПроцесса в список активных процессов
  GalaTheater.Lock;
  try
    GalaTheater.FListOfActive.Add(Self);
  finally
    GalaTheater.Unlock;
  end;
  FStarted := True;
  { ГалаПроцесс внесен в очередь и установлен флаг старта, теперь действительно
    можно стартовать поток, если не установлен флаг SuspendedOnStart
  }
  if not FSuspendedOnStart then
    Resume;
end;

destructor TGalaProcess.Destroy;
var
  chan, prev: TGalaProcessChannel;
begin
  // если в этой точке FHandle <> 0, то это аварийная ситуация
  if (FHandle <> 0) then begin
    ReturnValue := 1;
    TerminateThread(FHandle, ReturnValue);
    FileClose(FHandle); { *Преобразовано из CloseHandle* }
  end;
  chan := FListOfChannels;
  while Assigned(chan) do begin
    prev := chan.FPrev;
    chan.Free;
    chan := prev;
  end;
  if FTerminationEvent <> 0 then
    FileClose(FTerminationEvent); { *Преобразовано из CloseHandle* }
  if FRendezvousEvent <> 0 then
    FileClose(FRendezvousEvent); { *Преобразовано из CloseHandle* }
  FRendezvousLatch.Free;
  inherited Destroy;
end;

procedure TGalaProcess.Terminate;
var
  chan: TGalaProcessChannel;
begin
  if not FTerminated then begin
    // запускаем процесс, если он был приостановлен
    Resume;
    FTerminated := True;
    { посылаем уведомляющее событие, которое позволит завершить процесс,
      находящийся в состоянии ожидания
    }
    if FTerminationEvent <> 0 then
      SetEvent(FTerminationEvent);
    // посылаем всем каналам уведомление о завершении
    chan := FListOfChannels;
    while Assigned(chan) do begin
      chan.Terminate;
      chan := chan.FPrev;
    end;
  end;
end;

procedure TGalaProcess.Suspend;
begin
  if (not FSuspended) and (not FTerminated) then begin
    FSuspended := True;
    SuspendThread(FHandle);
  end
end;

procedure TGalaProcess.Resume;
begin
  if FSuspended and (not FTerminated) then begin
    FSuspended := False;
    ResumeThread(FHandle);
  end;
end;


{ TGalaTheater }

function TGalaTheater.GalaGetPriorityClass: Integer;
begin
  result := GetPriorityClass(GetCurrentProcess);
end;

procedure TGalaTheater.GalaSetPriorityClass(Value: Integer);
begin
  SetPriorityClass(GetCurrentProcess, Value);
end;

// Выполнение действия для всех активных процессов данной группы
procedure TGalaTheater.ForGroup(aAction: TGalaTheaterAction; aGroup: Integer);
var
  i: Integer;
  p: TGalaProcess;
begin
  Lock;
  try
    if FListOfActive.Count <> 0 then begin
      for i := 0 to Pred(FListOfActive.Count) do begin
        p := TGalaProcess(FListOfActive[i]);
        if Assigned(p) then begin
          if p.Group = aGroup then
            aAction(p);
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TGalaTheater.ActResume(p: TGalaProcess);
begin
  p.Resume;
end;

procedure TGalaTheater.ActSuspend(p: TGalaProcess);
begin
  p.Suspend;
end;

procedure TGalaTheater.ActTerminate(p: TGalaProcess);
begin
  p.Terminate;
end;

procedure TGalaTheater.ActCounting(p: TGalaProcess);
begin
  Inc(FCount);
end;

constructor TGalaTheater.Create;
begin
  inherited Create;
  FListOfActive     := TList.Create; // список активных ГалаПроцессов
  FListOfTerminated := TList.Create; // список завершенных ГалаПроцессов
  FLog        := TGalaLog.Create;
  LogFileName := AnsiLower(PChar(ChangeFileExt(Application.ExeName, '.log')));
  GalaTheater := Self;
end;

destructor TGalaTheater.Destroy;
begin
  FNotificationWindow := 0;
  DestroyAllProcesses;
  FListOfActive.Free;
  FListOfTerminated.Free;
  if Assigned(FLog) then begin
    FLog.Close;
    if FLog.IsError then
      MessageBox(NotificationWindow, PChar(SDebugLogError), PChar(ExeName),
        MB_OK or MB_ICONEXCLAMATION);
    FLog.Free;
  end;
  GalaTheater := nil;
  inherited Destroy;
end;

function TGalaTheater.ExeName: String;
begin
  result := ChangeFileExt(ExtractFileName(Application.ExeName), '');
  if Length(result) > 1 then
    AnsiLower(PChar(result) + 1);
end;

function TGalaTheater.ExePath: String;
begin
  result := ExtractFilePath(Application.ExeName);
end;

function TGalaTheater.GetNewGroup: Integer;
begin
  result := InterlockedIncrement(FIndexOfGroup);
end;

procedure TGalaTheater.ResumeGroup(aGroup: Integer);
begin
  ForGroup(ActResume, aGroup);
end;

procedure TGalaTheater.SuspendGroup(aGroup: Integer);
begin
  ForGroup(ActSuspend, aGroup);
end;

procedure TGalaTheater.TerminateAllProcesses;
var
  i: Integer;
  p: TGalaProcess;
begin
  Lock;
  try
    if FListOfActive.Count <> 0 then begin
      for i := 0 to Pred(FListOfActive.Count) do begin
        p := TGalaProcess(FListOfActive[i]);
        if Assigned(p) then
          p.Terminate;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TGalaTheater.TerminateGroup(aGroup: Integer);
begin
  ForGroup(ActTerminate, aGroup);
end;

function TGalaTheater.TryToDestroyAllProcesses: Boolean;
var
  i: Integer;
  p: TGalaProcess;
begin
  TerminateAllProcesses;
  Lock;
  try
    result := FListOfActive.Count = 0;
    if result then begin
      if FListOfTerminated.Count <> 0 then begin
        for i := 0 to Pred(FListOfTerminated.Count) do begin
          p := TGalaProcess(FListOfTerminated[i]);
          if Assigned(p) then begin
            if p.Finished then begin
              p.Free;
              FListOfTerminated[i] := nil;
            end
            else
              result := False;
          end;
        end;
        FListOfTerminated.Pack;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TGalaTheater.TryToDestroyGroup(aGroup: Integer): Boolean;
var
  i: Integer;
  p: TGalaProcess;
begin
  TerminateGroup(aGroup);
  FCount := 0;
  ForGroup(ActCounting, aGroup);
  result := FCount = 0;
  if result then begin
    Lock;
    try
      if FListOfTerminated.Count <> 0 then begin
        for i := 0 to Pred(FListOfTerminated.Count) do begin
          p := TGalaProcess(FListOfTerminated[i]);
          if Assigned(p) then begin
            if p.Group = aGroup then begin
              if p.Finished then begin
                p.Free;
                FListOfTerminated[i] := nil;
              end
              else
                result := False;
            end;
          end;
        end;
        FListOfTerminated.Pack;
      end;
    finally
      Unlock;
    end;
  end;
end;

procedure TGalaTheater.DestroyAllProcesses;
begin
  while not TryToDestroyAllProcesses do begin
    Application.ProcessMessages;
    Sleep(50);
  end;
end;

procedure TGalaTheater.DestroyGroup(aGroup: Integer);
begin
  while not TryToDestroyGroup(aGroup) do begin
    Application.ProcessMessages;
    Sleep(50);
  end;
end;

procedure TGalaTheater.Log(const S: String);
begin
  if not FLog.IsError then begin
    if not FLog.IsOpen then begin
      Lock;
      try
        if not FLog.IsOpen then
          FLog.Open(LogFileName);
      finally
        Unlock;
      end;
    end;
    FLog.Write(S);
  end;
end;

function TGalaTheater.ActiveCount(aGroup: Integer): Integer;
begin
  FCount := 0;
  ForGroup(ActCounting, aGroup);
  result := FCount;
end;

function TGalaTheater.AllActiveCount: Integer;
begin
  result := FListOfActive.Count;
end;

function TGalaTheater.AllTerminatedCount: Integer;
begin
  result := FListOfTerminated.Count;
end;

{ EGalaObjectCreationFail }

constructor EGalaObjectCreationFail.Create;
begin
  inherited Create(SGalaObjectCreationFail + '.'#13#10 +
    SysErrorMessage(GetLastError));
end;

{ EGalaPrematureTermination }

constructor EGalaPrematureTermination.Create;
begin
  inherited Create(SGalaPrematureTermination);
end;

{ EGalaProcessWasTerminated }

constructor EGalaProcessWasTerminated.Create;
begin
  inherited Create(SGalaProcessWasTerminated);
end;

{ EGalaTimeout }

constructor EGalaTimeout.Create;
begin
  inherited Create(SGalaTimeout);
end;

{ EGalaWaitFailed }

constructor EGalaWaitFailed.Create;
begin
  inherited Create(SGalaWaitFailed + '.'#13#10 + SysErrorMessage(GetLastError));
end;

{ EGalaInvalidArgument }

constructor EGalaInvalidArgument.Create(const aArgument: String;
  const aClassName: String; const aMethod: String);
begin
  inherited Create('Class: ' + aClassName + #13#10 +
                   'Method: ' + aMethod + #13#10 +
                   SGalaInvalidArgument + ': ' + aArgument);
end;

end.



