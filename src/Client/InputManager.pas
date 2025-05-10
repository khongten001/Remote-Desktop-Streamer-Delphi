unit InputManager;

interface

uses
  Winapi.Windows, System.SysUtils, System.UITypes, System.SyncObjs;

const
  MOUSEEVENTF_ABSOLUTE = $8000;
  MOUSEEVENTF_MOVE = $0001;
  MOUSEEVENTF_LEFTDOWN = $0002;
  MOUSEEVENTF_LEFTUP = $0004;
  MOUSEEVENTF_RIGHTDOWN = $0008;
  MOUSEEVENTF_RIGHTUP = $0010;
  MOUSEEVENTF_MIDDLEDOWN = $0020;
  MOUSEEVENTF_MIDDLEUP = $0040;
  MOUSEEVENTF_WHEEL = $0800;

type
  TMouseButtons = (mbLeft, mbRight, mbMiddle);

  TMouseButtonEvent = record
    Button: TMouseButtons;
    Pressed: Boolean;
  end;

  TMouseScrollEvent = record
    Offset: Integer;
  end;

  TMousePositionOffsetEvent = record
    X, Y: Integer;
  end;

  TMousePositionAbsoluteEvent = record
    X, Y: Integer;
    RemoteWidth, RemoteHeight: Integer;
  end;

  TInputManager = class
  private
    FLock: TCriticalSection;
    FStreamActive: Boolean;
    procedure SetStreamActive(Value: Boolean);
    function GetStreamActive: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // Simple keyboard method
    procedure SendKeyboardInput(VKCode: Word; ScanCode: Word;
      Flags: DWORD; IsKeyUp: Boolean);

    // Mouse input methods
    procedure SendInput(const MouseEvt: TMouseButtonEvent); overload;
    procedure SendInput(const ScrollEvt: TMouseScrollEvent); overload;
    procedure SendInput(const PosOffsetEvt: TMousePositionOffsetEvent); overload;
    procedure SendInput(const PosAbsoluteEvt: TMousePositionAbsoluteEvent); overload;

    property StreamActive: Boolean read GetStreamActive write SetStreamActive;
  end;

implementation

constructor TInputManager.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FStreamActive := False;
end;

destructor TInputManager.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TInputManager.SetStreamActive(Value: Boolean);
begin
  FLock.Enter;
  try
    FStreamActive := Value;
  finally
    FLock.Leave;
  end;
end;

function TInputManager.GetStreamActive: Boolean;
begin
  FLock.Enter;
  try
    Result := FStreamActive;
  finally
    FLock.Leave;
  end;
end;

// Simple keyboard input method
procedure TInputManager.SendKeyboardInput(VKCode: Word; ScanCode: Word;
  Flags: DWORD; IsKeyUp: Boolean);
var
  Input: TInput;
begin
  if not GetStreamActive then
    Exit;

  ZeroMemory(@Input, SizeOf(TInput));
  Input.Itype := INPUT_KEYBOARD;
  Input.ki.wVk := VKCode;
  Input.ki.wScan := ScanCode;

  if IsKeyUp then
    Input.ki.dwFlags := KEYEVENTF_KEYUP
  else
    Input.ki.dwFlags := 0;

  if (Flags and $01) <> 0 then
    Input.ki.dwFlags := Input.ki.dwFlags or KEYEVENTF_EXTENDEDKEY;

  Winapi.Windows.SendInput(1, Input, SizeOf(TInput));
end;

procedure TInputManager.SendInput(const MouseEvt: TMouseButtonEvent);
var
  Inputs: array [0 .. 0] of TInput;
begin
  if not GetStreamActive then
    Exit;

  ZeroMemory(@Inputs, SizeOf(Inputs));
  Inputs[0].Itype := INPUT_MOUSE;
  case MouseEvt.Button of
    mbLeft:
      if MouseEvt.Pressed then
        Inputs[0].mi.dwFlags := MOUSEEVENTF_LEFTDOWN
      else
        Inputs[0].mi.dwFlags := MOUSEEVENTF_LEFTUP;
    mbRight:
      if MouseEvt.Pressed then
        Inputs[0].mi.dwFlags := MOUSEEVENTF_RIGHTDOWN
      else
        Inputs[0].mi.dwFlags := MOUSEEVENTF_RIGHTUP;
    mbMiddle:
      if MouseEvt.Pressed then
        Inputs[0].mi.dwFlags := MOUSEEVENTF_MIDDLEDOWN
      else
        Inputs[0].mi.dwFlags := MOUSEEVENTF_MIDDLEUP;
  end;
  Winapi.Windows.SendInput(1, Inputs[0], SizeOf(TInput));
end;

procedure TInputManager.SendInput(const ScrollEvt: TMouseScrollEvent);
var
  Input: TInput;
begin
  if not GetStreamActive then
    Exit;

  ZeroMemory(@Input, SizeOf(Input));
  Input.Itype := INPUT_MOUSE;
  Input.mi.dwFlags := MOUSEEVENTF_WHEEL;
  Input.mi.mouseData := ScrollEvt.Offset;

  if Winapi.Windows.SendInput(1, Input, SizeOf(TInput)) <> 1 then
    raise Exception.CreateFmt('SendInput failed with error: %d',
      [GetLastError]);
end;

procedure TInputManager.SendInput(const PosOffsetEvt
  : TMousePositionOffsetEvent);
var
  Input: TInput;
begin
  if not GetStreamActive then
    Exit;

  ZeroMemory(@Input, SizeOf(Input));
  Input.Itype := INPUT_MOUSE;
  Input.mi.dwFlags := MOUSEEVENTF_MOVE;
  Input.mi.dx := PosOffsetEvt.X;
  Input.mi.dy := PosOffsetEvt.Y;

  if Winapi.Windows.SendInput(1, Input, SizeOf(TInput)) <> 1 then
    raise Exception.CreateFmt('SendInput failed with error: %d',
      [GetLastError]);
end;

procedure TInputManager.SendInput(const PosAbsoluteEvt
  : TMousePositionAbsoluteEvent);
var
  Input: TInput;
  NormalizedX, NormalizedY: Integer;
begin
  if not GetStreamActive then
    Exit;

  if (PosAbsoluteEvt.RemoteWidth <= 0) or (PosAbsoluteEvt.RemoteHeight <= 0)
  then
    Exit;

  // Convert to normalized coordinates (0-65535)
  NormalizedX := (PosAbsoluteEvt.X * 65535) div PosAbsoluteEvt.RemoteWidth;
  NormalizedY := (PosAbsoluteEvt.Y * 65535) div PosAbsoluteEvt.RemoteHeight;

  ZeroMemory(@Input, SizeOf(Input));
  Input.Itype := INPUT_MOUSE;
  Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE;
  Input.mi.dx := NormalizedX;
  Input.mi.dy := NormalizedY;

  if Winapi.Windows.SendInput(1, Input, SizeOf(TInput)) <> 1 then
    raise Exception.CreateFmt('SendInput failed with error: %d',
      [GetLastError]);
end;

end.
