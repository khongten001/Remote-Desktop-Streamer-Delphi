unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  ncSources, ncsocketlist, video_decoder_vpx, PanelDisplay, Math;

type

  PKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;

  KBDLLHOOKSTRUCT = record
    vkCode: DWORD; // Virtual key code of the key
    scanCode: DWORD; // Hardware scan code
    flags: DWORD; // Key event flags (extended key, context code, etc.)
    time: DWORD; // Timestamp of the event
    dwExtraInfo: ULONG_PTR; // Additional platform-specific information
  end;

type
  TCursorType = (IDC_APPSTARTING = 32650, IDC_ARROW = 32512, IDC_CROSS = 32515,
    IDC_HAND = 32649, IDC_HELP = 32651, IDC_IBEAM = 32513, IDC_ICON = 32641,
    IDC_NO = 32648, IDC_SIZE = 32640, IDC_SIZEALL = 32646, IDC_SIZENESW = 32643,
    IDC_SIZENS = 32645, IDC_SIZENWSE = 32642, IDC_SIZEWE = 32644,
    IDC_UPARROW = 32516, IDC_WAIT = 32514);

type
  TForm1 = class(TForm)
    lblSelectMonitor: TLabel;
    cmbSelectMonitor: TComboBox;
    btnCapture: TButton;
    cbMouseControl: TCheckBox;
    ncServerSource1: TncServerSource;
    Image1: TImage;
    Panel1: TPanel;
    cmdDisplayMode: TComboBox;
    lblDisplayMode: TLabel;
    gbControl: TGroupBox;
    cbKeyboardControl: TCheckBox;
    procedure ncServerSource1Connected(Sender: TObject; aLine: TncLine);
    procedure ncServerSource1Disconnected(Sender: TObject; aLine: TncLine);
    function ncServerSource1HandleCommand(Sender: TObject; aLine: TncLine;
      aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
      const aSenderComponent, aReceiverComponent: string): TBytes;
    procedure btnCaptureClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdateScreenshot(const aData: TBytes);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DisplayPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DisplayPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DisplayPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DisplayPanelMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint; var Handled: Boolean);
    procedure DisplayPanelMouseEnter(Sender: TObject);
    procedure DisplayPanelMouseLeave(Sender: TObject);
    procedure cbKeyboardControlClick(Sender: TObject);
    procedure cbMouseControlClick(Sender: TObject);
    procedure cmdDisplayModeChange(Sender: TObject);

  protected
    procedure WMEnterSizeMove(var Message: TMessage); message WM_ENTERSIZEMOVE;
    procedure WMExitSizeMove(var Message: TMessage); message WM_EXITSIZEMOVE;
    procedure CreateParams(var Params: TCreateParams); override;

  private
    { Private declarations }
    FStream: TMemoryStream;
    Decoder: TVpxDecoder;
    FSizeMoving: Boolean;

    DisplayPanel: TDisplayPanel;
    FSourceWidth: Integer;  // Width of the original image
    FSourceHeight: Integer; // Height of the original image

    FActiveButtons: set of TMouseButton; // Track currently pressed buttons
    FDraggingOutsideImage: Boolean;     // Track if we're dragging outside image
    FLastValidX, FLastValidY: Integer;  // Last valid coordinates inside image

    // Keyboard hook for remote control
    FHookHandle: HHOOK; // Handle to keyboard hook

    FDefaultCursor: TCursor; // Store the default cursor
    FPanelCursor: TCursor;   // Store the panel cursor

    // Keyboard hook management
    procedure StartHook; // Install keyboard hook for remote control
    procedure StopHook; // Remove keyboard hook

    function CalculateSourceCoordinates(X, Y: Integer): TPoint;
    procedure SendMouseCommand(const Command: string; X, Y: Integer;Button: TMouseButton = mbLeft);
    procedure SetupMouseHandlers;
    procedure SendCommandToAllClients(const Command: string; const Params: string = '');

  public
    { Public declarations }
    function IsPointInDisplayPanel(Pt: TPoint): Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function LowLevelKeyboardProc(nCode: Integer; wParam: wParam; lParam: lParam): LRESULT; stdcall;
var
  KeyboardStruct: PKBDLLHOOKSTRUCT; // Pointer to keyboard event structure
  KeyEventType: string; // Type of key event (up/down)
  ForegroundWindow: HWND;
  CursorPos: TPoint;
  ClientPoint: TPoint;
begin
  // Skip processing if hook code is negative (Windows requirement)
  if nCode < 0 then
    Exit(CallNextHookEx(Form1.FHookHandle, nCode, wParam, lParam));

  // Check if our form is active and keyboard control is enabled
  ForegroundWindow := GetForegroundWindow;

  if (ForegroundWindow = Form1.Handle) and Form1.cbKeyboardControl.Checked then
  begin
    // Get current cursor position
    GetCursorPos(CursorPos);

    // Convert to form's client coordinates
    ClientPoint := Form1.ScreenToClient(CursorPos);

    // Check if cursor is over the display panel area
    if Form1.IsPointInDisplayPanel(ClientPoint) then
    begin
      // Access the keyboard event structure
      KeyboardStruct := PKBDLLHOOKSTRUCT(lParam);

      // Determine the event type based on Windows message
      case wParam of
        WM_KEYDOWN, WM_SYSKEYDOWN:
          KeyEventType := 'KeyDown';
        WM_KEYUP, WM_SYSKEYUP:
          KeyEventType := 'KeyUp';
      else
        // Ignore other event types
        Exit(CallNextHookEx(Form1.FHookHandle, nCode, wParam, lParam));
      end;

      // Format keyboard command and send to all clients
      Form1.SendCommandToAllClients('KeyEvent',
        KeyEventType + '|' +
        IntToStr(KeyboardStruct^.vkCode) + '|' +
        IntToStr(KeyboardStruct^.scanCode) + '|' +
        IntToStr(KeyboardStruct^.flags));

      // Return 1 to prevent the keystroke from being processed locally
      // This ensures keystrokes only affect the remote system
      Exit(1);
    end;
  end;

  // If no interception occurred, pass the event to the next hook
  Result := CallNextHookEx(Form1.FHookHandle, nCode, wParam, lParam);
end;

procedure SetCursorFromType(aCursorType: TCursorType);
var
  CursorValue: TCursor;
  CursorPos: TPoint;
  ClientPoint: TPoint;
  IsInImage: Boolean;
begin
  // Map cursor type to VCL cursor
  case aCursorType of
    IDC_APPSTARTING:  CursorValue := crAppStart;
    IDC_ARROW:        CursorValue := crArrow;
    IDC_CROSS:        CursorValue := crCross;
    IDC_HAND:         CursorValue := crHandPoint;
    IDC_HELP:         CursorValue := crHelp;
    IDC_IBEAM:        CursorValue := crIBeam;
    IDC_ICON:         CursorValue := crCross;
    IDC_NO:           CursorValue := crNo;
    IDC_SIZENESW:     CursorValue := crSizeNESW;
    IDC_SIZENS:       CursorValue := crSizeNS;
    IDC_SIZENWSE:     CursorValue := crSizeNWSE;
    IDC_SIZEWE:       CursorValue := crSizeWE;
    IDC_UPARROW:      CursorValue := crUpArrow;
    IDC_WAIT:         CursorValue := crHourGlass;
    IDC_SIZE,
    IDC_SIZEALL:      CursorValue := crSizeAll;
  else
    CursorValue := crDefault;
  end;

  // Store the panel cursor
  Form1.FPanelCursor := CursorValue;

  // Only apply the cursor if mouse control is enabled
  if Form1.cbMouseControl.Checked then
  begin
    // Check if mouse is currently over the panel
    GetCursorPos(CursorPos);
    ClientPoint := Form1.ScreenToClient(CursorPos);

    // Check if cursor is in the actual image area
    IsInImage := Form1.IsPointInDisplayPanel(ClientPoint);

    if IsInImage then
    begin
      // If mouse is over the image, update the cursor immediately
      if Assigned(Form1.DisplayPanel) then
        Form1.DisplayPanel.Cursor := CursorValue;

      // Also directly set the screen cursor for immediate effect
      Screen.Cursor := CursorValue;
    end
    else
    begin
      // If outside image area, use default cursor
      if Assigned(Form1.DisplayPanel) then
        Form1.DisplayPanel.Cursor := Form1.FDefaultCursor;
      Screen.Cursor := Form1.FDefaultCursor;
    end;
  end;
end;

procedure TForm1.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or CS_HREDRAW or CS_VREDRAW;
end;

procedure TForm1.WMEnterSizeMove(var Message: TMessage);
begin
  // Set flag indicating we're sizing/moving
  FSizeMoving := True;
end;

procedure TForm1.WMExitSizeMove(var Message: TMessage);
begin
  // Clear sizing/moving flag
  FSizeMoving := False;

  // Force a refresh of the display panel
  if Assigned(DisplayPanel) then
    DisplayPanel.Invalidate;
end;

function TForm1.IsPointInDisplayPanel(Pt: TPoint): Boolean;
var
  ImageRect: TRect;
  AspectRatio: Double;
  NewWidth, NewHeight: Integer;
begin
  // First, check if DisplayPanel exists
  if not Assigned(DisplayPanel) then
    Exit(False);

  // If source dimensions are invalid, disallow interaction
  if (FSourceWidth <= 0) or (FSourceHeight <= 0) then
    Exit(False);

  // Calculate the actual image rectangle based on scale mode
  case DisplayPanel.ScaleMode of
    smStretch:
      begin
        // In stretch mode, image fills the entire panel
        ImageRect := Rect(DisplayPanel.Left, DisplayPanel.Top,
                         DisplayPanel.Left + DisplayPanel.Width,
                         DisplayPanel.Top + DisplayPanel.Height);
      end;

    smCenter:
      begin
        // In center mode, calculate where image is displayed
        ImageRect.Left := DisplayPanel.Left + (DisplayPanel.Width - FSourceWidth) div 2;
        ImageRect.Top := DisplayPanel.Top + (DisplayPanel.Height - FSourceHeight) div 2;
        ImageRect.Right := ImageRect.Left + FSourceWidth;
        ImageRect.Bottom := ImageRect.Top + FSourceHeight;
      end;

    smFit:
      begin
        // In fit mode, calculate scaled dimensions with aspect ratio
        AspectRatio := FSourceWidth / FSourceHeight;
        NewWidth := DisplayPanel.Width;
        NewHeight := Round(DisplayPanel.Width / AspectRatio);

        if NewHeight > DisplayPanel.Height then
        begin
          NewHeight := DisplayPanel.Height;
          NewWidth := Round(DisplayPanel.Height * AspectRatio);
        end;

        ImageRect.Left := DisplayPanel.Left + (DisplayPanel.Width - NewWidth) div 2;
        ImageRect.Top := DisplayPanel.Top + (DisplayPanel.Height - NewHeight) div 2;
        ImageRect.Right := ImageRect.Left + NewWidth;
        ImageRect.Bottom := ImageRect.Top + NewHeight;
      end;
  else
    Exit(False); // Unknown scale mode
  end;

  // Check if the point is inside the calculated image rectangle
  Result := PtInRect(ImageRect, Pt);
end;

// Helper function to send commands to all connected clients
procedure TForm1.SendCommandToAllClients(const Command: string; const Params: string = '');
var
  clients: TSocketList;
  I: Integer;
  CommandStr: string;
begin
  // Format the command string
  if Params <> '' then
    CommandStr := Command + '|' + Params
  else
    CommandStr := Command + '|';

  // Lock the client list to safely access it
  clients := ncServerSource1.Lines.LockList;
  try
    // Send the command to each connected client
    for I := 0 to clients.Count - 1 do
    begin
      ncServerSource1.ExecCommand(clients.Lines[I], 0,
        BytesOf(CommandStr), False);
    end;
  finally
    // Always unlock the client list
    ncServerSource1.Lines.UnlockList;
  end;
end;

procedure TForm1.StartHook;
begin
  if FHookHandle = 0 then
  begin
    FHookHandle := SetWindowsHookEx(WH_KEYBOARD_LL, @LowLevelKeyboardProc,
      HInstance, 0);
  end;
end;

procedure TForm1.StopHook;
begin
  if FHookHandle <> 0 then
  begin
    UnhookWindowsHookEx(FHookHandle);
    FHookHandle := 0;
  end;
end;

// Calculate the coordinates in the source image based on the display panel coordinates
function TForm1.CalculateSourceCoordinates(X, Y: Integer): TPoint;
var
  ScaleX, ScaleY: Double;
  R: TRect;
  AspectRatio: Double;
  NewWidth, NewHeight: Integer;
begin
  // Default values
  Result.X := X;
  Result.Y := Y;

  // Check if we have valid dimensions
  if (FSourceWidth <= 0) or (FSourceHeight <= 0) then
    Exit;

  // Calculate based on scale mode
  case DisplayPanel.ScaleMode of
    smStretch:
      begin
        // Simple scaling for stretch mode
        ScaleX := FSourceWidth / DisplayPanel.Width;
        ScaleY := FSourceHeight / DisplayPanel.Height;
        Result.X := Round(X * ScaleX);
        Result.Y := Round(Y * ScaleY);
      end;

    smCenter:
      begin
        // Center mode - need to account for the offset
        R.Left := (DisplayPanel.Width - FSourceWidth) div 2;
        R.Top := (DisplayPanel.Height - FSourceHeight) div 2;

        // Check if the click is within the image area
        if (X >= R.Left) and (X < R.Left + FSourceWidth) and
           (Y >= R.Top) and (Y < R.Top + FSourceHeight) then
        begin
          Result.X := X - R.Left;
          Result.Y := Y - R.Top;
        end
        else
        begin
          // Click outside the image area - clip to edges
          Result.X := Max(0, Min(FSourceWidth - 1, X - R.Left));
          Result.Y := Max(0, Min(FSourceHeight - 1, Y - R.Top));
        end;
      end;

    smFit:
      begin
        // Fit mode - maintain aspect ratio
        AspectRatio := FSourceWidth / FSourceHeight;
        NewWidth := DisplayPanel.Width;
        NewHeight := Round(DisplayPanel.Width / AspectRatio);

        if NewHeight > DisplayPanel.Height then
        begin
          NewHeight := DisplayPanel.Height;
          NewWidth := Round(DisplayPanel.Height * AspectRatio);
        end;

        R.Left := (DisplayPanel.Width - NewWidth) div 2;
        R.Top := (DisplayPanel.Height - NewHeight) div 2;

        // Check if the click is within the image area
        if (X >= R.Left) and (X < R.Left + NewWidth) and
           (Y >= R.Top) and (Y < R.Top + NewHeight) then
        begin
          // Calculate the scaled coordinates
          ScaleX := FSourceWidth / NewWidth;
          ScaleY := FSourceHeight / NewHeight;
          Result.X := Round((X - R.Left) * ScaleX);
          Result.Y := Round((Y - R.Top) * ScaleY);
        end
        else
        begin
          // Click outside the image area - clip to edges
          if X < R.Left then
            Result.X := 0
          else if X >= R.Left + NewWidth then
            Result.X := FSourceWidth - 1
          else
            Result.X := Round((X - R.Left) * FSourceWidth / NewWidth);

          if Y < R.Top then
            Result.Y := 0
          else if Y >= R.Top + NewHeight then
            Result.Y := FSourceHeight - 1
          else
            Result.Y := Round((Y - R.Top) * FSourceHeight / NewHeight);
        end;
      end;
  end;

  // Ensure coordinates are within bounds
  Result.X := Max(0, Min(FSourceWidth - 1, Result.X));
  Result.Y := Max(0, Min(FSourceHeight - 1, Result.Y));
end;

procedure TForm1.cbMouseControlClick(Sender: TObject);
var
  CursorPos: TPoint;
  ClientPoint: TPoint;
begin
  // Check if mouse is currently over the panel
  GetCursorPos(CursorPos);
  ClientPoint := ScreenToClient(CursorPos);

  if IsPointInDisplayPanel(ClientPoint) then
  begin
    // Update cursor based on checkbox state
    if cbMouseControl.Checked then
      Screen.Cursor := FPanelCursor
    else
      Screen.Cursor := FDefaultCursor;
  end;
end;

procedure TForm1.cbKeyboardControlClick(Sender: TObject);
begin
  if cbKeyboardControl.Checked then
  begin
    StartHook;
  end
  else
  begin
    StopHook;
  end;
end;

// Update the method signature to accept a TMouseButton parameter
procedure TForm1.SendMouseCommand(const Command: string; X, Y: Integer;
  Button: TMouseButton = mbLeft);
var
  clients: TSocketList;
  I: Integer;
  ButtonStr: string;
  CommandStr: string;
begin
  // Handle wheel command differently
  if Command = 'Wheel' then
  begin
    CommandStr := Format('Wheel|%d', [X]); // Simplified wheel format
  end
  else
  begin
    // Convert mouse button to string only once in this method
    case Button of
      mbLeft: ButtonStr := 'Left';
      mbRight: ButtonStr := 'Right';
      mbMiddle: ButtonStr := 'Middle';
    else
      ButtonStr := 'Left'; // Default
    end;

    // Format command correctly for client
    CommandStr := Format('Mouse|%s|%s|%d|%d', [Command, ButtonStr, X, Y]);
  end;

  // Lock the client list to safely access it
  clients := ncServerSource1.Lines.LockList;
  try
    // Send the command to each connected client
    for I := 0 to clients.Count - 1 do
    begin
      ncServerSource1.ExecCommand(clients.Lines[I], 0,
        BytesOf(CommandStr), False);
    end;
  finally
    // Always unlock the client list
    ncServerSource1.Lines.UnlockList;
  end;
end;

procedure TForm1.DisplayPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  SourcePoint: TPoint;
  ClientPoint: TPoint;
  IsInImage: Boolean;
begin
  // Only process if mouse control is enabled
  if not cbMouseControl.Checked then
    Exit;

  // Convert DisplayPanel coordinates to form client coordinates
  ClientPoint := Point(X + DisplayPanel.Left, Y + DisplayPanel.Top);

  // Check if the point is within the actual image area
  IsInImage := IsPointInDisplayPanel(ClientPoint);

  // Check if we're in an active drag operation
  if FActiveButtons <> [] then
  begin
    if IsInImage then
    begin
      // Inside image during drag - update last valid position
      FLastValidX := X;
      FLastValidY := Y;
      FDraggingOutsideImage := False;

      // Send move command
      SourcePoint := CalculateSourceCoordinates(X, Y);
      SendMouseCommand('Move', SourcePoint.X, SourcePoint.Y);
    end
    else
    begin
      // Outside image during drag - mark as dragging outside
      FDraggingOutsideImage := True;

      // Don't send move commands while outside
    end;
  end
  else if IsInImage then
  begin
    // Normal move inside image (no drag)
    SourcePoint := CalculateSourceCoordinates(X, Y);
    SendMouseCommand('Move', SourcePoint.X, SourcePoint.Y);
  end;

  // Update cursor based on whether we're in the image area
  if IsInImage then
  begin
    // Inside image - use panel cursor
    if Assigned(DisplayPanel) then
      DisplayPanel.Cursor := FPanelCursor;
    Screen.Cursor := FPanelCursor;
  end
  else
  begin
    // Outside image - use default cursor
    if Assigned(DisplayPanel) then
      DisplayPanel.Cursor := FDefaultCursor;
    Screen.Cursor := FDefaultCursor;
  end;
end;

procedure TForm1.DisplayPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  SourcePoint: TPoint;
  ClientPoint: TPoint;
begin
  // Only send mouse commands if mouse control is enabled
  if not cbMouseControl.Checked then
    Exit;

  // Convert DisplayPanel coordinates to form client coordinates
  ClientPoint := Point(X + DisplayPanel.Left, Y + DisplayPanel.Top);

  // Check if the point is within the actual image area
  if not IsPointInDisplayPanel(ClientPoint) then
  begin
    // Reset cursor to default if outside image
    if Assigned(DisplayPanel) then
      DisplayPanel.Cursor := FDefaultCursor;
    Screen.Cursor := FDefaultCursor;
    Exit;
  end;

  // Track this button as active
  Include(FActiveButtons, Button);

  // Store last valid coordinates inside image
  FLastValidX := X;
  FLastValidY := Y;
  FDraggingOutsideImage := False;

  SourcePoint := CalculateSourceCoordinates(X, Y);
  SendMouseCommand('Down', SourcePoint.X, SourcePoint.Y, Button);
end;

procedure TForm1.DisplayPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  SourcePoint: TPoint;
  ClientPoint: TPoint;
  IsInImage: Boolean;
begin
  // Only process if mouse control is enabled
  if not cbMouseControl.Checked then
    Exit;

  // Check if this button was active
  if not (Button in FActiveButtons) then
    Exit;

  // Convert DisplayPanel coordinates to form client coordinates
  ClientPoint := Point(X + DisplayPanel.Left, Y + DisplayPanel.Top);

  // Check if the point is within the actual image area
  IsInImage := IsPointInDisplayPanel(ClientPoint);

  // If we've been dragging outside and now releasing, use last valid coordinates
  if FDraggingOutsideImage and not IsInImage then
  begin
    SourcePoint := CalculateSourceCoordinates(FLastValidX, FLastValidY);
  end
  else
  begin
    SourcePoint := CalculateSourceCoordinates(X, Y);
  end;

  // Send the mouse up event
  SendMouseCommand('Up', SourcePoint.X, SourcePoint.Y, Button);

  // Remove this button from active set
  Exclude(FActiveButtons, Button);

  // If no more buttons pressed, reset dragging flag
  if FActiveButtons = [] then
    FDraggingOutsideImage := False;

  // Update cursor based on position
  if not IsInImage then
  begin
    if Assigned(DisplayPanel) then
      DisplayPanel.Cursor := FDefaultCursor;
    Screen.Cursor := FDefaultCursor;
  end;
end;

procedure TForm1.DisplayPanelMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // Only send mouse commands if mouse control is enabled
  if not cbMouseControl.Checked then
    Exit;

  // MousePos is already in form client coordinates

  // Check if the point is within the actual image area
  if not IsPointInDisplayPanel(MousePos) then
  begin
    // Reset cursor to default if outside image
    if Assigned(DisplayPanel) then
      DisplayPanel.Cursor := FDefaultCursor;
    Screen.Cursor := FDefaultCursor;
    Exit;
  end;

  SendMouseCommand('Wheel', WheelDelta, 0);
  Handled := True;
end;

procedure TForm1.DisplayPanelMouseEnter(Sender: TObject);
var
  CursorPos: TPoint;
  ClientPoint: TPoint;
  IsInImage: Boolean;
begin
  // Only handle cursor changes if DisplayPanel exists
  if not Assigned(DisplayPanel) then
    Exit;

  // Get current cursor position
  GetCursorPos(CursorPos);
  ClientPoint := ScreenToClient(CursorPos);

  // Check if cursor is in the actual image area
  IsInImage := IsPointInDisplayPanel(ClientPoint);

  if IsInImage then
  begin
    if cbMouseControl.Checked then
    begin
      // Use the panel cursor when mouse control is enabled and over image
      DisplayPanel.Cursor := FPanelCursor;
      Screen.Cursor := FPanelCursor;
    end
    else
    begin
      // Use the default cursor when mouse control is disabled
      DisplayPanel.Cursor := FDefaultCursor;
      Screen.Cursor := FDefaultCursor;
    end;
  end
  else
  begin
    // Use default cursor when outside image area
    DisplayPanel.Cursor := FDefaultCursor;
    Screen.Cursor := FDefaultCursor;
  end;
end;

procedure TForm1.DisplayPanelMouseLeave(Sender: TObject);
begin
  // When mouse leaves panel, reset cursor
  if Assigned(DisplayPanel) then
  begin
    DisplayPanel.Cursor := FDefaultCursor;
    Screen.Cursor := FDefaultCursor;
  end;

  // Do NOT release mouse buttons automatically
  // Just mark that we're dragging outside
  if (FActiveButtons <> []) and (cbMouseControl.Checked) then
  begin
    FDraggingOutsideImage := True;
  end;
end;

procedure TForm1.SetupMouseHandlers;
begin
  // Set up mouse event handlers based on the checkbox
  DisplayPanel.OnMouseMove := DisplayPanelMouseMove;
  DisplayPanel.OnMouseDown := DisplayPanelMouseDown;
  DisplayPanel.OnMouseUp := DisplayPanelMouseUp;
  DisplayPanel.OnMouseWheel := DisplayPanelMouseWheel;
  DisplayPanel.OnMouseEnter := DisplayPanelMouseEnter;
  DisplayPanel.OnMouseLeave := DisplayPanelMouseLeave;
end;

procedure TForm1.cmdDisplayModeChange(Sender: TObject);
begin
  if not Assigned(DisplayPanel) then
    Exit;

  case cmdDisplayMode.ItemIndex of
    0: DisplayPanel.ScaleMode := smStretch;  // Stretch
    1: DisplayPanel.ScaleMode := smCenter;   // Center
    2: DisplayPanel.ScaleMode := smFit;      // Fit to Window
  end;

  // Force a refresh of the panel
  DisplayPanel.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  PanelTop: Integer;
begin
  self.ncServerSource1.Port := 3434;
  self.ncServerSource1.Active := True;

  FStream := TMemoryStream.Create;
  Decoder := TVpxDecoder.Create;
  FSizeMoving := False;

  // Store the default cursor
  FDefaultCursor := crDefault;
  FPanelCursor := crDefault;

  // Calculate panel position to replace the Image control
  Image1.Visible := False;
  PanelTop := Image1.Top;

  // Initialize source dimensions
  FSourceWidth := 0;
  FSourceHeight := 0;

  // Create and configure display panel
  DisplayPanel := TDisplayPanel.Create(Self);
  DisplayPanel.Parent := Self;
  DisplayPanel.Left := Image1.Left;
  DisplayPanel.Top := PanelTop;
  DisplayPanel.Width := Image1.Width;
  DisplayPanel.Height := Image1.Height;
  DisplayPanel.Anchors := Image1.Anchors;
  DisplayPanel.ScaleMode := smCenter; //smStretch, smFit, smCenter

  // Initialize the display mode combo box
  cmdDisplayMode.Items.Clear;
  cmdDisplayMode.Items.Add('Stretch');
  cmdDisplayMode.Items.Add('Center');
  cmdDisplayMode.Items.Add('Fit to Window');

  // Set the default mode to match the initial panel mode
  cmdDisplayMode.ItemIndex := 1; // Index 1 corresponds to smCenter

  // Connect the event handler
  cmdDisplayMode.OnChange := cmdDisplayModeChange;


  // Set up mouse handlers
  SetupMouseHandlers();

  // Initialize mouse tracking variables
  FActiveButtons := [];
  FDraggingOutsideImage := False;
  FLastValidX := 0;
  FLastValidY := 0;

  // Initialize keyboard hook handle as inactive
  FHookHandle := 0;

  // Enable double buffering on form
  Self.DoubleBuffered := True;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SendCommandToAllClients('StopStream');
  ncServerSource1.Active := False;

  // Remove keyboard hook from system to prevent callback issues after termination
  StopHook;

  // Free resources
  if Assigned(DisplayPanel) then
    FreeAndNil(DisplayPanel);
  if Assigned(Decoder) then
    FreeAndNil(Decoder);
  if Assigned(FStream) then
    FreeAndNil(FStream);
end;

procedure TForm1.btnCaptureClick(Sender: TObject);
var
  MonitorId: Integer;
begin
  if btnCapture.Caption = 'Start Desktop Capture' then
  begin
    btnCapture.Caption := 'Stop Desktop Capture';

    // Get the monitor ID from the selected item's object
    if cmbSelectMonitor.ItemIndex >= 0 then
      MonitorId := Integer(cmbSelectMonitor.Items.Objects[cmbSelectMonitor.ItemIndex])
    else
      MonitorId := 0; // Default to first monitor if nothing is selected

    SendCommandToAllClients('StartStream', IntToStr(MonitorId));
  end
  else
  begin
    btnCapture.Caption := 'Start Desktop Capture';
    SendCommandToAllClients('StopStream');

    // Reset cursor to default when stopping the stream
    FPanelCursor := FDefaultCursor;

    // If mouse is currently over the panel, update the cursor immediately
    if Assigned(DisplayPanel) then
    begin
      DisplayPanel.Cursor := FDefaultCursor;

      // Check if mouse is over the panel
      var CursorPos: TPoint;
      var ClientPoint: TPoint;
      GetCursorPos(CursorPos);
      ClientPoint := ScreenToClient(CursorPos);

      if IsPointInDisplayPanel(ClientPoint) then
        Screen.Cursor := FDefaultCursor;
    end;
  end;
end;

procedure TForm1.ncServerSource1Connected(Sender: TObject; aLine: TncLine);
begin
  // Update the form's caption to indicate a successful connection
  self.Caption := 'Remote Desktop Streamer | <Connected!>';
  self.btnCapture.Enabled := True;
  self.cbMouseControl.Enabled := True;
  self.cbKeyboardControl.Enabled := True;

  // btnCapture starts disabled until we get monitor information
  self.btnCapture.Enabled := False;
end;

procedure TForm1.ncServerSource1Disconnected(Sender: TObject; aLine: TncLine);
begin
  // Update the form's caption to indicate disconnection
  self.Caption := 'Remote Desktop Streamer | <Not Connected>';
  self.btnCapture.Enabled := False;
  self.cbMouseControl.Enabled := False;
  self.cbKeyboardControl.Enabled := False;

  // reset button text back to Start Desktop Capture
  self.btnCapture.Caption := 'Start Desktop Capture';
end;

procedure TForm1.UpdateScreenshot(const aData: TBytes);
begin
  // Skip if we're in the middle of resize/move
  if FSizeMoving then
    Exit;

  TThread.Synchronize(nil, procedure
  var
    DecodedBitmap: TBitmap;
    OldWidth, OldHeight: Integer;
  begin
    try
      // Remember old dimensions to detect changes
      OldWidth := FSourceWidth;
      OldHeight := FSourceHeight;

      // Use existing class-level FStream
      FStream.Clear;
      FStream.Write(aData[0], Length(aData));
      FStream.Position := 0;

      // Decode the frame
      DecodedBitmap := Decoder.Decode(FStream);

      if Assigned(DecodedBitmap) and Assigned(DisplayPanel) then
      begin
        // Update source dimensions
        FSourceWidth := DecodedBitmap.Width;
        FSourceHeight := DecodedBitmap.Height;

        // Update the display panel with the bitmap data
        DisplayPanel.UpdateFrame(
          DecodedBitmap.ScanLine[DecodedBitmap.Height - 1],
          DecodedBitmap.Width,
          DecodedBitmap.Height
        );

        // Free the bitmap after using it
        DecodedBitmap.Free;
      end;
    except
      // Silently handle exceptions
      if Assigned(DecodedBitmap) then
        DecodedBitmap.Free;
    end;
  end);
end;

function TForm1.ncServerSource1HandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
var
  Command: string;
  sl: TStringList;
  i, j: Integer;
  MonitorCount: Integer;
  MonitorId, MonitorName, MonitorWidth, MonitorHeight, MonitorOffsetX, MonitorOffsetY: string;
  StreamData: TBytes;
  CursorTypeName: string;
  CurrentIdx: Integer;

begin
  sl := TStringList.Create;
  Command := stringof(aData);
  try
    sl.Delimiter := '|';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Command;

    if (sl.Count > 0) then
    begin
      if sl[0] = 'Monitors' then
      begin
        MonitorCount := StrToInt(sl[1]);
        cmbSelectMonitor.Items.Clear;

        // Clear existing tag objects if any
        for i := 0 to cmbSelectMonitor.Items.Count - 1 do
        begin
          if cmbSelectMonitor.Items.Objects[i] <> nil then
            cmbSelectMonitor.Items.Objects[i] := nil;
        end;

        // Process the monitor information from the client
        j := 2; // Start index for monitor details (after count)

        for i := 0 to MonitorCount - 1 do
        begin
          if j + 5 < sl.Count then // Ensure we have enough elements for all monitor properties
          begin
            MonitorId := sl[j];       // Monitor ID
            MonitorName := sl[j+1];   // Monitor Name
            MonitorWidth := sl[j+2];  // Width
            MonitorHeight := sl[j+3]; // Height
            MonitorOffsetX := sl[j+4]; // X Offset
            MonitorOffsetY := sl[j+5]; // Y Offset

            // Store the monitor ID as the object for the ComboBox item
            cmbSelectMonitor.Items.AddObject(
              Format('Monitor %s: %s (%sx%s)',
                [MonitorId, MonitorName, MonitorWidth, MonitorHeight]),
              TObject(StrToInt(MonitorId))
            );

            j := j + 6; // Move to the next monitor's data
          end;
        end;

        // Select the first monitor by default if available
        if cmbSelectMonitor.Items.Count > 0 then
        begin
          cmbSelectMonitor.ItemIndex := 0;
          // Only enable the capture button if we have monitors
          btnCapture.Enabled := True;
        end
        else
        begin
          // No monitors available, keep the capture button disabled
          btnCapture.Enabled := False;
        end;
      end;

      if sl[0] = 'Stream' then
      begin
        // Extract the stream data from the command
        StreamData := Copy(aData, 7, Length(aData));
        UpdateScreenshot(StreamData);
      end;

      if sl[0] = 'Cursor' then
      begin
        CursorTypeName := sl[1];
        SetCursorFromType(TCursorType(StrToInt(sl[1])));
      end;

    end;
  finally
    sl.Free;
  end;

  Result := nil;
end;

end.
