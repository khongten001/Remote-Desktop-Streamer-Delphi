unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  ncSources, Vcl.ExtCtrls, System.Threading, System.IOUtils, Vcl.StdCtrls,
  System.Math, System.Generics.Collections,
  libyuv, video_decoder_vpx, video_encoder_vpx, vp8cx, vp8dx,
  VPXRegionEncoder, vpx_codec, vpx_decoder, vpx_encoder, vpx_image,
  InputManager, GDIFrameProcessor, DXGIFrameProcessor, MonitorUtils,
  ScreenCaptureTypes;

type
  TCursorType = (IDC_APPSTARTING = 32650, IDC_ARROW = 32512, IDC_CROSS = 32515,
    IDC_HAND = 32649, IDC_HELP = 32651, IDC_IBEAM = 32513, IDC_ICON = 32641,
    IDC_NO = 32648, IDC_SIZE = 32640, IDC_SIZEALL = 32646, IDC_SIZENESW = 32643,
    IDC_SIZENS = 32645, IDC_SIZENWSE = 32642, IDC_SIZEWE = 32644,
    IDC_UPARROW = 32516, IDC_WAIT = 32514);

type
  TCursorInfo = record
    cbSize: DWORD;
    flags: DWORD;
    HCURSOR: HCURSOR;
    ptScreenPos: TPoint;
  end;

type
  TForm1 = class(TForm)
    ncClientSource1: TncClientSource;
    ConnectionTimer: TTimer;
    messagesLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ncClientSource1Connected(Sender: TObject; aLine: TncLine);
    procedure ncClientSource1Disconnected(Sender: TObject; aLine: TncLine);
    procedure ncClientSource1Reconnected(Sender: TObject; aLine: TncLine);
    procedure ConnectionTimerTimer(Sender: TObject);
    function ncClientSource1HandleCommand(Sender: TObject; aLine: TncLine;
      aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
      const aSenderComponent, aReceiverComponent: string): TBytes;
    procedure ncClientSource1AsyncExecCommandResult(Sender: TObject;
      aLine: TncLine; aCmd: Integer; const aResult: TBytes;
      aResultIsError: Boolean; const aSenderComponent,
      aReceiverComponent: string);
    procedure FormDestroy(Sender: TObject);
    procedure Display(p_sender: String; p_message: string);
    function GetNow(): String;

  private
    { Private declarations }
    FGDIProcessor: TGDIFrameProcessor;
    FDXGIProcessor: TDXFrameProcessor;

    FMonitors: TList<ScreenCaptureTypes.TMonitor>;
    FSelectedMonitor: ScreenCaptureTypes.TMonitor;
    FThreadData: TThreadData;
    FDesktopBitmap: TBitmap;

    // FPS tracking
    FLastFPSUpdate: Cardinal;
    FFramesSinceLastUpdate: Integer;
    FFPS: Double;

    // VPX Encoder for screen capture
    Encoder: TVpxEncoder;
    CaptureActive: Boolean;
    FrameCount: Integer;

    InputManager: TInputManager;
    CurrentMonitorIndex: Integer;
    FCursors: TDictionary<TCursorType, HCURSOR>;
    FOldCursor: HCURSOR;
    StreamingThread: TThread;

    procedure UpdateFPS;
    procedure LoadMonitors;

    procedure OnGDIFrameReceived(const Frame: TFrame; const Monitor: TMonitor);
    procedure StartGDICapture(MonitorId: Integer);
    procedure StopGDI;
    procedure OnDXGIFrameReceived(const Frame: TFrame; const Monitor: TMonitor);
    procedure StartDXGICapture(MonitorId: Integer);
    procedure StopDXGI;

    procedure StartStreaming(MonitorIndex: Integer);
    procedure StopStreaming;

    procedure SetupConnection;
    procedure CleanupResources;

  public
    ShouldStream: Boolean; // Flag to control streaming
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Display(p_sender: String; p_message: string);
var
  Sender: String;
  Message: String;
begin
  Sender := p_sender;
  Message := p_message;

  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          messagesLog.Lines.Add('[' + Sender + '] - ' + GetNow() + ': ' + Message);
        end);
    end).Start;
end;

function TForm1.GetNow(): String;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
end;

procedure SendMonitorInfo(ClientSocket: TncClientSource;
  Monitors: TList<ScreenCaptureTypes.TMonitor>);
var
  MonitorInfo: string;
  Monitor: ScreenCaptureTypes.TMonitor;
begin
  if Monitors.Count > 0 then
  begin
    MonitorInfo := IntToStr(Monitors.Count);
    for var i := 0 to Monitors.Count - 1 do
    begin
      Monitor := Monitors[i];
      MonitorInfo := MonitorInfo + '|' + IntToStr(Monitor.Id) +
                     '|' + Monitor.Name +
                     '|' + IntToStr(Monitor.Width) +
                     '|' + IntToStr(Monitor.Height) +
                     '|' + IntToStr(Monitor.OffsetX) +
                     '|' + IntToStr(Monitor.OffsetY);
    end;
    ClientSocket.ExecCommand(0, BytesOf('Monitors|' + MonitorInfo), False);
  end
  else
  begin
    ClientSocket.ExecCommand(0, BytesOf('Monitors|0'), False);
  end;
end;

// Converts a TMemoryStream to a byte array (TBytes)
function MemoryStreamToBytes(M: TMemoryStream): TBytes;
begin
  SetLength(Result, M.Size);
  // Set the length of the result array to match the stream size
  if M.Size > 0 then
    Move(M.Memory^, Result[0], M.Size);
  // Copy the stream's memory to the byte array
end;

function GetCursorInfo(var pci: TCursorInfo): BOOL; stdcall;
  external 'user32.dll';

function CursorTypeToString(CursorType: TCursorType): string;
begin
  case CursorType of
    IDC_APPSTARTING:
      Result := 'IDC_APPSTARTING';
    IDC_ARROW:
      Result := 'IDC_ARROW';
    IDC_CROSS:
      Result := 'IDC_CROSS';
    IDC_HAND:
      Result := 'IDC_HAND';
    IDC_HELP:
      Result := 'IDC_HELP';
    IDC_IBEAM:
      Result := 'IDC_IBEAM';
    IDC_ICON:
      Result := 'IDC_ICON';
    IDC_NO:
      Result := 'IDC_NO';
    IDC_SIZE:
      Result := 'IDC_SIZE';
    IDC_SIZEALL:
      Result := 'IDC_SIZEALL';
    IDC_SIZENESW:
      Result := 'IDC_SIZENESW';
    IDC_SIZENS:
      Result := 'IDC_SIZENS';
    IDC_SIZENWSE:
      Result := 'IDC_SIZENWSE';
    IDC_SIZEWE:
      Result := 'IDC_SIZEWE';
    IDC_UPARROW:
      Result := 'IDC_UPARROW';
    IDC_WAIT:
      Result := 'IDC_WAIT';
  else
    Result := 'Unknown';
  end;
end;

function InitializeCursors: TDictionary<TCursorType, HCURSOR>;
var
  CursorType: TCursorType;
  CursorHandle: HCURSOR;
  CursorDict: TDictionary<TCursorType, HCURSOR>;
begin
  CursorDict := TDictionary<TCursorType, HCURSOR>.Create;

  try
    for CursorType := Low(TCursorType) to High(TCursorType) do
    begin
      // Load cursor resource using the integer value of CursorType
      CursorHandle := LoadCursor(0, MakeIntResource(Integer(CursorType)));
      if CursorHandle <> 0 then
      begin
        // Add CursorType and HCURSOR to dictionary
        CursorDict.Add(CursorType, CursorHandle);
      end;
    end;

    Result := CursorDict;
  except
    CursorDict.Free;
    raise;
  end;
end;

function GetGlobalMouseCursorIconHandle: HCURSOR;
var
  cursorInfo: TCursorInfo;
begin
  cursorInfo.cbSize := SizeOf(TCursorInfo);
  if GetCursorInfo(cursorInfo) then
    Result := cursorInfo.HCURSOR
  else
  begin
    Result := 0;
  end;
end;

procedure MonitorCursorChanges;
var
  CurrentCursor: HCURSOR;
  CursorTypeValue: Integer;
  CursorType: TCursorType;
begin
  if Form1.FOldCursor = 0 then
    Form1.FOldCursor := GetGlobalMouseCursorIconHandle;

  CurrentCursor := GetGlobalMouseCursorIconHandle;

  if (CurrentCursor <> 0) and (CurrentCursor <> Form1.FOldCursor) then
  begin
    // Find the CursorType by matching the current cursor handle
    for CursorType in Form1.FCursors.Keys do
    begin
      if Form1.FCursors[CursorType] = CurrentCursor then
      begin
        CursorTypeValue := Integer(CursorType);
        // Get the integer value of the CursorType
        Break;
      end;
    end;

    // Send the integer value (e.g., 32649 for IDC_HAND)
    if Form1.ncClientSource1.Active then
      Form1.ncClientSource1.ExecCommand(0,
        BytesOf('Cursor|' + IntToStr(CursorTypeValue)), False);

    Form1.FOldCursor := CurrentCursor; // Update the old cursor reference
  end;
end;

// Monitoring cursor changes using the form's cursor dictionary
procedure StartMonitoringCursorChanges;
begin
  try
    MonitorCursorChanges;
  except
    on E: Exception do
      Form1.Display('Cursor', 'Monitoring error: ' + E.Message);
  end;
end;

procedure TForm1.UpdateFPS;
var
  CurrentTime: Cardinal;
  TimeDelta: Cardinal;
begin
  CurrentTime := GetTickCount;
  TimeDelta := CurrentTime - FLastFPSUpdate;

  // Update FPS every second
  if TimeDelta >= 1000 then
  begin
    FFPS := (FFramesSinceLastUpdate * 1000) / TimeDelta;
    FFramesSinceLastUpdate := 0;
    FLastFPSUpdate := CurrentTime;

    Display('Stream', Format('FPS: %.1f', [FFPS]));
  end;
end;

procedure TForm1.LoadMonitors;
begin
  // Free existing monitors list if it exists
  if Assigned(FMonitors) then
  begin
    for var Monitor in FMonitors do
      Monitor.Free;
    FMonitors.Free;
  end;

  // Get the list of monitors using the method from the second attachment
  FMonitors := GetMonitors;

  // Output monitor information
  if FMonitors.Count > 0 then
  begin
    Display('Monitors', Format('Found %d monitors', [FMonitors.Count]));
    for var i := 0 to FMonitors.Count - 1 do
    begin
      var Monitor := FMonitors[i];
      Display('Monitor ' + IntToStr(i), Format('ID: %d', [Monitor.Id]));
      Display('Monitor ' + IntToStr(i), Format('Name: %s', [Monitor.Name]));
      Display('Monitor ' + IntToStr(i), Format('Resolution: %dx%d',
        [Monitor.Width, Monitor.Height]));
      Display('Monitor ' + IntToStr(i), Format('Position: (%d,%d)',
        [Monitor.OffsetX, Monitor.OffsetY]));
      Display('Monitor ' + IntToStr(i), Format('Output: %d', [Monitor.Output]));
      Display('Monitor ' + IntToStr(i), Format('Scaling: %.2f', [Monitor.Scaling]));
    end;
  end
  else
  begin
    Display('Monitors', 'No monitors found');
  end;
end;

procedure TForm1.OnGDIFrameReceived(const Frame: TFrame; const Monitor: TMonitor);
var
  Y: Integer;
  FrameBitmap: TBitmap;
  DstRow, SrcRow: PByte;
  RowWidth: Integer;
  DesktopStream: TMemoryStream;
begin
  // Skip if stream is no longer active
  if not ShouldStream or not Assigned(ncClientSource1) or not ncClientSource1.Active then
    Exit;

  // Create bitmap from the frame data
  FrameBitmap := TBitmap.Create;
  try
    FrameBitmap.SetSize(Frame.Bounds.Right - Frame.Bounds.Left,
      Frame.Bounds.Bottom - Frame.Bounds.Top);
    FrameBitmap.PixelFormat := pf32bit;

    // Fast row copy
    RowWidth := (Frame.Bounds.Right - Frame.Bounds.Left) * SizeOf(TFrameBGRA);
    for Y := 0 to FrameBitmap.Height - 1 do
    begin
      DstRow := FrameBitmap.ScanLine[Y];
      SrcRow := PByte(Frame.Data) + (Y * Frame.RowStrideInBytes);
      Move(SrcRow^, DstRow^, RowWidth);
    end;

    // Update the frame counter for FPS calculation
    Inc(FFramesSinceLastUpdate);

    // Encode and send the bitmap
    DesktopStream := Encoder.Encode(FrameBitmap);
    try
      if (DesktopStream <> nil) and (DesktopStream.Size > 0) and
         Assigned(ncClientSource1) and ncClientSource1.Active then
      begin
        DesktopStream.Position := 0;
        ncClientSource1.ExecCommand(0, BytesOf('Stream|') + MemoryStreamToBytes(DesktopStream), False);

        // Update FPS calculation
        UpdateFPS;
      end;
    finally
      DesktopStream.Free;
    end;
  finally
    FrameBitmap.Free;
  end;
end;

procedure TForm1.StartGDICapture(MonitorId: Integer);
var
  SelectedMonitor: ScreenCaptureTypes.TMonitor;
  MonitorFound: Boolean;
begin
  if not CaptureActive then
  begin
    try
      // Find the monitor with the matching ID
      MonitorFound := False;
      for var Monitor in FMonitors do
      begin
        if Monitor.Id = MonitorId then
        begin
          SelectedMonitor := Monitor;
          FSelectedMonitor := SelectedMonitor;
          MonitorFound := True;
          break;
        end;
      end;

      // Exit if no matching monitor was found
      if not MonitorFound then
      begin
        Display('Stream', Format('Monitor with ID %d not found', [MonitorId]));
        Exit;
      end;

      // Make sure we have a clean encoder state
      if not Assigned(Encoder) then
        Encoder := TVpxEncoder.Create(vctVP8);

      // Reset frame counting
      FrameCount := 0;

      // Initialize ThreadData and GDIProcessor
      FThreadData := TThreadData.Create;
      try
        // Set up the callback for full screen GDI frames
        FThreadData.ScreenCaptureData.OnNewFrameGDI:= OnGDIFrameReceived;

        // Create a new processor for the selected monitor
        FGDIProcessor := TGDIFrameProcessor.Create;
        try
          if FGDIProcessor.Init(FThreadData, SelectedMonitor) <> DUPL_RETURN_SUCCESS then
          begin
            Display('Stream', Format('Failed to initialize the GDI processor for monitor %d.', [MonitorId]));
            Exit;
          end;

          // Reset FPS tracking
          FLastFPSUpdate := GetTickCount;
          FFramesSinceLastUpdate := 0;
          FFPS := 0;

          CaptureActive := True;
          ShouldStream := True;
          Display('Stream', Format('Started streaming monitor %d (%s): %dx%d at (%d,%d)',
            [SelectedMonitor.Id, SelectedMonitor.Name, SelectedMonitor.Width,
             SelectedMonitor.Height, SelectedMonitor.OffsetX, SelectedMonitor.OffsetY]));

          // Main streaming loop
          while (Assigned(ncClientSource1)) and (ncClientSource1.Active) and CaptureActive and ShouldStream do
          begin
            try
              // Process cursor changes
              MonitorCursorChanges;

              // Process a frame - this will trigger the callback if new frame is available
              if Assigned(FGDIProcessor) then
              begin
                FGDIProcessor.ProcessFrame(SelectedMonitor);
              end
              else
              begin
                Display('Stream', 'GDI processor was unexpectedly nil during streaming.');
                Break;
              end;

              // Allow UI to remain responsive
              Application.ProcessMessages;

              // Small delay to prevent CPU overload
              Sleep(10);
            except
              on E: Exception do
              begin
                Display('Stream Error ', E.Message);
                // Add a slightly longer delay after an error
                Sleep(100);
              end;
            end;
          end;

        except
          on E: Exception do
          begin
            Display('Stream', 'Error initializing GDIProcessor: ' + E.Message);
            FreeAndNil(FGDIProcessor);
            raise;
          end;
        end;

      except
        on E: Exception do
        begin
          Display('Stream', 'Error initializing ThreadData: ' + E.Message);
          FreeAndNil(FThreadData);
          raise;
        end;
      end;

      finally

      end;
  end;
end;

procedure TForm1.StopGDI;
begin
  // Set flags to stop the capture loop
  CaptureActive := False;

  // Wait for the capture loop to finish
  while CaptureActive do
    Application.ProcessMessages;

  // Clean up GDIProcessor
  if Assigned(FGDIProcessor) then
  begin
    try
      FreeAndNil(FGDIProcessor);
    except
      on E: Exception do
        Display('Stream', 'Error cleaning up GDIProcessor: ' + E.Message);
    end;
  end;

  // Clean up ThreadData
  if Assigned(FThreadData) then
  begin
    try
      FreeAndNil(FThreadData);
    except
      on E: Exception do
        Display('Stream', 'Error cleaning up ThreadData: ' + E.Message);
    end;
  end;

  // Reset encoder state
  if Assigned(Encoder) then
  begin
    try
      FreeAndNil(Encoder);
      Encoder := TVpxEncoder.Create(vctVP8);
    except
      on E: Exception do
        Display('Stream', 'Error resetting encoder: ' + E.Message);
    end;
  end;

  // Reset frame count
  FrameCount := 0;

  // Reset FPS tracking
  FLastFPSUpdate := 0;
  FFramesSinceLastUpdate := 0;
  FFPS := 0;

  Display('Stream', 'GDI streaming stopped');
end;

procedure TForm1.OnDXGIFrameReceived(const Frame: TFrame; const Monitor: TMonitor);
var
  Y: Integer;
  FrameBitmap: TBitmap;
  DstRow, SrcRow: PByte;
  RowWidth: Integer;
  DesktopStream: TMemoryStream;
begin
  // Skip if stream is no longer active
  if not ShouldStream or not Assigned(ncClientSource1) or not ncClientSource1.Active then
    Exit;

  // Create bitmap from the frame data
  FrameBitmap := TBitmap.Create;
  try
    FrameBitmap.SetSize(Frame.Bounds.Right - Frame.Bounds.Left,
      Frame.Bounds.Bottom - Frame.Bounds.Top);
    FrameBitmap.PixelFormat := pf32bit;

    // Fast row copy
    RowWidth := (Frame.Bounds.Right - Frame.Bounds.Left) * SizeOf(TFrameBGRA);
    for Y := 0 to FrameBitmap.Height - 1 do
    begin
      DstRow := FrameBitmap.ScanLine[Y];
      SrcRow := PByte(Frame.Data) + (Y * Frame.RowStrideInBytes);
      Move(SrcRow^, DstRow^, RowWidth);
    end;

    // Update the frame counter for FPS calculation
    Inc(FFramesSinceLastUpdate);

    // Encode and send the bitmap
    DesktopStream := Encoder.Encode(FrameBitmap);
    try
      if (DesktopStream <> nil) and (DesktopStream.Size > 0) and
         Assigned(ncClientSource1) and ncClientSource1.Active then
      begin
        DesktopStream.Position := 0;
        ncClientSource1.ExecCommand(0, BytesOf('Stream|') + MemoryStreamToBytes(DesktopStream), False);

        // Update FPS calculation
        UpdateFPS;
      end;
    finally
      DesktopStream.Free;
    end;
  finally
    FrameBitmap.Free;
  end;
end;

 procedure TForm1.StartDXGICapture(MonitorId: Integer);
var
  SelectedMonitor: ScreenCaptureTypes.TMonitor;
  MonitorFound: Boolean;
begin
  if not CaptureActive then
  begin
    try
      // Find the monitor with the matching ID
      MonitorFound := False;
      for var Monitor in FMonitors do
      begin
        if Monitor.Id = MonitorId then
        begin
          SelectedMonitor := Monitor;
          FSelectedMonitor := SelectedMonitor;
          MonitorFound := True;
          break;
        end;
      end;

      // Exit if no matching monitor was found
      if not MonitorFound then
      begin
        Display('Stream', Format('Monitor with ID %d not found', [MonitorId]));
        Exit;
      end;

      // Make sure we have a clean encoder state
      if not Assigned(Encoder) then
        Encoder := TVpxEncoder.Create(vctVP8);

      // Reset frame counting
      FrameCount := 0;

      // Initialize ThreadData and DXGIProcessor
      FThreadData := TThreadData.Create;
      try
        // Set up the callback for full screen DXGI frames
        FThreadData.ScreenCaptureData.OnNewFrameDXGI := OnDXGIFrameReceived;

        // Create a new processor for the selected monitor
        FDXGIProcessor := TDXFrameProcessor.Create;
        try
          // This is the key part - check if DXGI initialization succeeds
          if FDXGIProcessor.Init(FThreadData, SelectedMonitor) <> DUPL_RETURN_SUCCESS then
          begin
            Display('Stream', Format('Failed to initialize the DXGI processor for monitor %d.', [MonitorId]));

            // Clean up resources before exiting
            FreeAndNil(FDXGIProcessor);
            FreeAndNil(FThreadData);

            // Exit without setting CaptureActive or starting any streaming
            Exit;
          end;

          // If we reach here, DXGI initialization was successful
          // Reset FPS tracking
          FLastFPSUpdate := GetTickCount;
          FFramesSinceLastUpdate := 0;
          FFPS := 0;

          CaptureActive := True;
          ShouldStream := True;
          Display('Stream', Format('Started streaming monitor %d (%s): %dx%d at (%d,%d)',
            [SelectedMonitor.Id, SelectedMonitor.Name, SelectedMonitor.Width,
             SelectedMonitor.Height, SelectedMonitor.OffsetX, SelectedMonitor.OffsetY]));

          // Main streaming loop
          while (Assigned(ncClientSource1)) and (ncClientSource1.Active) and CaptureActive and ShouldStream do
          begin
            try
              // Process cursor changes
              MonitorCursorChanges;

              // Process a frame - this will trigger the callback if new frame is available
              if Assigned(FDXGIProcessor) then
              begin
                FDXGIProcessor.ProcessFrame(SelectedMonitor);
              end
              else
              begin
                Display('Stream', 'DXGI processor was unexpectedly nil during streaming.');
                Break;
              end;

              // Allow UI to remain responsive
              Application.ProcessMessages;

              // Small delay to prevent CPU overload
              Sleep(10);
            except
              on E: Exception do
              begin
                Display('Stream Error', E.Message);
                // Add a slightly longer delay after an error
                Sleep(100);
              end;
            end;
          end;

        except
          on E: Exception do
          begin
            Display('Stream', 'Error initializing DXGIProcessor: ' + E.Message);
            FreeAndNil(FDXGIProcessor);
            raise;
          end;
        end;

      except
        on E: Exception do
        begin
          Display('Stream', 'Error initializing ThreadData: ' + E.Message);
          FreeAndNil(FThreadData);
          raise;
        end;
      end;

      finally

      end;
  end;
end;

procedure TForm1.StopDXGI;
begin
  // Set flags to stop the capture loop
  CaptureActive := False;

  // Wait for the capture loop to finish
  while CaptureActive do
    Application.ProcessMessages;

  // Clean up FDXGIProcessor
  if Assigned(FDXGIProcessor) then
  begin
    try
      FreeAndNil(FDXGIProcessor);
    except
      on E: Exception do
        Display('Stream', 'Error cleaning up FDXGIProcessor: ' + E.Message);
    end;
  end;

  // Clean up ThreadData
  if Assigned(FThreadData) then
  begin
    try
      FreeAndNil(FThreadData);
    except
      on E: Exception do
        Display('Stream', 'Error cleaning up ThreadData: ' + E.Message);
    end;
  end;

  // Reset encoder state
  if Assigned(Encoder) then
  begin
    try
      FreeAndNil(Encoder);
      Encoder := TVpxEncoder.Create(vctVP8);
    except
      on E: Exception do
        Display('Stream', 'Error resetting encoder: ' + E.Message);
    end;
  end;

  // Reset frame count
  FrameCount := 0;

  // Reset FPS tracking
  FLastFPSUpdate := 0;
  FFramesSinceLastUpdate := 0;
  FFPS := 0;

  Display('Stream', 'FDXGI streaming stopped');
end;

procedure TForm1.StartStreaming(MonitorIndex: Integer);
begin
  // Update the global monitor index to the ID from the server
  CurrentMonitorIndex := MonitorIndex;

  // First try DXGI capture
  StartDXGICapture(MonitorIndex);

  // If we get here and CaptureActive is false, DXGI failed and we should try GDI
  if not CaptureActive then
  begin
    Display('Stream', 'DXGI initialization failed, falling back to GDI capture...');
    StartGDICapture(MonitorIndex);
  end;
end;

procedure TForm1.StopStreaming;
begin
  // Set flag to stop any active streaming
  ShouldStream := False;

  // Stop DXGI if it's running
  if Assigned(FDXGIProcessor) then
    StopDXGI;

  // Stop GDI if it's running
  if Assigned(FGDIProcessor) then
    StopGDI;

  Display('Stream', 'Streaming stopped');
end;

procedure TForm1.SetupConnection;
const
  RETRY_DELAY = 5000; // Delay in milliseconds between retries
begin
  // Configure client source connection parameters
  ncClientSource1.Port := 3434;
  ncClientSource1.Host := '192.168.10.30'; // Set the server IP address
  ncClientSource1.Line.ConnectTimeout := 5000;

  try
    ncClientSource1.Active := true;
    if ncClientSource1.Active then
    begin
      messagesLog.Lines.Add('Successfully connected to the server.');
    end
    else
    begin
      raise Exception.Create('Failed to connect to the server.');
    end;
  except
    on E: Exception do
    begin
      Display('Connection', 'Error during connection attempt: ' + E.Message);
      Display('Connection', 'Retrying connection in ' + IntToStr(RETRY_DELAY div 1000) + ' seconds...');

      ConnectionTimer.Enabled := True; // Enable timer for retry
    end;
  end;
end;

procedure TForm1.CleanupResources;
begin
  // Clean up FDesktopBitmap
  if Assigned(FDesktopBitmap) then
  begin
    FreeAndNil(FDesktopBitmap);
  end;

  // Clean up any active streaming
  if CaptureActive or ShouldStream then
    StopGDI;

  // Clean up Encoder
  if Assigned(Encoder) then
  begin
    FreeAndNil(Encoder);
  end;

  // Clean up FMonitors
  if Assigned(FMonitors) then
  begin
    for var Monitor in FMonitors do
      Monitor.Free;
    FreeAndNil(FMonitors);
  end;

  // Clean up InputManager
  if Assigned(InputManager) then
  begin
    FreeAndNil(InputManager);
  end;

  // Clean up ncClientSource
  if Assigned(ncClientSource1) then
  begin
    ncClientSource1.Active := False;
  end;

  // Clean up FCursors dictionary
  if Assigned(FCursors) then
  begin
    FCursors.Free;
  end;
end;

// Form create event to initialize components
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize global variables
  ShouldStream := False;
  CaptureActive := False;
  CurrentMonitorIndex := 0; // Initialize monitor index to 0 (primary)
  FOldCursor := 0;
  StreamingThread := nil;
  FDesktopBitmap := nil;
  FGDIProcessor := nil;
  FThreadData := nil;
  Encoder := nil;

  // Initialize FPS tracking
  FLastFPSUpdate := 0;
  FFramesSinceLastUpdate := 0;
  FFPS := 0;

  // Create encoder
  Encoder := TVpxEncoder.Create(vctVP8);

  // Initialize the cursors dictionary once at application start
  FCursors := InitializeCursors;

  // Create InputManager
  InputManager := TInputManager.Create;

  // Load monitors using the new method
  LoadMonitors;

  // Tell Windows this is a high-performance application
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_HIGHEST);

  // Output initial debug info
  messagesLog.Lines.Add('Starting client with screen size: ' + IntToStr(Screen.Width) + 'x' + IntToStr(Screen.Height));
  messagesLog.Lines.Add('Application started. Attempting to connect...');

  // Initial connection attempt
  SetupConnection;

  // Enable timer only if not connected
  ConnectionTimer.Enabled := not ncClientSource1.Active;

end;

{
procedure TForm1.FormDestroy(Sender: TObject);
begin
  CleanupResources;
end; }

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // First, stop any active operations
  ShouldStream := False;

  // Make sure streaming is stopped
  if CaptureActive then
    StopStreaming;

  // Now free all resources in proper order
  if Assigned(FGDIProcessor) then
    FreeAndNil(FGDIProcessor);

  if Assigned(FThreadData) then
    FreeAndNil(FThreadData);

  if Assigned(Encoder) then
    FreeAndNil(Encoder);

  if Assigned(InputManager) then
    FreeAndNil(InputManager);

  if Assigned(FDesktopBitmap) then
    FreeAndNil(FDesktopBitmap);

  // Free the monitors list
  if Assigned(FMonitors) then
  begin
    for var Monitor in FMonitors do
      Monitor.Free;
    FreeAndNil(FMonitors);
  end;

  // Explicitly free the FCursors dictionary - this fixes the memory leak
  if Assigned(FCursors) then
    FreeAndNil(FCursors);

  // Log completion
  if Assigned(messagesLog) and messagesLog.HandleAllocated then
end;

// Timer event to attempt connection to the remote server
procedure TForm1.ConnectionTimerTimer(Sender: TObject);
begin
  if not ncClientSource1.Active then
  begin
    messagesLog.Lines.Add('Attempting to reconnect...');
    SetupConnection;
  end
  else
  begin
    // If connected, disable the timer
    ConnectionTimer.Enabled := False;
  end;
end;

// Event triggered when the client source successfully connects
procedure TForm1.ncClientSource1Connected(Sender: TObject; aLine: TncLine);
begin
  Caption := 'Connected!';
  messagesLog.Lines.Add('Client Connected!');

  // Send detailed monitor information after connection
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(1000); // Wait a second before sending
      SendMonitorInfo(ncClientSource1, FMonitors);
    end
  ).Start;

  // Disable connection timer when connected
  ConnectionTimer.Enabled := False;
end;

// Event triggered when the client source disconnects
procedure TForm1.ncClientSource1Disconnected(Sender: TObject; aLine: TncLine);
begin
  Caption := 'Not Connected!';
  messagesLog.Lines.Add('Client Disconnected!');

  // Stop streaming if active when disconnected
  StopStreaming;

  // Enable timer for reconnection attempts
  ConnectionTimer.Enabled := True;
end;

// Event triggered when the client reconnects
procedure TForm1.ncClientSource1Reconnected(Sender: TObject; aLine: TncLine);
begin
  Caption := 'Reconnected!';
  messagesLog.Lines.Add('Client Reconnected!');

  // Disable connection timer when reconnected
  ConnectionTimer.Enabled := False;

  // Send monitor information after reconnection
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(1000); // Wait a second before sending
      SendMonitorInfo(ncClientSource1, FMonitors);
    end
  ).Start;
end;

// Event handler for receiving async command results
procedure TForm1.ncClientSource1AsyncExecCommandResult(Sender: TObject;
  aLine: TncLine; aCmd: Integer; const aResult: TBytes; aResultIsError: Boolean;
  const aSenderComponent, aReceiverComponent: string);
begin
//
end;

function TForm1.ncClientSource1HandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
var
  Command: string;
  sl: TStringList;
  MonitorIndex: Integer;
  MouseEvt: TMouseButtonEvent;
  PosEvt: TMousePositionAbsoluteEvent;
  ScrollEvt: TMouseScrollEvent;
  x, y: Integer;
  ScreenWidth, ScreenHeight: Integer;
  SelectedMonitor: ScreenCaptureTypes.TMonitor;
begin
  // Convert TBytes to string
  Command := TEncoding.UTF8.GetString(aData);

  // Create and configure TStringList for parsing
  sl := TStringList.Create;
  try
    sl.Delimiter := '|';
    sl.StrictDelimiter := true;
    sl.DelimitedText := Command;

    if sl.count > 0 then
    begin

      //------------------------------------------------------------------------
      // SCREEN CAPTURE COMMAND
      //------------------------------------------------------------------------
      if sl[0] = 'StartStream' then
      begin
        // Set the streaming flag to True
        ShouldStream := true;

        InputManager.StreamActive := True;

        var MonitorId := StrToInt(sl[1]);

        // Update the global monitor index to the ID from the server
        CurrentMonitorIndex := MonitorId;

        // Call Stream with monitor ID
        StartStreaming(MonitorId);
      end;

      if sl[0] = 'StopStream' then
      begin
        StopStreaming;
        InputManager.StreamActive := False;
      end;

      if sl[0] = 'RefreshMonitors' then
      begin
        LoadMonitors;
        SendMonitorInfo(ncClientSource1, FMonitors);
      end;
      //------------------------------------------------------------------------
      // KEYBOARD EVENT SIMULATION
      //------------------------------------------------------------------------
      if sl[0] = 'KeyEvent' then
      begin
        if sl.Count >= 5 then
        begin
          var KeyEventType := sl[1]; // "KeyDown" or "KeyUp"
          var VKCode := StrToIntDef(sl[2], 0); // Virtual key code
          var ScanCode := StrToIntDef(sl[3], 0); // Hardware scan code
          var Flags := StrToIntDef(sl[4], 0); // Additional flags

          InputManager.SendKeyboardInput(
            Word(VKCode),
            Word(ScanCode),
            Flags,
            KeyEventType = 'KeyUp'
          );
        end;
      end;
      //------------------------------------------------------------------------
      // MOUSE COMMAND
      //------------------------------------------------------------------------
      if sl[0] = 'Wheel' then
      begin
        try
          messagesLog.Lines.Add('Wheel event received from server');
          if sl.Count >= 2 then
          begin
            ScrollEvt.Offset := StrToIntDef(sl[1], 0);
          end
          else
          begin
            messagesLog.Lines.Add('Warning: Wheel event missing offset parameter');
          end;

          InputManager.SendInput(ScrollEvt);

        except
          on E: Exception do
            messagesLog.Lines.Add('Wheel event error: ' + E.Message);
        end;
      end;

      if sl[0] = 'Mouse' then
      begin
        if sl.Count >= 5 then
        begin
          try
            // Find the monitor with the current ID
            SelectedMonitor := nil;
            for var Monitor in FMonitors do
            begin
              if Monitor.Id = CurrentMonitorIndex then
              begin
                SelectedMonitor := Monitor;
                break;
              end;
            end;

            // Default to first monitor if none found with matching ID
            if (SelectedMonitor = nil) and (FMonitors.Count > 0) then
            begin
              SelectedMonitor := FMonitors[0];
            end;

            if SelectedMonitor = nil then
            begin
              Display('Mouse', 'No monitor available for mouse event');
              Exit;
            end;

            ScreenWidth := SelectedMonitor.Width;
            ScreenHeight := SelectedMonitor.Height;

            x := StrToIntDef(sl[3], 0);
            y := StrToIntDef(sl[4], 0);

            // Set mouse position using coordinates relative to the selected monitor
            // by adding the monitor's offset
            PosEvt.x := x + SelectedMonitor.OffsetX;
            PosEvt.y := y + SelectedMonitor.OffsetY;

            PosEvt.RemoteWidth := ScreenWidth;
            PosEvt.RemoteHeight := ScreenHeight;
            InputManager.SendInput(PosEvt);

            if (sl[1] = 'Down') or (sl[1] = 'Up') then
            begin
              MouseEvt.Pressed := sl[1] = 'Down';

              if sl[2] = 'Left' then
              begin
                MouseEvt.Button := mbLeft;
                InputManager.SendInput(MouseEvt);
              end
              else if sl[2] = 'Right' then
              begin
                MouseEvt.Button := mbRight;
                InputManager.SendInput(MouseEvt);
              end
              else if sl[2] = 'Middle' then
              begin
                MouseEvt.Button := mbMiddle;
                InputManager.SendInput(MouseEvt);
              end;
            end;
          except
            on E: Exception do
              messagesLog.Lines.Add('Mouse event error: ' + E.Message);
          end;
        end;
      end
      //------------------------------------------------------------------------
    end;
  finally
    sl.Free;
  end;

  result := nil;
end;

end.