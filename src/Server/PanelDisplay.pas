unit PanelDisplay;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls;

type
  TScaleMode = (smStretch, smFit, smCenter);

  TDisplayPanel = class(TCustomControl)
  private
    FBuffer: TBitmap;
    FScaleMode: TScaleMode;
    FOffscreenBitmap: TBitmap;
    FCustomCursor: TCursor;
    procedure SetScaleMode(const Value: TScaleMode);
    procedure SetCustomCursor(const Value: TCursor);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateFrame(const Buffer: Pointer; Width, Height: Integer);
    property ScaleMode: TScaleMode read FScaleMode write SetScaleMode;
    property Color;
    property CustomCursor: TCursor read FCustomCursor write SetCustomCursor default crDefault;
  published
    // Publish the mouse event properties so they can be accessed
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnClick;
    property OnDblClick;
    property Cursor;  // Publish the Cursor property
  end;

implementation

// Rest of the implementation stays the same
constructor TDisplayPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque, csParentBackground];
  StyleElements := [];
  ParentBackground := False;
  ParentColor := False;
  Color := clBlack;
  FCustomCursor := crDefault;
  Cursor := crDefault;  // Initialize cursor

  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf32bit;
  FBuffer.HandleType := bmDIB;
  FBuffer.Canvas.Font.Quality := fqClearType;

  FOffscreenBitmap := TBitmap.Create;
  FOffscreenBitmap.PixelFormat := pf32bit;
  FOffscreenBitmap.HandleType := bmDIB;
  FOffscreenBitmap.Canvas.Font.Quality := fqClearType;

  FScaleMode := smFit;
end;

destructor TDisplayPanel.Destroy;
begin
  FBuffer.Free;
  FOffscreenBitmap.Free;
  inherited;
end;

procedure TDisplayPanel.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_COMPOSITED;
end;

procedure TDisplayPanel.SetCustomCursor(const Value: TCursor);
begin
  if FCustomCursor <> Value then
  begin
    FCustomCursor := Value;
    // Apply the cursor if the mouse is over the control
    if PtInRect(ClientRect, ScreenToClient(Mouse.CursorPos)) then
      Cursor := FCustomCursor;
  end;
end;

procedure TDisplayPanel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  // Set the custom cursor when the mouse enters the panel
  Cursor := FCustomCursor;
end;

procedure TDisplayPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  // Reset to default cursor when the mouse leaves the panel
  Cursor := crDefault;
end;

procedure TDisplayPanel.SetScaleMode(const Value: TScaleMode);
begin
  if FScaleMode <> Value then
  begin
    FScaleMode := Value;
    Invalidate;
  end;
end;

procedure TDisplayPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TDisplayPanel.WMMouseWheel(var Message: TWMMouseWheel);
var
  ShiftState: TShiftState;
  Handled: Boolean;
  MousePos: TPoint;
begin
  Handled := False;

  // Convert keys to shift state
  ShiftState := [];
  if (Message.Keys and MK_SHIFT) <> 0 then Include(ShiftState, ssShift);
  if (Message.Keys and MK_CONTROL) <> 0 then Include(ShiftState, ssCtrl);
  if (Message.Keys and MK_LBUTTON) <> 0 then Include(ShiftState, ssLeft);
  if (Message.Keys and MK_RBUTTON) <> 0 then Include(ShiftState, ssRight);
  if (Message.Keys and MK_MBUTTON) <> 0 then Include(ShiftState, ssMiddle);

  // Get mouse position (needed for DoMouseWheel method signature)
  MousePos := SmallPointToPoint(Message.Pos);
  MousePos := ScreenToClient(MousePos);

  // Call DoMouseWheel method which will trigger the OnMouseWheel event
  Handled := DoMouseWheel(ShiftState, Message.WheelDelta, MousePos);

  // Set the result to indicate whether the message was handled
  Message.Result := Ord(Handled);

  // If not handled, pass to default handler
  if not Handled then
    inherited;
end;

procedure TDisplayPanel.Paint;
const
  QUALITY_HIGH = 2; // Multiplier for high quality scaling
var
  R: TRect;
  AspectRatio: Double;
  NewWidth, NewHeight: Integer;
  TempBmp1, TempBmp2: TBitmap;
begin
  if (FOffscreenBitmap.Width <> Width) or (FOffscreenBitmap.Height <> Height) then
  begin
    FOffscreenBitmap.SetSize(Width, Height);
  end;

  with FOffscreenBitmap.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(ClientRect);

    if not FBuffer.Empty then
    begin
      case FScaleMode of
        smStretch:
          begin
            // Multi-pass scaling for high quality stretch
            TempBmp1 := TBitmap.Create;
            try
              TempBmp1.PixelFormat := pf32bit;
              TempBmp1.HandleType := bmDIB;
              TempBmp1.SetSize(Width * QUALITY_HIGH, Height * QUALITY_HIGH);

              // First pass - scale up to intermediate size
              SetStretchBltMode(TempBmp1.Canvas.Handle, HALFTONE);
              StretchBlt(TempBmp1.Canvas.Handle, 0, 0, TempBmp1.Width, TempBmp1.Height,
                         FBuffer.Canvas.Handle, 0, 0, FBuffer.Width, FBuffer.Height, SRCCOPY);

              // Second pass - scale down to target size
              SetStretchBltMode(Handle, HALFTONE);
              StretchBlt(Handle, 0, 0, Width, Height,
                         TempBmp1.Canvas.Handle, 0, 0, TempBmp1.Width, TempBmp1.Height, SRCCOPY);
            finally
              TempBmp1.Free;
            end;
          end;

        smCenter:
          begin
            R.Left := (Width - FBuffer.Width) div 2;
            R.Top := (Height - FBuffer.Height) div 2;
            R.Right := R.Left + FBuffer.Width;
            R.Bottom := R.Top + FBuffer.Height;

            // Center the image with anti-aliasing
            SetStretchBltMode(Handle, HALFTONE);
            BitBlt(Handle, R.Left, R.Top, FBuffer.Width, FBuffer.Height,
                   FBuffer.Canvas.Handle, 0, 0, SRCCOPY);
          end;

        smFit:
          begin
            AspectRatio := FBuffer.Width / FBuffer.Height;
            NewWidth := Width;
            NewHeight := Round(Width / AspectRatio);

            if NewHeight > Height then
            begin
              NewHeight := Height;
              NewWidth := Round(Height * AspectRatio);
            end;

            R.Left := (Width - NewWidth) div 2;
            R.Top := (Height - NewHeight) div 2;
            R.Right := R.Left + NewWidth;
            R.Bottom := R.Top + NewHeight;

            // Multi-pass scaling for best quality in fit mode
            TempBmp1 := TBitmap.Create;
            TempBmp2 := TBitmap.Create;
            try
              // Step 1: Scale up to intermediate size (2x target)
              TempBmp1.PixelFormat := pf32bit;
              TempBmp1.HandleType := bmDIB;
              TempBmp1.SetSize(NewWidth * QUALITY_HIGH, NewHeight * QUALITY_HIGH);

              SetStretchBltMode(TempBmp1.Canvas.Handle, HALFTONE);
              StretchBlt(TempBmp1.Canvas.Handle, 0, 0, TempBmp1.Width, TempBmp1.Height,
                         FBuffer.Canvas.Handle, 0, 0, FBuffer.Width, FBuffer.Height, SRCCOPY);

              // Step 2: Additional smoothing pass to target size
              TempBmp2.PixelFormat := pf32bit;
              TempBmp2.HandleType := bmDIB;
              TempBmp2.SetSize(NewWidth, NewHeight);

              SetStretchBltMode(TempBmp2.Canvas.Handle, HALFTONE);
              StretchBlt(TempBmp2.Canvas.Handle, 0, 0, NewWidth, NewHeight,
                         TempBmp1.Canvas.Handle, 0, 0, TempBmp1.Width, TempBmp1.Height, SRCCOPY);

              // Final render pass with smoothing
              SetStretchBltMode(Handle, HALFTONE);
              BitBlt(Handle, R.Left, R.Top, NewWidth, NewHeight,
                     TempBmp2.Canvas.Handle, 0, 0, SRCCOPY);
            finally
              TempBmp1.Free;
              TempBmp2.Free;
            end;
          end;
      end;
    end;
  end;

  // Final copy to screen
  SetStretchBltMode(Canvas.Handle, HALFTONE);
  BitBlt(Canvas.Handle, 0, 0, Width, Height,
         FOffscreenBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TDisplayPanel.UpdateFrame(const Buffer: Pointer; Width, Height: Integer);
var
  BitmapInfo: TBitmapInfo;
begin
  if not Assigned(Buffer) or (Width <= 0) or (Height <= 0) then Exit;

  if (FBuffer.Width <> Width) or (FBuffer.Height <> Height) then
  begin
    FBuffer.SetSize(Width, Height);
  end;

  FillChar(BitmapInfo, SizeOf(TBitmapInfo), 0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
  end;

  SetDIBits(FBuffer.Canvas.Handle,
            FBuffer.Handle,
            0, Height,
            Buffer,
            BitmapInfo,
            DIB_RGB_COLORS);

  Invalidate;
end;

end.
