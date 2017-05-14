unit PeripheralFormDefine;

interface

uses
  Forms, Classes, SysUtils, FileUtil, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, ActnList, ProcessorDefine,
  VisiblePinSelectionDefine, Lists, Math, FPCanvas, PinDefine, Color;

type

  { TDisplayPinArray }

  TDisplayPinArray = class
  private
    FPosition: TPoint;
    FFlipped: Boolean;

    FPinArray: TPinArray;

    FControl: TCustomControl;

    function GetBounds: TRect;
    function GetBoxCaptionPosition: TPoint;
    function GetPin(AIndex: Integer): TPin;
    function GetPinCaptionPosition(AIndex: Integer): TPoint;
    function GetPinCount: Integer;
    function GetWidth: Integer;

    function GetPinPosition(AIndex: Integer): TPoint;

    procedure InitDrawPin(APin: TPin);
    procedure InitDrawBoxCaption;
    procedure InitDrawPinCaption;
    procedure InitDrawBox;
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);

  public
    constructor Create(AControl: TCustomControl; APosition: TPoint; APinArray: TPinArray; AFlipped: Boolean = False);

    procedure Draw;

    property PinArray: TPinArray read FPinArray;

    property Left: Integer read FPosition.X write SetLeft;
    property Top: Integer read FPosition.Y write SetTop;

    property PinCount: Integer read GetPinCount;
    property PinPosition[AIndex: Integer]: TPoint read GetPinPosition;
    property PinCaptionPosition[AIndex: Integer]: TPoint read GetPinCaptionPosition;
    property Pins[AIndex: Integer]: TPin read GetPin;
    property BoxCaptionPosition: TPoint read GetBoxCaptionPosition;
    property Width: Integer read GetWidth;
    property Bounds: TRect read GetBounds;
    property Flipped: Boolean read FFlipped;

    const
      Height = 50;
      PinDistance = 20;

  end;

  { TDisplayPinConnection }

  TDisplayPinConnection = class
  public
    type

      { TSinglePin }

      TSinglePin = record
        DisplayPinArray: TDisplayPinArray;
        Index: Integer;
      end;

  private
    FFrom, FTo: TSinglePin;
    FCanvas: TCanvas;

    procedure InitDrawConnection;

  public
    constructor Create(ACanvas: TCanvas; AFromPinArray, AToPinArray: TDisplayPinArray; AFromPinIndex, AToPinIndex: Integer);

    procedure Draw;
  end;

  { TPinConnectionForm }

  TPinConnectionForm = class (TForm)
    actPeripheralSettings: TAction;
    actShowPins: TAction;
    actVisiblePins: TAction;
    alPeripheral: TActionList;
    gbPins: TGroupBox;
    miPeripheralSeperator1: TMenuItem;
    miSettings: TMenuItem;
    miShowPins: TMenuItem;
    miVisiblePins: TMenuItem;
    pnlPins: TPanel;
    pnlDrawSurface: TPanel;
    pmPeripheral: TPopupMenu;
    pmPins: TPopupMenu;
    procedure actShowPinsExecute(Sender: TObject);
    procedure actShowPinsUpdate(Sender: TObject);
    procedure actVisiblePinsExecute(Sender: TObject);
    procedure actVisiblePinsUpdate(Sender: TObject);
    procedure pnlDrawSurfaceMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlDrawSurfaceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pnlDrawSurfaceMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlDrawSurfacePaint(Sender: TObject);
    procedure pnlPinsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlPinsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pnlPinsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlPinsPaint(Sender: TObject);
  private
    FPinsVisible: Boolean;

    FDisplayPinArrays: TObjectArray<TDisplayPinArray>;
    FDisplayPinConnections: TObjectArray<TDisplayPinConnection>;

    FClickedPinArray: TDisplayPinArray;
    FClickedPin: Integer;
    FPinMousePos: TPoint;

    function GetDrawSurfaceHeight: Integer;
    function GetDrawSurfaceWidth: Integer;
    function GetPinSurfaceHeight: Integer;
    function GetPinSurfaceWidth: Integer;

    procedure OnPinChange(Sender: TPin);
    procedure OnPinWritePowerChange(Sender: TPin);
    procedure OnPinRedirect(Sender: TPin);
    procedure OnNameChange(Sender: TPinArray);

    procedure OnPinConnectionChange(Sender, AOther: TPin);
    procedure OnVisibilityChanged(Sender: TPinArray);

    procedure SetDrawSurfaceHeight(AValue: Integer);
    procedure SetPinSurfaceHeight(AValue: Integer);

    procedure SetPinsVisible(AValue: Boolean);

    function FindPinAt(APinArray: TDisplayPinArray; APoint: TPoint): Integer;
    function FindPinArrayAt(APoint: TPoint): TDisplayPinArray;

    procedure AddVisiblePinArrayEvents;
    procedure DelVisiblePinArrayEvents;

  protected
    procedure DrawPeripheral; virtual; abstract;

    procedure DrawSurfaceMouseDown(X, Y: Integer; AButton: TMouseButton); virtual;
    procedure DrawSurfaceMouseMove(X, Y: Integer); virtual;
    procedure DrawSurfaceMouseUp(X, Y: Integer; AButton: TMouseButton); virtual;

    function GetPinArray: TPinArray; virtual; abstract;
    property PinArray: TPinArray read GetPinArray;

    function GetMinWidth: Integer; virtual;
    procedure AutoWidth;

    procedure SetSettingsFunc(AEvent: TNotifyEvent);

    procedure GeneratePinArrays;
    procedure GeneratePinConnections;

  public
    constructor Create(TheOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;

    property PinsVisible: Boolean read FPinsVisible write SetPinsVisible;

    property DrawSurfaceHeight: Integer read GetDrawSurfaceHeight write SetDrawSurfaceHeight;
    property PinSurfaceHeight: Integer read GetPinSurfaceHeight write SetPinSurfaceHeight;

  private
    type

      { TFindDisplayPinArray }

      TFindDisplayPinArray = class (TFindFunctionClass<TDisplayPinArray>)
      private
        FPinArray: TPinArray;
      protected
        function Find(AElement: TDisplayPinArray): Boolean; override;
      public
        constructor Create(APinArray: TPinArray);
      end;
  end;

  { TPeripheralForm }

  TPeripheralForm = class (TPinConnectionForm)
  private
    FPinArray: TPinArray;

    function GetPeripheralName: String;
    procedure SetPeripheralName(AValue: String);
    function MakeUniqueName(AName: String): String;

    procedure OnTryChangeName(Sender: TPinArray; var ANewName: String);
    procedure OnNameChange(Sender: TPinArray);

    procedure OnFormClose(Sender: TObject; var CloseAction: TCloseAction);

  protected
    function GetPinArray: TPinArray; override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    class function GetDefaultPeripheralName: String; virtual; abstract;

    property PeripheralName: String read GetPeripheralName write SetPeripheralName;
  end;

  TPeripheralFormClass = class of TPeripheralForm;

implementation

{$R *.lfm}

{ TPeripheralForm }

procedure TPeripheralForm.SetPeripheralName(AValue: String);
begin
  if PeripheralName = AValue then
    Exit;
  FPinArray.Name := AValue;
end;

function TPeripheralForm.GetPeripheralName: String;
begin
  Result := PinArray.Name;
end;

function TPeripheralForm.MakeUniqueName(AName: String): String;
var
  Suffix, I: Integer;
  Unique: Boolean;
begin
  Result := AName;
  Suffix := 0;
  repeat
    Unique := True;
    for I := 0 to frmVisiblePinSelection.PinArrayCount - 1 do
    begin
      if (frmVisiblePinSelection.PinArrays[I] <> FPinArray) and
         (frmVisiblePinSelection.PinArrays[I].Name = Result) then
      begin
        Inc(Suffix);
        Result := AName + ' [' + Suffix.ToString + ']';
        Unique := False;
        Break;
      end;
    end;
  until Unique;
end;

procedure TPeripheralForm.OnTryChangeName(Sender: TPinArray; var ANewName: String);
begin
  ANewName := MakeUniqueName(ANewName);
end;

procedure TPeripheralForm.OnNameChange(Sender: TPinArray);
begin
  Caption := 'Peripheral - ' + FPinArray.Name;
end;

procedure TPeripheralForm.OnFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

function TPeripheralForm.GetPinArray: TPinArray;
begin
  Result := FPinArray;
end;

constructor TPeripheralForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPinArray := TPinArray.Create;
  PinArray.OnTryChangeName.Add(OnTryChangeName);
  PinArray.OnNameChange.Add(OnNameChange);
  PeripheralName := GetDefaultPeripheralName;
  OnClose := OnFormClose;
end;

destructor TPeripheralForm.Destroy;
begin
  PinArray.OnTryChangeName.Del(OnTryChangeName);
  PinArray.OnNameChange.Del(OnNameChange);
  FPinArray.Free;
  inherited Destroy;
end;

{ TPinConnectionForm.TFindDisplayPinArray }

function TPinConnectionForm.TFindDisplayPinArray.Find(AElement: TDisplayPinArray): Boolean;
begin
  Result := AElement.FPinArray = FPinArray;
end;

constructor TPinConnectionForm.TFindDisplayPinArray.Create(APinArray: TPinArray);
begin
  FPinArray := APinArray;
end;

{ TDisplayPinConnection }

procedure TDisplayPinConnection.InitDrawConnection;
begin
  with FCanvas do
  begin
    Pen.Style := psSolid;
    Pen.Width := 4;
    Pen.Color := $FF2F2F;
    Pen.EndCap := pecRound;
  end;
end;

constructor TDisplayPinConnection.Create(ACanvas: TCanvas; AFromPinArray, AToPinArray: TDisplayPinArray;
  AFromPinIndex, AToPinIndex: Integer);
begin
  FCanvas := ACanvas;
  FFrom.DisplayPinArray := AFromPinArray;
  FTo.DisplayPinArray := AToPinArray;
  FFrom.Index := AFromPinIndex;
  FTo.Index := AToPinIndex;
end;

procedure TDisplayPinConnection.Draw;
begin
  InitDrawConnection;
  FCanvas.Line(FFrom.DisplayPinArray.PinPosition[FFrom.Index],
               FTo.DisplayPinArray.PinPosition[FTo.Index]);
end;

{ TDisplayPinArray }

function TDisplayPinArray.GetPinPosition(AIndex: Integer): TPoint;
begin
  Result.X := FPosition.X + Width - PinDistance div 2 - PinDistance * AIndex;
  if FFlipped then
    Result.Y := FPosition.Y + 10
  else
    Result.Y := FPosition.Y + Height - 10;
end;

function TDisplayPinArray.GetWidth: Integer;
begin
  Result := Max(PinDistance * FPinArray.Count, FControl.Canvas.GetTextWidth(FPinArray.Name) + 10);
end;

function TDisplayPinArray.GetBoxCaptionPosition: TPoint;
begin
  Result.X := FPosition.X + (Width - FControl.Canvas.TextWidth(FPinArray.Name)) div 2;
  if FFlipped then
    Result.Y := FPosition.Y + Height - 2 - FControl.Canvas.TextHeight(FPinArray.Name)
  else
    Result.Y := FPosition.Y + 2;
end;

function TDisplayPinArray.GetPin(AIndex: Integer): TPin;
begin
  Result := FPinArray[AIndex];
end;

function TDisplayPinArray.GetBounds: TRect;
begin
  Result := TRect.Create(FPosition, Width, Height);
end;

function TDisplayPinArray.GetPinCaptionPosition(AIndex: Integer): TPoint;
begin
  Result.X := PinPosition[AIndex].X - FControl.Canvas.TextWidth(AIndex.ToString) div 2;
  Result.Y := FPosition.Y + (Height - FControl.Canvas.TextHeight(AIndex.ToString)) div 2;
end;

function TDisplayPinArray.GetPinCount: Integer;
begin
  Result := FPinArray.Count;
end;

procedure TDisplayPinArray.InitDrawPin(APin: TPin);
var
  C: TColorRGB;
begin
  with FControl.Canvas do
  begin
    Brush.Style := bsSolid;
    Pen.Style := psSolid;
    Pen.Width := 2;
    case APin.Direction of
      pdWrite:
      begin
        if APin.WritePower then
          C := TColorRGB.Create(0.9, 0.9, 0.2)
        else
          C := TColorRGB.Create(1.0, 0.3, 0.1);
      end;
      pdRead:
        C := TColorRGB.Create(0.3, 0.9, 0.1);
    end;
    if not APin.State then
      C := C * 0.6;
    Brush.Color := C.ToWinColor;
    Pen.Color := (C * 0.8).ToWinColor;
  end;
end;

procedure TDisplayPinArray.InitDrawBoxCaption;
begin
  with FControl.Canvas do
  begin
    Brush.Style := bsClear;
  end;
end;

procedure TDisplayPinArray.InitDrawPinCaption;
begin
  with FControl.Canvas do
  begin
    Brush.Style := bsClear;
  end;
end;

procedure TDisplayPinArray.InitDrawBox;
begin
  with FControl.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := $FFBFCF;
    Pen.Style := psSolid;
    Pen.Color := $EF8F9F;
    Pen.Width := 2;
  end;
end;

procedure TDisplayPinArray.SetLeft(AValue: Integer);
begin
  if Left = AValue then
    Exit;
  FPosition.X := AValue;
  FControl.Invalidate;
end;

procedure TDisplayPinArray.SetTop(AValue: Integer);
begin
  if Top = AValue then
    Exit;
  FPosition.Y := AValue;
  FControl.Invalidate;
end;

constructor TDisplayPinArray.Create(AControl: TCustomControl; APosition: TPoint; APinArray: TPinArray;
  AFlipped: Boolean);
begin
  FControl := AControl;
  FPosition := APosition;
  FPinArray := APinArray;
  FFlipped := AFlipped;
end;

procedure TDisplayPinArray.Draw;
var
  I: Integer;
  P: TPoint;
begin
  // Draw Box
  InitDrawBox;
  FControl.Canvas.Rectangle(TRect.Create(FPosition, Width, Height));

  // Draw PinArray Caption
  InitDrawBoxCaption;
  P := BoxCaptionPosition;
  FControl.Canvas.TextOut(P.X, P.Y, FPinArray.Name);

  // Draw Pins
  for I := 0 to FPinArray.Count - 1 do
  begin
    InitDrawPin(Pins[I]);
    FControl.Canvas.Ellipse(PinPosition[I].X - 6, PinPosition[I].Y - 6, PinPosition[I].X + 6, PinPosition[I].Y + 6);
    //FControl.Canvas.TextOut(PinPosition[I].X - 6, PinPosition[I].Y - 6, PinArray[I].PowerSources.ToString);
  end;

  // Draw Pin Captions
  InitDrawPinCaption;
  for I := 0 to FPinArray.Count - 1 do
  begin
    P := PinCaptionPosition[I];
    FControl.Canvas.TextOut(P.X, P.Y, I.ToString);
  end;

end;

{ TPinConnectionForm }

procedure TPinConnectionForm.pnlDrawSurfacePaint(Sender: TObject);
begin
  pnlDrawSurface.Canvas.Pen.Style := psClear;
  pnlDrawSurface.Canvas.Brush.Style := bsSolid;
  pnlDrawSurface.Canvas.Brush.Color := clBtnFace;
  pnlDrawSurface.Canvas.Rectangle(0, 0, pnlDrawSurface.Width, pnlDrawSurface.Height);
  DrawPeripheral;
end;

procedure TPinConnectionForm.pnlPinsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPinMousePos := Point(X, Y);
  case Button of
    mbLeft:
    begin
      FClickedPinArray := FindPinArrayAt(FPinMousePos);
      if FClickedPinArray <> nil then
      begin
        FClickedPin := FindPinAt(FClickedPinArray, FPinMousePos);
        pnlPins.Invalidate;
      end;
    end;
  end;
end;

procedure TPinConnectionForm.pnlPinsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FPinMousePos := Point(X, Y);
  if FClickedPinArray <> nil then
  begin
    pnlPins.Invalidate;
  end;
end;

procedure TPinConnectionForm.pnlPinsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  EndPinArray: TDisplayPinArray;
  EndPin, MinPins, I: Integer;
  PinA, PinB: TPin;
  AllPinsExist: Boolean;
begin
  case Button of
    mbLeft:
    begin
      if FClickedPinArray <> nil then
      begin
        EndPinArray := FindPinArrayAt(FPinMousePos);
        if EndPinArray <> nil then
        begin
          if FClickedPin <> -1 then
          begin
            if (EndPinArray <> nil) and (EndPinArray.Flipped <> FClickedPinArray.Flipped) then
            begin
              EndPin := FindPinAt(EndPinArray, FPinMousePos);
              if EndPin <> -1 then
              begin
                PinA := FClickedPinArray.Pins[FClickedPin];
                PinB := EndPinArray.Pins[EndPin];
                if PinA.ConnectedTo(PinB) then
                  PinA.Disconnect(PinB)
                else
                  PinA.Connect(PinB);
              end;

              FClickedPin := -1;
            end;
          end
          else
          begin
            EndPin := FindPinAt(EndPinArray, FPinMousePos);
            if EndPin = -1 then
              EndPin := 0;
            if (EndPinArray <> nil) and (EndPinArray.Flipped <> FClickedPinArray.Flipped) then
            begin
              MinPins := Min(FClickedPinArray.PinCount, EndPinArray.PinCount - EndPin);
              AllPinsExist := True;
              for I := 0 to MinPins - 1 do
              begin
                PinA := FClickedPinArray.Pins[I];
                PinB := EndPinArray.Pins[EndPin + I];
                if not PinA.ConnectedTo(PinB) then
                begin
                  AllPinsExist := False;
                  Break;
                end;
              end;

              for I := 0 to MinPins - 1 do
              begin
                PinA := FClickedPinArray.Pins[I];
                PinB := EndPinArray.Pins[EndPin + I];
                if AllPinsExist then
                  PinA.Disconnect(PinB)
                else
                  PinA.Connect(PinB);
              end;
            end;
          end;
        end;
        FClickedPinArray := nil;
        pnlPins.Invalidate;
      end;
    end;
  end;
end;

procedure TPinConnectionForm.pnlPinsPaint(Sender: TObject);
var
  DisplayPinArray: TDisplayPinArray;
  DisplayPinConnection: TDisplayPinConnection;
  I: Integer;
  Offset: TPoint;
begin
  pnlPins.Canvas.Pen.Style := psClear;
  pnlPins.Canvas.Brush.Style := bsSolid;
  pnlPins.Canvas.Brush.Color := clBtnFace;
  pnlPins.Canvas.Rectangle(0, 0, pnlPins.Width, pnlPins.Height);

  for DisplayPinArray in FDisplayPinArrays do
    DisplayPinArray.Draw;
  for DisplayPinConnection in FDisplayPinConnections do
    DisplayPinConnection.Draw;

  if FClickedPinArray <> nil then
  begin
    pnlPins.Canvas.Pen.Style := psSolid;
    pnlPins.Canvas.Pen.Color := $0F1FEF;
    pnlPins.Canvas.Pen.Width := 6;
    pnlPins.Canvas.Pen.EndCap := pecRound;
    if FClickedPin <> -1 then
    begin
      pnlPins.Canvas.Line(FClickedPinArray.PinPosition[FClickedPin], FPinMousePos);
    end
    else
    begin
      for I := 0 to FClickedPinArray.PinCount - 1 do
      begin
        Offset := Point(I * TDisplayPinArray.PinDistance, 0);
        pnlPins.Canvas.Line(FClickedPinArray.PinPosition[0] - Offset, FPinMousePos - Offset);
      end;
    end;
  end;
end;

procedure TPinConnectionForm.actShowPinsExecute(Sender: TObject);
begin
  PinsVisible := not PinsVisible;
  AutoWidth;
end;

procedure TPinConnectionForm.actShowPinsUpdate(Sender: TObject);
begin
  if PinsVisible then
    actShowPins.Caption := 'Hide Pins'
  else
    actShowPins.Caption := 'Show Pins';
end;

procedure TPinConnectionForm.actVisiblePinsExecute(Sender: TObject);
begin
  frmVisiblePinSelection.Execute(PinArray);
  GeneratePinArrays;
end;

procedure TPinConnectionForm.actVisiblePinsUpdate(Sender: TObject);
begin
  actVisiblePins.Enabled := PinsVisible;
end;

procedure TPinConnectionForm.pnlDrawSurfaceMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  DrawSurfaceMouseDown(X, Y, Button);
end;

procedure TPinConnectionForm.pnlDrawSurfaceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  DrawSurfaceMouseMove(X, Y);
end;

procedure TPinConnectionForm.pnlDrawSurfaceMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  DrawSurfaceMouseUp(X, Y, Button);
end;

function TPinConnectionForm.GetDrawSurfaceHeight: Integer;
begin
  Result := pnlDrawSurface.Height;
end;

function TPinConnectionForm.GetDrawSurfaceWidth: Integer;
begin
  Result := pnlDrawSurface.Width;
end;

function TPinConnectionForm.GetPinSurfaceHeight: Integer;
begin
  Result := pnlPins.Height;
end;

function TPinConnectionForm.GetPinSurfaceWidth: Integer;
begin
  Result := pnlPins.Width;
end;

procedure TPinConnectionForm.OnPinRedirect(Sender: TPin);
begin
  pnlPins.Invalidate;
end;

procedure TPinConnectionForm.OnNameChange(Sender: TPinArray);
begin
  GeneratePinArrays;
end;

procedure TPinConnectionForm.OnVisibilityChanged(Sender: TPinArray);
begin
  DelVisiblePinArrayEvents;
  AddVisiblePinArrayEvents;
  GeneratePinArrays;
end;

procedure TPinConnectionForm.SetDrawSurfaceHeight(AValue: Integer);
begin
  Height := Height - DrawSurfaceHeight + AValue;
end;

procedure TPinConnectionForm.SetPinSurfaceHeight(AValue: Integer);
var
  HeightDiff: Integer;
begin
  HeightDiff := AValue - PinSurfaceHeight;
  Height := Height + HeightDiff;
  gbPins.Height := gbPins.Height + HeightDiff;
end;

procedure TPinConnectionForm.OnPinConnectionChange(Sender, AOther: TPin);
begin
  GeneratePinConnections;
end;

procedure TPinConnectionForm.SetPinsVisible(AValue: Boolean);
begin
  if FPinsVisible = AValue then
    Exit;
  FPinsVisible := AValue;
  DisableAlign;
  if PinsVisible then
  begin
    gbPins.BorderSpacing.Around := 5;
    Height := Height + gbPins.Height + gbPins.BorderSpacing.Around * 2;
    gbPins.Visible := True;
  end
  else
  begin
    Height := Height - gbPins.Height - gbPins.BorderSpacing.Around * 2;
    gbPins.Visible := False;
    gbPins.BorderSpacing.Around := 0;
  end;
  EnableAlign;
  UpdateActions;
end;

procedure TPinConnectionForm.GeneratePinArrays;
var
  I, L, Diff: Integer;
  DisplayPinArray: TDisplayPinArray;
begin
  FDisplayPinArrays.DelAll;

  FDisplayPinArrays.Add(TDisplayPinArray.Create(pnlPins, Point(10, 10), PinArray));

  L := 10;
  for I := 0 to PinArray.VisiblePinArrayCount - 1 do
  begin
    DisplayPinArray := FDisplayPinArrays.Add(TDisplayPinArray.Create(
      pnlPins,
      Point(L, pnlPins.Height - TDisplayPinArray.Height - 10),
      PinArray.VisiblePinArrays[I],
      True));
    L := L + DisplayPinArray.Width + 10;
  end;

  AutoWidth;

  FDisplayPinArrays.First.Left := pnlPins.Width - FDisplayPinArrays.First.Width - 10;
  Diff := pnlPins.Width - FDisplayPinArrays.Last.Bounds.Right - 10;
  for I := 1 to PinArray.VisiblePinArrayCount do
    FDisplayPinArrays[I].Left := FDisplayPinArrays[I].Left + Diff;

  GeneratePinConnections;
end;

procedure TPinConnectionForm.GeneratePinConnections;
var
  Main, DisplayPinArray: TDisplayPinArray;
  Connected: TPin;
  I: Integer;
begin
  Main := FDisplayPinArrays.First;

  FDisplayPinConnections.DelAll;
  for I := 0 to PinArray.Count - 1 do
  begin
    for Connected in PinArray.Pins[I] do
    begin
      DisplayPinArray := FDisplayPinArrays.FindFirst(TFindDisplayPinArray.Create(Connected.PinArray));

      FDisplayPinConnections.Add(TDisplayPinConnection.Create(
        pnlPins.Canvas,
        Main, DisplayPinArray,
        I, Connected.Index));
    end;
  end;

  pnlPins.Invalidate;
end;

function TPinConnectionForm.FindPinAt(APinArray: TDisplayPinArray; APoint: TPoint): Integer;
var
  I: Integer;
begin
  for I := 0 to APinArray.PinCount - 1 do
    if TPoint.PointInCircle(APoint, APinArray.PinPosition[I], 10) then
      Exit(I);
  Result := -1;
end;

function TPinConnectionForm.FindPinArrayAt(APoint: TPoint): TDisplayPinArray;
var
  DisplayPinArray: TDisplayPinArray;
  I: Integer;
begin
  for DisplayPinArray in FDisplayPinArrays do
    for I := 0 to DisplayPinArray.PinCount - 1 do
      if DisplayPinArray.Bounds.Contains(APoint) then
        Exit(DisplayPinArray);
  Result := nil;
end;

procedure TPinConnectionForm.AddVisiblePinArrayEvents;
var
  I: Integer;
begin
  for I := 0 to PinArray.VisiblePinArrayCount - 1 do
  begin
    PinArray.VisiblePinArrays[I].OnPinRedirect.Add(OnPinRedirect);
    PinArray.VisiblePinArrays[I].OnPinChange.Add(OnPinChange);
    PinArray.VisiblePinArrays[I].OnPinWritePowerChange.Add(OnPinWritePowerChange);
    PinArray.VisiblePinArrays[I].OnNameChange.Add(OnNameChange);
  end;
end;

procedure TPinConnectionForm.DelVisiblePinArrayEvents;
var
  I: Integer;
begin
  for I := 0 to PinArray.VisiblePinArrayCount - 1 do
  begin
    PinArray.VisiblePinArrays[I].OnPinRedirect.Del(OnPinRedirect);
    PinArray.VisiblePinArrays[I].OnPinChange.Del(OnPinChange);
    PinArray.VisiblePinArrays[I].OnPinWritePowerChange.Del(OnPinWritePowerChange);
    PinArray.VisiblePinArrays[I].OnNameChange.Del(OnNameChange);
  end;
end;

procedure TPinConnectionForm.OnPinChange(Sender: TPin);
begin
  pnlPins.Invalidate;
end;

procedure TPinConnectionForm.OnPinWritePowerChange(Sender: TPin);
begin
  pnlPins.Invalidate;
end;

procedure TPinConnectionForm.DrawSurfaceMouseDown(X, Y: Integer; AButton: TMouseButton);
begin
  // nothing by default
end;

procedure TPinConnectionForm.DrawSurfaceMouseMove(X, Y: Integer);
begin
  // nothing by default
end;

procedure TPinConnectionForm.DrawSurfaceMouseUp(X, Y: Integer; AButton: TMouseButton);
begin
  // nothing by default
end;

function TPinConnectionForm.GetMinWidth: Integer;
begin
  Result := 0;
  if PinsVisible then
  begin
    Result := Max(Result, FDisplayPinArrays.First.Bounds.Right + 10 + ClientWidth - pnlPins.Width);
    if FDisplayPinArrays.Count > 1 then
      Result := Max(Result, FDisplayPinArrays.Last.Bounds.Right + 10 + ClientWidth - pnlPins.Width);
  end;
end;

procedure TPinConnectionForm.AutoWidth;
begin
  Width := GetMinWidth;
end;

procedure TPinConnectionForm.SetSettingsFunc(AEvent: TNotifyEvent);
begin
  actPeripheralSettings.OnExecute := AEvent;
end;

constructor TPinConnectionForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  pnlDrawSurface.DoubleBuffered := True;
  PinSurfaceHeight := 150;
  FPinsVisible := True;
  FDisplayPinArrays := TObjectArray<TDisplayPinArray>.Create;
  FDisplayPinConnections := TObjectArray<TDisplayPinConnection>.Create;
  FClickedPin := -1;
end;

procedure TPinConnectionForm.AfterConstruction;
begin
  inherited AfterConstruction;
  PinArray.OnPinChange.Add(OnPinChange);
  PinArray.OnPinWritePowerChange.Add(OnPinWritePowerChange);
  PinArray.OnPinConnect.Add(OnPinConnectionChange);
  PinArray.OnPinDisconnect.Add(OnPinConnectionChange);
  PinArray.OnVisibilityChange.Add(OnVisibilityChanged);
  PinArray.OnPinRedirect.Add(OnPinRedirect);
  PinArray.OnNameChange.Add(OnNameChange);
  GeneratePinArrays;
end;

procedure TPinConnectionForm.BeforeDestruction;
begin
  DelVisiblePinArrayEvents;
  PinArray.OnPinChange.Del(OnPinChange);
  PinArray.OnPinWritePowerChange.Del(OnPinWritePowerChange);
  PinArray.OnPinConnect.Del(OnPinConnectionChange);
  PinArray.OnPinDisconnect.Del(OnPinConnectionChange);
  PinArray.OnVisibilityChange.Del(OnVisibilityChanged);
  PinArray.OnPinRedirect.Del(OnPinRedirect);
  inherited BeforeDestruction;
end;

destructor TPinConnectionForm.Destroy;
begin
  FDisplayPinArrays.Free;
  FDisplayPinConnections.Free;
  inherited Destroy;
end;

end.

