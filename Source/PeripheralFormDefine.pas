unit PeripheralFormDefine;

interface

uses
  Forms, Classes, SysUtils, FileUtil, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, ActnList, ProcessorDefine,
  VisiblePinSelectionDefine, Lists, Math, FPCanvas, PinDefine;

type

  { TDisplayPinArray }

  TDisplayPinArray = class
  private
    FPosition: TPoint;
    FFlipped: Boolean;

    FPinArray: TPinArray;

    FCanvas: TCanvas;

    function GetBounds: TRect;
    function GetBoxCaptionPosition: TPoint;
    function GetPin(AIndex: Integer): TPin;
    function GetPinCaptionPosition(AIndex: Integer): TPoint;
    function GetPinCount: Integer;
    function GetWidth: Integer;

    function GetPinPosition(AIndex: Integer): TPoint;

    procedure InitDrawPin;
    procedure InitDrawBoxCaption;
    procedure InitDrawPinCaption;
    procedure InitDrawBox;

  public
    constructor Create(ACanvas: TCanvas; APosition: TPoint; APinArray: TPinArray; AFlipped: Boolean = False);

    procedure Draw;

    property PinArray: TPinArray read FPinArray;

    property PinCount: Integer read GetPinCount;
    property PinPosition[AIndex: Integer]: TPoint read GetPinPosition;
    property PinCaptionPosition[AIndex: Integer]: TPoint read GetPinCaptionPosition;
    property Pins[AIndex: Integer]: TPin read GetPin;
    property BoxCaptionPosition: TPoint read GetBoxCaptionPosition;
    property Width: Integer read GetWidth;
    property Bounds: TRect read GetBounds;

    const
      Height = 50;

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

  { TPeripheralForm }

  TPeripheralForm = class (TForm)
    actShowPins: TAction;
    actVisiblePins: TAction;
    pmPeripheral: TActionList;
    gbPins: TGroupBox;
    miVisiblePins: TMenuItem;
    miShowPins: TMenuItem;
    miSplitter1: TMenuItem;
    pbDrawSurface: TPaintBox;
    pbPins: TPaintBox;
    pmPins: TPopupMenu;
    procedure actShowPinsExecute(Sender: TObject);
    procedure actShowPinsUpdate(Sender: TObject);
    procedure actVisiblePinsExecute(Sender: TObject);
    procedure actVisiblePinsUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure pbDrawSurfacePaint(Sender: TObject);
    procedure pbPinsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbPinsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbPinsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbPinsPaint(Sender: TObject);
  private
    FPinArray: TPinArray;
    FPeripheralName: String;
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
    procedure OnVisibilityChanged(Sender: TPinArray);

    procedure SetDrawSurfaceHeight(AValue: Integer);
    procedure SetDrawSurfaceWidth(AValue: Integer);
    procedure SetPinSurfaceHeight(AValue: Integer);
    procedure SetPinSurfaceWidth(AValue: Integer);

    procedure SetPeripheralName(AValue: String);
    function MakeUniqueName(AName: String): String;

    procedure OnPinConnect(AIndex: Cardinal; AOther: TPin);
    procedure OnPinDisconnect(AIndex: Cardinal; AOther: TPin);

    procedure SetPinsVisible(AValue: Boolean);

    procedure GenerateDisplayPins;

    function FindPinAt(APinArray: TDisplayPinArray; APoint: TPoint): Integer;
    function FindPinArrayAt(APoint: TPoint): TDisplayPinArray;

  protected
    procedure DrawPeripheral; virtual; abstract;
    function GetDefaultPeripheralName: String; virtual; abstract;
    procedure OnPinChanged({%H-}APinIndex: Cardinal); virtual;
    property PinArray: TPinArray read FPinArray;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property PinsVisible: Boolean read FPinsVisible write SetPinsVisible;

    property DrawSurfaceWidth: Integer read GetDrawSurfaceWidth write SetDrawSurfaceWidth;
    property DrawSurfaceHeight: Integer read GetDrawSurfaceHeight write SetDrawSurfaceHeight;

    property PinSurfaceWidth: Integer read GetPinSurfaceWidth write SetPinSurfaceWidth;
    property PinSurfaceHeight: Integer read GetPinSurfaceHeight write SetPinSurfaceHeight;

    property PeripheralName: String read FPeripheralName write SetPeripheralName;

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

implementation

{$R *.lfm}

{ TPeripheralForm.TFindDisplayPinArray }

function TPeripheralForm.TFindDisplayPinArray.Find(AElement: TDisplayPinArray): Boolean;
begin
  Result := AElement.FPinArray = FPinArray;
end;

constructor TPeripheralForm.TFindDisplayPinArray.Create(APinArray: TPinArray);
begin
  FPinArray := APinArray;
end;

{ TDisplayPinConnection }

procedure TDisplayPinConnection.InitDrawConnection;
begin
  with FCanvas do
  begin
    Pen.Style := psSolid;
    Pen.Width := 8;
    Pen.Color := $2FFF2F;
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
  Result.X := FPosition.X + Width - 10 - 20 * AIndex;
  if FFlipped then
    Result.Y := FPosition.Y + 10
  else
    Result.Y := FPosition.Y + Height - 10;
end;

function TDisplayPinArray.GetWidth: Integer;
begin
  Result := Max(20 + 20 * (FPinArray.Count - 1), FCanvas.GetTextWidth(FPinArray.Name) + 10);
end;

function TDisplayPinArray.GetBoxCaptionPosition: TPoint;
begin
  Result.X := FPosition.X + (Width - FCanvas.TextWidth(FPinArray.Name)) div 2;
  if FFlipped then
    Result.Y := FPosition.Y + Height - 2 - FCanvas.TextHeight(FPinArray.Name)
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
  Result.X := PinPosition[AIndex].X - FCanvas.TextWidth(AIndex.ToString) div 2;
  Result.Y := FPosition.Y + (Height - FCanvas.TextHeight(AIndex.ToString)) div 2;
end;

function TDisplayPinArray.GetPinCount: Integer;
begin
  Result := FPinArray.Count;
end;

procedure TDisplayPinArray.InitDrawPin;
begin
  with FCanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := $FF4F1F;
    Pen.Style := psSolid;
    Pen.Width := 2;
    Pen.Color := $CF3F1F;
  end;
end;

procedure TDisplayPinArray.InitDrawBoxCaption;
begin
  with FCanvas do
  begin
    Brush.Style := bsClear;
  end;
end;

procedure TDisplayPinArray.InitDrawPinCaption;
begin
  with FCanvas do
  begin
    Brush.Style := bsClear;
  end;
end;

procedure TDisplayPinArray.InitDrawBox;
begin
  with FCanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := $BFAF9F;
    Pen.Style := psSolid;
    Pen.Color := $AF9F8F;
    Pen.Width := 2;
  end;
end;

constructor TDisplayPinArray.Create(ACanvas: TCanvas; APosition: TPoint; APinArray: TPinArray; AFlipped: Boolean);
begin
  FCanvas := ACanvas;
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
  FCanvas.Rectangle(TRect.Create(FPosition, Width, Height)); //TRect.Create(FPosition, Width, Height));

  // Draw PinArray Caption
  InitDrawBoxCaption;
  P := BoxCaptionPosition;
  FCanvas.TextOut(P.X, P.Y, FPinArray.Name);

  // Draw Pins
  InitDrawPin;
  for I := 0 to FPinArray.Count - 1 do
    FCanvas.Ellipse(PinPosition[I].X - 6, PinPosition[I].Y - 6, PinPosition[I].X + 6, PinPosition[I].Y + 6);

  // Draw Pin Captions
  InitDrawPinCaption;
  for I := 0 to FPinArray.Count - 1 do
  begin
    P := PinCaptionPosition[I];
    FCanvas.TextOut(P.X, P.Y, I.ToString);
  end;

end;

{ TPeripheralForm }

procedure TPeripheralForm.pbDrawSurfacePaint(Sender: TObject);
begin
  pbDrawSurface.Canvas.Pen.Style := psClear;
  pbDrawSurface.Canvas.Brush.Style := bsSolid;
  pbDrawSurface.Canvas.Brush.Color := clBtnFace;
  pbDrawSurface.Canvas.Rectangle(0, 0, DrawSurfaceWidth, DrawSurfaceHeight);
  DrawPeripheral;
end;

procedure TPeripheralForm.pbPinsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft:
    begin
      FClickedPinArray := FindPinArrayAt(FPinMousePos);
      if FClickedPinArray <> nil then
      begin
        FClickedPin := FindPinAt(FClickedPinArray, FPinMousePos);
        Invalidate;
      end;
    end;
  end;
end;

procedure TPeripheralForm.pbPinsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FPinMousePos := Point(X, Y);
  if FClickedPin <> -1 then
  begin
    pbPins.Canvas.Line(FClickedPinArray.PinPosition[FClickedPin], FPinMousePos);
    Invalidate;
  end
  else if FClickedPinArray <> nil then
  begin

  end;
end;

procedure TPeripheralForm.pbPinsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  EndPinArray: TDisplayPinArray;
  EndPin: Integer;
begin
  if FClickedPin <> -1 then
  begin
    EndPinArray := FindPinArrayAt(FPinMousePos);
    if (EndPinArray <> nil) and (EndPinArray <> FClickedPinArray) then
    begin
      EndPin := FindPinAt(EndPinArray, FPinMousePos);
      if EndPin <> -1 then
      begin
        FClickedPinArray.Pins[FClickedPin].Connect(EndPinArray.Pins[EndPin]);
      end;
    end;

    FClickedPinArray := nil;
    FClickedPin := -1;

    Invalidate;
  end;
end;

procedure TPeripheralForm.pbPinsPaint(Sender: TObject);
var
  DisplayPinArray: TDisplayPinArray;
  DisplayPinConnection: TDisplayPinConnection;
begin
  pbPins.Canvas.Pen.Style := psClear;
  pbPins.Canvas.Brush.Style := bsSolid;
  pbPins.Canvas.Brush.Color := clBtnFace;
  pbPins.Canvas.Rectangle(0, 0, PinSurfaceWidth, PinSurfaceHeight);

  for DisplayPinArray in FDisplayPinArrays do
    DisplayPinArray.Draw;
  for DisplayPinConnection in FDisplayPinConnections do
    DisplayPinConnection.Draw;

  if FClickedPin <> -1 then
  begin
    pbPins.Canvas.Pen.Style := psSolid;
    pbPins.Canvas.Pen.Color := $0F1FEF;
    pbPins.Canvas.Pen.Width := 7;
    pbPins.Canvas.Pen.EndCap := pecRound;
    pbPins.Canvas.Line(FClickedPinArray.PinPosition[FClickedPin], FPinMousePos);
  end;
end;

procedure TPeripheralForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TPeripheralForm.actShowPinsExecute(Sender: TObject);
begin
  PinsVisible := not PinsVisible;
end;

procedure TPeripheralForm.actShowPinsUpdate(Sender: TObject);
begin
  if PinsVisible then
    actShowPins.Caption := 'Hide Pins'
  else
    actShowPins.Caption := 'Show Pins';
end;

procedure TPeripheralForm.actVisiblePinsExecute(Sender: TObject);
begin
  frmVisiblePinSelection.Execute(FPinArray);
  GenerateDisplayPins;
end;

procedure TPeripheralForm.actVisiblePinsUpdate(Sender: TObject);
begin
  actVisiblePins.Enabled := PinsVisible;
end;

function TPeripheralForm.GetDrawSurfaceHeight: Integer;
begin
  Result := pbDrawSurface.Height;
end;

function TPeripheralForm.GetDrawSurfaceWidth: Integer;
begin
  Result := pbDrawSurface.Width;
end;

function TPeripheralForm.GetPinSurfaceHeight: Integer;
begin
  Result := pbPins.Height;
end;

function TPeripheralForm.GetPinSurfaceWidth: Integer;
begin
  Result := pbPins.Width;
end;

procedure TPeripheralForm.OnVisibilityChanged(Sender: TPinArray);
begin
  Invalidate;
end;

procedure TPeripheralForm.SetDrawSurfaceHeight(AValue: Integer);
begin
  Height := Height - DrawSurfaceHeight + AValue;
end;

procedure TPeripheralForm.SetDrawSurfaceWidth(AValue: Integer);
begin
  Width := Width - DrawSurfaceWidth + AValue;
end;

procedure TPeripheralForm.SetPeripheralName(AValue: String);
begin
  AValue := MakeUniqueName(AValue);
  if FPeripheralName = AValue then
    Exit;
  FPeripheralName := AValue;
  Caption := 'Perpiheral - ' + PeripheralName;
end;

procedure TPeripheralForm.SetPinSurfaceHeight(AValue: Integer);
var
  HeightDiff: Integer;
begin
  HeightDiff := AValue - PinSurfaceHeight;
  Height := Height + HeightDiff;
  gbPins.Height := gbPins.Height + HeightDiff;
end;

procedure TPeripheralForm.SetPinSurfaceWidth(AValue: Integer);
var
  WidthDiff: Integer;
begin
  WidthDiff := AValue - PinSurfaceWidth;
  Width := Width + WidthDiff;
  gbPins.Width := gbPins.Width + WidthDiff;
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

procedure TPeripheralForm.OnPinConnect(AIndex: Cardinal; AOther: TPin);
begin
  GenerateDisplayPins;
end;

procedure TPeripheralForm.OnPinDisconnect(AIndex: Cardinal; AOther: TPin);
begin
  GenerateDisplayPins;
end;

procedure TPeripheralForm.SetPinsVisible(AValue: Boolean);
begin
  if FPinsVisible = AValue then
    Exit;
  FPinsVisible := AValue;
  DisableAlign;
  if PinsVisible then
  begin
    Height := Height + gbPins.Height + gbPins.BorderSpacing.Around;
    gbPins.Visible := True
  end
  else
  begin
    Height := Height - gbPins.Height - gbPins.BorderSpacing.Around;
    gbPins.Visible := False;
  end;
  EnableAlign;
  UpdateActions;
end;

procedure TPeripheralForm.GenerateDisplayPins;
var
  I, L, Connections: Integer;
  DisplayPinArray, Main: TDisplayPinArray;
  Connected: TPin;
begin
  FDisplayPinArrays.DelAll;
  FDisplayPinConnections.DelAll;

  L := 10;
  for I := 0 to FPinArray.VisiblePinArrayCount - 1 do
  begin
    DisplayPinArray := FDisplayPinArrays.Add(TDisplayPinArray.Create(
      pbPins.Canvas,
      Point(L, pbPins.Height - TDisplayPinArray.Height - 10),
      FPinArray.VisiblePinArrays[I],
      True));
    L := L + DisplayPinArray.Width + 10;
  end;

  Main := FDisplayPinArrays.Add(TDisplayPinArray.Create(pbPins.Canvas, Point(10, 10), FPinArray));

  Connections := 0;
  for I := 0 to FPinArray.Count - 1 do
  begin
    for Connected in FPinArray.Pins[I] do
    begin
      DisplayPinArray := FDisplayPinArrays.FindFirst(TFindDisplayPinArray.Create(Connected.PinArray));

      FDisplayPinConnections.Add(TDisplayPinConnection.Create(
        pbPins.Canvas,
        Main, DisplayPinArray,
        I, Connected.Index));
      Inc(Connections);
    end;
  end;

  Caption := Connections.ToString;

  pbPins.Invalidate;
end;

function TPeripheralForm.FindPinAt(APinArray: TDisplayPinArray; APoint: TPoint): Integer;
var
  I: Integer;
begin
  for I := 0 to APinArray.PinCount - 1 do
    if TPoint.PointInCircle(APoint, APinArray.PinPosition[I], 8) then
      Exit(I);
  Result := -1;
end;

function TPeripheralForm.FindPinArrayAt(APoint: TPoint): TDisplayPinArray;
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

procedure TPeripheralForm.OnPinChanged(APinIndex: Cardinal);
begin
  // nothing by default
end;

constructor TPeripheralForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  PeripheralName := GetDefaultPeripheralName;
  PinSurfaceHeight := 150;
  FPinArray := TPinArray.Create(PeripheralName, OnPinChanged, 4);
  FPinArray.OnPinConnect := OnPinConnect;
  FPinArray.OnPinDisconnect := OnPinDisconnect;
  FPinArray.OnVisibilityChange := OnVisibilityChanged;
  FPinsVisible := True;
  FDisplayPinArrays := TObjectArray<TDisplayPinArray>.Create;
  FDisplayPinConnections := TObjectArray<TDisplayPinConnection>.Create;
  FClickedPin := -1;
  GenerateDisplayPins;
end;

destructor TPeripheralForm.Destroy;
begin
  FPinArray.Free;
  FDisplayPinArrays.Free;
  FDisplayPinConnections.Free;
  inherited Destroy;
end;

end.

