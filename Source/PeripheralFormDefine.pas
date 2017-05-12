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

    FCanvas: TCanvas;

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
    pbDrawSurface: TPaintBox;
    pbPins: TPaintBox;
    pmPeripheral: TPopupMenu;
    pmPins: TPopupMenu;
    procedure actShowPinsExecute(Sender: TObject);
    procedure actShowPinsUpdate(Sender: TObject);
    procedure actVisiblePinsExecute(Sender: TObject);
    procedure actVisiblePinsUpdate(Sender: TObject);
    procedure pbDrawSurfaceMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbDrawSurfaceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbDrawSurfaceMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbDrawSurfacePaint(Sender: TObject);
    procedure pbPinsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbPinsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbPinsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbPinsPaint(Sender: TObject);
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
    procedure OnPinRedirect(APin: TPin);

    procedure SetDrawSurfaceHeight(AValue: Integer);
    procedure SetPinSurfaceHeight(AValue: Integer);

    procedure OnPinConnectionChange(ASelf, AOther: TPin);
    procedure OnVisibilityChanged(Sender: TPinArray);

    procedure SetPinsVisible(AValue: Boolean);

    function FindPinAt(APinArray: TDisplayPinArray; APoint: TPoint): Integer;
    function FindPinArrayAt(APoint: TPoint): TDisplayPinArray;

    procedure AddVisiblePinArrayEvents;
    procedure DelVisiblePinArrayEvents;

  protected
    procedure DrawPeripheral; virtual; abstract;
    procedure OnPinChange({%H-}APin: TPin); virtual;

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
    FPeripheralName: String;

    procedure SetPeripheralName(AValue: String);
    function MakeUniqueName(AName: String): String;

    procedure OnFormClose(Sender: TObject; var CloseAction: TCloseAction);

  protected
    function GetPinArray: TPinArray; override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    class function GetDefaultPeripheralName: String; virtual; abstract;

    property PeripheralName: String read FPeripheralName write SetPeripheralName;
  end;

  TPeripheralFormClass = class of TPeripheralForm;

implementation

{$R *.lfm}

{ TPeripheralForm }

procedure TPeripheralForm.SetPeripheralName(AValue: String);
begin
  AValue := MakeUniqueName(AValue);
  if PeripheralName = AValue then
    Exit;
  FPeripheralName := AValue;
  if Assigned(FPinArray) then
    FPinArray.Name := AValue;
  Caption := 'Perpiheral - ' + PeripheralName;
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
  PeripheralName := GetDefaultPeripheralName;
  FPinArray := TPinArray.Create(PeripheralName);
  OnClose := OnFormClose;
end;

destructor TPeripheralForm.Destroy;
begin
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
  Result := Max(PinDistance * FPinArray.Count, FCanvas.GetTextWidth(FPinArray.Name) + 10);
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

procedure TDisplayPinArray.InitDrawPin(APin: TPin);
var
  C: TColorRGB;
begin
  with FCanvas do
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
    Brush.Color := $FFBFCF;
    Pen.Style := psSolid;
    Pen.Color := $EF8F9F;
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
  for I := 0 to FPinArray.Count - 1 do
  begin
    InitDrawPin(Pins[I]);
    FCanvas.Ellipse(PinPosition[I].X - 6, PinPosition[I].Y - 6, PinPosition[I].X + 6, PinPosition[I].Y + 6);
    // FCanvas.TextOut(PinPosition[I].X - 6, PinPosition[I].Y - 6, PinArray[I].PowerSources.ToString);
  end;

  // Draw Pin Captions
  InitDrawPinCaption;
  for I := 0 to FPinArray.Count - 1 do
  begin
    P := PinCaptionPosition[I];
    FCanvas.TextOut(P.X, P.Y, I.ToString);
  end;

end;

{ TPinConnectionForm }

procedure TPinConnectionForm.pbDrawSurfacePaint(Sender: TObject);
begin
  pbDrawSurface.Canvas.Pen.Style := psClear;
  pbDrawSurface.Canvas.Brush.Style := bsSolid;
  pbDrawSurface.Canvas.Brush.Color := clBtnFace;
  pbDrawSurface.Canvas.Rectangle(0, 0, pbDrawSurface.Width, pbDrawSurface.Height);
  DrawPeripheral;
end;

procedure TPinConnectionForm.pbPinsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPinMousePos := Point(X, Y);
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

procedure TPinConnectionForm.pbPinsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FPinMousePos := Point(X, Y);
  if FClickedPinArray <> nil then
  begin
    Invalidate;
  end;
end;

procedure TPinConnectionForm.pbPinsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
        Invalidate;
      end;
    end;
  end;
end;

procedure TPinConnectionForm.pbPinsPaint(Sender: TObject);
var
  DisplayPinArray: TDisplayPinArray;
  DisplayPinConnection: TDisplayPinConnection;
  I: Integer;
  Offset: TPoint;
begin
  pbPins.Canvas.Pen.Style := psClear;
  pbPins.Canvas.Brush.Style := bsSolid;
  pbPins.Canvas.Brush.Color := clBtnFace;
  pbPins.Canvas.Rectangle(0, 0, pbPins.Width, pbPins.Height);

  for DisplayPinArray in FDisplayPinArrays do
    DisplayPinArray.Draw;
  for DisplayPinConnection in FDisplayPinConnections do
    DisplayPinConnection.Draw;

  if FClickedPinArray <> nil then
  begin
    pbPins.Canvas.Pen.Style := psSolid;
    pbPins.Canvas.Pen.Color := $0F1FEF;
    pbPins.Canvas.Pen.Width := 6;
    pbPins.Canvas.Pen.EndCap := pecRound;
    if FClickedPin <> -1 then
    begin
      pbPins.Canvas.Line(FClickedPinArray.PinPosition[FClickedPin], FPinMousePos);
    end
    else
    begin
      for I := 0 to FClickedPinArray.PinCount - 1 do
      begin
        Offset := Point(I * TDisplayPinArray.PinDistance, 0);
        pbPins.Canvas.Line(FClickedPinArray.PinPosition[0] - Offset, FPinMousePos - Offset);
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

procedure TPinConnectionForm.pbDrawSurfaceMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  DrawSurfaceMouseDown(X, Y, Button);
end;

procedure TPinConnectionForm.pbDrawSurfaceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  DrawSurfaceMouseMove(X, Y);
end;

procedure TPinConnectionForm.pbDrawSurfaceMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  DrawSurfaceMouseUp(X, Y, Button);
end;

function TPinConnectionForm.GetDrawSurfaceHeight: Integer;
begin
  Result := pbDrawSurface.Height;
end;

function TPinConnectionForm.GetDrawSurfaceWidth: Integer;
begin
  Result := pbDrawSurface.Width;
end;

function TPinConnectionForm.GetPinSurfaceHeight: Integer;
begin
  Result := pbPins.Height;
end;

function TPinConnectionForm.GetPinSurfaceWidth: Integer;
begin
  Result := pbPins.Width;
end;

procedure TPinConnectionForm.OnPinRedirect(APin: TPin);
begin
  Invalidate;
end;

procedure TPinConnectionForm.OnVisibilityChanged(Sender: TPinArray);
begin
  DelVisiblePinArrayEvents;
  AddVisiblePinArrayEvents;
  GeneratePinArrays;
  AutoWidth;
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

procedure TPinConnectionForm.OnPinConnectionChange(ASelf, AOther: TPin);
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
  I, L: Integer;
  DisplayPinArray: TDisplayPinArray;
begin
  FDisplayPinArrays.DelAll;

  FDisplayPinArrays.Add(TDisplayPinArray.Create(pbPins.Canvas, Point(10, 10), PinArray));

  PinArray.OnVisibilityChange.Add(OnVisibilityChanged);

  L := 10;
  for I := 0 to PinArray.VisiblePinArrayCount - 1 do
  begin
    DisplayPinArray := FDisplayPinArrays.Add(TDisplayPinArray.Create(
      pbPins.Canvas,
      Point(L, pbPins.Height - TDisplayPinArray.Height - 10),
      PinArray.VisiblePinArrays[I],
      True));
    L := L + DisplayPinArray.Width + 10;
  end;

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
        pbPins.Canvas,
        Main, DisplayPinArray,
        I, Connected.Index));
    end;
  end;

  pbPins.Invalidate;
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
  end;
end;

procedure TPinConnectionForm.OnPinChange(APin: TPin);
begin
  Invalidate;
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
    Result := Max(Result, FDisplayPinArrays.First.Bounds.Right + 10 + ClientWidth - pbPins.Width);
    Result := Max(Result, FDisplayPinArrays.Last.Bounds.Right + 10 + ClientWidth - pbPins.Width);
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
  PinArray.OnPinConnect.Add(OnPinConnectionChange);
  PinArray.OnPinDisconnect.Add(OnPinConnectionChange);
  PinArray.OnVisibilityChange.Add(OnVisibilityChanged);
  PinArray.OnPinRedirect.Add(OnPinRedirect);
  GeneratePinArrays;
  AutoWidth;
end;

procedure TPinConnectionForm.BeforeDestruction;
begin
  DelVisiblePinArrayEvents;
  PinArray.OnPinChange.Del(OnPinChange);
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

