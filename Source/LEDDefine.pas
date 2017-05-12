unit LEDDefine;

interface

uses
  Classes, SysUtils, Color, PinDefine, Graphics, Controls, Math, Lists;

type

  { TBaseLED }

  TBaseLED = class
  private
    FColor: TColorRGB;
    FOffFactor: Single;
    FState: Boolean;

    FPin: TPin;
    FControl: TGraphicControl;

    FLeft: Integer;
    FTop: Integer;
    FWidth: Cardinal;
    FHeight: Cardinal;

    FInverted: Boolean;

    function GetCanvas: TCanvas;
    function GetCurrentColor: TColorRGB;
    procedure SetColor(AValue: TColorRGB);
    procedure SetInverted(AValue: Boolean);
    procedure SetOffFactor(AValue: Single);
    procedure SetState(AValue: Boolean);

    procedure SetHeight(AValue: Cardinal);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Cardinal);

    procedure OnPinChange(APin: TPin);

    class function GetShapeName: String; virtual; abstract;

    property State: Boolean read FState write SetState;

  protected
    procedure NotifyChanges;

    property Canvas: TCanvas read GetCanvas;

  public
    constructor Create(AControl: TGraphicControl; APin: TPin); virtual;

    property Color: TColorRGB read FColor write SetColor;
    property OffFactor: Single read FOffFactor write SetOffFactor;
    property Powered: Boolean read FState;

    property CurrentColor: TColorRGB read GetCurrentColor;

    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Cardinal read FWidth write SetWidth;
    property Height: Cardinal read FHeight write SetHeight;
    property Inverted: Boolean read FInverted write SetInverted;

    procedure Draw; virtual;

    property TypeName: String read GetShapeName;
  end;

  TLEDClass = class of TBaseLED;

  { TRoundLED }

  TRoundLED = class (TBaseLED)
  public
    class function GetShapeName: String; override;
    procedure Draw; override;
  end;

  { TRectLED }

  TRectLED = class (TBaseLED)
  public
    class function GetShapeName: String; override;
    procedure Draw; override;
  end;

  { TStarLED }

  TStarLED = class (TBaseLED)
  public
    class function GetShapeName: String; override;
    procedure Draw; override;
  end;

  { TLEDArray }

  TLEDArray = class
  public
    type

      TLEDCount = 0 .. 8;

      TLEDArrayChangeEvent = procedure (ALEDArray: TLEDArray) of object;

  private
    FControl: TGraphicControl;

    FPinArray: TPinArray;

    FLEDs: TObjectArray<TBaseLED>;

    FLEDClass: TLEDClass;

    FLEDColor: TColorRGB;
    FLEDOffFactor: Single;
    FLEDInverted: Boolean;

    FLEDWidth: Integer;
    FLEDHeight: Integer;

    FReverseOrder: Boolean;

    FChanged: Boolean;

    function GetDisplayHeight: Integer;
    function GetDisplayWidth: Integer;

    function GetLEDCount: TLEDCount;

    procedure SetLEDCount(AValue: TLEDCount);
    procedure SetLEDClass(AValue: TLEDClass);

    procedure SetLEDColor(AValue: TColorRGB);
    procedure SetLEDOffFactor(AValue: Single);
    procedure SetLEDInverted(AValue: Boolean);

    procedure SetLEDWidth(AValue: Integer);
    procedure SetLEDHeight(AValue: Integer);

    procedure SetReverseOrder(AValue: Boolean);

    procedure GenerateLEDs;

  protected
    procedure NotifyChanges;

  public

    OnWidthChanged: TDelegate<TLEDArrayChangeEvent>;
    OnHeightChanged: TDelegate<TLEDArrayChangeEvent>;

    constructor Create(AControl: TGraphicControl; APinArray: TPinArray);
    destructor Destroy; override;

    procedure Draw;

    property LEDCount: TLEDCount read GetLEDCount write SetLEDCount;
    property LEDType: TLEDClass read FLEDClass write SetLEDClass;

    property LEDColor: TColorRGB read FLEDColor write SetLEDColor;
    property LEDOffFactor: Single read FLEDOffFactor write SetLEDOffFactor;
    property LEDInverted: Boolean read FLEDInverted write SetLEDInverted;

    property LEDWidth: Integer read FLEDWidth write SetLEDWidth;
    property LEDHeight: Integer read FLEDHeight write SetLEDHeight;

    property DisplayWidth: Integer read GetDisplayWidth;
    property DisplayHeight: Integer read GetDisplayHeight;

    property ReverseOrder: Boolean read FReverseOrder write SetReverseOrder;

  end;

implementation

{ TStarLED }

class function TStarLED.GetShapeName: String;
begin
  Result := 'Star';
end;

procedure TStarLED.Draw;
const
  Peaks = 5;
var
  Points: array [0 .. Peaks * 2 - 1] of TPoint;
  I: Integer;
  W, RW, RH: Single;
  M: TPoint;
begin
  inherited Draw;
  M := Point(Left + Width div 2, Top + Height div 2);
  RW := Width / 2;
  RH := Height / 2;
  for I := 0 to Peaks * 2 - 1 do
  begin
    W := I / Peaks * Pi;
    if I mod 2 = 0 then
      Points[I] := M + Point(Floor(Sin(W) * RW + 0.5), Floor(-Cos(W) * RH + 0.5))
    else
      Points[I] := M + Point(Floor(Sin(W) * RW * 0.5 + 0.5), Floor(-Cos(W) * RH * 0.5 + 0.5));
  end;
  FControl.Canvas.Polygon(Points);
end;

{ TRectLED }

class function TRectLED.GetShapeName: String;
begin
  Result := 'Rectangle';
end;

procedure TRectLED.Draw;
begin
  inherited;
  FControl.Canvas.Rectangle(TRect.Create(Point(Left, Top), Width, Height));
end;

{ TBaseLED }

procedure TBaseLED.SetColor(AValue: TColorRGB);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  NotifyChanges;
end;

procedure TBaseLED.SetInverted(AValue: Boolean);
begin
  if FInverted = AValue then
    Exit;
  FInverted := AValue;
  NotifyChanges;
end;

function TBaseLED.GetCanvas: TCanvas;
begin
  Result := FControl.Canvas;
end;

function TBaseLED.GetCurrentColor: TColorRGB;
begin
  if Powered <> Inverted then
    Result := Color
  else
    Result := Color * OffFactor;
end;

procedure TBaseLED.SetOffFactor(AValue: Single);
begin
  if FOffFactor = AValue then
    Exit;
  FOffFactor := AValue;
  NotifyChanges;
end;

procedure TBaseLED.SetState(AValue: Boolean);
begin
  if State = AValue then
    Exit;
  FState := AValue;
  NotifyChanges;
end;

procedure TBaseLED.OnPinChange(APin: TPin);
begin
  if APin = FPin then
    State := APin.State;
end;

procedure TBaseLED.NotifyChanges;
begin
  FControl.Invalidate;
end;

constructor TBaseLED.Create(AControl: TGraphicControl; APin: TPin);
begin
  FControl := AControl;
  FPin := APin;
  FPin.PinArray.OnPinChange.Add(OnPinChange);
end;

procedure TBaseLED.Draw;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := (CurrentColor * 0.7).ToWinColor;
  Canvas.Pen.Width := 5;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := CurrentColor.ToWinColor;
end;

procedure TBaseLED.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then Exit;
  FLeft := AValue;
end;

procedure TBaseLED.SetHeight(AValue: Cardinal);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
  NotifyChanges;
end;

procedure TBaseLED.SetTop(AValue: Integer);
begin
  if FTop = AValue then
    Exit;
  FTop := AValue;
  NotifyChanges;
end;

procedure TBaseLED.SetWidth(AValue: Cardinal);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
  NotifyChanges;
end;

{ TRoundLED }

class function TRoundLED.GetShapeName: String;
begin
  Result := 'Round';
end;

procedure TRoundLED.Draw;
begin
  inherited;
  Canvas.Ellipse(Left, Top, Left + Width, Top + Height);
end;

{ TLEDArray }

function TLEDArray.GetLEDCount: TLEDCount;
begin
  Result := FPinArray.Count;
end;

function TLEDArray.GetDisplayHeight: Integer;
begin
  Result := LEDHeight + 10;
end;

function TLEDArray.GetDisplayWidth: Integer;
begin
  Result := (LEDWidth + 10) * LEDCount;
end;

procedure TLEDArray.SetLEDClass(AValue: TLEDClass);
begin
  if FLEDClass = AValue then
    Exit;
  FLEDClass := AValue;
  NotifyChanges;
end;

procedure TLEDArray.SetLEDColor(AValue: TColorRGB);
begin
  if FLEDColor = AValue then
    Exit;
  FLEDColor := AValue;
  NotifyChanges;
end;

procedure TLEDArray.SetLEDCount(AValue: TLEDCount);
begin
  if LEDCount = AValue then
    Exit;
  FPinArray.Count := AValue;
  NotifyChanges;
end;

procedure TLEDArray.GenerateLEDs;
var
  LED: TBaseLED;
  I: Integer;
begin
  FLEDs.DelAll;
  for I := 0 to LEDCount - 1 do
  begin
    LED := LEDType.Create(FControl, FPinArray.Pins[I]);
    if ReverseOrder then
      LED.Left := 5 + I * (LEDWidth + 10)
    else
      LED.Left := 5 + (LEDCount - 1 - I) * (LEDWidth + 10);
    LED.Top := 5;
    LED.Width := LEDWidth;
    LED.Height := LEDHeight;
    LED.Color := LEDColor;
    LED.OffFactor := LEDOffFactor;
    FLEDs.Add(LED);
  end;
  FChanged := False;
end;

procedure TLEDArray.SetLEDHeight(AValue: Integer);
begin
  if FLEDHeight = AValue then
    Exit;
  FLEDHeight := AValue;
  NotifyChanges;
end;

procedure TLEDArray.SetLEDInverted(AValue: Boolean);
begin
  if FLEDInverted = AValue then
    Exit;
  FLEDInverted := AValue;
  NotifyChanges;
end;

procedure TLEDArray.SetLEDOffFactor(AValue: Single);
begin
  if FLEDOffFactor = AValue then
    Exit;
  FLEDOffFactor := AValue;
  NotifyChanges;
end;

procedure TLEDArray.SetLEDWidth(AValue: Integer);
begin
  if FLEDWidth = AValue then
    Exit;
  FLEDWidth := AValue;
  NotifyChanges;
end;

procedure TLEDArray.SetReverseOrder(AValue: Boolean);
begin
  if FReverseOrder = AValue then
    Exit;
  FReverseOrder := AValue;
  NotifyChanges;
end;

procedure TLEDArray.Draw;
var
  LED: TBaseLED;
begin
  if FChanged then
    GenerateLEDs;
  for LED in FLEDs do
    LED.Draw;
end;

procedure TLEDArray.NotifyChanges;
begin
  FChanged := True;
end;

constructor TLEDArray.Create(AControl: TGraphicControl; APinArray: TPinArray);
begin
  FControl := AControl;
  FPinArray := APinArray;
  FLEDs := TObjectArray<TBaseLED>.Create;
  FLEDClass := TStarLED;
  FLEDColor := TColorRGB.Create(1, 0, 0);
  FLEDHeight := 50;
  FLEDWidth := 50;
  FLEDOffFactor := 0.3;
  LEDCount := 4;
end;

destructor TLEDArray.Destroy;
begin
  FLEDs.Free;
  inherited Destroy;
end;

end.

