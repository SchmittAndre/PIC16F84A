unit PeripheralLEDArray;

interface

uses
  Classes, SysUtils, PeripheralFormDefine, Graphics, Dialogs, Color, PinDefine, Lists, Controls;

type

  { TBaseLED }

  TBaseLED = class
  private
    FColor: TColorRGB;
    FOffFactor: Single;
    FState: Boolean;

    FPin: TPin;
    FControl: TGraphicControl;

    function GetCanvas: TCanvas;
    procedure SetColor(AValue: TColorRGB);
    procedure SetOffFactor(AValue: Single);
    procedure SetState(AValue: Boolean);

  protected
    procedure NotifyChanges;

    property Canvas: TCanvas read GetCanvas;

  public
    constructor Create(AControl: TGraphicControl; APin: TPin);

    property Color: TColorRGB read FColor write SetColor;
    property OffFactor: Single read FOffFactor write SetOffFactor;
    property State: Boolean read FState write SetState;

    procedure Draw; virtual; abstract;
  end;

  { TRoundLED }

  TRoundLED = class (TBaseLED)
  private
    FLeft: Integer;
    FTop: Integer;
    FWidth: Cardinal;
    FHeight: Cardinal;

    procedure SetHeight(AValue: Cardinal);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Cardinal);

  public
    constructor Create(AControl: TGraphicControl; APin: TPin);

    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Cardinal read FWidth write SetWidth;
    property Height: Cardinal read FHeight write SetHeight;

    procedure Draw; override;
  end;

  { TPeripheralLEDArray }

  TPeripheralLEDArray = class (TPeripheralForm)
  private
    FLEDs: TObjectArray<TBaseLED>;

  protected
    procedure DrawPeripheral; override;
    function GetName: String; override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{ TRoundLED }

procedure TRoundLED.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then Exit;
  FLeft := AValue;
end;

procedure TRoundLED.SetHeight(AValue: Cardinal);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
  NotifyChanges;
end;

procedure TRoundLED.SetTop(AValue: Integer);
begin
  if FTop = AValue then
    Exit;
  FTop := AValue;
  NotifyChanges;
end;

procedure TRoundLED.SetWidth(AValue: Cardinal);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
  NotifyChanges;
end;

constructor TRoundLED.Create(AControl: TGraphicControl; APin: TPin);
begin
  inherited Create(AControl, APin);
  Width := 50;
  Height := 50;
end;

procedure TRoundLED.Draw;
begin
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Style := bsSolid;
  if State then
    Canvas.Brush.Color := Color.ToWinColor
  else
    Canvas.Brush.Color := (Color * OffFactor).ToWinColor;
  Canvas.Ellipse(Left, Top, Left + Width, Top + Height);
end;

{ TBaseLED }

procedure TBaseLED.SetColor(AValue: TColorRGB);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  NotifyChanges;
end;

function TBaseLED.GetCanvas: TCanvas;
begin
  Result := FControl.Canvas;
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
  if FState = AValue then
    Exit;
  FState := AValue;
  NotifyChanges;
end;

procedure TBaseLED.NotifyChanges;
begin
  FControl.Invalidate;
end;

constructor TBaseLED.Create(AControl: TGraphicControl; APin: TPin);
begin
  FControl := AControl;
  FPin := APin;
end;

{ TPeripheralLEDArray }

procedure TPeripheralLEDArray.DrawPeripheral;
var
  LED: TBaseLED;
begin
  for LED in FLEDs do
    LED.Draw;
end;

function TPeripheralLEDArray.GetName: String;
begin
  Result := 'LED-Array';
end;

constructor TPeripheralLEDArray.Create(TheOwner: TComponent);
var
  I: Integer;
  LED: TRoundLED;
begin
  inherited Create(TheOwner);
  FLEDs := TObjectArray<TBaseLED>.Create;
  for I := 0 to 3 do
  begin
    LED := TRoundLED.Create(pbDrawSurface, nil);
    LED.Left := I * 60;
    LED.Top := 0;
    LED.Width := 50;
    LED.Height := 50;
    LED.Color := TColorRGB.Create(1, 0, 0);
    LED.OffFactor := 0.2;
    FLEDs.Add(LED);
  end;
  DrawSurfaceWidth := 230;
  DrawSurfaceHeight := 50;
end;

destructor TPeripheralLEDArray.Destroy;
begin
  FLEDs.Free;
  inherited Destroy;
end;

end.

