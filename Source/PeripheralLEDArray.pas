unit PeripheralLEDArray;

interface

uses
  Classes, SysUtils, PeripheralFormDefine, Graphics, Dialogs, Color, Lists, Controls, LEDDefine;

type

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
  DrawSurfaceWidth := 4 * 60 - 10;
  DrawSurfaceHeight := 50;
end;

destructor TPeripheralLEDArray.Destroy;
begin
  FLEDs.Free;
  inherited Destroy;
end;

end.

