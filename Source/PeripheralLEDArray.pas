unit PeripheralLEDArray;

interface

uses
  Classes, SysUtils, PeripheralFormDefine, Graphics, Dialogs, Controls, LEDDefine, PinDefine, Math,
  Menus, ActnList;

type

  { TPeripheralLEDArray }

  TPeripheralLEDArray = class (TPeripheralForm)
  public
    type

      TLEDCount = 0 .. 8;

  private
    FLEDArray: TLEDArray;

    procedure OnShowSettings(Sender: TObject);

    procedure OnLEDArrayWidthChange(ALEDArray: TLEDArray);
    procedure OnLEDArrayHeightChange(ALEDArray: TLEDArray);

  protected
    procedure DrawPeripheral; override;

    function GetMinWidth: Integer; override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    class function GetDefaultPeripheralName: String; override;

  end;

implementation

{ TPeripheralLEDArray }

procedure TPeripheralLEDArray.OnShowSettings(Sender: TObject);
begin
  raise ENotImplemented.Create('LED-Array Settings not implemented');
end;

procedure TPeripheralLEDArray.OnLEDArrayWidthChange(ALEDArray: TLEDArray);
begin
  AutoWidth;
end;

procedure TPeripheralLEDArray.OnLEDArrayHeightChange(ALEDArray: TLEDArray);
begin
  DrawSurfaceHeight := FLEDArray.DisplayHeight;
end;

procedure TPeripheralLEDArray.DrawPeripheral;
begin
  FLEDArray.Draw;
end;

class function TPeripheralLEDArray.GetDefaultPeripheralName: String;
begin
  Result := 'LED-Array';
end;

function TPeripheralLEDArray.GetMinWidth: Integer;
begin
  Result := inherited GetMinWidth;
  Result := Max(Result, FLEDArray.DisplayWidth);
end;

constructor TPeripheralLEDArray.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLEDArray := TLEDArray.Create(pbDrawSurface, PinArray);
  FLEDArray.OnWidthChanged.Add(OnLEDArrayWidthChange);
  FLEDArray.OnHeightChanged.Add(OnLEDArrayHeightChange);
  OnLEDArrayHeightChange(FLEDArray);
  SetSettingsFunc(OnShowSettings);
end;

destructor TPeripheralLEDArray.Destroy;
begin
  FLEDArray.OnWidthChanged.Del(OnLEDArrayWidthChange);
  FLEDArray.OnHeightChanged.Del(OnLEDArrayHeightChange);
  FLEDArray.Free;
  inherited Destroy;
end;

end.

