unit PeripheralLEDArray;

interface

uses
  Classes, SysUtils, PeripheralFormDefine, Graphics, Dialogs, Controls, LEDDefine, PinDefine, Math,
  Menus, ActnList, LEDArraySettingsForm;

type

  { TPeripheralLEDArray }

  TPeripheralLEDArray = class (TPeripheralForm)
  private
    FLEDArray: TLEDArray;

    procedure OnShowSettings(Sender: TObject);

    procedure OnLEDArrayHeightChange(Sender: TLEDArray);

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
  if frmLEDArraySettings.Execute(FLEDArray) then
    GeneratePinArrays;
end;

procedure TPeripheralLEDArray.OnLEDArrayHeightChange(Sender: TLEDArray);
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
  FLEDArray := TLEDArray.Create(pnlDrawSurface, PinArray);
  FLEDArray.OnHeightChange.Add(OnLEDArrayHeightChange);
  GeneratePinArrays;
  OnLEDArrayHeightChange(FLEDArray);
  SetSettingsFunc(OnShowSettings);
end;

destructor TPeripheralLEDArray.Destroy;
begin
  FLEDArray.OnHeightChange.Del(OnLEDArrayHeightChange);
  FLEDArray.Free;
  inherited Destroy;
end;

end.

