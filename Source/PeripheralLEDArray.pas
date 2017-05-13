unit PeripheralLEDArray;

interface

uses
  Classes, SysUtils, PeripheralFormDefine, Graphics, Dialogs, Controls, LEDDefine, PinDefine, Math,
  Menus, ActnList, LEDArraySettingsForm;

type

  { TPeripheralLEDArray }

  TPeripheralLEDArray = class (TPeripheralForm)
  public
    type

      TLEDCount = 0 .. 8;

  private
    FLEDArray: TLEDArray;

    procedure OnShowSettings(Sender: TObject);

    procedure OnLEDArrayWidthChange(Sender: TLEDArray);
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
  frmLEDArraySettings.Execute(FLEDArray);
  GeneratePinArrays;
end;

procedure TPeripheralLEDArray.OnLEDArrayWidthChange(Sender: TLEDArray);
begin
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
  FLEDArray := TLEDArray.Create(pbDrawSurface, PinArray);
  FLEDArray.OnWidthChange.Add(OnLEDArrayWidthChange);
  FLEDArray.OnHeightChange.Add(OnLEDArrayHeightChange);
  OnLEDArrayHeightChange(FLEDArray);
  SetSettingsFunc(OnShowSettings);
end;

destructor TPeripheralLEDArray.Destroy;
begin
  FLEDArray.OnWidthChange.Del(OnLEDArrayWidthChange);
  FLEDArray.OnHeightChange.Del(OnLEDArrayHeightChange);
  FLEDArray.Free;
  inherited Destroy;
end;

end.

