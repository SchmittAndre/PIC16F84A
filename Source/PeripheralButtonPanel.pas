unit PeripheralButtonPanel;

interface

uses
  PeripheralFormDefine, Classes, PinDefine, ButtonPanelDefine, Math, Controls;

type

  { TPeripheralButtonPanel }

  TPeripheralButtonPanel = class (TPeripheralForm)
  private
    FButtonPanel: TButtonPanel;

    procedure OnShowSettings(Sender: TObject);

  protected
    procedure DrawPeripheral; override;

    function GetMinWidth: Integer; override;

    procedure OnButtonPanelHeightChange(Sender: TButtonPanel);

    procedure DrawSurfaceMouseDown(X, Y: Integer; AButton: TMouseButton); override;
    procedure DrawSurfaceMouseMove(X, Y: Integer); override;
    procedure DrawSurfaceMouseUp(X, Y: Integer; AButton: TMouseButton); override;
    procedure DrawSurfaceMouseLeave; override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    class function GetDefaultPeripheralName: String; override;

  end;

implementation

{ TPeripheralButtonPanel }

procedure TPeripheralButtonPanel.OnShowSettings(Sender: TObject);
begin
  // TODO: Button Settings
end;

procedure TPeripheralButtonPanel.DrawPeripheral;
begin
  FButtonPanel.Draw;
end;

function TPeripheralButtonPanel.GetMinWidth: Integer;
begin
  Result := inherited GetMinWidth;
  Result := Max(Result, FButtonPanel.DisplayWidth);
end;

procedure TPeripheralButtonPanel.OnButtonPanelHeightChange(Sender: TButtonPanel);
begin
  DrawSurfaceHeight := FButtonPanel.DisplayHeight;
end;

procedure TPeripheralButtonPanel.DrawSurfaceMouseDown(X, Y: Integer; AButton: TMouseButton);
begin
  FButtonPanel.MouseDown(X, Y, AButton);
end;

procedure TPeripheralButtonPanel.DrawSurfaceMouseMove(X, Y: Integer);
begin
  FButtonPanel.MouseMove(X, Y);
end;

procedure TPeripheralButtonPanel.DrawSurfaceMouseUp(X, Y: Integer; AButton: TMouseButton);
begin
  FButtonPanel.MouseUp(X, Y, AButton);
end;

procedure TPeripheralButtonPanel.DrawSurfaceMouseLeave;
begin
  FButtonPanel.MouseLeave;
end;

constructor TPeripheralButtonPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FButtonPanel := TButtonPanel.Create(pnlDrawSurface, PinArray);
  FButtonPanel.OnHeightChange.Add(OnButtonPanelHeightChange);
  GeneratePinArrays;
  OnButtonPanelHeightChange(FButtonPanel);
  SetSettingsFunc(OnShowSettings);
end;

destructor TPeripheralButtonPanel.Destroy;
begin
  FButtonPanel.OnHeightChange.Del(OnButtonPanelHeightChange);
  FButtonPanel.Free;
  inherited Destroy;
end;

class function TPeripheralButtonPanel.GetDefaultPeripheralName: String;
begin
  Result := 'Button-Panel';
end;

end.

