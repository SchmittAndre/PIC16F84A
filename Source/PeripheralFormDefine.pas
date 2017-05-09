unit PeripheralFormDefine;

interface

uses
  Forms, Classes, SysUtils, FileUtil, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ProcessorDefine, PinDefine,
  VisiblePinSelectionDefine;

type

  { TPeripheralForm }

  TPeripheralForm = class (TForm)
    gbPins: TGroupBox;
    pbDrawSurface: TPaintBox;
    pbPins: TPaintBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure pbDrawSurfacePaint(Sender: TObject);
    procedure pbPinsDblClick(Sender: TObject);
    procedure pbPinsPaint(Sender: TObject);
  private
    FPinArray: TPinArray;

    function GetDrawSurfaceHeight: Integer;
    function GetDrawSurfaceWidth: Integer;
    function GetPinSurfaceHeight: Integer;
    function GetPinSurfaceWidth: Integer;
    procedure SetDrawSurfaceHeight(AValue: Integer);
    procedure SetDrawSurfaceWidth(AValue: Integer);
    procedure SetPinSurfaceHeight(AValue: Integer);
    procedure SetPinSurfaceWidth(AValue: Integer);

  protected
    procedure DrawPeripheral; virtual;

    function GetName: String; virtual; abstract;

  public
    constructor Create(TheOwner: TComponent); override;

    property DrawSurfaceWidth: Integer read GetDrawSurfaceWidth write SetDrawSurfaceWidth;
    property DrawSurfaceHeight: Integer read GetDrawSurfaceHeight write SetDrawSurfaceHeight;

    property PinSurfaceWidth: Integer read GetPinSurfaceWidth write SetPinSurfaceWidth;
    property PinSurfaceHeight: Integer read GetPinSurfaceHeight write SetPinSurfaceHeight;

  end;

implementation

{$R *.lfm}

{ TPeripheralForm }

procedure TPeripheralForm.pbDrawSurfacePaint(Sender: TObject);
begin
  DrawPeripheral;
end;

procedure TPeripheralForm.pbPinsDblClick(Sender: TObject);
begin
  if Sender = pbPins then
    frmVisiblePinSelection.Execute(FPinArray);
end;

procedure TPeripheralForm.pbPinsPaint(Sender: TObject);
begin

end;

procedure TPeripheralForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
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

procedure TPeripheralForm.SetDrawSurfaceHeight(AValue: Integer);
begin
  Height := Height - DrawSurfaceHeight + AValue;
end;

procedure TPeripheralForm.SetDrawSurfaceWidth(AValue: Integer);
begin
  Width := Width - DrawSurfaceWidth + AValue;
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

procedure TPeripheralForm.DrawPeripheral;
begin
  pbDrawSurface.Canvas.Pen.Style := psClear;
  pbDrawSurface.Canvas.Brush.Style := bsSolid;
  pbDrawSurface.Canvas.Brush.Color := clBtnFace;
  pbDrawSurface.Canvas.Rectangle(0, 0, DrawSurfaceWidth, DrawSurfaceHeight);
end;

constructor TPeripheralForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption := Name;
  PinSurfaceHeight := 50;
end;

end.

