unit PeripheralFormDefine;

interface

uses
  Forms, Classes, SysUtils, FileUtil, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ProcessorDefine;

type

  { TPeripheralForm }

  TPeripheralForm = class (TForm)
    gbPins: TGroupBox;
    pbDrawSurface: TPaintBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure pbDrawSurfacePaint(Sender: TObject);
  private
    FProcessor: TProcessor;

    function GetDrawSurfaceHeight: Integer;
    function GetDrawSurfaceWidth: Integer;
    procedure SetDrawSurfaceHeight(AValue: Integer);
    procedure SetDrawSurfaceWidth(AValue: Integer);

  protected
    property Processor: TProcessor read FProcessor write FProcessor;

    procedure DrawPeripheral; virtual;
    function GetName: String; virtual; abstract;

  public
    constructor Create(TheOwner: TComponent); override;

    procedure SetProcessor(AProcessor: TProcessor);

    property DrawSurfaceWidth: Integer read GetDrawSurfaceWidth write SetDrawSurfaceWidth;
    property DrawSurfaceHeight: Integer read GetDrawSurfaceHeight write SetDrawSurfaceHeight;

  end;

implementation

{$R *.lfm}

{ TPeripheralForm }

procedure TPeripheralForm.pbDrawSurfacePaint(Sender: TObject);
begin
  DrawPeripheral;
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

procedure TPeripheralForm.SetDrawSurfaceHeight(AValue: Integer);
begin
  Height := Height - DrawSurfaceHeight + AValue;
end;

procedure TPeripheralForm.SetDrawSurfaceWidth(AValue: Integer);
begin
  Width := Width - DrawSurfaceWidth + AValue;
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
  Caption := GetName;
end;

procedure TPeripheralForm.SetProcessor(AProcessor: TProcessor);
begin
  FProcessor := AProcessor;
end;

end.

