unit ProcessorFormDefine;

interface

uses
  PeripheralFormDefine, ProcessorDefine, PinDefine, Classes, SysUtils, Controls, Math;

type

  { TBaseProcessorForm }

  TBaseProcessorForm = class (TPinConnectionForm)
  private
    FProcessor: TProcessor;

    procedure InitDrawCaption;
    procedure OnFormShow(Sender: TObject);

  protected
    property Processor: TProcessor read FProcessor;

    function GetMinWidth: Integer; override;
    procedure DrawPeripheral; override;

  public
    constructor Create(TheOwner: TComponent); override;

  end;

  { TProcessorPortAForm }

  TProcessorPortAForm = class (TBaseProcessorForm)
  protected
    function GetPinArray: TPinArray; override;

  end;

  { TProcessorPortBForm }

  TProcessorPortBForm = class (TBaseProcessorForm)
  protected
    function GetPinArray: TPinArray; override;

  end;

  { TProcessorMasterClearForm }

  TProcessorMasterClearForm = class (TBaseProcessorForm)
  protected
    function GetPinArray: TPinArray; override;
  end;

implementation

{ TProcessorMasterClearForm }

function TProcessorMasterClearForm.GetPinArray: TPinArray;
begin
  Result := Processor.MasterClearPin;
end;

{ TProcessorPortBForm }

function TProcessorPortBForm.GetPinArray: TPinArray;
begin
  Result := Processor.PortBPins;
end;

{ TProcessorPortAForm }

function TProcessorPortAForm.GetPinArray: TPinArray;
begin
  Result := Processor.PortAPins;
end;

{ TBaseProcessorForm }

procedure TBaseProcessorForm.InitDrawCaption;
begin
  with pnlDrawSurface.Canvas.Font do
  begin
    Height := 40;
    Bold := True;
  end;
end;

procedure TBaseProcessorForm.OnFormShow(Sender: TObject);
begin
  GeneratePinArrays;
  AutoWidth;
end;

function TBaseProcessorForm.GetMinWidth: Integer;
begin
  Result := inherited GetMinWidth;
  InitDrawCaption;
  Result := Max(Result, pnlDrawSurface.Canvas.TextWidth(PinArray.Name) + 40);
end;

procedure TBaseProcessorForm.DrawPeripheral;
begin
  InitDrawCaption;
  with pnlDrawSurface do
    Canvas.TextOut((Width - Canvas.TextWidth(PinArray.Name)) div 2, 5, PinArray.Name);
end;

constructor TBaseProcessorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Visible := False;
  DrawSurfaceHeight := 50;
  FProcessor := (TheOwner as IHasProcessor).GetProcessor;
  Caption := 'Processor';
  OnShow := OnFormShow;
end;

end.

