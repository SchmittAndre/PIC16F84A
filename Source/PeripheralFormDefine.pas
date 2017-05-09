unit PeripheralFormDefine;

interface

uses
  Forms, Classes, SysUtils, FileUtil, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ProcessorDefine,
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
    FPeripheralName: String;

    function GetDrawSurfaceHeight: Integer;
    function GetDrawSurfaceWidth: Integer;
    function GetPinSurfaceHeight: Integer;
    function GetPinSurfaceWidth: Integer;
    procedure SetDrawSurfaceHeight(AValue: Integer);
    procedure SetDrawSurfaceWidth(AValue: Integer);
    procedure SetPeripheralName(AValue: String);
    procedure SetPinSurfaceHeight(AValue: Integer);
    procedure SetPinSurfaceWidth(AValue: Integer);

    function MakeUniqueName(AName: String): String;

  protected
    procedure DrawPeripheral; virtual;
    function GetDefaultPeripheralName: String; virtual; abstract;
    procedure OnPinChanges({%H-}APinIndex: Cardinal); virtual;
    property PinArray: TPinArray read FPinArray;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property DrawSurfaceWidth: Integer read GetDrawSurfaceWidth write SetDrawSurfaceWidth;
    property DrawSurfaceHeight: Integer read GetDrawSurfaceHeight write SetDrawSurfaceHeight;

    property PinSurfaceWidth: Integer read GetPinSurfaceWidth write SetPinSurfaceWidth;
    property PinSurfaceHeight: Integer read GetPinSurfaceHeight write SetPinSurfaceHeight;

    property PeripheralName: String read FPeripheralName write SetPeripheralName;

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

  procedure InitPin;
  begin
    with pbPins.Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := $FF4F1F;
      Pen.Style := psSolid;
      Pen.Width := 2;
      Pen.Color := $CF3F1F;
    end;
  end;

  procedure InitCaptions;
  begin
    with pbPins.Canvas do
    begin
      Brush.Style := bsClear;
      Font.Style := [];
      Font.Color := clBlack;
      Font.Name := 'Consolas';
      Font.Height := 20;
    end;
  end;

  procedure DrawPin(X, Y: Integer);
  begin
    pbPins.Canvas.Ellipse(X - 6, Y - 6, X + 7, Y + 7);
  end;

  procedure DrawCaption(X, Y: Integer; ACaption: String);
  begin
    pbPins.Canvas.TextOut(X - pbPins.Canvas.TextWidth(ACaption) div 2, Y, ACaption);
  end;

var
  I, L, J: Integer;
begin
  InitPin;
  for I := 0 to FPinArray.Count - 1 do
    DrawPin(15 + I * 30, 30);

  L := 15;
  for I := 0 to FPinArray.VisiblePinArrayCount - 1 do
  begin
    for J := 0 to FPinArray.VisiblePinArrays[I].Count - 1 do
    begin
      DrawPin(L, pbPins.Height - 30);
      L := L + 30;
    end;
    L := L + 10;
  end;

  InitCaptions;
  for I := 0 to FPinArray.Count - 1 do
    DrawCaption(15 + I * 30, 0, I.ToString);

  L := 15;
  for I := 0 to FPinArray.VisiblePinArrayCount - 1 do
  begin
    for J := 0 to FPinArray.VisiblePinArrays[I].Count - 1 do
    begin
      DrawCaption(L, pbPins.Height - 20, J.ToString);
      L := L + 30;
    end;
    L := L + 10;
  end;

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

procedure TPeripheralForm.SetPeripheralName(AValue: String);
begin
  AValue := MakeUniqueName(AValue);
  if FPeripheralName = AValue then
    Exit;
  FPeripheralName := AValue;
  Caption := 'Perpiheral - ' + PeripheralName;
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

function TPeripheralForm.MakeUniqueName(AName: String): String;
var
  Suffix, I: Integer;
  Unique: Boolean;
begin
  Result := AName;
  Suffix := 0;
  repeat
    Unique := True;
    for I := 0 to frmVisiblePinSelection.PinArrayCount - 1 do
    begin
      if (frmVisiblePinSelection.PinArrays[I] <> FPinArray) and
         (frmVisiblePinSelection.PinArrays[I].Name = Result) then
      begin
        Inc(Suffix);
        Result := AName + ' [' + Suffix.ToString + ']';
        Unique := False;
        Break;
      end;
    end;
  until Unique;
end;

procedure TPeripheralForm.DrawPeripheral;
begin
  pbDrawSurface.Canvas.Pen.Style := psClear;
  pbDrawSurface.Canvas.Brush.Style := bsSolid;
  pbDrawSurface.Canvas.Brush.Color := clBtnFace;
  pbDrawSurface.Canvas.Rectangle(0, 0, DrawSurfaceWidth, DrawSurfaceHeight);
end;

procedure TPeripheralForm.OnPinChanges(APinIndex: Cardinal);
begin
  // nothing by default
end;

constructor TPeripheralForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  PeripheralName := GetDefaultPeripheralName;
  PinSurfaceHeight := 100;
  FPinArray := TPinArray.Create(PeripheralName, OnPinChanges, 4);
end;

destructor TPeripheralForm.Destroy;
begin
  FPinArray.Free;
  inherited Destroy;
end;

end.

