unit LEDArraySettingsForm;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, LEDDefine, Color, Math;

type

  { TfrmLEDArraySettings }

  TfrmLEDArraySettings = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    btnSelectColor: TButton;
    cdColorDialog: TColorDialog;
    lbInverted: TLabel;
    lbReversed: TLabel;
    pnlInverted: TPanel;
    pnlReversed: TPanel;
    pnlSettingsArea: TPanel;
    tbOffFactor: TTrackBar;
    cbCount: TComboBox;
    cbShape: TComboBox;
    edtName: TEdit;
    lbColor: TLabel;
    lbOffFactor: TLabel;
    lbShape: TLabel;
    lbName: TLabel;
    lbCount: TLabel;
    pnlOffFactor: TPanel;
    pnlColorPreview: TPanel;
    pnlColor: TPanel;
    pnlOffFactorPreview: TPanel;
    pnlShape: TPanel;
    pnlName: TPanel;
    pnlCount: TPanel;
    pnlControl: TPanel;
    cbInverted: TCheckBox;
    cbReversed: TCheckBox;
    procedure btnSelectColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbOffFactorChange(Sender: TObject);
  private
    procedure LoadLEDArray(ALEDArray: TLEDArray);
    procedure SaveLEDArray(ALEDArray: TLEDArray);

    procedure UpdateColorPreviews;
    procedure UpdateOffColorPreview;

  public
    function Execute(ALEDArray: TLEDArray): Boolean;

  end;

var
  frmLEDArraySettings: TfrmLEDArraySettings;

implementation

{$R *.lfm}

{ TfrmLEDArraySettings }

procedure TfrmLEDArraySettings.FormCreate(Sender: TObject);
var
  I: Integer;
  LEDClass: TLEDClass;
begin
  // Init Count
  cbCount.Clear;
  for I := Low(TLEDArray.TLEDCount) to High(TLEDArray.TLEDCount) do
    cbCount.Items.Add(IntToStr(I));

  // Init Shape
  cbShape.Clear;
  for LEDClass in LEDClasses do
    cbShape.Items.Add(LEDClass.GetShapeName);

  // Having it on via the Object-Inspector messes with the docked Form-Editor biiig time
  AutoSize := True;
end;

procedure TfrmLEDArraySettings.tbOffFactorChange(Sender: TObject);
begin
  UpdateOffColorPreview;
end;

procedure TfrmLEDArraySettings.btnSelectColorClick(Sender: TObject);
begin
  if cdColorDialog.Execute then
    UpdateColorPreviews;
end;

procedure TfrmLEDArraySettings.LoadLEDArray(ALEDArray: TLEDArray);
begin
  edtName.Text := ALEDArray.PinArray.Name;
  cbCount.ItemIndex := ALEDArray.LEDCount - 1;
  cbShape.ItemIndex := Ord(ALEDArray.LEDShape);
  cdColorDialog.Color := ALEDArray.LEDColor.ToWinColor;
  tbOffFactor.Position := EnsureRange(Floor(ALEDArray.LEDOffFactor * 10 + 0.5), 0, 9);
  cbInverted.Checked := ALEDArray.LEDInverted;
  cbReversed.Checked := ALEDArray.ReverseOrder;
  UpdateColorPreviews;
end;

procedure TfrmLEDArraySettings.SaveLEDArray(ALEDArray: TLEDArray);
begin
  ALEDArray.PinArray.Name := edtName.Text;
  ALEDArray.BeginUpdate;
  ALEDArray.LEDCount := cbCount.ItemIndex + 1;
  ALEDArray.LEDShape := TLEDShape(cbShape.ItemIndex);
  ALEDArray.LEDColor := TColorRGB.Create(cdColorDialog.Color);
  ALEDArray.LEDOffFactor := tbOffFactor.Position / 10;
  ALEDArray.LEDInverted := cbInverted.Checked;
  ALEDArray.ReverseOrder := cbReversed.Checked;
  ALEDArray.EndUpdate;
end;

procedure TfrmLEDArraySettings.UpdateColorPreviews;
begin
  pnlColorPreview.Color := cdColorDialog.Color;
  UpdateOffColorPreview;
end;

procedure TfrmLEDArraySettings.UpdateOffColorPreview;
begin
  pnlOffFactorPreview.Color := (TColorRGB.Create(cdColorDialog.Color) * tbOffFactor.Position * 0.1).ToWinColor;
end;

function TfrmLEDArraySettings.Execute(ALEDArray: TLEDArray): Boolean;
begin
  LoadLEDArray(ALEDArray);
  Result := ShowModal = mrOK;
  if Result then
    SaveLEDArray(ALEDArray);
end;

end.

