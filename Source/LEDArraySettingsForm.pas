unit LEDArraySettingsForm;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LEDDefine;

type

  { TfrmLEDArraySettings }

  TfrmLEDArraySettings = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    cbCount: TComboBox;
    edtName: TEdit;
    lbName: TLabel;
    lbCount: TLabel;
    pnlName: TPanel;
    pnlCount: TPanel;
    pnlSettingsArea: TPanel;
    pnlControl: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadLEDArray(ALEDArray: TLEDArray);
    procedure SaveLEDArray(ALEDArray: TLEDArray);

  public
    procedure Execute(ALEDArray: TLEDArray);

  end;

var
  frmLEDArraySettings: TfrmLEDArraySettings;

implementation

{$R *.lfm}

{ TfrmLEDArraySettings }

procedure TfrmLEDArraySettings.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  cbCount.Clear;
  for I := Low(TLEDArray.TLEDCount) to High(TLEDArray.TLEDCount) do
    cbCount.Items.Add(IntToStr(I));
end;

procedure TfrmLEDArraySettings.LoadLEDArray(ALEDArray: TLEDArray);
begin
  edtName.Text := ALEDArray.PinArray.Name;
  cbCount.ItemIndex := ALEDArray.LEDCount - 1;
end;

procedure TfrmLEDArraySettings.SaveLEDArray(ALEDArray: TLEDArray);
begin
  ALEDArray.PinArray.Name := edtName.Text;
  ALEDArray.LEDCount := cbCount.ItemIndex + 1;
end;

procedure TfrmLEDArraySettings.Execute(ALEDArray: TLEDArray);
begin
  LoadLEDArray(ALEDArray);
  if ShowModal = mrOK then
    SaveLEDArray(ALEDArray);
end;

end.

