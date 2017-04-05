unit Main;

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnEpicEvent: TButton;
    OpenDialog1: TOpenDialog;
    Splitter1: TSplitter;
    SynEdit1: TSynEdit;
    procedure btnEpicEventClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPortA: String;
    procedure SetPortA(AValue: String);
    procedure parseStringlist(List: TStringList);
  public
     property PortA: String read FPortA write SetPortA;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnEpicEventClick(Sender: TObject);
var
  LstFile: TStringList;
begin

  if OpenDialog1.Execute then
  begin
    LstFile := TStringList.Create;
    LstFile.LoadFromFile(OpenDialog1.FileName);
    parseStringlist(LstFile);
    SynEdit1.Text := LstFile.Text;
    LstFile.Free;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.SetPortA(AValue: String);
begin
  if FPortA = AValue then Exit;
  FPortA := AValue;
end;

procedure TForm1.parseStringlist(List: TStringList);
var
  Counter: Integer;
begin
  for Counter := List.Count - 1 downto 0 do
  begin
    if List[Counter].StartsWith(' ') then
    begin
      List.Delete(Counter);
    end
    else
    begin
      List[Counter] := Copy(List[Counter],1 , 9);
    end;
  end;
end;

end.


