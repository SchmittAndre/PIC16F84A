program BinaryParser;

uses
  SysUtils;

{$R *.res}

var
  S: String;
  I: Cardinal;
  K: Integer;
begin
  while not EOF do
  begin
    ReadLn(S);
    I := 0;
    for K := 1 to S.Length do
    begin
      if S[K] = '1' then
        I := I or (1 shl (S.Length - K));
    end;
    WriteLn(Format('$%.4x', [I]));
  end;
end.

