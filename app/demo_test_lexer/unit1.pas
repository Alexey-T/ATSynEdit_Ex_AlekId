unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, atsynedit_lexer_lite,
  Dialogs, StdCtrls;

type
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  L: TATLiteLexer;
begin
  L:= TATLiteLexer.Create;
  L.LoadFromFile(ExtractFilePath(Application.ExeName)+'test_lexer.json');
  Memo1.Lines.Clear;
  Memo1.Lines.Add(L.GetDump);
end;

end.

