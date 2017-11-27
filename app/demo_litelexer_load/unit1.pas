unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, atsynedit_lexer_lite,
  Dialogs, StdCtrls;

type
  { TForm1 }

  TForm1 = class(TForm)
    btnLoadLexer: TButton;
    Memo1: TMemo;
    procedure btnLoadLexerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Lexer: TATLiteLexer;
    Styles: TStringList;
    procedure LexerGetStyleHash(Sender: TObject; const AStyle: string; var AHash: integer);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnLoadLexerClick(Sender: TObject);
begin
  Lexer.LoadFromFile(ExtractFilePath(Application.ExeName)+'test_lexer.json');
  Memo1.Lines.Clear;
  Memo1.Lines.Add(Lexer.GetDump);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Lexer:= TATLiteLexer.Create;
  Lexer.OnGetStyleHash:= @LexerGetStyleHash;

  Styles:= TStringList.Create;
  Styles.Add('Id');
  Styles.Add('IdKeyword');
  Styles.Add('Number');
  Styles.Add('String');
  Styles.Add('Symbol');
  Styles.Add('Comment');
end;

procedure TForm1.LexerGetStyleHash(Sender: TObject; const AStyle: string;
  var AHash: integer);
begin
  AHash:= Styles.IndexOf(AStyle);
end;

end.

