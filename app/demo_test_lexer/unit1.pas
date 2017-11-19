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
    procedure FormCreate(Sender: TObject);
  private
    Lexer: TATLiteLexer;
    ListStyles: TStringList;
    procedure LexerGetStyleHash(Sender: TObject; const AStyle: string; var AHash: integer);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Lexer.LoadFromFile(ExtractFilePath(Application.ExeName)+'test_lexer.json');
  Memo1.Lines.Clear;
  Memo1.Lines.Add(Lexer.GetDump);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Lexer:= TATLiteLexer.Create;
  Lexer.OnGetStyleHash:= @LexerGetStyleHash;

  ListStyles:= TStringList.Create;
  ListStyles.Add('Id');
  ListStyles.Add('IdKeyword');
  ListStyles.Add('Number');
  ListStyles.Add('String');
  ListStyles.Add('Symbol');
  ListStyles.Add('Comment');
end;

procedure TForm1.LexerGetStyleHash(Sender: TObject; const AStyle: string;
  var AHash: integer);
begin
  AHash:= ListStyles.IndexOf(AStyle);
end;

end.

