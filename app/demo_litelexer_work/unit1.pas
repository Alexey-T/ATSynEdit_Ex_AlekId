unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  ATSynEdit,
  ATSynEdit_Adapters,
  ATSynEdit_CanvasProc,
  atsynedit_adapter_litelexer;

type
  { TForm1 }

  TForm1 = class(TForm)
    ButtonOpen: TButton;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    procedure ButtonOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    LexerLib: TATLiteLexers;
    ed: TATSynEdit;
    Styles: TStringList;
    function LexerGetStyleHash(Sender: TObject; const AStyleName: string): integer;
    procedure LexerApplyStyle(Sender: TObject; AStyleHash: integer; var APart: TATLinePart);
    procedure DoOpen(const fn: string);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  S: string;
  i: integer;
begin
  ed:= TATSynEdit.Create(Self);
  ed.Parent:= Self;
  ed.Font.Name:= 'Courier New';
  ed.Font.Size:= 10;
  ed.Align:= alClient;
  ed.OptUnprintedVisible:= false;
  ed.OptRulerVisible:= false;
  ed.OptWrapMode:= cWrapOff;
  ed.Colors.TextBG:= $e0f0f0;

  Styles:= TStringList.Create;
  Styles.Add('Id');
  Styles.Add('IdKeyword');
  Styles.Add('Number');
  Styles.Add('String');
  Styles.Add('Symbol');
  Styles.Add('Comment');
  Styles.Add('CommentDoc');
  Styles.Add('TagId');
  Styles.Add('TagProp');
  Styles.Add('TagBound');

  LexerLib:= TATLiteLexers.Create;
  LexerLib.OnGetStyleHash:= @LexerGetStyleHash;
  LexerLib.OnApplyStyle:= @LexerApplyStyle;
  LexerLib.LoadFromDir(ExtractFilePath(ExtractFileDir(Application.ExeName))+'litelexers');

  {
  S:= '';
  for i:= 0 to LexerLib.Count-1 do
    S:= S+LexerLib.GetLexer(i).LexerName+' ';
  ShowMessage('Lexers found:'#10+S);
  }
end;

procedure TForm1.FormShow(Sender: TObject);
var
  fn: string;
begin
  fn:= ExtractFilePath(Application.ExeName)+'unit1.pas';
  DoOpen(fn);
end;

procedure TForm1.DoOpen(const fn: string);
var
  Lexer: TATLiteLexer;
begin
  Lexer:= LexerLib.FindLexer(fn);
  if Assigned(Lexer) then
    Caption:= ExtractFileName(fn)+' - '+Lexer.LexerName;
  ed.AdapterForHilite:= Lexer;
  ed.LoadFromFile(fn);
  ActiveControl:= ed;
end;

procedure TForm1.ButtonOpenClick(Sender: TObject);
begin
  with OpenDialog1 do
    if Execute then
      DoOpen(FileName);
end;

function TForm1.LexerGetStyleHash(Sender: TObject; const AStyleName: string): integer;
begin
  Result:= Styles.IndexOf(AStyleName);
end;

procedure TForm1.LexerApplyStyle(Sender: TObject; AStyleHash: integer; var APart: TATLinePart);
begin
  {
  Styles.Add('Id');
  Styles.Add('IdKeyword');
  Styles.Add('Number');
  Styles.Add('String');
  Styles.Add('Symbol');
  Styles.Add('Comment');
  Styles.Add('CommentDoc');
  Styles.Add('TagId')
  Styles.Add('TagProp')
  Styles.Add('TagBound')
  }
  case AStyleHash of
    0: begin APart.ColorFont:= clBlack; end;
    1: begin APart.ColorFont:= clBlack; APart.FontBold:= true; end;
    2: begin APart.ColorFont:= clNavy; APart.FontBold:= true; end;
    3: begin APart.ColorFont:= clTeal; end;
    4: begin APart.ColorFont:= clRed; end;
    5: begin APart.ColorFont:= clGray; APart.FontBold:= false; APart.FontItalic:= true; end;
    6: begin APart.ColorFont:= $40A040; APart.FontBold:= false; APart.FontItalic:= true; end;
    7: begin APart.ColorFont:= $4040E0; end;
    8: begin APart.ColorFont:= clGreen; end;
    9: begin APart.ColorFont:= $A04040; end;
  end;
end;

end.

