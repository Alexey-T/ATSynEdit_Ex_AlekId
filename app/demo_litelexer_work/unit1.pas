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
    ed: TATSynEdit;
    Lexer: TATLiteLexer;
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
begin
  Lexer:= TATLiteLexer.Create(Self);
  Lexer.OnGetStyleHash:= @LexerGetStyleHash;
  Lexer.OnApplyStyle:= @LexerApplyStyle;

  ed:= TATSynEdit.Create(Self);
  ed.Parent:= Self;
  ed.Font.Name:= 'Courier New';
  ed.Font.Size:= 10;
  ed.Align:= alClient;
  ed.OptUnprintedVisible:= false;
  ed.OptRulerVisible:= false;
  ed.OptWrapMode:= cWrapOff;
  ed.Colors.TextBG:= $e0f0f0;
  ed.AdapterForHilite:= Lexer;

  Styles:= TStringList.Create;
  Styles.Add('Id');
  Styles.Add('IdKeyword');
  Styles.Add('Number');
  Styles.Add('String');
  Styles.Add('Symbol');
  Styles.Add('Comment');
  Lexer.LoadFromFile(ExtractFilePath(Application.ExeName)+'test_lexer.json');
end;

procedure TForm1.FormShow(Sender: TObject);
var
  fn: string;
begin
  fn:= ExtractFilePath(Application.ExeName)+'unit1.pas';
  DoOpen(fn);
end;

procedure TForm1.DoOpen(const fn: string);
begin
  ed.LoadFromFile(fn);
  ActiveControl:= ed;
  Caption:= 'Demo - '+ExtractFileName(fn);
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
  }
  case AStyleHash of
    0: begin APart.ColorFont:= clBlack; APart.FontBold:= false; end;
    1: begin APart.ColorFont:= clBlack; APart.FontBold:= true; end;
    2: begin APart.ColorFont:= clNavy; APart.FontBold:= true;  end;
    3: begin APart.ColorFont:= clTeal; APart.FontBold:= false;  end;
    4: begin APart.ColorFont:= clRed; APart.FontBold:= false;  end;
    5: begin APart.ColorFont:= clGray; APart.FontBold:= false; APart.FontItalic:= true;  end;
  end;
end;

end.

