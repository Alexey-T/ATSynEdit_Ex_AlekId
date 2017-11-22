unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  ATSynEdit,
  ATSynEdit_Adapters,
  ATSynEdit_CanvasProc,
  ATSynEdit_Lexer_Lite,
  ec_RegExpr;

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
    RegexObj: TecRegExpr;
    procedure LexerGetStyleHash(Sender: TObject; const AStyle: string;
      var AHash: integer);
    procedure DoOpen(const fn: string);
    procedure EditorCalcHilite(Sender: TObject; var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
    procedure ApplyStyleToPart(AStyleIndex: integer; var APart: TATLinePart);
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
  ed:= TATSynEdit.Create(Self);
  ed.Parent:= Self;
  ed.Font.Name:= 'Courier New';
  ed.Font.Size:= 11;
  ed.Align:= alClient;
  ed.OptUnprintedVisible:= false;
  ed.OptRulerVisible:= false;
  ed.OptWrapMode:= cWrapOff;
  ed.Colors.TextBG:= $e0f0f0;
  ed.OnCalcHilite:=@EditorCalcHilite;

  Lexer:= TATLiteLexer.Create;
  Lexer.OnGetStyleHash:= @LexerGetStyleHash;

  Styles:= TStringList.Create;
  Styles.Add('Id');
  Styles.Add('IdKeyword');
  Styles.Add('Number');
  Styles.Add('String');
  Styles.Add('Symbol');
  Styles.Add('Comment');

  Lexer.LoadFromFile(ExtractFilePath(Application.ExeName)+'test_lexer.json');

  RegexObj:= TecRegExpr.Create;
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

procedure TForm1.LexerGetStyleHash(Sender: TObject; const AStyle: string;
  var AHash: integer);
begin
  AHash:= Styles.IndexOf(AStyle);
end;

function SRegexFindLen(Obj: TecRegExpr;
  const ARegex, AInputStr: UnicodeString;
  AFromPos: integer): integer;
var
  i: integer;
begin
  Result:= 0;
  if ARegex='' then exit;

  Obj.ModifierS:= false; //don't catch all text by .*
  Obj.ModifierM:= true; //allow to work with ^$

  Obj.Expression:= ARegex;
  Result:= Obj.MatchLength(AInputStr, AFromPos);
end;

procedure TForm1.EditorCalcHilite(Sender: TObject; var AParts: TATLineParts;
  ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
var
  EdLine: UnicodeString;
  ch: WideChar;
  NParts, NPos, NLen, IndexRule: integer;
  Rule: TATLiteLexerRule;
  bLastFound, bRuleFound: boolean;
begin
  EdLine:= Copy(ed.Strings.Lines[ALineIndex], ACharIndex, ALineLen);
  NParts:= 0;
  NPos:= 1;
  bLastFound:= false;

  RegexObj.ModifierI:= not Lexer.CaseSens;

  repeat
    if NPos>Length(EdLine) then Break;
    bRuleFound:= false;

    ch:= EdLine[NPos];
    if (ch<>' ') and (ch<>#9) then
      for IndexRule:= 0 to Lexer.Rules.Count-1 do
      begin
        Rule:= Lexer.GetRule(IndexRule);
        NLen:= SRegexFindLen(RegexObj, Rule.Regex, EdLine, NPos);
        if NLen>0 then
        begin
          bRuleFound:= true;
          Break;
        end;
      end;

    if not bRuleFound then
    begin
      if (NParts=0) or (bLastFound) then
      begin
        Inc(NParts);
        AParts[NParts-1].Offset:= NPos-1;
        AParts[NParts-1].Len:= 1;
      end
      else
      begin
        Inc(AParts[NParts-1].Len);
      end;
      AParts[NParts-1].ColorBG:= clNone; //Random($fffff);
      AParts[NParts-1].ColorFont:= clBlack;
      Inc(NPos);
    end
    else
    begin
      Inc(NParts);
      AParts[NParts-1].Offset:= NPos-1;
      AParts[NParts-1].Len:= NLen;
      AParts[NParts-1].ColorBG:= clNone; //Random($fffff);
      ApplyStyleToPart(Rule.StyleHash, AParts[NParts-1]);
      Inc(NPos, NLen);
    end;

    bLastFound:= bRuleFound;
  until false;
end;

procedure TForm1.ApplyStyleToPart(AStyleIndex: integer; var APart: TATLinePart);
begin
  {
  Styles.Add('Id');
  Styles.Add('IdKeyword');
  Styles.Add('Number');
  Styles.Add('String');
  Styles.Add('Symbol');
  Styles.Add('Comment');
  }
  case AStyleIndex of
    0: begin APart.ColorFont:= clBlack; APart.FontBold:= false; end;
    1: begin APart.ColorFont:= clBlack; APart.FontBold:= true; end;
    2: begin APart.ColorFont:= clNavy; APart.FontBold:= true;  end;
    3: begin APart.ColorFont:= clTeal; APart.FontBold:= false;  end;
    4: begin APart.ColorFont:= clRed; APart.FontBold:= false;  end;
    5: begin APart.ColorFont:= clGray; APart.FontBold:= false; APart.FontItalic:= true;  end;
  end;
end;

end.

