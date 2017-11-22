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
  RegExpr;

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
    RegexObj: TRegExpr;
    procedure LexerGetStyleHash(Sender: TObject; const AStyle: string;
      var AHash: integer);
    procedure DoOpen(const fn: string);
    procedure EditorCalcHilite(Sender: TObject; var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
    procedure GetColorsOfStyle(AStyleIndex: integer; out AColorFont: TColor; out
      ABold: ByteBool);
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

  RegexObj:= TRegExpr.Create;
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

function SRegexFind(Obj: TRegExpr;
  const ARegex, AInputStr: string;
  AFromPos: integer;
  out AFoundLen: integer): boolean;
var
  i: integer;
begin
  Result:= false;
  AFoundLen:= 0;

  if ARegex='' then exit;
  if AInputStr='' then exit;

  Obj.ModifierS:= false; //don't catch all text by .*
  Obj.ModifierM:= true; //allow to work with ^$

  try
    Obj.Expression:= ARegex;
    Obj.InputString:= AInputStr;
    Result:= Obj.ExecPos(AFromPos);
    if Result and (Obj.MatchPos[0]<>AFromPos) then
      Result:= false;
    if Result then
      AFoundLen:= Obj.MatchLen[0];
  except
    Result:= false;
  end;
end;

procedure TForm1.EditorCalcHilite(Sender: TObject; var AParts: TATLineParts;
  ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
var
  EdLine: UnicodeString;
  NParts, NPos, NLen, IndexRule: integer;
  Rule: TATLiteLexerRule;
  bLastFound, bRuleFound: boolean;
begin
  EdLine:= Copy(ed.Strings.Lines[ALineIndex], ACharIndex, ALineLen);
  NParts:= 0;
  NPos:= 1;

  RegexObj.ModifierI:= not Lexer.CaseSens;
  bLastFound:= false;

  repeat
    if NPos>Length(EdLine) then Break;

    bRuleFound:= false;
    for IndexRule:= 0 to Lexer.Rules.Count-1 do
    begin
      Rule:= Lexer.GetRule(IndexRule);
      if SRegexFind(RegexObj, Rule.Regex, EdLine, NPos, NLen) then
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
      AParts[NParts-1].ColorBG:= clNone;
      GetColorsOfStyle(Rule.StyleHash,
        AParts[NParts-1].ColorFont,
        AParts[NParts-1].FontBold
        );
      Inc(NPos, NLen);
    end;

    bLastFound:= bRuleFound;
  until false;
end;

procedure TForm1.GetColorsOfStyle(AStyleIndex: integer;
  out AColorFont: TColor; out ABold: ByteBool);
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
    0: begin AColorFont:= clBlack; ABold:= false; end;
    1: begin AColorFont:= clBlack; ABold:= true;  end;
    2: begin AColorFont:= clNavy; ABold:= true;  end;
    3: begin AColorFont:= clTeal; ABold:= false;  end;
    4: begin AColorFont:= clRed; ABold:= false;  end;
    5: begin AColorFont:= clGray; ABold:= false;  end;
  end;
end;

end.

