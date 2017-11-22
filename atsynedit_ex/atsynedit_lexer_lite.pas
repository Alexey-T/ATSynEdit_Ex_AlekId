unit atsynedit_lexer_lite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs,
  jsonConf,
  ATSynEdit,
  ATSynEdit_Adapters,
  ATSynEdit_CanvasProc,
  ec_RegExpr;

type
  TATLiteLexerRule = class
  public
    Name, Regex, Style: string;
    StyleHash: integer;
  end;

type
  TATLiteLexer_GetStyleHash = procedure (Sender: TObject; const AStyleName: string; var AStyleHash: integer) of object;
  TATLiteLexer_ApplyStyle = procedure (Sender: TObject; const AStyleHash: integer; var APart: TATLinePart) of object;

type
  { TATLiteLexer }

  TATLiteLexer = class(TATAdapterHilite)
  private
    RegexObj: TecRegExpr;
    FOnGetStyleHash: TATLiteLexer_GetStyleHash;
    FOnApplyStyle: TATLiteLexer_ApplyStyle;
  public
    LexerName: string;
    CaseSens: boolean;
    FileTypes: TStringList;
    Rules: TList;
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const AFilename: string);
    procedure Clear;
    function GetRule(AIndex: integer): TATLiteLexerRule;
    function GetDump: string;
    procedure OnEditorCalcHilite(Sender: TObject; var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor); override;
    property OnGetStyleHash: TATLiteLexer_GetStyleHash read FOnGetStyleHash write FOnGetStyleHash;
    property OnApplyStyle: TATLiteLexer_ApplyStyle read FOnApplyStyle write FOnApplyStyle;
  end;

implementation

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


{ TATLiteLexer }

constructor TATLiteLexer.Create(AOnwer: TComponent);
begin
  inherited;
  FileTypes:= TStringList.Create;
  Rules:= TList.Create;
  RegexObj:= TecRegExpr.Create;
end;

destructor TATLiteLexer.Destroy;
begin
  Clear;
  FreeAndNil(RegexObj);
  FreeAndNil(FileTypes);
  FreeAndNil(Rules);
  inherited;
end;

procedure TATLiteLexer.Clear;
var
  i: integer;
begin
  LexerName:= '?';
  CaseSens:= false;
  FileTypes.Clear;

  for i:= Rules.Count-1 downto 0 do
    TObject(Rules[i]).Free;
  Rules.Clear;
end;

function TATLiteLexer.GetRule(AIndex: integer): TATLiteLexerRule;
begin
  Result:= TATLiteLexerRule(Rules[AIndex]);
end;

procedure TATLiteLexer.LoadFromFile(const AFilename: string);
var
  c: TJSONConfig;
  keys: TStringList;
  rule: TATLiteLexerRule;
  s_name, s_regex, s_style: string;
  i: integer;
begin
  Clear;
  if not FileExists(AFilename) then exit;

  c:= TJSONConfig.Create(nil);
  keys:= TStringList.Create;
  try
    try
      c.Filename:= AFileName;
    except
      ShowMessage('Cannot load JSON lexer file:'#10+AFilename);
      exit;
    end;

    LexerName:= ChangeFileExt(ExtractFileName(AFilename), '');
    CaseSens:= c.GetValue('/case_sens', false);
    c.GetValue('/files', FileTypes, '');

    c.EnumSubKeys('/rules', keys);
    for i:= 0 to keys.Count-1 do
    begin
      s_name:= keys[i];
      s_regex:= c.GetValue('/rules/'+s_name+'/regex', '');
      s_style:= c.GetValue('/rules/'+s_name+'/style', '');
      if (s_name='') or (s_regex='') or (s_style='') then Continue;

      rule:= TATLiteLexerRule.Create;
      rule.Name:= s_name;
      rule.Regex:= s_regex;
      rule.Style:= s_style;
      if Assigned(FOnGetStyleHash) then
        FOnGetStyleHash(Self, rule.Style, rule.StyleHash);
      Rules.Add(rule);
    end;
  finally
    keys.Free;
    c.Free;
  end;
end;

function TATLiteLexer.GetDump: string;
const
  cBool: array[boolean] of string = ('false', 'true');
var
  i: integer;
begin
  FileTypes.LineBreak:= ' ';
  Result:=
    'name: '+LexerName+#10+
    'case_sens: '+cBool[CaseSens]+#10+
    'files: '+FileTypes.Text+#10+
    'rules:';
  for i:= 0 to Rules.Count-1 do
    with GetRule(i) do
      Result:= Result+#10+Format('(name: "%s", re: "%s", st: "%s", st_n: %d)', [Name, Regex, Style, StyleHash]);
end;


procedure TATLiteLexer.OnEditorCalcHilite(Sender: TObject;
  var AParts: TATLineParts; ALineIndex, ACharIndex, ALineLen: integer;
  var AColorAfterEol: TColor);
var
  Ed: TATSynEdit;
  EdLine: UnicodeString;
  ch: WideChar;
  NParts, NPos, NLen, IndexRule: integer;
  Rule: TATLiteLexerRule;
  bLastFound, bRuleFound: boolean;
begin
  Ed:= Sender as TATSynEdit;
  EdLine:= Copy(Ed.Strings.Lines[ALineIndex], ACharIndex, ALineLen);
  NParts:= 0;
  NPos:= 1;
  bLastFound:= false;

  RegexObj.ModifierI:= not CaseSens;

  repeat
    if NPos>Length(EdLine) then Break;
    if NParts>=High(TATLineParts) then Break;
    bRuleFound:= false;

    ch:= EdLine[NPos];
    if (ch<>' ') and (ch<>#9) then
      for IndexRule:= 0 to Rules.Count-1 do
      begin
        Rule:= GetRule(IndexRule);
        NLen:= SRegexFindLen(RegexObj, Rule.Regex, EdLine, NPos);
        if NLen>0 then
        begin
          bRuleFound:= true;
          Break;
        end;
      end;

    if not bRuleFound then
    begin
      if (NParts=0) or bLastFound then
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
      if Assigned(FOnApplyStyle) then
        FOnApplyStyle(Self, Rule.StyleHash, AParts[NParts-1]);
      Inc(NPos, NLen);
    end;

    bLastFound:= bRuleFound;
  until false;
end;

end.

