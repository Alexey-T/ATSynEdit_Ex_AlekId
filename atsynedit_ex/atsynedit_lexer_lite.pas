unit atsynedit_lexer_lite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  jsonConf;

type
  TATLiteLexerRule = class
  public
    Name, Regex, Style: string;
  end;

type
  { TATLiteLexer }

  TATLiteLexer = class
  public
    LexerName: string;
    CaseSens: boolean;
    FileTypes: TStringList;
    Rules: TList;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromFile(const AFilename: string);
    procedure Clear;
    function GetRule(AIndex: integer): TATLiteLexerRule;
    function GetDump: string;
  end;

implementation

{ TATLiteLexer }

constructor TATLiteLexer.Create;
begin
  inherited;
  FileTypes:= TStringList.Create;
  Rules:= TList.Create;
end;

destructor TATLiteLexer.Destroy;
begin
  Clear;
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
      s_regex:= c.GetValue('/rules/'+keys[i]+'/regex', '');
      s_style:= c.GetValue('/rules/'+keys[i]+'/style', '');
      if (s_name='') or (s_regex='') or (s_style='') then Continue;

      rule:= TATLiteLexerRule.Create;
      rule.Name:= s_name;
      rule.Regex:= s_regex;
      rule.Style:= s_style;
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
      Result:= Result+#10+Format('(name: "%s", regex: "%s", style: "%s")', [Name, Regex, Style]);
end;


end.

