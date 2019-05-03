{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Adapter_EControl;

{$mode Delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, ComCtrls,
  Forms, Dialogs,
  ATSynEdit,
  ATSynEdit_CanvasProc,
  ATSynEdit_Adapters,
  ATSynEdit_Carets,
  ATSynEdit_Ranges,
  ATSynEdit_FGL,
  ATStringProc,
  ATStringProc_TextBuffer,
  ATStrings,
  ec_SyntAnal;

var
  //interval of TimerDuringAnalyze
  cAdapterTimerDuringAnalyzeInterval: integer = 200;
  //ATSynEdit.OnIdle timer interval
  cAdapterIdleInterval: integer = 500;
  //ATSynEdit.OnIdle will fire only if text size is bigger
  cAdapterIdleTextSize: integer = 10*1000;

type

  { TATRangeInCodeTree }

  TATRangeInCodeTree = class
  public
    PosBegin: TPoint;
    PosEnd: TPoint;
    procedure Assign(Src: TATRangeInCodeTree);
  end;

type
  { TATSortedRange }

  TATSortedRange = record
    Pos1, Pos2: TPoint;
    Token1, Token2: integer;
    Color: TColor;
    Rule: TecTagBlockCondition;
    ActiveAlways: boolean;
    Active: array[0..Pred(cMaxStringsClients)] of boolean;
    class operator Equal(const a, b: TATSortedRange): boolean;
    procedure Init(
      APos1, APos2: TPoint;
      AToken1, AToken2: integer;
      AColor: TColor; ARule: TecTagBlockCondition;
      AActiveAlways: boolean);
    function IsPosInside(const APos: TPoint): boolean;
  end;

  { TATSortedRanges }

  TATSortedRanges = class(TFPGList<TATSortedRange>)
  public
    function Find(const APos: TPoint): integer;
  end;

  TATRangeCond = (cCondInside, cCondAtBound, cCondOutside);

procedure ClearTreeviewWithData(ATree: TTreeView);

type
  { TATAdapterEControl }

  TATAdapterEControl = class(TATAdapterHilite, ISynEditAdapter)
  strict private
    EdList: TList;
    Buffer: TATStringBuffer;
    TimerDuringAnalyze: TTimer;
    CurrentIdleInterval: integer;
    FRangesColored: TATSortedRanges;
    FRangesColoredBounds: TATSortedRanges;
    FRangesSublexer: TATSortedRanges;
    FEnabledLineSeparators: boolean;
    FEnabledSublexerTreeNodes: boolean;
    FBusyTreeUpdate: boolean;
    FBusyTimer: boolean;
    FStopTreeUpdate: boolean;
    FUpdateSyntaxPending:boolean;
    FTimeParseBegin: QWORD;
    FTimeParseElapsed: integer;
    FOnLexerChange: TNotifyEvent;
    FOnParseBegin: TNotifyEvent;
    FOnParseDone: TNotifyEvent;
    procedure DebugRangesColored;
    procedure DoCheckEditorList; inline;
    procedure DoFoldAdd(AX, AY, AY2: integer; AStaple: boolean; const AHint: string);
    procedure DoCalcParts(var AParts: TATLineParts; ALine, AX, ALen: integer;
      AColorFont, AColorBG: TColor; var AColorAfter: TColor; AEditorIndex: integer);
    procedure DoClearRanges;
    function DoFindToken(APos: TPoint): integer; inline;
    procedure DoUpdatePending;
    function GetTokenColor_FromBoundRanges(ATokenIndex, AEditorIndex: integer): TecSyntaxFormat;
    procedure DoFoldFromLinesHidden;
    procedure DoChangeLog(Sender: TObject; ALine: integer);
    procedure DoParseBegin;
    procedure DoParseDone;
    function GetIdleInterval: integer;
    function GetRangeParent(const R: TecTextRange): TecTextRange;
    function IsCaretInRange(AEdit: TATSynEdit; APos1, APos2: TPoint; ACond: TATRangeCond): boolean;
    function GetTokenColorBG_FromColoredRanges(APos: TPoint;
      ADefColor: TColor; AEditorIndex: integer): TColor;
    function GetTokenColorBG_FromMultiLineTokens(APos: TPoint;
      ADefColor: TColor; AEditorIndex: integer): TColor;
    function EditorRunningCommand: boolean;
    procedure TimerDuringAnalyzeTimer(Sender: TObject);
    procedure UpdateBuffer;
    procedure UpdateRanges;
    procedure UpdateRangesActive(AEdit: TATSynEdit);
    procedure UpdateRangesActiveAll;
    procedure UpdateRangesActive_Ex(AEdit: TATSynEdit; List: TATSortedRanges);
    procedure UpdateRangesSublex;
    procedure UpdateData(AAnalyze: boolean);
    procedure UpdateRangesFoldAndColored;
    procedure UpdateEditors(ARepaint, AClearCache: boolean);
    function GetLexer: TecSyntAnalyzer;
    procedure SetLexer(AAnalizer: TecSyntAnalyzer);
    function GetLexerSuportsDynamicHilite: boolean;
    function IsDynamicHiliteEnabled: boolean;
    procedure SyntaxDoneHandler();
    procedure AppendToPosDone();
  public
    AnClient: TecClientSyntAnalyzer;
    //
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddEditor(AEditor: TComponent); override;
    //
    property Lexer: TecSyntAnalyzer read GetLexer write SetLexer;
    property LexerParsingElapsed: integer read FTimeParseElapsed;
    function LexerAtPos(Pnt: TPoint): TecSyntAnalyzer;
    property EnabledSublexerTreeNodes: boolean read FEnabledSublexerTreeNodes write FEnabledSublexerTreeNodes default false;
    procedure DoAnalize(AEdit: TATSynEdit; AForceAnalizeAll: boolean);
    procedure DoAnalyzeFromLine(ALine: integer; AWait: boolean);
    function Stop: boolean;
    procedure StopTreeUpdate;

    //tokens
    procedure GetTokenWithIndex(AIndex: integer; out APntFrom, APntTo: TPoint; out
      ATokenString, ATokenStyle: string);
    procedure GetTokenAtPos(APos: TPoint; out APntFrom, APntTo: TPoint; out
      ATokenString, ATokenStyle: string);
    function GetTokenString(const token: TecSyntToken): string;
    procedure GetTokenProps(const token: TecSyntToken; out APntFrom, APntTo: TPoint;
      out ATokenString, ATokenStyle: string);

    //support for syntax-tree
    property TreeBusy: boolean read FBusyTreeUpdate;
    procedure TreeFill(ATree: TTreeView);
    procedure TreeGetPositionOfRange_EC(const R: TecTextRange; out APosBegin, APosEnd: TPoint);
    function TreeGetRangeOfPosition(APos: TPoint): TecTextRange;

    //sublexers
    function SublexerRangeCount: integer;
    function SublexerRangeProps(AIndex: integer; out AStart, AEnd: TPoint; out
      ALexerName: string): boolean;

  public
    procedure OnEditorCaretMove(Sender: TObject); override;
    procedure OnEditorChange(Sender: TObject); override;
    procedure OnEditorIdle(Sender: TObject); override;
    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer;
      var AColorAfterEol: TColor); override;
    procedure OnEditorCalcPosColor(Sender: TObject;
      AX, AY: integer; var AColor: TColor); override;
  published
    property OnLexerChange: TNotifyEvent read FOnLexerChange write FOnLexerChange;
    property OnParseBegin: TNotifyEvent read FOnParseBegin write FOnParseBegin;
    property OnParseDone: TNotifyEvent read FOnParseDone write FOnParseDone;
  end;

procedure ApplyPartStyleFromEcontrolStyle(var part: TATLinePart; st: TecSyntaxFormat);
procedure CodetreeSelectItemForPosition(ATree: TTreeView; APos: TPoint);


implementation

uses
  {$IFDEF DEBUGLOG} SynCommons, SynLog,mORMotHttpClient,  {$ENDIF}
  Math;

const
  cBorderEc: array[TecBorderLineType] of TATLineStyle = (
    cLineStyleNone,
    cLineStyleSolid,
    cLineStyleDash,
    cLineStyleDash,
    cLineStyleDash,
    cLineStyleDash,
    cLineStyleSolid2px,
    cLineStyleSolid2px,
    cLineStyleWave,
    cLineStyleDotted
    );

function ComparePoints(const P1, P2: TPoint): integer; inline;
begin
  if (P1.X=P2.X) and (P1.Y=P2.Y) then exit(0);
  if (P1.Y>P2.Y) then exit(1);
  if (P1.Y<P2.Y) then exit(-1);
  if (P1.X>P2.X) then exit(1) else exit(-1);
end;

procedure ClearTreeviewWithData(ATree: TTreeView);
var
  i: integer;
begin
  for i:= ATree.Items.Count-1 downto 0 do
    with ATree.Items[i] do
      if Data<>nil then
      begin
        TObject(Data).Free;
        Data:= nil;
      end;
  ATree.Items.Clear;
end;


procedure ApplyPartStyleFromEcontrolStyle(var part: TATLinePart; st: TecSyntaxFormat);
begin
  if Assigned(st.Font) then
  if st.FormatType in [ftCustomFont, ftFontAttr, ftColor] then
  begin
    if st.Font.Color<>clNone then
      part.ColorFont:= st.Font.Color;
  end;
  if st.FormatType in [ftCustomFont, ftFontAttr, ftColor, ftBackGround] then
  begin
    if st.BgColor<>clNone then
      part.ColorBG:= st.BgColor;
  end;
  if Assigned(st.Font) then
  if st.FormatType in [ftCustomFont, ftFontAttr] then
  begin
    part.FontBold:= fsBold in st.Font.Style;
    part.FontItalic:= fsItalic in st.Font.Style;
    part.FontStrikeOut:= fsStrikeOut in st.Font.Style;
  end;
  part.ColorBorder:= st.BorderColorBottom;
  part.BorderUp:= cBorderEc[st.BorderTypeTop];
  part.BorderDown:= cBorderEc[st.BorderTypeBottom];
  part.BorderLeft:= cBorderEc[st.BorderTypeLeft];
  part.BorderRight:= cBorderEc[st.BorderTypeRight];
end;

{ TATSortedRanges }

function TATSortedRanges.Find(const APos: TPoint): integer;

  function CompProc(ItemIndex: integer): integer; inline;
  var
    Item: TATSortedRange;
  begin
    Item:= Get(ItemIndex);
    if Item.IsPosInside(APos) then
      Result:= 0
    else
      Result:= ComparePoints(Item.Pos1, APos);
  end;

var
  L, H, I, C, NCount: Integer;
begin
  Result := -1;
  NCount := Count;
  if NCount = 0 then
    Exit;

  L := 0;
  H := NCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompProc(I);
    if C < 0 then
      L := I + 1
    else
    if C = 0 then
      Exit(I)
    else
      H := I - 1;
  end;

  Result := L;
  if Result >= NCount then
    Result := NCount - 1;
  if Result >= 0 then
    if CompProc(Result) > 0 then
      Dec(Result);
end;

{ TATRangeInCodeTree }

procedure TATRangeInCodeTree.Assign(Src: TATRangeInCodeTree);
begin
  PosBegin:= Src.PosBegin;
  PosEnd:= Src.PosEnd;
end;

{ TATSortedRange }

class operator TATSortedRange.Equal(const a, b: TATSortedRange): boolean;
begin
  Result:= false;
end;

procedure TATSortedRange.Init(APos1, APos2: TPoint; AToken1,
  AToken2: integer; AColor: TColor; ARule: TecTagBlockCondition;
  AActiveAlways: boolean);
var
  i: integer;
begin
  Pos1:= APos1;
  Pos2:= APos2;
  Token1:= AToken1;
  Token2:= AToken2;
  Color:= AColor;
  Rule:= ARule;
  ActiveAlways:= AActiveAlways;
  for i:= Low(Active) to High(Active) do
    Active[i]:= false;
end;

function TATSortedRange.IsPosInside(const APos: TPoint): boolean;
begin
  Result:= IsPosInRange(
    APos.X, APos.Y,
    Pos1.X, Pos1.Y,
    Pos2.X, Pos2.Y
    ) = cRelateInside;
end;

{ TATAdapterEControl }

procedure TATAdapterEControl.DoCheckEditorList; inline;
begin
  if EdList.Count=0 then
    raise Exception.Create('Adapter: Empty editor list');
end;

procedure TATAdapterEControl.OnEditorCalcHilite(Sender: TObject;
  var AParts: TATLineParts; ALineIndex, ACharIndex, ALineLen: integer;
  var AColorAfterEol: TColor);
var
  Ed: TATSynEdit;
begin
  if not Assigned(AnClient) then Exit;
  DoCheckEditorList;
  Ed:= Sender as TATSynEdit;

  AColorAfterEol:= clNone;
  DoCalcParts(AParts, ALineIndex, ACharIndex-1, ALineLen,
    Ed.Colors.TextFont,
    clNone,
    AColorAfterEol,
    Ed.EditorIndex);
end;

procedure TATAdapterEControl.OnEditorCalcPosColor(Sender: TObject; AX,
  AY: integer; var AColor: TColor);
var
  Ed: TATSynEdit;
begin
  Ed:= Sender as TATSynEdit;
  AColor:= GetTokenColorBG_FromColoredRanges(Point(AX, AY), AColor, Ed.EditorIndex);
end;

function TATAdapterEControl.IsCaretInRange(AEdit: TATSynEdit; APos1,
  APos2: TPoint; ACond: TATRangeCond): boolean;
var
  Caret: TATCaretItem;
  Pnt: TPoint;
  dif1, dif2: integer;
  i: integer;
  ok: boolean;
begin
  Result:= false;

  for i:= 0 to AEdit.Carets.Count-1 do
  begin
    Caret:= AEdit.Carets[i];
    Pnt:= Point(Caret.PosX, Caret.PosY);

    dif1:= ComparePoints(Pnt, APos1);
    dif2:= ComparePoints(Pnt, APos2);

    case ACond of
      cCondInside:
        ok:= (dif1>=0) and (dif2<0);
      cCondOutside:
        ok:= (dif1<0) or (dif2>=0);
      cCondAtBound:
        ok:= (dif1=0) or (dif2=0);
      else
        ok:= false;
    end;

    if ok then exit(true);
  end;
end;

function TATAdapterEControl.GetTokenColorBG_FromMultiLineTokens(APos: TPoint;
  ADefColor: TColor; AEditorIndex: integer): TColor;
var
  Token: TecSyntToken;
  n: integer;
begin
  Result:= ADefColor;
  n:= DoFindToken(APos);
  if n<0 then exit;

  Token:= AnClient.Tags[n];
  if IsPosInRange(
    APos.X, APos.Y,
    Token.Range.PointStart.X, Token.Range.PointStart.Y,
    Token.Range.PointEnd.X, Token.Range.PointEnd.Y) = cRelateInside then
    if Token.Style<>nil then
      Result:= Token.Style.BgColor;
end;


procedure TATAdapterEControl.DebugRangesColored;
var
  Rng: TATSortedRange;
begin
  if FRangesColored.Count>0 then
  begin
    Rng:= FRangesColored[0];
    Application.MainForm.Caption:= Format('RngColored: (%d,%d..%d,%d)', [Rng.Pos1.X, Rng.Pos1.Y, Rng.Pos2.X, Rng.Pos2.Y]);
  end;
end;

function TATAdapterEControl.GetTokenColorBG_FromColoredRanges(APos: TPoint;
  ADefColor: TColor; AEditorIndex: integer): TColor;
var
  Rng: TATSortedRange;
  N: integer;
begin
  Result:= ADefColor;

  N:= FRangesColored.Find(APos);
  if N>=0 then
  begin
    Rng:= FRangesColored[N];
    if Rng.IsPosInside(APos) then
      if Rng.ActiveAlways or Rng.Active[AEditorIndex] then
        exit(Rng.Color);
  end;

  N:= FRangesSublexer.Find(APos);
  if N>=0 then
  begin
    Rng:= FRangesSublexer[N];
    if Rng.IsPosInside(APos) then
      exit(Rng.Color);
  end;
end;

procedure TATAdapterEControl.UpdateRangesActive_Ex(AEdit: TATSynEdit; List: TATSortedRanges);
var
  Rng: TATSortedRange;
  act: boolean;
  i: integer;
begin
  for i:= 0 to List.Count-1 do
  begin
    Rng:= List[i];
    if Rng.ActiveAlways then
      act:= true
    else
    begin
      if Rng.Rule=nil then Continue;
      if not (Rng.Rule.DynHighlight in [dhRange, dhRangeNoBound, dhBound]) then Continue;
      case Rng.Rule.HighlightPos of
        cpAny:
          act:= true;
        cpBound:
          act:= IsCaretInRange(AEdit, Rng.Pos1, Rng.Pos2, cCondAtBound);
        cpBoundTag:
          act:= false;//todo
        cpRange:
          act:= IsCaretInRange(AEdit, Rng.Pos1, Rng.Pos2, cCondInside);
        cpBoundTagBegin:
          act:= false;//todo
        cpOutOfRange:
          act:= IsCaretInRange(AEdit, Rng.Pos1, Rng.Pos2, cCondOutside);
        else
          act:= false;
      end;
    end;
    if Rng.Active[AEdit.EditorIndex]<>act then
    begin
      Rng.Active[AEdit.EditorIndex]:= act;
      List[i]:= Rng;
    end;
  end;
end;

procedure TATAdapterEControl.UpdateRangesActive(AEdit: TATSynEdit);
var
  Rng, RngOut: TATSortedRange;
  i, j: integer;
begin
  if not IsDynamicHiliteEnabled then Exit;
  {$IFDEF DEBUGLOG}
  TSynLog.Add.Log(sllCustom1, 'UpdateRangesActive');
  {$ENDIF}
  UpdateRangesActive_Ex(AEdit, FRangesColored);
  UpdateRangesActive_Ex(AEdit, FRangesColoredBounds);

  //deactivate ranges by DynSelectMin
  //cycle back, to see first nested ranges

  for i:= FRangesColored.Count-1 downto 0 do
  begin
    Rng:= FRangesColored[i];
    if not Rng.Active[AEdit.EditorIndex] then Continue;
    if Rng.Rule=nil then Continue;
    if not Rng.Rule.DynSelectMin then Continue;
    if not (Rng.Rule.DynHighlight in [dhBound, dhRange, dhRangeNoBound]) then Continue;
    //take prev ranges which contain this range
    for j:= i-1 downto 0 do
    begin
      RngOut:= FRangesColored[j];
      if RngOut.Rule=Rng.Rule then
        if RngOut.Active[AEdit.EditorIndex] then
          if (ComparePoints(RngOut.Pos1, Rng.Pos1)<=0) and
             (ComparePoints(RngOut.Pos2, Rng.Pos2)>=0) then
            RngOut.Active[AEdit.EditorIndex]:= false;
    end;
  end;
end;


procedure TATAdapterEControl.DoCalcParts(var AParts: TATLineParts; ALine, AX,
  ALen: integer; AColorFont, AColorBG: TColor; var AColorAfter: TColor; AEditorIndex: integer);
var
  Ed: TATSynEdit;
  Strings: TATStrings;
  nColorText: TColor;
  partindex: integer;
  //
  procedure AddMissingPart(AOffset, ALen: integer);
  var
    part: PATLinePart;
  begin
    if ALen<=0 then Exit;
    part:= @AParts[partindex];
    FillChar(part^, SizeOf(TATLinePart), 0);

    part^.Offset:= AOffset;
    part^.Len:= ALen;

    (*
    ////cannot make this code OK for test Markdown file with long wrapped lines,
    ////some text chars have clNone, like white

    //check that part's last char is space (ie it's space part),
    //and set for it clNone
    if Strings.LineSub(ALine, AOffset+ALen+AX-1, 1)=' ' then
      part^.ColorFont:= clNone
    else
    *)
      part^.ColorFont:= nColorText;

    part^.ColorBG:= GetTokenColorBG_FromColoredRanges(
      Point(AX+AOffset, ALine),
      AColorBG,
      AEditorIndex);

    Inc(partindex);
  end;
  //
var
  tokenStart, tokenEnd, TestPoint: TPoint;
  startindex, mustOffset: integer;
  token: TecSyntToken;
  tokenStyle, tokenStyle2: TecSyntaxFormat;
  part: TATLinePart;
  nColor: TColor;
  i: integer;
begin
  partindex:= 0;
  FillChar(part{%H-}, SizeOf(part), 0);

  Ed:= TATSynEdit(EdList[0]);
  Strings:= Ed.Strings;
  nColorText:= Ed.Colors.TextFont;

  startindex:= DoFindToken(Point(0, ALine));
  if startindex<0 then
    startindex:= 0;

  //debug
  //Application.MainForm.Caption:= Format('adapter startindex %d', [startindex]);

  for i:= startindex to AnClient.TagCount-1 do  begin
    token:= AnClient.Tags[i];
    tokenStart:= token.Range.PointStart;
    tokenEnd:= token.Range.PointEnd;

    Dec(tokenStart.x, AX);
    Dec(tokenEnd.x, AX);

    if (tokenStart.y>ALine) then Break;
    if (tokenStart.y>ALine) or (tokenEnd.y<ALine) then Continue;
    if (tokenEnd.y<=ALine) and (tokenEnd.x<0) then Continue;
    if (tokenStart.y>=ALine) and (tokenStart.x>=ALen) then Continue;

    FillChar(part{%H-}, SizeOf(part), 0);
    if (tokenStart.y<ALine) or (tokenStart.x<0) then
      part.Offset:= 0
    else
      part.Offset:= tokenStart.X;

    if (tokenEnd.y>ALine) or (tokenEnd.x>=ALen) then
      part.Len:= ALen-part.Offset
    else
      part.Len:= tokenEnd.X-part.Offset;

    part.ColorFont:= AColorFont;
    part.ColorBG:= GetTokenColorBG_FromColoredRanges(token.Range.PointStart, AColorBG, AEditorIndex);

    tokenStyle:= token.Style;
    tokenStyle2:= GetTokenColor_FromBoundRanges(i, AEditorIndex);
    if tokenStyle2<>nil then
      tokenStyle:= tokenStyle2;
    if tokenStyle<>nil then
      ApplyPartStyleFromEcontrolStyle(part, tokenStyle);

    //add missing part
    if partindex=0 then
      mustOffset:= 0
    else
      with AParts[partindex-1] do
        mustOffset:= Offset+Len;

    if part.Offset>mustOffset then
    begin
      AddMissingPart(mustOffset, part.Offset-mustOffset);
      if partindex>=High(AParts) then Exit;
    end;

    //add calculated part
    if part.Len>0 then
    begin
      AParts[partindex]:= part;
      Inc(partindex);
      if partindex>=High(AParts) then Exit;
    end;
  end;

  //application.MainForm.Caption:= 'startindex '+inttostr(startindex)+' count-tokens '+inttostr(count);

  //add ending missing part
  //(not only if part.Len>0)
  mustOffset:= part.Offset+part.Len;
  if mustOffset<ALen then
    AddMissingPart(mustOffset, ALen-mustOffset);

  //calc AColorAfter
  TestPoint:= Point(AX+ALen, ALine);

  //a) calc it from colored-ranges
  nColor:= GetTokenColorBG_FromColoredRanges(TestPoint, clNone, AEditorIndex);
  //if (nColor=clNone) and (ALen>0) then
  //  nColor:= GetTokenColorBG_FromColoredRanges(mustOffset-1, clNone, AEditorIndex);

  //b) calc it from multi-line tokens (with bg-color)
  if (nColor=clNone) then
    nColor:= GetTokenColorBG_FromMultiLineTokens(TestPoint, clNone, AEditorIndex);

  if (nColor=clNone) then
    nColor:= AColorAfter;
  AColorAfter:= nColor;
end;

procedure TATAdapterEControl.DoClearRanges;
var
  j: integer;
  Ed: TATSynEdit;
begin
  FRangesColored.Clear;
  FRangesColoredBounds.Clear;
  FRangesSublexer.Clear;

  for j:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[j]);
    Ed.Fold.Clear;
    //Ed.Strings.ClearSeparators; //separators are not used in this adapter
  end;
end;

(*
procedure TATAdapterEControl.DoClearRanges_OnlySimple;
var
  Ed: TATSynEdit;
  R: TATSynRange;
  i, j: integer;
begin
  for j:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[j]);
    for i:= Ed.Fold.Count-1 downto 0 do
    begin
      R:= Ed.Fold.Items[i];
      if R.IsSimple then
        Ed.Fold.Delete(i);
    end;
  end;
end;
*)

constructor TATAdapterEControl.Create(AOwner: TComponent);
begin
  inherited;

  EdList:= TList.Create;
  AnClient:= nil;
  Buffer:= TATStringBuffer.Create;
  FRangesColored:= TATSortedRanges.Create;
  FRangesColoredBounds:= TATSortedRanges.Create;
  FRangesSublexer:= TATSortedRanges.Create;
  FEnabledLineSeparators:= false;
  FEnabledSublexerTreeNodes:= false;

  TimerDuringAnalyze:= TTimer.Create(Self);
  TimerDuringAnalyze.Enabled:= false;
  TimerDuringAnalyze.Interval:= cAdapterTimerDuringAnalyzeInterval;
  TimerDuringAnalyze.OnTimer:= TimerDuringAnalyzeTimer;
end;

destructor TATAdapterEControl.Destroy;
begin
  AddEditor(nil);

  if Assigned(AnClient) then
    FreeAndNil(AnClient);

  FreeAndNil(FRangesSublexer);
  FreeAndNil(FRangesColoredBounds);
  FreeAndNil(FRangesColored);

  FreeAndNil(Buffer);
  FreeAndNil(EdList);

  inherited;
end;

procedure TATAdapterEControl.AddEditor(AEditor: TComponent);
var
  i: integer;
begin
  if AEditor=nil then
  begin
    for i:= 0 to EdList.Count-1 do
      TATSynEdit(EdList[i]).AdapterForHilite:= nil;
    EdList.Clear;
  end
  else
  begin
    if EdList.IndexOf(AEditor)<0 then
    begin
      EdList.Add(AEditor);
      TATSynEdit(AEditor).Strings.OnLog:= DoChangeLog;
      TATSynEdit(AEditor).AdapterForHilite:= Self;
    end;
  end;
end;

function TATAdapterEControl.LexerAtPos(Pnt: TPoint): TecSyntAnalyzer;
begin
  Result:= nil;
  if AnClient<>nil then
    Result:= AnClient.AnalyzerAtPos(Buffer.CaretToStr(Pnt));
end;

procedure TATAdapterEControl.StopTreeUpdate;
begin
  FStopTreeUpdate:= true;
end;

function TATAdapterEControl.Stop: boolean;
begin
  Result:= true;
  TimerDuringAnalyze.Enabled:= false;

  if not Application.Terminated then
  begin
    if FBusyTreeUpdate then
    begin
      Sleep(100);
      //Application.ProcessMessages;
    end;
    if FBusyTimer then
    begin
      Sleep(TimerDuringAnalyze.Interval+50);
      //Application.ProcessMessages;
    end;
  end;

  if Assigned(AnClient) then
    Result:= AnClient.StopSyntax(false);
end;



function TATAdapterEControl.GetTokenString(const token: TecSyntToken): string;
begin
  if Assigned(Buffer) then
    Result:= Utf8Encode(Buffer.SubString(token.Range.StartPos+1, token.Range.EndPos-token.Range.StartPos))
  else
    Result:= '';
end;

procedure TATAdapterEControl.GetTokenProps(const token: TecSyntToken;
  out APntFrom, APntTo: TPoint; out ATokenString, ATokenStyle: string);
begin
  APntFrom:= token.Range.PointStart;
  APntTo:= token.Range.PointEnd;
  ATokenString:= GetTokenString(token);
  if Assigned(token.Style) then
    ATokenStyle:= token.Style.DisplayName
  else
    ATokenStyle:= '';
end;

procedure TATAdapterEControl.GetTokenWithIndex(AIndex: integer;
  out APntFrom, APntTo: TPoint; out ATokenString, ATokenStyle: string);
begin
  APntFrom:= Point(-1, -1);
  APntTo:= Point(-1, -1);
  ATokenString:= '';
  ATokenStyle:= '';

  if AnClient=nil then exit;
  if Buffer=nil then exit;

  if (AIndex>=0) and (AIndex<AnClient.TagCount) then
    GetTokenProps(AnClient.Tags[AIndex], APntFrom, APntTo, ATokenString, ATokenStyle);
end;

procedure TATAdapterEControl.GetTokenAtPos(APos: TPoint;
  out APntFrom, APntTo: TPoint;
  out ATokenString, ATokenStyle: string);
var
  n: integer;
begin
  APntFrom:= Point(-1, -1);
  APntTo:= Point(-1, -1);
  ATokenString:= '';
  ATokenStyle:= '';

  if AnClient=nil then exit;
  if Buffer=nil then exit;

  n:= DoFindToken(APos);
  if n>=0 then
    GetTokenProps(AnClient.Tags[n], APntFrom, APntTo, ATokenString, ATokenStyle);
end;


function TATAdapterEControl.GetRangeParent(const R: TecTextRange): TecTextRange;
//cannot use R.Parent!
var
  RTest: TecTextRange;
  i: integer;
begin
  Result:= nil;
  for i:= R.Index-1 downto 0 do
  begin
    RTest:= AnClient.Ranges[i];
    if (RTest.StartIdx<=R.StartIdx) and
       (RTest.EndIdx>=R.EndIdx) and
       (RTest.Level<R.Level) then
    begin
      Result:= RTest;
      Exit
    end;
  end;
end;

function TreeFindNode(ATree: TTreeView; ANode: TTreeNode; const ANodeText: string): TTreeNode;
var
  N: TTreeNode;
begin
  Result:= nil;
  if ATree.Items.Count=0 then exit;
  if ANode<>nil then
    N:= ANode.GetFirstChild
  else
    N:= ATree.Items[0];
  repeat
    if N=nil then exit;
    if N.Text=ANodeText then Exit(N);
    N:= N.GetNextSibling;
  until false;
end;

procedure TATAdapterEControl.TreeFill(ATree: TTreeView);
const
  cProgressRangeCount = 5000;
var
  R, RangeParent: TecTextRange;
  NodeParent, NodeGroup: TTreeNode;
  NodeText, NodeTextGroup, SItem: string;
  NameRule, NameLexer: string;
  NodeData: pointer;
  RangeNew: TATRangeInCodeTree;
  i: integer;
  isSynataxBusy:boolean;
begin
  isSynataxBusy:= assigned(AnClient) and (AnClient.ParserStatus<psAborted);
  if isSynataxBusy then exit;
  FStopTreeUpdate:= false;
  FBusyTreeUpdate:= true;
  //ATree.Items.BeginUpdate;

  try
    ClearTreeviewWithData(ATree);
    if AnClient=nil then exit;
    AnClient.WaitTillCoherent();
    try
    NameLexer:= AnClient.Owner.LexerName;

    for i:= 0 to AnClient.RangeCount-1 do  begin
      if FStopTreeUpdate then exit;
      if Application.Terminated then exit;
      if (i mod cProgressRangeCount)=0 then
        Application.ProcessMessages;

      R:= AnClient.Ranges[i];
      if R.Rule=nil then Continue;
      if not R.Rule.DisplayInTree then Continue;

      if not FEnabledSublexerTreeNodes then
      begin
        NameRule:= R.Rule.SyntOwner.LexerName;
        //must allow lexer name "PHP_" if main lexer is "PHP"
        if NameRule[Length(NameRule)]='_' then
          SetLength(NameRule, Length(NameRule)-1);
        if NameRule<>NameLexer then Continue;
      end;

      NodeText:= Trim(Utf8Encode(AnClient.GetRangeName(R)));
      NodeTextGroup:= Trim(Utf8Encode(AnClient.GetRangeGroup(R)));
      NodeData:= R;
      NodeParent:= nil;
      NodeGroup:= nil;

      //strip tree items from #10
      SDeleteFromEol(NodeText);
      SDeleteFromEol(NodeTextGroup);

      RangeParent:= GetRangeParent(R);
      while (RangeParent<>nil) and (not RangeParent.Rule.DisplayInTree) do
        RangeParent:= GetRangeParent(RangeParent);
      if RangeParent<>nil then
        NodeParent:= ATree.Items.FindNodeWithData(RangeParent);

      if NodeTextGroup<>'' then
        repeat
          SItem:= SGetItem(NodeTextGroup, '\');
          if (SItem='') and (NodeTextGroup='') then Break;

          if SItem='' then
            NodeGroup:= nil
          else
          begin
            NodeGroup:= TreeFindNode(ATree, NodeParent, SItem);
            if NodeGroup=nil then
            begin
              NodeGroup:= ATree.Items.AddChild(NodeParent, SItem);
              NodeGroup.ImageIndex:= R.Rule.TreeGroupImage;
              NodeGroup.SelectedIndex:= NodeGroup.ImageIndex;
            end;
          end;
          NodeParent:= NodeGroup;
        until false;

      NodeParent:= ATree.Items.AddChildObject(NodeParent, NodeText, NodeData);
      NodeParent.ImageIndex:= R.Rule.TreeItemImage;
      NodeParent.SelectedIndex:= NodeParent.ImageIndex;
    end;

    //tree filled with Data as TecTextRange
    //now replace all Data to TATRangeInCodetree
    for i:= 0 to ATree.Items.Count-1 do
    begin
      NodeParent:= ATree.Items[i];
      if NodeParent.Data=nil then Continue;
      R:= TecTextRange(NodeParent.Data);

      RangeNew:= TATRangeInCodeTree.Create;

      if R.StartIdx>=0 then
        RangeNew.PosBegin:= AnClient.Tags[R.StartIdx].Range.PointStart
      else
        RangeNew.PosBegin:= Point(-1, -1);

      if R.EndIdx>=0 then
        RangeNew.PosEnd:= AnClient.Tags[R.EndIdx].Range.PointEnd
      else
        RangeNew.PosEnd:= Point(-1, -1);

      NodeParent.Data:= RangeNew;
    end;

   finally AnClient.ReleaseBackgroundLock();end;
   finally
    //ATree.Items.EndUpdate;
    ATree.Invalidate;
    FBusyTreeUpdate:= false;
  end;
end;

procedure TATAdapterEControl.TreeGetPositionOfRange_EC(const R: TecTextRange;
  out APosBegin, APosEnd: TPoint);
begin
  APosBegin:= Point(-1, -1);
  APosEnd:= Point(-1, -1);
  if R=nil then exit;
  if AnClient=nil then exit;

  if R.StartIdx>=0 then
    APosBegin:= AnClient.Tags[R.StartIdx].Range.PointStart;

  if R.EndIdx>=0 then
    APosEnd:=  AnClient.Tags[R.EndIdx].Range.PointEnd;
end;

function TATAdapterEControl.TreeGetRangeOfPosition(APos: TPoint): TecTextRange;
var
  R: TecTextRange;
  NTokenOrig: integer;
  i: integer;
begin
  Result:= nil;
  if AnClient=nil then exit;

  NTokenOrig:= DoFindToken(APos);
  if NTokenOrig<0 then exit;

  //find last range, which contains our token
  for i:= AnClient.RangeCount-1 downto 0 do
  begin
    R:= AnClient.Ranges[i];
    if not R.Rule.DisplayInTree then Continue;

    if (R.StartIdx<=NTokenOrig) and
       (R.EndIdx>=NTokenOrig) then
       exit(R);
  end;
end;

function TATAdapterEControl.SublexerRangeCount: integer;
begin
  if Assigned(AnClient) then
    Result:= AnClient.SubLexerRangeCount
  else
    Result:= 0;
end;

function TATAdapterEControl.SublexerRangeProps(AIndex: integer;
  out AStart, AEnd: TPoint; out ALexerName: string): boolean;
var
  Range: TecSubLexerRange;
begin
  Result:= false;
  AStart:= Point(0, 0);
  AEnd:= Point(0, 0);
  ALexerName:= '';

  if AnClient=nil then exit;
  if Buffer=nil then exit;

  Result:= (AIndex>=0) and (AIndex<SublexerRangeCount);
  if Result then
  begin
    Range:= AnClient.SubLexerRanges[AIndex];
    if Range.Range.StartPos<0 then exit;
    AStart:= Range.Range.PointStart;
    AEnd:= Range.Range.PointEnd;
    if Assigned(Range.Rule) and Assigned(Range.Rule.SyntAnalyzer) then
      ALexerName:= Range.Rule.SyntAnalyzer.LexerName;
  end;
end;

procedure CodetreeSelectItemForPosition(ATree: TTreeView; APos: TPoint);
var
  Node, NodeNear, NodeResult: TTreeNode;
  Range: TATRangeInCodeTree;
  Pos1, Pos2: TPoint;
  i: integer;
begin
  NodeResult:= nil;
  NodeNear:= nil;

  //ranges are sorted, so we find _last_ range which
  //includes APos
  for i:= ATree.Items.Count-1 downto 0 do
  begin
    Node:= ATree.Items[i];
    if Node.Data<>nil then
      if TObject(Node.Data) is TATRangeInCodeTree then
      begin
        Range:= TATRangeInCodeTree(Node.Data);
        Pos1:= Range.PosBegin;
        Pos2:= Range.PosEnd;

        if NodeNear=nil then
          if (Pos1.Y>=0) and IsPosSorted(Pos1.X, Pos1.Y, APos.X, APos.Y, true) then
            NodeNear:= Node;

        if IsPosInRange(
          APos.X, APos.Y,
          Pos1.X, Pos1.Y,
          Pos2.X, Pos2.Y,
          true) = cRelateInside then
        begin
          NodeResult:= Node;
          Break;
        end;
      end;
  end;

  if NodeResult=nil then
    if NodeNear<>nil then
      NodeResult:= NodeNear;

  if Assigned(NodeResult) then
  begin
    NodeResult.MakeVisible;
    ATree.Selected:= NodeResult;
  end;
end;


procedure TATAdapterEControl.OnEditorCaretMove(Sender: TObject);
begin
  if not FUpdateSyntaxPending then
     UpdateRangesActive(Sender as TATSynEdit);
end;


procedure TATAdapterEControl.SetLexer(AAnalizer: TecSyntAnalyzer);
begin
  DoClearRanges;
  UpdateEditors(false, true);

  if Assigned(AnClient) then
    FreeAndNil(AnClient);

  DoParseBegin;

  if Assigned(AAnalizer) then
  begin
    AnClient:= TecClientSyntAnalyzer.Create(AAnalizer, Buffer, nil,self, true);

    if Buffer.TextLength=0 then
      UpdateBuffer;

    UpdateData(true);
  end;

  if Assigned(FOnLexerChange) then
    FOnLexerChange(Self);

  DynamicHiliteSupportedInCurrentSyntax:= GetLexerSuportsDynamicHilite;
end;

procedure TATAdapterEControl.OnEditorChange(Sender: TObject);
begin
  DoCheckEditorList;
  UpdateBuffer;
  //if CurrentIdleInterval=0, OnEditorIdle will not fire, analyze here
  UpdateData(CurrentIdleInterval=0);
end;

procedure TATAdapterEControl.OnEditorIdle(Sender: TObject);
begin
  DoCheckEditorList;
  UpdateData(true);
 // UpdateEditors(true, true);
end;

procedure TATAdapterEControl.UpdateBuffer;
var
  Lens: array of integer;
  Strs: TATStrings;
  i: integer;
begin
  AnClient.StopSyntax(false);

  if EdList.Count=0 then Exit;
  Strs:= TATSynEdit(EdList[0]).Strings;
  SetLength(Lens{%H-}, Strs.Count);

  for i:= 0 to Length(Lens)-1 do
    Lens[i]:= Strs.LinesLen[i];

  AnClient.WaitTillCoherent();
  try
    Buffer.Setup(Strs.TextString_Unicode(cMaxLenToTokenize), Lens);
  finally
    AnClient.ReleaseBackgroundLock();
  end;
end;

procedure TATAdapterEControl.UpdateData(AAnalyze: boolean);
var
  Ed: TATSynEdit;
begin
  if EdList.Count=0 then Exit;
  if not Assigned(AnClient) then Exit;

  if AAnalyze then
  begin
    DoUpdatePending;

    Ed:= TATSynEdit(EdList[0]);
    DoAnalize(Ed, false);

    {
    //don't clear ranges too early (avoid flicker with empty fold bar)
    if not EditorRunningCommand
      or IsDynamicHiliteEnabled then
      UpdateRanges;
      }
  end;
end;

procedure TATAdapterEControl.UpdateRanges;
begin
  {$IFDEF DEBUGLOG}
  TSynLog.Add.Log(sllCustom1, 'UpdateRanges');
  {$ENDIF}
  DoClearRanges;
  UpdateRangesFoldAndColored;
  UpdateRangesSublex; //sublexer ranges last
  UpdateRangesActiveAll;
end;

procedure TATAdapterEControl.UpdateRangesActiveAll;
var
  i: integer;
begin
  for i:= 0 to EdList.Count-1 do
    UpdateRangesActive(TATSynEdit(EdList[i]));
end;

function TATAdapterEControl.EditorRunningCommand: boolean;
var
  i: integer;
begin
  Result:= false;
  if EdList.Count>0 then
    for i:= 0 to EdList.Count-1 do
      if TATSynEdit(EdList[i]).IsRunningCommand then
        exit(true);
end;

procedure TATAdapterEControl.DoAnalize(AEdit: TATSynEdit; AForceAnalizeAll: boolean);
var
  NLine, NPos: integer;
begin
  if AnClient=nil then exit;
  if Buffer.TextLength=0 then exit;

  DoParseBegin;

  if AForceAnalizeAll then
  begin
    AnClient.TextChanged(0);
    AnClient.Analyze;
    AnClient.HandleAddWork();

  end
  else  begin
    //LineBottom=0, if file just opened at beginning.
    //or >0 of file is edited at some scroll pos
    NLine:= AEdit.LineBottom;
    if NLine=0 then
      NLine:= AEdit.GetVisibleLines;
    NLine:= Min(NLine, Buffer.Count-1);
    NPos:= Buffer.CaretToStr(Point(0, NLine));
    AnClient.AppendToPos(NPos);
    AnClient.HandleAddWork;
  end;

  if AnClient.ParserStatus>=psAborted then
  begin
    DoParseDone;
  end
  else
  begin
    //UpdateEditors(false, true);
      //some portion is parsed already
      //ARepaint=false, otherwise we get 2 unneeded repaints per each edit

    TimerDuringAnalyze.Enabled:= true;
  end;
end;

procedure TATAdapterEControl.DoFoldAdd(AX, AY, AY2: integer; AStaple: boolean; const AHint: string);
var
  j: integer;
begin
  if EdList.Count>0 then
    for j:= 0 to EdList.Count-1 do
      TATSynEdit(EdList[j]).Fold.Add(AX, AY, AY2, AStaple, AHint);
end;

procedure TATAdapterEControl.UpdateEditors(ARepaint, AClearCache: boolean);
var
  Ed: TATSynEdit;
  i: integer;
begin
  for i:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[i]);

    CurrentIdleInterval:= GetIdleInterval;
    Ed.OptIdleInterval:= CurrentIdleInterval;

    if AClearCache then
      Ed.InvalidateHilitingCache;
    if ARepaint then
      Ed.Update;
  end;
end;


procedure TATAdapterEControl.DoFoldFromLinesHidden;
var
  i: integer;
begin
  for i:= 0 to EdList.Count-1 do
    TATSynEdit(EdList[i]).UpdateFoldedFromLinesHidden;
end;


procedure TATAdapterEControl.UpdateRangesFoldAndColored;
var
  R: TecTextRange;
  Pnt1, Pnt2: TPoint;
  Style: TecSyntaxFormat;
  SHint: string;
  tokenStart, tokenEnd: TecSyntToken;
  ColoredRange: TATSortedRange;
  i: integer;
begin
  if not Assigned(AnClient) then Exit;

  //check folding enabled
  if EdList.Count>0 then
    if not TATSynEdit(EdList[0]).OptFoldEnabled then exit;

  for i:= 0 to AnClient.RangeCount-1 do
  begin
    if Application.Terminated then exit;

    R:= AnClient.Ranges[i];
    if R.Rule.BlockType<>btRangeStart then Continue;

    /////issue: rules in C# with 'parent' set give wrong ranges;
    //rule "function begin", "prop begin";
    //e.g. range from } bracket to some token before "else"
    //temp workard: skip rule with 'parent'
    {$ifdef skip_some_rules}
    if R.Rule.NotParent then Continue;
    {$endif}

    if R.StartIdx<0 then Continue;
    if R.EndIdx<0 then Continue;

    tokenStart:= AnClient.Tags[R.StartIdx];
    tokenEnd:= AnClient.Tags[R.EndIdx];
    Pnt1:= tokenStart.Range.PointStart;
    Pnt2:= tokenEnd.Range.PointEnd;
    if Pnt1.Y<0 then Continue;
    if Pnt2.Y<0 then Continue;

    //fill fold ranges
    if not R.Rule.NotCollapsed then
    begin
      SHint:= UTF8Encode(AnClient.GetCollapsedText(R)); //+'/'+R.Rule.GetNamePath;
      DoFoldAdd(Pnt1.X+1, Pnt1.Y, Pnt2.Y, R.Rule.DrawStaple, SHint);
    end;

    //fill FRangesColored
    //not only if DymamicHilite enabled (e.g. AutoIt has always hilited blocks)
    if R.Rule.DynHighlight<>dhNone then
    begin
      Style:= R.Rule.Style;
      if Style<>nil then
        if Style.BgColor<>clNone then
        begin
          //support lexer opt "Hilite lines of block"
          if R.Rule.Highlight then
          begin
            Pnt2.X:= Buffer.LineLength(Pnt2.Y) + 1;
              //+1 to make range longer, to hilite line to screen end
          end;

          ColoredRange.Init(
            Pnt1,
            Pnt2,
            R.StartIdx,
            R.EndIdx,
            Style.BgColor,
            R.Rule,
            (R.Rule.HighlightPos=cpAny)
            );

          if R.Rule.DynHighlight=dhBound then
            FRangesColoredBounds.Add(ColoredRange)
          else
            FRangesColored.Add(ColoredRange);
        end;
    end;
  end;

  //keep folded blks that were folded
  DoFoldFromLinesHidden;
end;

procedure TATAdapterEControl.UpdateRangesSublex;
var
  R: TecSubLexerRange;
  Style: TecSyntaxFormat;
  Range: TATSortedRange;
  i: integer;
begin
  for i:= 0 to AnClient.SubLexerRangeCount-1 do
  begin
    if Application.Terminated then exit;

    R:= AnClient.SubLexerRanges[i];
    if R.Rule=nil then Continue;
    if R.Range.StartPos<0 then Continue;
    if R.Range.EndPos<0 then Continue;

    Style:= R.Rule.Style;
    if Style=nil then Continue;
    if Style.BgColor<>clNone then
    begin
      Range.Init(
        R.Range.PointStart,
        R.Range.PointEnd,
        -1,
        -1,
        Style.BgColor,
        nil,
        true
        );
      FRangesSublexer.Add(Range);
    end;
  end;
end;


function TATAdapterEControl.DoFindToken(APos: TPoint): integer; inline;
begin
  Result:= AnClient.PriorTokenAt(AnClient.Buffer.CaretToStr(APos));
end;

procedure TATAdapterEControl.DoUpdatePending;
begin
  FUpdateSyntaxPending:= true;
  //FRangesColored.Clear;
  //FRangesColoredBounds.Clear;
  //FRangesSublexer.Clear;
end;

function TATAdapterEControl.GetLexer: TecSyntAnalyzer;
begin
  if Assigned(AnClient) then
    Result:= AnClient.Owner
  else
    Result:= nil;
end;

procedure TATAdapterEControl.DoChangeLog(Sender: TObject; ALine: integer);
var
  Pos: integer;
begin
  if not Assigned(AnClient) then Exit;

  //clear?
  if ALine=-1 then
  begin
    AnClient.TextChanged(-1);
    Exit
  end;

  if ALine>=Buffer.Count then
    Pos:= Buffer.TextLength
  else
    Pos:= Buffer.CaretToStr(Point(0, ALine));

  AnClient.TextChanged(Pos);
end;

procedure TATAdapterEControl.TimerDuringAnalyzeTimer(Sender: TObject);
begin
  if Application.Terminated then
  begin
    TimerDuringAnalyze.Enabled:= false;
    exit
  end;
  exit;
  if not Assigned(AnClient) then Exit;
  if (FBusyTreeUpdate or FBusyTimer) then Exit;
  FBusyTimer:= true;
  try
    if AnClient.ParserStatus>=psAborted then begin
      TimerDuringAnalyze.Enabled:= false;
      UpdateRanges;
      DoParseDone;
    end;
  finally
    FBusyTimer:= false;
  end;
end;


function TATAdapterEControl.GetTokenColor_FromBoundRanges(ATokenIndex, AEditorIndex: integer): TecSyntaxFormat;
var
  Rng: TATSortedRange;
  i: integer;
begin
  Result:= nil;
  if not IsDynamicHiliteEnabled then exit;

  //Cannot use binary search here, because FRangesColoredBounds has overlapping ranges,
  //so using Find() will miss some tokens
  for i:= 0 to FRangesColoredBounds.Count-1 do  begin
    Rng:= FRangesColoredBounds[i];
    if Rng.Active[AEditorIndex] then
      //if Rng.Rule<>nil then
        //if Rng.Rule.DynHighlight=dhBound then //all items in FRangesColredBounds have dhBound
          if (Rng.Token1=ATokenIndex) or (Rng.Token2=ATokenIndex) then
            exit(Rng.Rule.Style);
  end;
end;

function TATAdapterEControl.GetLexerSuportsDynamicHilite: boolean;
var
  An: TecSyntAnalyzer;
  Rule: TecTagBlockCondition;
  i: integer;
begin
  Result:= false;
  if not Assigned(AnClient) then exit;
  An:= AnClient.Owner;
  for i:= 0 to An.BlockRules.Count-1 do
  begin
    Rule:= An.BlockRules[i];
    if Assigned(Rule) and
      (Rule.HighlightPos in [cpBound, cpRange, cpOutOfRange]) and
      (Rule.DynHighlight in [dhRange, dhRangeNoBound, dhBound]) then exit(true);
  end;
end;

function TATAdapterEControl.IsDynamicHiliteEnabled: boolean;
var
  Ed: TATSynEdit;
begin
  Ed:= TATSynEdit(EdList[0]);
  Result:= DynamicHiliteActiveNow(Ed.Strings.Count);
end;

procedure TATAdapterEControl.SyntaxDoneHandler();
begin
  FUpdateSyntaxPending:=false;
  DoParseDone;
end;

procedure TATAdapterEControl.AppendToPosDone();
var edit:Pointer;
begin
  {$IFDEF DEBUGLOG}
  TSynLog.Add.Log(sllClient, 'AppendToPosDone');
  {$ENDIF}
 for edit in  EdList do begin
   if assigned(edit) and (TObject(edit) is TATSynEdit) then
      TATSynEdit(edit).Invalidate;
 end;

end;

procedure TATAdapterEControl.DoParseBegin;
begin
  if Assigned(FOnParseBegin) then
    FOnParseBegin(Self);
  FStopTreeUpdate:= false;
  FTimeParseBegin:= GetTickCount64;
end;

procedure TATAdapterEControl.DoParseDone;
begin
  //UpdateRanges call needed for small files, which are parsed to end by one IdleAppend call,
  //and timer didn't tick
  UpdateRanges;
  FTimeParseElapsed:= GetTickCount64-FTimeParseBegin;
  if Assigned(FOnParseDone) then
    FOnParseDone(Self);

  UpdateEditors(true, true);
end;

procedure TATAdapterEControl.DoAnalyzeFromLine(ALine: integer; AWait: boolean);
var
  NPos: integer;
begin
  if not Assigned(AnClient) then exit;
  DoParseBegin;
  NPos:= Buffer.CaretToStr(Point(0, ALine));
  AnClient.ChangedAtPos(NPos);
  AnClient.AppendToPos(Buffer.TextLength);
  AnClient.HandleAddWork;

  if AnClient.ParserStatus>=psAborted then
  begin
    DoParseDone;
  end
  else
  begin
    TimerDuringAnalyze.Enabled:= true;

    if AWait then
      while AnClient.ParserStatus<psAborted do begin
        Sleep(TimerDuringAnalyze.Interval+20);
        Application.ProcessMessages;
      end;
  end;
end;

function TATAdapterEControl.GetIdleInterval: integer;
begin
  if Buffer.TextLength < cAdapterIdleTextSize then
    Result:= 0
  else
    Result:= cAdapterIdleInterval;
end;

end.

