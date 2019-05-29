unit SizeControl;

(*
 ---------------------------------------------------------------------------
Component Name:  TSizeCtrl
Module:          SizeControl
Description:     Enables both moving and resizing of controls at runtime.
Version:         8.1
Date:            28-MAY-2019
Author:          Leu Zenin, kashaket@protonmail.com
Copyright:      © 2019 Leu Zenin
                {near 90% code is refactored}
                © 1997-2007 Angus Johnson
 --------------------------------------------------------------------------- *)

interface

{$R SIZECONTROL}
{$IFDEF VER80}
  {$DEFINE VER3D}
{$ENDIF}
{$IFDEF VER90}
  {$DEFINE VER3D}
{$ENDIF}
{$IFDEF VER100}
  {$DEFINE VER3D}
{$ENDIF}
{$IFNDEF VER80} {$IFNDEF VER90} {$DEFINE VER3U} {$ENDIF} {$ENDIF}
{$IFDEF VER3U} {$IFNDEF VER100} {$DEFINE VER3UP} {$ENDIF} {$ENDIF}
uses
  Windows, Messages, SysUtils, Classes,
  Controls, Graphics,
  Dialogs,
  Menus,   //To hook the TSizeCtrl.PopupMenu
  ComCtrls, //To check the TTabSheet, TPageControl
 {$IFDEF VER3U} TypInfo, {$ENDIF} //To hook the OnClick event
  Forms, Math;
  (* [TSizeBtn reqs]
    To make transparent and topmost at the same time...
    Another way is to use TGraphicControl, but, it doesn't gives ability
    to use the Alpha-Blending :)
  *)

function getAbsoluteX(cntrl: TControl; LastControl: TControl): integer;
function getAbsoluteY(cntrl: TControl; LastControl: TControl): integer;

type
  TSizeCtrl = class;
  TTargetObj = class;
  TBtnPos = (bpNone, bpLeft, bpTop, bpRight, bpBottom, bpTopLeft, bpTopRight,
    bpBottomRight, bpBottomLeft);
  TBtnPosSet = set of TBtnPos;
  TSizeCtrlBtnCount = (szctrl4btns, szctrl8btns);
  TSCState = (scsReady, scsMoving, scsSizing);

  TStartEndEvent = procedure(Sender: TObject; State: TSCState) of object;
  TDuringEvent = procedure(Sender: TObject; dx, dy: integer; State: TSCState) of object;
  TMouseDownEvent = procedure(Sender: TObject; Target: TControl;
    TargetPt: TPoint; var handled: boolean) of object;
  TSetCursorEvent = procedure(Sender: TObject; Target: TControl;
    TargetPt: TPoint; var handled: boolean) of object;

  TContextPopupEvent = procedure(Sender: TObject; MousePos: TPoint;
    var Handled: boolean) of object;
  TReSizeFrameType = (tszfNone, tszfButtons, tzfRButtons, tszfRButton);
  TReSizeHideType = (tszhNone, tszhMove, tszhHide);
  TSizeBtnShapeType = (tszbSquare, tszbTriangle, tszbCircle,
  tszbRoundRect, tszbRhombus, tszbMockTube);
  //TSizeBtn is used internally by TSizeCtrl.
  //There are 8 TSizeBtns for each target which are the target's resize handles.
  TSizeBtn = class(TCustomForm)
  private
    fTargetObj: TTargetObj;
    fPos: TBtnPos;
    fHoverDown, fHover: boolean;
    fLeft, fTop: integer;
    fColor, fPen: TColor;
    fImage: TPicture;
    fOldWindow: TWndMethod;
  protected
    procedure DrawTriangle(l, t:integer);
    procedure PaintAs(l,t:integer);
    procedure doPaint(Sender:TObject);
    procedure mEnter(Sender:TObject);
    procedure mLeave(Sender:TObject);
    procedure UpdateBtnCursorAndColor;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure dMouseUp;
    function GetTop: integer;
    function GetLeft: integer;
    procedure setLeft(value: integer);
    procedure SetTop(value: integer);
  public
    procedure Reset;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    constructor Create(TargetObj: TTargetObj; BtnPos: TBtnPos);
 {$IFNDEF VER3U} reintroduce; {$ENDIF}
  end;

  TMovePanel = class(TCustomForm)
  private
    fSizeCtrl: TSizeCtrl;
    procedure setfcanvas(fCanvas: TCanvas);
  protected
    procedure SetBoundsRect(Sender: TObject);
    procedure DoPaint(Sender: TObject);
  public
    constructor Create(AOwner: TComponent;AsizeCtrl:TSizeCtrl);
    {$IFNDEF VER3U} reintroduce; {$ENDIF}
    property RectCanvas: TCanvas write setfCanvas;
  end;


  //TRegisteredObj is used internally by TSizeCtrl. Each TRegisteredObj
  //contains info about a possible target control.
  TRegisteredObj = class
  protected
    fSizeCtrl: TSizeCtrl; //the owner of TRegisteredObj
    fControl: TControl;
    fHooked: boolean;
    fOldWindowProc: TWndMethod;
    fOldClickMethod: TMethod;
    procedure Hook;
    procedure UnHook;
    procedure NewWindowProc(var Msg: TMessage);
    procedure NewWindowProcB(var Msg: TMessage);
  public
    constructor Create(aSizeCtrl: TSizeCtrl; aControl: TControl);
  {$IFNDEF VER3U} reintroduce; {$ENDIF}
    destructor Destroy; override;
  end;

  //TTargetObj is the container for each current target, and contains the 8
  //TSizeBtn objects. Any number of TTargetObj's can be contained by TSizeCtrl.
  TTargetObj = class
  private
    fSizeCtrl: TSizeCtrl; //the owner of TTargetObj
    fTarget: TControl;
    fPanels: TList;
    fPanelsNames: TStrings;
    fBtns: array [TBtnPos] of TSizeBtn;
    fFocusRect: TRect;
    fLastRect: TRect;
    fStartRec: TRect;
    procedure Update;
    procedure UpdateExtra(const Rectange: TRect;const typer: integer);
    procedure StartFocus();
    function MoveFocus(dx, dy: integer): boolean;
    function SizeFocus(dx, dy: integer; BtnPos: TBtnPos): boolean;
    procedure EndFocus;
    procedure DrawRect(dc: hDC; obj: TControl);
  public
    constructor Create(aSizeCtrl: TSizeCtrl; aTarget: TControl);
  {$IFNDEF VER3U} reintroduce; {$ENDIF}
    destructor Destroy; override;
    procedure ReconfButtons;
  end;
  TSizeCtrlTags = record
  private
    var
    fMove, fNMove,   //Move  --/ /-- fTopLeft
    fmMove, fnmMove,   //Multi-Move
    fResize, fnResize,  //Resize  --/ /-- fHeightWidth
    fMResize, fNMResize,  //Multi-Resize
     fChange, fnChange,    //Any change
     fMChange, fnMChange,   //Any change in selected group
      { x; y; w; h; }
    fLeft, fTop, fWidth, fHeight,
      { x, h; x, w; }
    fLeftHeight, fLeftWidth,
      { y, h; y, w; }
    fTopHeight, fTopWidth,
      { x, y, w, h; x, y, w; x, y, h; }
    fTLWH, fTLW, fTLH: integer;
  public
    //ChangeTopLeft: integer read fnTopLeft write fnTopLeft;
    property AllowMove: integer read fMove write fMove;
    property DenyMove: integer read fNMove write fNMove;
    property AllowMultiMove: integer read fMMove write fMMove;
    property DenyMultiMove: integer read fNMMove write fNMMove;

    property AllowChange: integer read fChange write fChange;
    property DenyChange: integer read fNChange write fNChange;
    property AllowMultiChange: integer read fmChange write fmChange;
    property DenyMultiChange: integer read fnMChange write fnMChange;

    property ChangeTop: integer read fTop write fTop;
    property ChangeLeft: integer read fLeft write fLeft;
    property ChangeWidth: integer read fWidth write fWidth;
    property ChangeHeight: integer read fHeight write fHeight;

    property ChangeTopHeight: integer read fTopHeight write fTopHeight;
    property ChangeTopWidth: integer read fTopWidth write fTopWidth;
    property ChangeLeftHeight: integer read fLeftHeight write fLeftHeight;
    property ChangeLeftWidth: integer read fLeftWidth write fLeftWidth;

    property ChangeTopLeftWidth: integer read fTLW write fTLW;
    property ChangeTopLeftHeight: integer read fTLH write fTLH;
    property ChangeTopLeftWidthHeight: integer read fTLWH write fTLWH;

    // ChangeHeightWidth: integer read fnHeightWidth write fnHeightWidth;
    property AllowResize:  integer read fResize write fResize;
    property DenyResize: integer read fNResize write fNResize;

    property AllowMultiResize: integer read fMResize write fMResize;
    property DenyMultiResize: integer read fNMResize write fNMresize;
  end;

  TSizeCtrl = class(TComponent)
  private
    fLastBtn: TBtnPos;
    fTargetList: TList; //list of TTargetObj (current targets)
    fRegList: TList;    //list of TRegisteredObj (possible targets)
    fState: TSCState;
    fMoveOnly, fEditDisabled: boolean;
    fBtnAlpha, fBtnSize: integer;
    fClipRec: TRect;
    fStartPt: TPoint;
    fEnabledBtnColor: TColor;
    fHoverBtnColor: TColor;
    fDisabledBtnColor: TColor;
    fValidBtns: TBtnPosSet;
    fMultiResize: boolean;
    fEnabled: boolean;
    fCapturedCtrl: TControl;
    fCapturedBtnPos: TBtnPos;
    fGridSize: integer;
    fOldWindowProc: TWndMethod;
    fEscCancelled: boolean;
    fParentForm: TCustomForm;
    fHandle: THandle;
    fPopupMenu: TPopupMenu;
    fOnContextPopup: TContextPopupEvent;
    fLMouseDownPending: boolean;
    fForm: TWinControl;
    fTags: TSizeCtrlTags;
    fGridWhite: TColor;
    fGridBlack: TColor;
    fBtnFrameColor: TColor;
    fHoverBtnFrameColor: TColor;
    fDisabledBtnFrameColor: TColor;
    fBtnShape: TSizeBtnShapeType;
    fReSizeType: TReSizeFrameType;
    fReIgnBtn: TReSizeHideType;
    fShowFrame, fApplySizes: boolean;
    fBtnCount: TSizeCtrlBtnCount;
    FSelKey,
      fSelActionCancelKey,
      fSelToggleKey,
      fMoveLeftKey, fMoveTopKey,
      fMoveRightKey, fMoveBottomKey,
     FDGKey: integer;
    fStartEvent: TStartEndEvent;
    fDuringEvent: TDuringEvent;
    fEndEvent: TStartEndEvent;
    fTargetChangeEvent, fOnBh, fOnBtnUh: TNotifyEvent;
    fOnMouseDown: TMouseDownEvent;
    fOnMouseEnter: TMouseDownEvent;
    fOnSetCursor: TSetCursorEvent;
    fOnKeyDown: TKeyEvent;
    FShowGrid: boolean;
    fCanv: TCanvas;
    fMovePanelAlpha: integer;
    fPanelImage: TPicture;
    fBtnImage: TPicture;
    fHoverBtnImage: TPicture;
    fDisabledBtnImage: TPicture;
    fStretchBtnImage, fStretchPanelImage: boolean;
    function GetTargets(index: integer): TControl;
    function GetTargetCount: integer;

    procedure SetEnabled(Value: boolean);
    procedure WinProc(var Msg: TMessage);
    procedure FormWindowProc(var Msg: TMessage);
    procedure DoWindowProc(DefaultProc: TWndMethod; var Msg: TMessage);

    procedure DrawRect;
    procedure SetMoveOnly(Value: boolean);
    function IsValidSizeBtn(BtnPos: TBtnPos): boolean;
    function IsValidMove: boolean;
    procedure SetMultiResize(Value: boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure DoPopupMenuStuff;
    procedure setGridSize(Value: integer);
    procedure setGridWhite(Value: TColor);
    procedure setGridBlack(Value: TColor);
    procedure SetBtnCount(Value: TSizeCtrlBtnCount);
    procedure setBtnSize(Value: integer);
    procedure setBtnAlphaBlend(Value: integer);
    procedure setMovePanelAlphaBlend(Value: integer);
    procedure setPanelImage(Value: TPicture);
    procedure setBtnImage(Value: TPicture);
    procedure setHoverBtnImage(Value: TPicture);
    procedure setDisabledBtnImage(Value: TPicture);
    procedure setStretchBtnImage(Value: boolean);
    procedure SetEnabledBtnColor(aColor: TColor);
    procedure SetHoverBtnColor(aColor: TColor);
    procedure SetDisabledBtnColor(aColor: TColor);
    procedure SetBtnShape(v: TSizeBtnShapeType);
    procedure SetBtnFrameColor(v: TColor);
    procedure SetHoverBtnFrameColor(v: TColor);
    procedure SetDisabledBtnFrameColor(v: TColor);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState);
    procedure SetShowGrid(const Value: boolean);
  protected
    fGrid: TBitmap; // сетка
    fGridForm: TForm;
    lastW: integer;
    lastH: integer; // последняя ширина и высота формы
    lastColor: TColor; // последний цвет формы
    FConstraints: TSizeConstraints;
    procedure CreateGrid;
    procedure Hide;
    procedure Show;
    procedure UpdateGrid;
    procedure HardReset(sizes:boolean=false);
    procedure UpdateBtnCursors;
    procedure MoveTargets(dx, dy: integer);
    procedure SizeTargets(dx, dy: integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function DoKeyDown(var Message: TWMKey): boolean;

    procedure formPaint(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FixSize(input, wh: integer): integer;
    function KeysToShiftState(Keys: Word): TShiftState;
    function KeyDataToShiftState(KeyData: Longint): TShiftState;
    property LastBtn: TBtnPos read fLastBtn;
    //Targets: used to access individual targets (read-only)
    property Targets[index: integer]: TControl read GetTargets;
  published
  function RegisteredCtrlFromPt(screenPt: TPoint;
      ParentX: TWinControl = nil): TControl;
    //Update: it is the responsibility of the component user to call Update
    //if the target(s) are moved or resized independently of this control
    //(eg if the form is resized and targets are aligned with it.)
    procedure Update;
    procedure UpdateBtns;

    procedure toFront(CNTR: TControl);
    procedure toBack(CNTR: TControl);

    //RegisterControl: Register potential target controls with TSizeCtrl
    function RegisterControl(Control: TControl): integer;
    procedure UnRegisterControl(Control: TControl);
    procedure UnRegisterAll;
    function RegisteredIndex(Control: TControl): integer;

    function getRegObj(C: TComponent): TRegisteredObj;

    //AddTarget: Add any number of targets to TSizeCtrl so they can be
    //resized or moved together.
    //(nb: The programmer doesn't normally need to call this method directly
    //since TSizeCtrl will call it whenever a target is clicked.)
    function AddTarget(Control: TControl): integer;
    function getSelected(): TList;

    procedure DeleteTarget(Control: TControl);
    procedure ClearTargets;
    function TargetIndex(Control: TControl): integer;
    function TargetCtrlFromPt(screenPt: TPoint): TControl;

    //Enabled: This key property should be self-explanatory.
    property Enabled: boolean read fEnabled write SetEnabled;
    //<summary>
    //Used for getting targets count
    //</summary>
    property TargetCount: integer read GetTargetCount;
    property Parent:TWinControl read FForm;
    //MinWidth: minimal target (resizing) width
    //MinHeight: minimal target (resizing) height
    //MaxWidth: maximal target (resizing) width
    //MaxHeight: maximal target (resizing) height
    property Constraints: TSizeConstraints read FConstraints write FConstraints;
    property TagOptions: TSizeCtrlTags read fTags write fTags;
    //MoveOnly: ie prevents resizing
    property MoveOnly: boolean read fMoveOnly write SetMoveOnly;
    //EditDisabled: ie allows disabled control(s) editing
    property EditDisabled: boolean read fEditDisabled write fEditDisabled;
    //BtnCount: Count of the grab buttons
    property BtnCount: TSizeCtrlBtnCount read fBtnCount write setBtnCount;
    //BtnSize: Size of a grab-handle buttons
    property BtnSize: integer read FBtnSize write setBtnSize;
    //BtnAlphaBlend: Alpha-blend semitransparent multiplier for grab btns
    property BtnAlphaBlend: integer read fBtnAlpha write setBtnAlphaBlend;
    //BtnColor: Color of grab-handle buttons
    property BtnColor: TColor read fEnabledBtnColor write SetEnabledBtnColor;
    //BtnColor: Color of grab-handle buttons
    property HoverBtnColor: TColor read fHoverBtnColor write SetHoverBtnColor;

    //BtnColorDisabled: eg grab buttons along aligned edges of target controls
    property DisabledBtnColor: TColor read fDisabledBtnColor write SetDisabledBtnColor;
    property BtnShape: TSizeBtnShapeType read fBtnShape write setBtnShape;
    property BtnFrameColor: TColor read fBtnFrameColor write setBtnFrameColor;
    property HoverBtnFrameColor: TColor read fHoverBtnFrameColor write setHoverBtnFrameColor;
    property DisabledBtnFrameColor: TColor read fDisabledBtnFrameColor write setDisabledBtnFrameColor;
    //BtnImage eg grab buttons along 8 edges of target controls
    property BtnImage: TPicture read fBtnImage write setBtnImage;
    //Hover-state button(s)
    property HoverBtnImage: TPicture read fHoverBtnImage write setHoverBtnImage;
    //DisabledBtnImage - you will understand
    property DisabledBtnImage: TPicture read fDisabledBtnImage write setDisabledBtnImage;

    //BtnImage eg grab buttons along 8 edges of target controls
    property StretchBtnImage: Boolean read fStretchBtnImage write setStretchBtnImage;

    //Custom keymaps
    property SelectionKey: integer read FSelKey write FSelKey;
    property SelectionCancelActionKey:
      integer read fSelActionCancelKey write fSelActionCancelKey;
    property SelectionTabKey:
      integer read  fSelToggleKey write fSelToggleKey;
    property MoveLeftKey: integer read fMoveLeftKey write fMoveLeftKey;
    property MoveRightKey:integer read fMoveRightKey write fMoveRightKey;
    property MoveUpKey:integer read  fMoveTopKey write fMoveTopKey;
    property MoveDownKey:integer read fMoveBottomKey write fMoveBottomKey;
    property DisableGridAlignKey: integer read FDGKey write FDGKey;

    property ShowGrid: boolean read FShowGrid write SetShowGrid;
    //GridSize: aligns mouse moved/resized controls to nearest grid dimensions
    property GridSize: integer read fGridSize write setGridSize;
    property GridColor: TColor read fGridBlack write setGridBlack;
    property GridColorContrast: TColor read fGridWhite write setGridWhite;
    //MultiTargetResize: Resizing of multiple targets is allowed by default
    //as long as this isn't impeded by specific Target control alignments
    property MultiTargetResize: boolean read fMultiResize write SetMultiResize;
    //The TMovePanel canvases (applicable to all descedants)
    property MovePanelCanvas: TCanvas read fCanv;
    property MovePanelImage: TPicture read fPanelImage write setPanelImage;
    property StretchMovePanelImage: boolean read fStretchPanelImage write fStretchPanelImage;
    //The TMovePanel alphachannel (applicable to all descedants)
    property MovePanelAlphaBlend: Integer read fMovePanelAlpha write setMovePanelAlphaBlend;
    //Apply sizes when moving
    property ApplySizes: boolean read fApplySizes write fApplySizes default false;
    //Show selection frame
    property ShowFrame: boolean read fShowFrame write fShowFrame default true;
    //Resize showing type
    property ResizeFrameType: TReSizeFrameType read fReSizeType write fReSizeType;
    //Ignoring method for unwanted btns
    property ResizeIgnoreMethod: TReSizeHideType read fReIgnBtn write fReIgnBtn;
    property PopupMenu: TPopupMenu read fPopupMenu write SetPopupMenu;
    //Self-explanatory Events ...
    property OnStartSizeMove: TStartEndEvent read fStartEvent write fStartEvent;
    property OnDuringSizeMove: TDuringEvent read fDuringEvent write fDuringEvent;
    property OnEndSizeMove: TStartEndEvent read fEndEvent write fEndEvent;
    property OnTargetChange: TNotifyEvent read fTargetChangeEvent
      write fTargetChangeEvent;
    property onButtonHover: TNotifyEvent read fOnBH write fOnBH;
    property onButtonUnhover: TNotifyEvent read fOnBtnUh write fOnBtnUh;
    property OnKeyDown: TKeyEvent read fOnKeyDown write fOnKeyDown;
    property OnMouseDown: TMouseDownEvent read fOnMouseDown write fOnMouseDown;
    property OnMouseEnter: TMouseDownEvent read fOnMouseEnter write fOnMouseEnter;
    property OnSetCursor: TSetCursorEvent read fOnSetCursor write fOnSetCursor;
    property OnContextPopup: TContextPopupEvent
      read fOnContextPopup write fOnContextPopup;
  end;

const
  CM_LMOUSEDOWN = WM_USER + $1;
  CM_RMOUSEDOWN = WM_USER + $2;

procedure Register;

implementation

uses Types;

type
  THackedControl = class(TControl);
  THackedWinControl = class(TWinControl);

procedure Register;
begin
  RegisterComponents('Samples', [TSizeCtrl]);
end;

{$IFDEF VER3D} type
  TAlignSet = set of TAlign; {$ENDIF}


//turn warnings off concerning unsafe typecasts since we know they're safe...
{$WARNINGS OFF}


//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function getAbsoluteX(cntrl: TControl; LastControl: TControl): integer;
begin
  Result := cntrl.Left;

  if integer(cntrl.Parent) <> integer(LastControl) then
    Result := Result + getAbsoluteX(cntrl.Parent, LastControl);
end;

function getAbsoluteY(cntrl: TControl; LastControl: TControl): integer;
begin
  Result := cntrl.top;

  if integer(cntrl.Parent) <> integer(LastControl) then
    Result := Result + getAbsoluteY(cntrl.Parent, LastControl);
end;

//------------------------------------------------------------------------------

function IsVisible(Control: TControl): boolean;
begin
  Result := True;
  while assigned(Control) do
    if Control is TCustomForm then
      exit
    else if not Control.Visible then
      break
    else
      Control := Control.Parent;
  Result := False;
end;

//------------------------------------------------------------------------------

function GetBoundsAsScreenRect(Control: TControl): TRect;
begin
  //GetBoundsAsScreenRect() assumes 'Control' is both assigned and has a parent.
  //Not all TControls have handles (ie only TWinControls) so ...
  with Control do
  begin
    Result.TopLeft := parent.ClientToScreen(BoundsRect.TopLeft);
    Result.Right := Result.Left + Width;
    Result.Bottom := Result.Top + Height;
  end;
end;

//------------------------------------------------------------------------------

function PointIsInControl(screenPt: TPoint; Control: TControl): boolean;
begin
  //PointIsInControl() assumes 'Control' is both assigned and has a parent.
  Result := PtInRect(GetBoundsAsScreenRect(Control), screenPt);
end;

//------------------------------------------------------------------------------

function KeyIsPressed(key: integer): boolean;
begin
  if key = -1 then
    Result := true
  else
   if key = 0 then
     Result := false
    Else
     Result := GetKeyState(key) < 0;
end;

//------------------------------------------------------------------------------
{
function inSizeTag(key: integer;arr: array of integer): boolean;
var i: integer;
begin
  Result := False;
  for i in arr do
    if i = key then
    begin
      Result := true;
      Exit;
    end;
end;
 //P.s Delphi compiler is kind of a shit
}
//-----------------------------------------------------------------------

function checkTag(tag: integer; _pi: TBtnPos; ts: TSizeCtrlTags;tCount:integer):boolean;
begin
If Tag = 0 then
  Result := False
Else
Result :=
    (Tag = ts.DenyChange)
    or
    (Tag = ts.DenyResize)
    or
    ((Tag = ts.DenyMultiResize) and (tCount > 1))
    or
    ((Tag = ts.DenyMultiChange) and (tCount > 1))
    or
    ((Tag = ts.ChangeTop) and (_pi <> bpTop))
    or
    ((Tag = ts.ChangeLeft) and (_pi <> bpLeft))
    or
    ((Tag = ts.ChangeHeight) and (_pi <> bpBottom))
    or
    ((Tag = ts.ChangeWidth) and (_pi <> bpRight))
    or
    ((Tag = ts.ChangeTopHeight) and
      (_pi <> bpTop) and (_pi <> bpBottom))
    or
    ((Tag = ts.ChangeTopWidth) and
      (_pi <> bpTop) and (_pi <> bpRight))
    or
    ((Tag = ts.ChangeLeftHeight) and
      (_pi <> bpLeft) and (_pi <> bpBottom))
    or
    ((Tag = ts.ChangeLeftWidth) and
      (_pi <> bpLeft) and (_pi <> bpRight))
    or
    ((Tag = ts.ChangeTopLeftWidth) and
      (_pi <> bpTop) and (_pi <> bpLeft) and (_pi<>bpRight))
    or
    ((Tag = ts.ChangeTopLeftHeight) and
      (_pi <> bpTop) and (_pi <> bpLeft) and (_pi<>bpBottom))
    or
    ((Tag = ts.ChangeTopLeftWidthHeight) and
       (_pi <> bpTop) and (_pi <> bpLeft)
       and
       (_pi <> bpBottom) and (_pi <> bpRight));

end;

//------------------------------------------------------------------------------

procedure AlignToGrid(Ctrl: TControl; ProposedBoundsRect: TRect; GridSize: integer);
begin
  with ProposedBoundsRect do
    Ctrl.SetBounds(left, top, right, bottom);
end;

//------------------------------------------------------------------------------
// TRegisteredObj functions
//------------------------------------------------------------------------------
{ TregisteredObj }

constructor TRegisteredObj.Create(aSizeCtrl: TSizeCtrl; aControl: TControl);
begin
  inherited Create;
  fSizeCtrl := aSizeCtrl;
  fControl := aControl;
    if fSizeCtrl.Enabled then
      Hook;
end;

//------------------------------------------------------------------------------

destructor TRegisteredObj.Destroy;
begin
  UnHook;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TRegisteredObj.Hook;
var
  meth: TMethod;
begin
  if fHooked then
    exit;

  if fControl is TTabSheet then
    exit;

  fOldWindowProc := fControl.WindowProc;
  if fControl is TCustomForm then
    fControl.WindowProc := NewWindowProcB
  else
    fControl.WindowProc := NewWindowProc;

  //The following is needed to block OnClick events when TSizeCtrl is enabled.
  //(If compiling with Delphi 3, you'll need to block OnClick events manually.)
  {$IFDEF VER3U}
  if IsPublishedProp(fControl, 'OnClick') then
  begin
    meth := GetMethodProp(fControl, 'OnClick');
    fOldClickMethod.Code := meth.Code;
    fOldClickMethod.Data := meth.Data;

    meth.Code := nil;
    meth.Data := nil;
    SetMethodProp(fControl, 'OnClick', meth);
  end;
  {$ENDIF}

  fHooked := True;
end;

//------------------------------------------------------------------------------

procedure TRegisteredObj.UnHook;
var
  meth: TMethod;
begin
  if not fHooked then
    exit;
  if fControl is TTabSheet then
    exit;

  fControl.WindowProc := fOldWindowProc;

  {$IFDEF VER3U}
  try
    if IsPublishedProp(fControl, 'OnClick') then
    begin
      meth.Code := fOldClickMethod.Code;
      meth.Data := fOldClickMethod.Data;
      SetMethodProp(fControl, 'OnClick', meth);
    end;
  except
  end;
  {$ENDIF}

  fHooked := False;
end;

//------------------------------------------------------------------------------

procedure TRegisteredObj.NewWindowProc(var Msg: TMessage);
begin
  fSizeCtrl.DoWindowProc(fOldWindowProc, Msg);
end;

//------------------------------------------------------------------------------

procedure TRegisteredObj.NewWindowProcB(var Msg: TMessage);
begin
  CASE Msg.Msg of
  WM_SYSCOMMAND:
  case TWMSysCommand(Msg).CmdType of
   SC_SIZE, SC_MOVE, SC_MINIMIZE, SC_MAXIMIZE, SC_CLOSE,
   SC_MOUSEMENU, SC_KEYMENU, SC_RESTORE: TWMSysCommand(Msg).CmdType := SC_DEFAULT;
  END;

  WM_NCLBUTTONDBLCLK, WM_NCLBUTTONDOWN:
  begin
    if TWMNCHitMessage(Msg).HitTest <> HTCLIENT then
      TWMNCHitMessage(Msg).HitTest := HTCLIENT;
    if Msg.Msg = WM_NCLBUTTONDOWN then
      Msg.Msg := WM_LBUTTONDOWN;
  end;
  WM_NCLBUTTONUP: Msg.Msg := WM_LBUTTONUP;
  WM_NCRBUTTONDOWN: Msg.Msg := WM_RBUTTONDOWN;
  WM_NCMOUSEMOVE: Msg.Msg := WM_MOUSEMOVE;
  END;
  fSizeCtrl.DoWindowProc(fOldWindowProc, Msg);
end;

//------------------------------------------------------------------------------
// TSizeBtn methods
//------------------------------------------------------------------------------

constructor TSizeBtn.Create(TargetObj: TTargetObj; BtnPos: TBtnPos);
begin
  inherited CreateNew(nil);
  Loaded;
  fTargetObj := TargetObj;
  AutoSize := False;
  Visible := False;
  Position := poDesigned;
  FormStyle := fsStayOnTop;
  Width := fTargetObj.fSizeCtrl.BtnSize;
  Height := fTargetObj.fSizeCtrl.BtnSize;
  Color :=
  Floor(
  (fTargetObj.fSizeCtrl.BtnColor + fTargetObj.fSizeCtrl.DisabledBtnColor
  + integer(fTargetObj.fSizeCtrl.BtnShape) + fTargetObj.fSizeCtrl.BtnFrameColor)
  /4) + 1;
  TransparentColorValue := Color;
  TransparentColor := true;
  FormStyle := fsStayOnTop;
  BorderIcons := [];
  BorderStyle := bsNone;
  fHover := false;
  fHoverDown := false;
  OnPaint := doPaint;
  OnMouseEnter := mEnter;
  OnMouseLeave := mLeave;
  fPos := BtnPos;
  UpdateBtnCursorAndColor;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.UpdateBtnCursorAndColor;
begin
  if not (fPos in fTargetObj.fSizeCtrl.fValidBtns) or
    fTargetObj.fSizeCtrl.fMoveOnly or
    checkTag(fTargetObj.fTarget.Tag, fPos, fTargetObj.fSizeCtrl.TagOptions,
    fTargetObj.fSizeCtrl.TargetCount)
     then
  begin
    Cursor := crDefault;
    fColor := fTargetObj.fSizeCtrl.DisabledBtnColor;
    fPen := fTargetObj.fSizeCtrl.DisabledBtnFrameColor;
    fImage := fTargetObj.fSizeCtrl.DisabledBtnImage;
  end
  else
  begin
    case fPos of
      bpLeft, bpRight: Cursor := crSizeWE;
      bpTop, bpBottom: Cursor := crSizeNS;
      bpTopLeft, bpBottomRight: Cursor := crSizeNWSE;
      bpTopRight, bpBottomLeft: Cursor := crSizeNESW;
    end;
    if (fHover) or (fHoverDown) then
    Begin
      fColor := fTargetObj.fSizeCtrl.HoverBtnColor;
      fPen := fTargetObj.fSizeCtrl.HoverBtnFrameColor;
      fImage := fTargetObj.fSizeCtrl.HoverBtnImage;
    END
    ELSE
    Begin
      fColor := fTargetObj.fSizeCtrl.BtnColor;
      fPen := fTargetObj.fSizeCtrl.BtnFrameColor;
      fImage := fTargetObj.fSizeCtrl.BtnImage;
    End;
  end;
  if ParentWindow <> 0 then
  Repaint;
end;

//------------------------------------------------------------------------------

function TSizeBtn.GetTop; //1/2 Top-position flickering fix
                        //in delphi XE8 developers removed
                        //duplicate value checking from VCL...
begin
  Result := inherited Top;
end;

//------------------------------------------------------------------------------

function TSizeBtn.GetLeft;
begin
  fLeft := inherited Left;
  Result := fLeft;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.SetLeft(value: integer);
begin
  if fLeft <> value then
  begin
    inherited Left := value;
    fLeft := value;
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.SetTop(value: integer);//2/2 Top-position flickering fix
begin
  if fTop <> value then
  begin
    inherited Top := value;
    fTop := value;
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    fHoverDown := True;
    fTargetObj.fSizeCtrl.DoMouseDown(self, Button, Shift);
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.dMouseUp;
begin
  fHover := False;
  fHoverDown := False;
  if Assigned( fTargetObj.fSizeCtrl.onButtonUnhover) then
    fTargetObj.fSizeCtrl.onButtonUnhover(Self as TObject);
  UpdateBtnCursorAndColor;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.DrawTriangle(l,t:integer);
begin
  case fPos of
    bpLeft: Canvas.Polygon(
      [Point(l, t+Floor(Height/2)),
      Point(l+Width-1, t),
      Point(l+Width-1, t+Height)
      ]);
    bpRight: Canvas.Polygon(
      [Point(l+Width, t+Floor(Height/2)),
      Point(l, t),
      Point(l, t+Height-1)
      ]);
    bpTop:
      Canvas.Polygon(
      [Point(l, t+Height-1),
      Point(l+Floor(Width/2), t),
      Point(l+Width, t+Height-1)
      ]);
    bpTopLeft: Canvas.Polygon(
      [Point(l, t),
       Point(l+Width-1, t+Floor(Height/2)),
       Point(l+Floor(Width/2),t+Height-1)
      ]);
    bpTopRight:
      Canvas.Polygon(
      [Point(l, t+Floor(Height/2)),
       Point(l+Floor(Width/2),t+Height-1),
       Point(l+Width, t-1)
      ]);
    bpBottom:
    Canvas.Polygon(
      [Point(l, t),
      Point(l+Floor(Width/2), t+Height-1),
      Point(l+Width, t)
      ]);
    bpBottomLeft:
    Canvas.Polygon(
      [Point(l+Width-1, t+Floor(Height/2)),
       Point(l-1, t+Height),
       Point(l+Floor(Width/2),t)
      ]);
    bpBottomRight:
    Canvas.Polygon(
      [Point(l, t+Floor(Height/2)),
       Point(l+Width, t+Height),
       Point(l+Floor(Width/2),t)
      ]);
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.PaintAs(l,t:integer);
begin
  if Assigned(fImage.Graphic) and not(fImage.Graphic.Empty) then
  begin
    if fTargetObj.fSizeCtrl.StretchBtnImage then
      Canvas.StretchDraw(Rect(l,t, width, height), fImage.Graphic)
    else
      Canvas.Draw(l,t, fImage.Graphic);
  end
  else
  case fTargetObj.fSizeCtrl.BtnShape of
     tszbSquare:
      Canvas.Rectangle(l,t, l+Width, t+Height);
     tszbTriangle:
      DrawTriangle(l,t);
     tszbCircle:
      Canvas.Ellipse(l,t, l+Width, t+Height);
     tszbMockTube:
     begin
      Canvas.Ellipse(l,t,l+Width,t+Height);
      DrawTriangle(l,t);
     end;
     tszbRoundRect:
      Canvas.RoundRect(l,t,l+Width,t+Height, (Width+Height) div 4,(Width+Height) div 4);
     tszbRhombus:
      Canvas.Polygon(
      [Point(l,t+Ceil(Height/2)-1),
      Point(l+Ceil(Width/2)-1, t),
      Point(l+Width-1,t+Ceil(Height/2)-1),
      Point(l+Ceil(Width/2)-1, t+Height-1)]);
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.Reset;
begin
  SetBounds(Left, Top, fTargetObj.fSizeCtrl.BtnSize, fTargetObj.fSizeCtrl.BtnSize);
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.DoPaint(Sender:TObject);
begin
  AlphaBlend := fTargetObj.fSizeCtrl.BtnAlphaBlend <> 255;
  AlphaBlendValue := fTargetObj.fSizeCtrl.BtnAlphaBlend;
  Canvas.Brush.Color := fColor;
  if fPen = clNone then
    Canvas.Pen.Color := fColor
  else
    Canvas.Pen.Color := fPen;
  PaintAs(0,0);
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.mEnter(Sender:TObject);
begin
  fHover := True;
  UpdateBtnCursorAndColor;
end;

//------------------------------------------------------------------------------

procedure TSizeBtn.mLeave(Sender:TObject);
begin
  fHover := False;
  if not fHoverDown then
    if Assigned( fTargetObj.fSizeCtrl.onButtonUnhover) then
      fTargetObj.fSizeCtrl.onButtonUnhover(Sender);

  UpdateBtnCursorAndColor;
end;

//------------------------------------------------------------------------------
//  TTargetObj methods
//------------------------------------------------------------------------------

constructor TTargetObj.Create(aSizeCtrl: TSizeCtrl; aTarget: TControl);
var
  i: TBtnPos;
begin
  inherited Create;
  fSizeCtrl := aSizeCtrl;
  fTarget := aTarget;
  fPanels := TList.Create;
  fPanelsNames := TStringList.Create;
  for i := bpLeft to High(TBtnPos) do
    fBtns[i] := TSizeBtn.Create(self, i);
end;

//------------------------------------------------------------------------------

destructor TTargetObj.Destroy;
var
  i: TBtnPos;
  k: integer;
begin
  fPanelsNames.Clear;
  for k := 0 to fPanels.Count - 1 do
    TObject(fPanels[k]).Free();
  fPanels.Clear;

  fPanels.Free;
  fPanelsNames.Free;

  for i := bpLeft to high(TBtnPos) do
  begin
    if fBtns[i] <> nil then
      fBtns[i].Free;
  end;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TTargetObj.ReconfButtons;
var i: TBtnPos;
begin
  for i := bpLeft to high(TBtnPos) do
  begin
    if i > fSizeCtrl.LastBtn then
      fBtns[i].Hide
    else
      fBtns[i].Show;
  end;
end;

//------------------------------------------------------------------------------

procedure TTargetObj.Update;
var
  i: TBtnPos;
  st: boolean;
  parentForm: TCustomForm;
  tl: TPoint;
  bsDiv2: integer;
begin
    parentForm := fSizeCtrl.fParentForm;
  if not assigned(parentForm) then
    exit;
  //get topleft of Target relative to parentForm ...
  tl := GetBoundsAsScreenRect(fTarget).TopLeft;
  //topleft := fTarget.BoundsRect.TopLeft;
  tl := parentForm.ScreenToClient(tl);
  bsDiv2 := (fSizeCtrl.BtnSize div 2);

  for i := bpLeft to fSizeCtrl.LastBtn do
  begin
  if fBtns[i].ParentWindow <> parentForm.Handle then
  begin
    fBtns[i].ParentWindow := parentForm.Handle; //ie keep btns separate !!!
    fBtns[i].SetZOrder(true); //force btns to the top ...
    //just to be sure, that our button will be displayed correctly
    fBtns[i].Position := poDesigned;
    st := true;
  end;
    fBtns[i].Left := tl.X - bsDiv2;
    case i of
      bpTop, bpBottom:
        fBtns[i].Left := fBtns[i].Left + (fTarget.Width div 2);
      bpRight, bpTopRight, bpBottomRight:
        fBtns[i].Left := fBtns[i].Left + fTarget.Width - 1;
    end;
    fBtns[i].Top := tl.Y - bsDiv2;
    case i of
      bpLeft, bpRight:
        fBtns[i].Top := fBtns[i].Top + (fTarget.Height div 2);
      bpBottomLeft, bpBottom, bpBottomRight:
        fBtns[i].Top := fBtns[i].Top + fTarget.Height - 1;
    end;
    //force btns to the top ...
    if st then begin
      SetWindowPos(fBtns[i].Handle, HWND_TOP, fBtns[i].Left,
      fBtns[i].Top, fBtns[i].Left + fBtns[i].Width, fBtns[i].Left + fBtns[i].Top,
      SWP_NOACTIVATE or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
   end;
    fBtns[i].Visible := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TTargetObj.UpdateExtra(const Rectange: TRect; const typer: integer);
var
  i: TBtnPos;
  lLeft, lTop: integer;
  tl: TPoint;
  check: boolean;
  ilr, ilrd: TBtnPosSet;
  bsdiv2: integer;
begin
  //TL := TopLeft
  tl := Rectange.TopLeft;
  if  fSizeCtrl.BtnCount = szctrl4btns then
    ilr := [bpLeft, bpTop, bpRight, bpBottom]
  else
    ilr := [bpLeft, bpTop, bpRight, bpBottom, bpTopLeft, bpTopRight,
    bpBottomRight, bpBottomLeft];
  check := true;
  bsDiv2 := (fSizeCtrl.BtnSize div 2);
  case typer of
    0: check := false;
    1:
    case fSizeCtrl.fCapturedBtnPos  of
    bpNone: check := false;
    bpLeft: ilrd := [bpTopLeft, bpLeft, bpBottomLeft];
    bpTop: ilrd := [bpTopLeft, bpTop, bpTopRight];
    bpRight: ilrd := [bpTopRight, bpRight, bpBottomRight];
    bpBottom: ilrd := [bpBottomLeft, bpBottom, bpBottomRight];
    bpTopLeft: ilrd := [bpTopRight, bpTop, bpTopLeft, bpLeft, bpBottomLeft];
    bpTopRight: ilrd := [bpTopLeft, bpTop, bpTopRight, bpRight, bpBottomRight];
    bpBottomRight: ilrd := [bpBottomRight, bpBottom, bpBottomLeft, bpRight, bpTopRight];
    bpBottomLeft: ilrd := [bpTopLeft, bpLeft, bpBottomLeft, bpBottom, bpBottomRight];
    end;
    2:
     if fSizeCtrl.fCapturedBtnPos <> bpNone
      then ilrd := [fSizeCtrl.fCapturedBtnPos]
     else if fSizeCtrl.ResizeIgnoreMethod = tszhMove then ilrd := ilr;
  end;

  for i in ilr do
  begin
    if check and (not (i in ilrd)) then
    begin
      case fSizeCtrl.ResizeIgnoreMethod of
          tszhHide: fBtns[i].Hide
          else continue;
      end;
    end
    else if fBtns[i].Visible = False then
      fBtns[i].Show;
    lLeft := tl.X - bsDiv2;
    case i of
      bpTop, bpBottom:
        fBtns[i].Left := lLeft + (Rectange.Width div 2);
      bpRight, bpTopRight, bpBottomRight:
        fBtns[i].Left := lLeft + Rectange.Width - 1;
      else fBtns[i].Left := lLeft;
    end;
    lTop := tl.Y - bsDiv2;
    case i of
      bpLeft, bpRight:
        fBtns[i].Top := lTop + (Rectange.Height div 2);
      bpBottomLeft, bpBottom, bpBottomRight:
        fBtns[i].Top := lTop + Rectange.Height - 1;
      else fBtns[i].Top := lTop;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TTargetObj.StartFocus();
begin
  fFocusRect := fTarget.BoundsRect;
  fStartRec := fFocusRect;
end;

//------------------------------------------------------------------------------

function TTargetObj.MoveFocus(dx, dy: integer): boolean;
var
  L, T: integer;
begin
  if fTarget.Tag = fSizeCtrl.TagOptions.DenyChange then
    exit;

  L := fFocusRect.Left;
  T := fFocusRect.Top;
  fFocusRect := fStartRec;
  offsetRect(fFocusRect, dx, dy);
  Result := (L <> fFocusRect.Left) or (T <> fFocusRect.Top);
end;

//------------------------------------------------------------------------------

function TTargetObj.SizeFocus(dx, dy: integer; BtnPos: TBtnPos): boolean;
var
  L, T, R, B: integer;
begin
  if fTarget.Tag = fSizeCtrl.TagOptions.DenyChange then
    exit;

  L := fFocusRect.Left;
  T := fFocusRect.Top;
  R := fFocusRect.Right;
  B := fFocusRect.Bottom;

  fFocusRect := fStartRec;
  case BtnPos of
    bpLeft: Inc(fFocusRect.Left, dx);
    bpTopLeft:
    begin
      Inc(fFocusRect.Left, dx);
      Inc(fFocusRect.Top, dy);
    end;
    bpTop: Inc(fFocusRect.Top, dy);
    bpTopRight:
    begin
      Inc(fFocusRect.Right, dx);
      Inc(fFocusRect.Top, dy);
    end;
    bpRight: Inc(fFocusRect.Right, dx);
    bpBottomRight:
    begin
      Inc(fFocusRect.Right, dx);
      Inc(fFocusRect.Bottom, dy);
    end;
    bpBottom: Inc(fFocusRect.Bottom, dy);
    bpBottomLeft:
    begin
      Inc(fFocusRect.Left, dx);
      Inc(fFocusRect.Bottom, dy);
    end;
  end;
  Result := (L <> fFocusRect.Left) or (R <> fFocusRect.Right) or
    (T <> fFocusRect.Top) or (B <> fFocusRect.Bottom);
end;

//------------------------------------------------------------------------------

procedure TTargetObj.EndFocus;
var
  w, h: integer;
  i: TBtnPos;
begin
  for i := bpLeft to fSizeCtrl.fLastBtn do
    fBtns[i].dMouseUp;
  if not fSizeCtrl.ApplySizes then
  begin
    //update target position ...

    w := fFocusRect.Right - fFocusRect.Left;
    h := fFocusRect.Bottom - fFocusRect.Top;
    fFocusRect.Left := fTarget.Left - (fStartRec.Left - fFocusRect.Left);
    fFocusRect.Top := fTarget.Top - (fStartRec.Top - fFocusRect.Top);
    fFocusRect.Right := fFocusRect.Left + w;
    fFocusRect.Bottom := fFocusRect.Top + h;
  end;
  with fFocusRect do
    AlignToGrid(fTarget, Rect(Left, top, fSizeCtrl.FixSize(right - left,0),
      fSizeCtrl.FixSize(bottom - top,1)), fSizeCtrl.fGridSize);
  Update;
  fTarget.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TTargetObj.DrawRect(dc: hDC; obj: TControl);
var
  pr: TWinControl;
  panel: TMovePanel;
  s: string;
  k: integer;
  r: TRect;
begin
  if fTarget.Tag = fSizeCtrl.TagOptions.DenyChange then
    exit;

  fLastRect := fFocusRect;

  pr := obj.Parent;
  s := IntToStr(integer(obj));
  k := Self.fPanelsNames.IndexOf(s);
  if k > -1 then
    panel := TMovePanel(fPanels[k])
  else
  begin
    panel := TMovePanel.Create(pr, fSizeCtrl);
    fPanelsNames.Add(IntToStr(integer(obj)));
    fPanels.Add(panel);
  end;

  panel.Width := fFocusRect.Right - fFocusRect.Left;
  panel.Height := fFocusRect.Bottom - fFocusRect.Top;

  panel.Left := fFocusRect.Left;
  panel.Top := fFocusRect.Top;
  r := TRect.Create( fFocusRect.TopLeft );
  r.BottomRight := fFocusRect.BottomRight;
  if fSizeCtrl.ApplySizes then
  begin
    obj.Width := fSizeCtrl.FixSize(fFocusRect.Right - fFocusRect.Left,0);
    obj.Height := fSizeCtrl.FixSize(fFocusRect.Bottom - fFocusRect.Top,0);
    obj.Left := fFocusRect.Left;
    obj.Top := fFocusRect.Top;
     if fSizeCtrl.Parent.DoubleBuffered then
    begin
         fSizeCtrl.Parent.Update;
         obj.Repaint;
         panel.Repaint;
    end;
  end;
  //fix for poly-parenthed controls
  while obj.Parent <> fSizeCtrl.fParentForm  do
  begin
    obj := obj.Parent;
    r.TopLeft := obj.ClientToParent(r.TopLeft);
    r.BottomRight := obj.ClientToParent(r.BottomRight)
  end;
  case fSizeCtrl.ResizeFrameType of
    tszfNone:
      self.UpdateExtra(r, 3);
    tszfButtons:
      self.UpdateExtra(r,0);
    tzfRButtons:
      self.UpdateExtra(r,1);
    tszfRButton:
      self.UpdateExtra(r,2);
  end;
  if (panel.Visible = False) and (fSizeCtrl.ShowFrame) then
  begin
        panel.BringToFront;
        panel.Show;
  end;
end;

//------------------------------------------------------------------------------
//  TSizeCtrl methods
//------------------------------------------------------------------------------

constructor TSizeCtrl.Create(AOwner: TComponent);
begin
  if not (aOwner is TWinControl) then
    raise Exception.Create('TSizeCtrl.Create: Owner must be a TWinControl');
  inherited Create(AOwner);
  fTargetList := TList.Create;
  fReIgnBtn := tszhNone;
  fRegList := TList.Create;
  fCanv := TCanvas.Create;
  fCanv.Brush.Color := clBtnFace;
  fCanv.Pen.Color := clBlack;
  fBtnCount := TSizeCtrlBtnCount.szctrl8btns;
  fLastBtn := bpBottomLeft;
  fTags.DenyMove := 2011;
  fTags.DenyChange := fTags.DenyChange + 1;
  fTags.DenyResize := fTags.DenyChange + 2;
  fTags.DenyMultiResize := fTags.DenyChange + 3;
  fGridSize := 8;
  fBtnAlpha := 255;
  fMovePanelAlpha := 255;
  fBtnSize := 5;
  FSelKey := VK_SHIFT;
  //Multi-escape key
  fSelActionCancelKey := VK_ESCAPE;

  //Tabulation key
  fSelToggleKey := VK_TAB;

  //Awwwrows keys
  fMoveLeftKey  := VK_LEFT;
  fMoveTopKey   := VK_LEFT + 1;
  fMoveRightKey := VK_LEFT + 2;
  fMoveBottomKey:= VK_LEFT + 3;

  FDGKey := VK_MENU;
  fCanv.Pen.Style := psDot;
  fCanv.Pen.Mode := pmCopy;
  fCanv.Pen.Width := 1;
  fCanv.Pen.Color := clBlack;
  fCanv.Brush.Style := bsSolid;
  fCanv.Brush.Color := clBtnFace;
  fGridWhite := clWhite;
  fGridBlack := clGray;
  fBtnShape := tszbCircle;
  fEnabledBtnColor := clAqua;
  fHoverBtnColor := clAqua;
  fDisabledBtnColor := clGray;
  fApplySizes:= False;
  fShowFrame := True;
  fReSizeType:= TReSizeFrameType.tszfNone;
  fBtnFrameColor := clBlue;
  fHoverBtnFrameColor := clBlue;
  fDisabledBtnFrameColor := clNone;
  fPanelImage := TPicture.Create;
  fBtnImage := TPicture.Create;
  fHoverBtnImage := TPicture.Create;
  fDisabledBtnImage := TPicture.Create;
  fStretchBtnImage := True;
  fStretchPanelImage := True;
  fMultiResize := True;
  FConstraints := TSizeConstraints.Create(TControl(Self));
  fValidBtns := [bpLeft, bpTopLeft, bpTop, bpTopRight, bpRight,
    bpBottomRight, bpBottom, bpBottomLeft];
  fHandle := AllocateHWnd(WinProc);
  fForm := TWinControl(AOwner);
  if fForm is TForm then
    TForm(fForm).OnPaint := Self.formPaint
  else
    begin
      CreateGrid;
    end;
{$IFDEF VER3D}
  screen.Cursors[crSize] := loadcursor(hInstance, 'NSEW');
{$ENDIF}
end;

//------------------------------------------------------------------------------

destructor TSizeCtrl.Destroy;
begin
  if assigned(fTargetList) then
  begin
    DeallocateHWnd(fHandle);
    UnRegisterAll;
    fTargetList.Free;
    fRegList.Free;
    FConstraints.Free;
    fCanv.Free;
    fPanelImage.Free;
    fBtnImage.Free;
    fHoverBtnImage.Free;
    fDisabledBtnImage.Free;
    fGrid.Free;
  end;

  if Assigned(fGridForm) then
    fGridForm := nil;

  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.CreateGrid;
begin
      fGridForm := TForm.CreateNew(Self);
      fGridForm.Parent := fForm;
      fGridForm.Position := poDesigned;
      fGridForm.BorderStyle := bsNone;
      fGridForm.Color := ((fGridWhite + fGridBlack) div 5) + 17 + 28;
      fGridForm.TransparentColorValue := fGridForm.Color;
      fGridForm.TransparentColor := True;
      fGridForm.Anchors := [akLeft, akTop, akRight, akBottom];
      fGridForm.FormStyle := fsNormal;
      fGridForm.OnPaint := Self.formPaint;
      fGridForm.SendToBack;
      fGridForm.Visible := True;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.FixSize(input, wh: integer): integer;
begin
      if wh = 0 then
      begin
        Result := max(Constraints.MinWidth, input);
        if Constraints.MaxWidth > 0 then
          Result := min(Result, Constraints.MaxWidth);
      end
      else
      begin
        Result := max(Constraints.MinHeight, input);
        if Constraints.MaxHeight > 0 then
          Result := min(Result, Constraints.MaxHeight);
      end;
end;


//------------------------------------------------------------------------------

procedure TSizeCtrl.SetEnabled(Value: boolean);
var
  i: integer;
begin
  if Value = fEnabled then
    exit;

  fParentForm := GetParentForm(TWinControl(owner));
  if fParentForm = nil then
    exit;

  fEnabled := Value;
  ClearTargets;

  if fEnabled then
  begin
    //hook all registered controls and disable their OnClick events ...
    for i := 0 to fRegList.Count - 1 do
      TRegisteredObj(fRegList[i]).Hook;
    //hook the parent form too ...
    fOldWindowProc := fParentForm.WindowProc;
    fParentForm.WindowProc := FormWindowProc;
  end
  else
  begin
    //unhook all registered controls and reenable their OnClick events ...
    for i := 0 to fRegList.Count - 1 do
      TRegisteredObj(fRegList[i]).UnHook;
    //unhook the parent form too ...
    fParentForm.WindowProc := fOldWindowProc;
  end;
end;

procedure TSizeCtrl.FormWindowProc(var Msg: TMessage);
begin
  if (Msg.Msg <> WM_PARENTNOTIFY) or
      ((Msg.Msg = WM_PARENTNOTIFY) and (fEditDisabled)) then
  DoWindowProc(fOldWindowProc, Msg);
end;

//------------------------------------------------------------------------------

//TSizeCtrl's own message handler to process CM_CUSTOM_MSE_DOWN message
procedure TSizeCtrl.WinProc(var Msg: TMessage);
var
  Button: TMouseButton;
  ShiftState: TShiftState;
begin
  with Msg do
    if Msg = CM_LMOUSEDOWN then
      try
        fLMouseDownPending := False;
        if bool(WParam) then
          Button := mbLeft
        else
          Button := mbRight;
        if bool(LParam) then
          ShiftState := [ssShift]
        else
          ShiftState := [];
        DoMouseDown(nil, Button, ShiftState);
      except
        Application.HandleException(Self);
      end
    else
      Result := DefWindowProc(fHandle, Msg, wParam, lParam);
end;

//------------------------------------------------------------------------------

function TSizeCtrl.KeysToShiftState(Keys: Word): TShiftState;
begin
  Result := [];
  if KeyIsPressed( Self.FSelKey ) then Include(Result, ssShift);
  if Keys and MK_CONTROL <> 0 then Include(Result, ssCtrl);
  if Keys and MK_LBUTTON <> 0 then Include(Result, ssLeft);
  if Keys and MK_RBUTTON <> 0 then Include(Result, ssRight);
  if Keys and MK_MBUTTON <> 0 then Include(Result, ssMiddle);
  if KeyIsPressed( Self.FDGKey ) then Include(Result, ssAlt);
end;

//------------------------------------------------------------------------------

function TSizeCtrl.KeyDataToShiftState(KeyData: Longint): TShiftState;
begin
  Result := [];
  if KeyIsPressed( Self.FSelKey ) then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if KeyIsPressed( Self.FDGKey ) then Include(Result, ssAlt);
end;

//------------------------------------------------------------------------------

//WindowProc for the 'hooked' form and all 'hooked' controls
procedure TSizeCtrl.DoWindowProc(DefaultProc: TWndMethod; var Msg: TMessage);
var
  i, k: integer;
  list: TList;
  ShiftState: TShiftState;
  controlPt, screenPt: TPoint;
  regCtrl: TControl;
  handled: boolean;

  //this seems the only reasonably simple way of managing both 'owned' and
  //'notified' WM_LBUTTONDOWN messages ...
  procedure PostMouseDownMessage(isLeftBtn, shiftKeyPressed: boolean;
  isVirtual:boolean=false);
  begin
    if fLMouseDownPending then
      exit;

    if assigned(fOnMouseDown) then
    begin
      getCursorPos(screenPt);
      regCtrl := RegisteredCtrlFromPt(screenPt, nil);
      if assigned(regCtrl) then
      begin
        handled := False;
        controlPt := regCtrl.ScreenToClient(screenPt);
        fOnMouseDown(self, regCtrl, controlPt, handled);
        if handled then
          exit;
      end else if isVirtual then exit;

    end;

    fLMouseDownPending := True;
    PostMessage(fHandle, CM_LMOUSEDOWN, Ord(isLeftBtn), Ord(shiftKeyPressed));
  end;

begin
  case Msg.Msg of

    WM_MOUSEFIRST .. WM_MOUSELAST:
    begin
      ShiftState := Self.KeysToShiftState(word(TWMMouse(Msg).Keys));
      case Msg.Msg of
        WM_LBUTTONDOWN: PostMouseDownMessage(True, ssShift in ShiftState);
        WM_RBUTTONDOWN: DoPopupMenuStuff;
        WM_MOUSEMOVE: DoMouseMove(nil, ShiftState);
        WM_LBUTTONUP: DoMouseUp(nil, mbLeft, ShiftState);
        //Could also add event handlers for right click events here.
      end;
      Msg.Result := 0;
    end;

    WM_PARENTNOTIFY:
      if not (TWMParentNotify(Msg).Event in [WM_CREATE, WM_DESTROY]) then
      begin
        if KeyIsPressed(Self.FSelKey) then
          ShiftState := [ssShift]
        else
          ShiftState := [];
        case TWMParentNotify(Msg).Event of
          WM_LBUTTONDOWN: PostMouseDownMessage(True, ssShift in ShiftState, true);
        end;
        Msg.Result := 0;
      end;

    WM_SETCURSOR:
    Begin
      if (HIWORD(Msg.lParam) <> 0) then
      begin
        Msg.Result := 1;
        getCursorPos(screenPt);
        regCtrl := RegisteredCtrlFromPt(screenPt);

        handled := False;
        if assigned(fOnSetCursor) and assigned(regCtrl) then
        begin
          controlPt := regCtrl.ScreenToClient(screenPt);
          fOnSetCursor(self, RegisteredCtrlFromPt(screenPt), controlPt, handled);
        end;

        if handled then //do nothing
        else if TargetIndex(regCtrl) >= 0 then
        begin

          if not IsValidMove then
            DefaultProc(Msg)
          else
            Windows.SetCursor(screen.Cursors[crSize]);

        end
        else if assigned(regCtrl) then
          Windows.SetCursor(screen.Cursors[crHandPoint])
        else
          DefaultProc(Msg);
      end
      else
        DefaultProc(Msg);
    End;

    WM_GETDLGCODE: Msg.Result :=  DLGC_WANTTAB or DLGC_WANTARROWS;
    WM_KEYDOWN:
    begin
      Msg.Result := 0;
      if DoKeyDown(TWMKey(Msg)) then
        exit;
        if Msg.WParam = fMoveTopKey then
          if KeyIsPressed(Self.FSelKey) then
          begin
            SizeTargets(0, -GridSize);
            if assigned(fEndEvent) then
              fEndEvent(self, scsSizing);
          end
          else
          begin
            MoveTargets(0, -GridSize);
            if assigned(fEndEvent) then
              fEndEvent(self, scsMoving);
          end;
        if Msg.WParam = fMoveBottomKey then
          if KeyIsPressed(Self.FSelKey) then
          begin
            SizeTargets(0, +GridSize);
            if assigned(fEndEvent) then
              fEndEvent(self, scsSizing);
          end
          else
          begin
            MoveTargets(0, +Gridsize);
            if assigned(fEndEvent) then
              fEndEvent(self, scsMoving);
          end;
        if Msg.WParam = fMoveLeftKey then
          if KeyIsPressed(Self.FSelKey) then
          begin
            SizeTargets(-GridSize, 0);
            if assigned(fEndEvent) then
              fEndEvent(self, scsSizing);
          end
          else
          begin
            MoveTargets(-GridSize, 0);
            if assigned(fEndEvent) then
              fEndEvent(self, scsMoving);
          end;
        if Msg.WParam = fMoveRightKey then
          if KeyIsPressed(Self.FSelKey) then
          begin
            SizeTargets(+GridSize, 0);
            if assigned(fEndEvent) then
              fEndEvent(self, scsSizing);
          end
          else
          begin
            MoveTargets(+GridSize, 0);
            if assigned(fEndEvent) then
              fEndEvent(self, scsMoving);
          end;
        if Msg.WParam = fSelToggleKey then
        begin
          if fRegList.Count = 0 then
            exit
          else if targetCount = 0 then
            AddTarget(TRegisteredObj(fRegList[0]).fControl)
          else
          begin
            i := RegisteredIndex(Targets[0]);
            if KeyIsPressed(Self.FSelKey) then
              Dec(i)
            else
              Inc(i);
            if i < 0 then
              i := fRegList.Count - 1
            else if i = fRegList.Count then
              i := 0;
            ClearTargets;
            AddTarget(TRegisteredObj(fRegList[i]).fControl);
          end;
        end;
        if Msg.WParam = fSelActionCancelKey then
          //ESCAPE is used for both -
          //  1. cancelling a mouse move/resize operation, and
          //  2. selecting the parent of the currenctly selected target
          if fState <> scsReady then
          begin
            fEscCancelled := True;
            DoMouseUp(nil, mbLeft, []);
          end
          else
          begin
            if (targetCount = 0) then
              exit;
            i := RegisteredIndex(Targets[0].Parent);
            ClearTargets;
            if i >= 0 then
              AddTarget(TRegisteredObj(fRegList[i]).fControl);
          end;
    end;

    WM_KEYUP: Msg.Result := 0;
    WM_CHAR: Msg.Result := 0;
    WM_CANCELMODE: //Cancel all modal operations
    //What is that? Do you ask me? You'd better ask Embarcadero,
      //Because they do not supplied truely focus-lose event
      //P.s thanks to "furious programming" from Lazarus forum,
      //Because now i'm know, that in FP i do not have to
      //do this...
      if ( GetForegroundWindow <> fParentForm.Handle)
          and (fState <> scsReady) then
      begin
        fState := scsReady;
        if TargetCount > 0 then
        for i := 0 to TargetCount - 1 do
          begin
            TTargetObj(fTargetList[i]).EndFocus;
            TTargetObj(fTargetList[i]).fPanelsNames.Clear;
            list := TTargetObj(fTargetList[i]).fPanels;
            for k := 0 to list.Count - 1 do
              TObject(list[k]).Free();
            list.Clear;
          end;
      end;
    else
      DefaultProc(Msg);
  end;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.DoKeyDown(var Message: TWMKey): boolean;
var
  ShiftState: TShiftState;
begin
  Result := True;
  if fParentForm.KeyPreview and THackedWinControl(fParentForm).DoKeyDown(Message) then
    Exit;
  if Assigned(fOnKeyDown) then
    with Message do
    begin
      ShiftState := Self.KeyDataToShiftState(KeyData);
      fOnKeyDown(Self, CharCode, ShiftState);
      if CharCode = 0 then
        Exit;
    end;
  Result := False;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.GetTargets(index: integer): TControl;
begin
  if (index < 0) or (index >= TargetCount) then
    Result := nil
  else
    Result := TTargetObj(fTargetList[index]).fTarget;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.TargetIndex(Control: TControl): integer;
var
  i: integer;
begin

  Result := -1;
  if assigned(Control) then
    for i := 0 to fTargetList.Count - 1 do
      if TTargetObj(fTargetList[i]).fTarget = Control then
      begin
        Result := i;
        break;
      end;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.AddTarget(Control: TControl): integer;
var
  TargetObj: TTargetObj;
  r: TRect;
begin

  Result := -1;
  if (csDestroying in ComponentState) or (fState <> scsReady) then
    exit;

  Result := TargetIndex(Control);
  if not assigned(Control) or not Control.Visible or (integer(Control) = integer(fParentForm))
   or
    (Result >= 0) then
    exit;
  Result := fTargetList.Count;
  TargetObj := TTargetObj.Create(self, Control);
  fTargetList.Add(TargetObj);
  RegisterControl(Control);
  fParentForm.ActiveControl := nil;
  UpdateBtnCursors;
  TargetObj.Update;
  TargetObj.Update; //b.f: TCustomForm cannot be placed right after show
                    //Delphi VCL bug?
  if assigned(fTargetChangeEvent) then
    fTargetChangeEvent(self);

  {for i := 0 to fTargetList.Count -1 do
      MessageBox(0,pchar(TTargetObj(fTargetList[i]).fTarget.Name),'',mb_ok);  }
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.DeleteTarget(Control: TControl);
var
  i: integer;
begin
  i := TargetIndex(Control);
  if i < 0 then
    exit;
  TTargetObj(fTargetList[i]).Free;
  fTargetList.Delete(i);
  UpdateBtnCursors;
  if assigned(fTargetChangeEvent) then
    fTargetChangeEvent(self);
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.ClearTargets;
var
  i: integer;
begin
  if fTargetList.Count = 0 then
    exit;
  for i := 0 to fTargetList.Count - 1 do
  begin
    if fTargetList[i] <> nil then
      TTargetObj(fTargetList[i]).Free;
  end;

  fTargetList.Clear;
  if (csDestroying in ComponentState) then
    exit;
  UpdateBtnCursors;
  if assigned(fTargetChangeEvent) then
    fTargetChangeEvent(self);
end;

//------------------------------------------------------------------------------

function TSizeCtrl.RegisterControl(Control: TControl): integer;
var
  RegisteredObj: TRegisteredObj;
begin
  if Control is TMovePanel or
  (Assigned(fGridForm) and (integer(Control) = integer(fGridForm))) then //b.f with 1000 objects selected
    Exit;

  if RegisteredIndex(Control) >= 0 then
    exit;

  Result := fRegList.Count;
  RegisteredObj := TRegisteredObj.Create(self, Control);
  fRegList.Add(RegisteredObj);
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.UnRegisterControl(Control: TControl);
var
  i: integer;
begin
  //first, make sure it's not a current target ...
  DeleteTarget(Control);
  //now unregister it ...
  i := RegisteredIndex(Control);
  if i < 0 then
    exit;
  TRegisteredObj(fRegList[i]).Free;
  fRegList.Delete(i);
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.UnRegisterAll;
var
  i: integer;
begin
  //first, clear any targets
  ClearTargets;
  //now, clear all registered controls ...
  for i := 0 to fRegList.Count - 1 do
  begin
    if Assigned(fRegList[i]) then
      TRegisteredObj(fRegList[i]).Free;
  end;
  fRegList.Clear;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.RegisteredIndex(Control: TControl): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to fRegList.Count - 1 do
    if TRegisteredObj(fRegList[i]).fControl = Control then
    begin
      Result := i;
      break;
    end;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.TargetCtrlFromPt(screenPt: TPoint): TControl;
var
  i: integer;
  tmpCtrl: TWinControl;
begin
  //nb: If controls overlap at screenPt, then the (top-most) child control
  //is selected if there is a parent-child relationship. Otherwise, simply
  //the first control under screenPt is returned.
  Result := nil;
  for i := fTargetList.Count - 1 downto 0 do
    with TTargetObj(fTargetList[i]) do
    begin
      if not PointIsInControl(screenPt, fTarget) then
        continue;
      if not (fTarget is TWinControl) then
      begin
        Result := fTarget;
        exit; //ie assume this is top-most since it can't be a parent.
      end
      else if not assigned(Result) then
        Result := fTarget
      else
      begin
        tmpCtrl := TWinControl(fTarget).Parent;
        while assigned(tmpCtrl) and (tmpCtrl <> Result) do
          tmpCtrl := tmpCtrl.Parent;
        if assigned(tmpCtrl) then
          Result := fTarget;
      end;
    end;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.getRegObj(C: TComponent): TRegisteredObj;
var
  i: integer;
begin
  for i := fRegList.Count - 1 downto 0 do
  begin
    Result := TRegisteredObj(fRegList[i]);
    if (integer(Result.fControl) = integer(C)) then
      exit;
  end;
  Result := nil;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.RegisteredCtrlFromPt(screenPt: TPoint;
  ParentX: TWinControl = nil): TControl;
var
  i: integer;
  rO: TRegisteredObj;
  tmp: TControl;
begin
  //nb: If controls overlap at screenPt, then the (top-most) child control
  //is selected if there is a parent-child relationship. Otherwise, simply
  //the first control under screenPt is returned.

  if (ParentX = nil) then
    ParentX := fForm;

  // for i := fRegList.Count -1 downto 0 do
  for i := ParentX.ControlCount - 1 downto 0 do
  begin

    rO := self.getRegObj(ParentX.Controls[i]);

    if rO = nil then
      continue;

    if rO.fControl.Parent is TTabSheet then
    begin
      if not rO.fControl.Parent.Visible then
        continue;
    end;

    with rO do
    begin

      if not PointIsInControl(screenPt, fControl) then
        continue;

      Result := fControl;

      if (Result is TPageControl) then
      begin

        tmp := RegisteredCtrlFromPt(screenPt, TPageControl(Result).ActivePage);

        if (tmp <> nil) then
        begin

          Result := tmp;
        end;
      end;

      if (Result is TWinControl) then
      begin
        tmp := RegisteredCtrlFromPt(screenPt, (Result as TWinControl));
        if (tmp <> nil) then
          Result := tmp;
      end;

      exit;
    end;
  end;

  Result := nil;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.GetTargetCount: integer;
begin
  Result := fTargetList.Count;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.MoveTargets(dx, dy: integer);
var
  i: integer;       //ihere
begin
  if not IsValidMove then exit;
  for i := 0 to fTargetList.Count -1 do
    with TTargetObj(fTargetList[i]) do
    begin
      with fTarget do SetBounds(Left + dx, Top + dy, Width, Height);
      Update;
    end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SizeTargets(dx, dy: integer);
var
  i, q, r: integer;
begin
  if MoveOnly then
    exit;
  if (dx <> 0) and not (IsValidSizeBtn(bpLeft) or IsValidSizeBtn(bpRight)) then
    exit;
  if (dy <> 0) and not (IsValidSizeBtn(bpBottom) or IsValidSizeBtn(bpTop)) then
    exit;

  for i := 0 to fTargetList.Count - 1 do
    with TTargetObj(fTargetList[i]) do
    begin
      with fTarget do
        SetBounds(Left, Top,
          FixSize(Width + dx,0), FixSize(Height + dy,1));
      Update;
    end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.Update;
var
  i: integer;
begin
  for i := 0 to fTargetList.Count - 1 do
    TTargetObj(fTargetList[i]).Update;
end;

procedure TSizeCtrl.UpdateBtns;
var
  i: integer;
  x: TBtnPos;
begin
  for i := 0 to fTargetList.Count - 1 do
    for x := bpLeft to LastBtn do
      TTargetObj(fTargetList[i]).fBtns[x].UpdateBtnCursorAndColor;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.DrawRect;
var
  i: integer;
  dc: hDC;
  //  MaxX,MaxY,MinX,MinY: Integer;
begin
  if TargetCount = 0 then
    exit;
  dc := GetDC(0);
  try
    for i := 0 to TargetCount - 1 do
      // DrawFocusRect(dc,Rect(MinX,MinY,MaxX,MaxY));
      TTargetObj(fTargetList[i]).DrawRect(dc, TTargetObj(fTargetList[i]).fTarget);
  finally
    ReleaseDC(0, dc);
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState);
var
  i, targetIdx: integer;
  p: TWinControl;
  parentClientRec: TRect;
  targetObj: TTargetObj;
begin
  fEscCancelled := False;
  GetCursorPos(fStartPt);



  if (Sender is TSizeBtn) then
  begin
    if fMoveOnly then
      exit; //should never happen
    targetObj := TSizeBtn(Sender).fTargetObj;
    fCapturedCtrl := targetObj.fTarget;
    fCapturedBtnPos := TSizeBtn(Sender).fPos;
    //make sure we're allowed to size these targets with this button ...
    if not IsValidSizeBtn(fCapturedBtnPos) then
      exit;
    fState := scsSizing;
  end
  else
  begin
    fCapturedBtnPos := bpNone;
    //First find the top-most control that's clicked ...
    //nb: It's so much simpler to do this here than try and work it out from
    //the WindowProc owner (because of disabled controls & non-TWinControls.)

    fCapturedCtrl := RegisteredCtrlFromPt(fStartPt);

    targetIdx := TargetIndex(fCapturedCtrl);
    if not (ssShift in Shift) and (targetIdx < 0) then
      ClearTargets;
    if not assigned(fCapturedCtrl) then
      exit;

    //if the control isn't a target then add it ...
    if targetIdx < 0 then
    begin

      AddTarget(fCapturedCtrl);
      exit;
      //if the control's already a target but the Shift key's pressed then delete it ...
    end
    else if (ssShift in Shift) then
    begin
      DeleteTarget(fCapturedCtrl);
      fCapturedCtrl := nil;
      exit;
    end;
    fParentForm.ActiveControl := nil;
    if not IsValidMove then
      exit;
    targetObj := TTargetObj(fTargetList[targetIdx]);
    fState := scsMoving;
  end;


  for i := 0 to TargetCount - 1 do
  begin
    //TTargetObj(fTargetList[i]).fTarget.Hide;
    TTargetObj(fTargetList[i]).StartFocus();
  end;

  if assigned(fStartEvent) then
    fStartEvent(self, fState);

  //now calculate and set the clipping region in screen coords ...
  p := targetObj.fTarget.Parent;
  parentClientRec := p.ClientRect;
  parentClientRec.TopLeft := p.ClientToScreen(parentClientRec.TopLeft);
  parentClientRec.BottomRight := p.ClientToScreen(parentClientRec.BottomRight);
  if fState = scsMoving then
  begin
    fClipRec := parentClientRec;
  end
  else
    with targetObj do //ie sizing
    begin
      fClipRec := fFocusRect;
      case TSizeBtn(Sender).fPos of
        bpLeft: fClipRec.Left := parentClientRec.Left;
        bpTopLeft:
        begin
          fClipRec.Left := parentClientRec.Left;
          fClipRec.Top := parentClientRec.Top;
        end;
        bpTop: fClipRec.Top := parentClientRec.Top;
        bpTopRight:
        begin
          fClipRec.Right := parentClientRec.Right;
          fClipRec.Top := parentClientRec.Top;
        end;
        bpRight: fClipRec.Right := parentClientRec.Right;
        bpBottomRight:
        begin
          fClipRec.Right := parentClientRec.Right;
          fClipRec.Bottom := parentClientRec.Bottom;
        end;
        bpBottom: fClipRec.Bottom := parentClientRec.Bottom;
        bpBottomLeft:
        begin
          fClipRec.Left := parentClientRec.Left;
          fClipRec.Bottom := parentClientRec.Bottom;
        end;
      end;
    end;
  //ClipCursor(@fClipRec);

  Hide;
  DrawRect;
  THackedControl(fCapturedCtrl).MouseCapture := True;
end;

//------------------------------------------------------------------------------

function WinVer: double;
var
  WinV: word;
begin
  WinV := GetVersion and $0000FFFF;
  Result := StrToFloat(IntToStr(Lo(WinV)) + FormatSettings.DecimalSeparator + IntToStr(Hi(WinV)));
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.DoMouseMove(Sender: TObject; Shift: TShiftState);
var
  i, dx, dy: integer;
  newPt: TPoint;
  Q, R: integer;
begin

  if (fState = scsReady) or not assigned(fCapturedCtrl) then
    exit;
  DrawRect;

  GetCursorPos(newPt);

  dx := newPt.X - fStartPt.X;
  dy := newPt.Y - fStartPt.Y;
  Q := 0;
  R := 0;

  if (fState = scsSizing) then
  begin
    case fCapturedBtnPos of
      bpLeft, bpRight: dy := 0;
      bpTop, bpBottom: dx := 0;
    end;

    if (not KeyIsPressed(Self.FDGKey)) then
    begin
      Q := Dx mod GridSize;
      R := Dy mod GridSize;
    end;

    for i := 0 to TargetCount - 1 do
      TTargetObj(fTargetList[i]).SizeFocus(dx - q, dy - r, fCapturedBtnPos);
    if assigned(fDuringEvent) then
      fDuringEvent(self, dx - q, dy - r, fState);
  end
  else
  begin

    if (not KeyIsPressed(Self.FDGKey)) then
    begin
      Q := Dx mod GridSize;
      R := Dy mod GridSize;
    end;

    for i := 0 to TargetCount - 1 do
      TTargetObj(fTargetList[i]).MoveFocus(dx - q, dy - r);
    if assigned(fDuringEvent) then
      fDuringEvent(self, dx - q, dy - r, fState);
  end;
  //windows.SetCursor(screen.Cursors[crHandPoint]);
  DrawRect;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState);
var
  i, k: integer;
  list: TList;
  //  t: Integer;
begin
  if fState = scsReady then
    exit;
  DrawRect;
  ClipCursor(nil);
  THackedControl(fCapturedCtrl).MouseCapture := False;
  fCapturedCtrl := nil;
  if not fEscCancelled then
    for i := 0 to TargetCount - 1 do
    begin
      //TTargetObj(fTargetList[i]).fTarget.Show;
      TTargetObj(fTargetList[i]).EndFocus;
      TTargetObj(fTargetList[i]).fPanelsNames.Clear;
      list := TTargetObj(fTargetList[i]).fPanels;
      for k := 0 to list.Count - 1 do
        TObject(list[k]).Free();
      list.Clear;
    end;

  //  t := GetTickCount;

  fEscCancelled := False;
  if assigned(fEndEvent) then
    fEndEvent(self, fState);

  Show;
  fState := scsReady;

  // windows.SetCursor(screen.Cursors[crDefault]);
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.Hide;
{var
  i: integer;
}begin
  // for i := 0 to TargetCount -1 do TTargetObj(fTargetList[i]).Hide;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.Show;
{var
  i: integer;
}begin
  // for i := 0 to TargetCount -1 do TTargetObj(fTargetList[i]).Show;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.UpdateBtnCursors;
var
  i: integer;
  j: TBtnPos;
begin

  if fMultiResize or (TargetCount = 1) then
  begin
    fValidBtns := [bpLeft, bpTopLeft, bpTop, bpTopRight, bpRight,
      bpBottomRight, bpBottom, bpBottomLeft];
    for i := 0 to TargetCount - 1 do
      case TTargetObj(fTargetList[i]).fTarget.Align of
        alTop: fValidBtns := fValidBtns - [bpLeft, bpTopLeft, bpTop,
            bpTopRight, bpRight, bpBottomRight, bpBottomLeft];
        alBottom: fValidBtns :=
            fValidBtns - [bpLeft, bpTopLeft, bpTopRight, bpRight,
            bpBottomRight, bpBottom, bpBottomLeft];
        alLeft: fValidBtns := fValidBtns - [bpLeft, bpTopLeft, bpTop,
            bpTopRight, bpBottomRight, bpBottom, bpBottomLeft];
        alRight: fValidBtns :=
            fValidBtns - [bpTopLeft, bpTop, bpTopRight, bpRight,
            bpBottomRight, bpBottom, bpBottomLeft];
        alClient: fValidBtns := [];
        {$IFDEF VER3U}
        alCustom: fValidBtns := [];
        {$ENDIF}
      end;
  end
  else
    fValidBtns := [];

  for i := 0 to TargetCount - 1 do
    with TTargetObj(fTargetList[i]) do
      for j := bpLeft to LastBtn do
        fBtns[j].UpdateBtnCursorAndColor;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetMoveOnly(Value: boolean);
begin
  if fMoveOnly = Value then
    exit;
  fMoveOnly := Value;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

function TSizeCtrl.IsValidSizeBtn(BtnPos: TBtnPos): boolean;
begin
  Result := (TargetCount > 0) and (TTargetObj(fTargetList[0]).fBtns[BtnPos].Cursor <>
    crDefault);
end;

//------------------------------------------------------------------------------

function TSizeCtrl.IsValidMove: boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to TargetCount - 1 do
    if (TTargetObj(fTargetList[i]).fTarget.Align <> alNone) then
      exit;
  Result := True;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetMultiResize(Value: boolean);
begin
  if Value = fMultiResize then
    exit;
  fMultiResize := Value;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetEnabledBtnColor(aColor: TColor);
begin
  if fEnabledBtnColor = aColor then
    exit;
  fEnabledBtnColor := aColor;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetHoverBtnColor(aColor: TColor);
begin
  if fHoverBtnColor = aColor then
    exit;
  fHoverBtnColor := aColor;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetHoverBtnFrameColor(v: TColor);
begin
  if fHoverBtnFrameColor = v then
    exit;
  fHoverBtnFrameColor := v;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetDisabledBtnColor(aColor: TColor);
begin
  if fDisabledBtnColor = aColor then
    exit;
  fDisabledBtnColor := aColor;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.HardReset(sizes:boolean=false);
var i: integer;
j: TBtnPos;
begin
if TargetCount > 0 then
 for i := 0 to TargetCount - 1 do
    with TTargetObj(fTargetList[i]) do
    begin
      for j := bpLeft to LastBtn do
        fBtns[j].Reset;
      if sizes then
        Update;
    end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.UpdateGrid;
begin
  if Assigned(TWinControl(Owner)) then
  begin
     if TWinControl(Owner).Visible then
        TWinControl(Owner).Repaint;
     if not (fForm is TCustomForm) then
     begin
       if Assigned(fGridForm) then
          fGridForm.Repaint
       else
        CreateGrid;
     end;
  end;

end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.setGridSize(Value: integer);
begin
  if (fGridSize <> Value) and (1 <= Value ) and (Value <= 50)
  then
  begin
    fGridSize := Value;
    UpdateGrid;
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.setGridWhite(Value: TColor);
begin
  if fGridWhite = Value then Exit;
  fGridWhite := Value;
  UpdateGrid;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.setGridBlack(Value: TColor);
begin
  if fGridBlack = Value then Exit;
  fGridBlack := Value;
  UpdateGrid;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetBtnCount(Value: TSizeCtrlBtnCount);
var i: integer;
begin
  if fBtnCount <> Value then
  begin
    fBtnCount := Value;
    fLastBtn := TBtnPos( (integer(fBtnCount) + 1) * 4 );
    if TargetCount > 0 then
     for i := 0 to TargetCount - 1 do
      with TTargetObj(fTargetList[i]) do
        ReconfButtons;
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetBtnSize(Value: integer);
begin
  if (fBtnSize <> Value) and (3 <= Value) and (Value <= 50) then
  begin
    fBtnSize := Value;
    HardReset(true);
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetBtnAlphaBlend(Value: integer);
begin
  if (fBtnAlpha <> Value) and (0 <= Value) and (Value <= 255) then
  begin
    fBtnAlpha := Value;
    HardReset;
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.setMovePanelAlphaBlend(Value: integer);
begin
  if (fMovePanelAlpha <> Value) and (0 <= Value) and (Value <= 255) then
    fMovePanelAlpha := Value;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.setPanelImage(Value: TPicture);
begin
  if  Value = fPanelImage then
  Exit;
  fPanelImage.Assign( Value );
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetBtnImage(Value: TPicture);
begin
  if  Value = fBtnImage then
  Exit;
  fBtnImage.Assign( Value );
  HardReset;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetHoverBtnImage(Value: TPicture);
begin
  if  Value = fHoverBtnImage then
  Exit;
  fHoverBtnImage.Assign( Value );
  HardReset;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetDisabledBtnImage(Value: TPicture);
begin
  if  Value = fDisabledBtnImage then
  Exit;
  fDisabledBtnImage.Assign( Value );
  HardReset;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetStretchBtnImage(Value: boolean);
begin
  if  Value = fStretchBtnImage then
  Exit;
  fStretchBtnImage := Value;
  HardReset;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetBtnShape(v: TSizeBtnShapeType);

begin
  if fBtnShape = v then
  exit;
   fBtnShape := v;
  HardReset;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetBtnFrameColor(v: TColor);
begin
  if fBtnFrameColor = v then
  exit;
   fBtnFrameColor := v;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetDisabledBtnFrameColor(v: TColor);
begin
  if fDisabledBtnFrameColor = v then
  exit;
   fDisabledBtnFrameColor := v;
  UpdateBtnCursors;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetPopupMenu(Value: TPopupMenu);
begin
  fPopupMenu := Value;
  if Value = nil then
    exit;
  Value.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = PopupMenu) then
    PopupMenu := nil;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.DoPopupMenuStuff;
var
  Handled: boolean;
  pt: TPoint;
  targetCtrl: TControl;
begin
  if not assigned(fPopupMenu) then
    exit;
  GetCursorPos(pt);
  targetCtrl := TargetCtrlFromPt(pt);
  if not assigned(targetCtrl) then
    exit;
  Handled := False;
  if Assigned(FOnContextPopup) then
    fOnContextPopup(Self, pt, Handled);
  if Handled then
    exit;
  THackedControl(owner).SendCancelMode(nil);
  fPopupMenu.PopupComponent := targetCtrl;
  PopupMenu.Popup(Pt.X, Pt.Y);
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.formPaint(Sender: TObject);
var
  i, j, w, h: integer;
  c, g: TColor;
  obj: TWinControl;
begin
  if not Assigned(Self) then
    exit;

  if not ShowGrid then
    exit;
  if GridSize < 5 then
    exit;

  w := TControl(fForm).Width;
  h := TControl(fForm).Height;
  if fForm is TForm then
    c := TForm(fForm).Color
  else
  begin
  obj := TWinControl(Sender);
  while not (obj is TCustomForm) and Assigned(obj) do
      obj := obj.parent;
    if obj is TCustomForm then
      begin
        c := TCustomForm(obj).Color;
        TForm(Sender).Top := 0;
        TForm(Sender).Left := 0;
        TForm(Sender).Width := TWinControl(fForm).Width;
        TForm(Sender).Height := TWinControl(fForm).Height;
      end;
  end;

  if (fGrid <> nil) and (lastW = w) and (lastH = h) and (lastColor = c) then
  begin
    //  exit;
  end;

  if not Assigned(fForm) then
    exit;

  lastW := w;
  lastH := h;
  lastColor := c;
  if fForm is TForm then
  g := GetGValue(TForm(fForm).Color)
   else if Assigned(fParentForm) then
      g := GetGValue(c);

  if fGrid = nil then
    fGrid := TBitmap.Create;

  fGrid.Width := w;
  fGrid.Height := h;
  fGrid.Canvas.Brush.Color := c;
  fGrid.Canvas.Pen.Style := psClear;
  fGrid.Canvas.Rectangle(0, 0, w + 1, h + 1);
  for i := 0 to w div GridSize do
    for j := 0 to h div GridSize do
    begin
      if g < 180 then
        fGrid.Canvas.Pixels[I * GridSize, J * GridSize] := fGridWhite
      else
        fGrid.Canvas.Pixels[I * GridSize, J * GridSize] := fGridBlack;
    end;

  TForm(Sender).Canvas.Draw(0, 0, fGrid);

  //fGrid.Canvas.CopyRect(Rect(0,0,w,h), TForm(Sender).Canvas, Rect(0,0,w,h));

end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.SetShowGrid(const Value: boolean);
begin
  if FShowGrid = Value then Exit;
  FShowGrid := Value;
  UpdateGrid;
end;
{
procedure TSizeCtrl.SetgetSelected(const Value: TList);
begin
  FgetSelected := Value;
end;
}
function TSizeCtrl.getSelected: TList;
var
  i: integer;
begin
  Result := TList.Create;
  for i := 0 to fTargetList.Count - 1 do
  begin
    Result.Add(TTargetObj(fTargetList[i]).fTarget);
  end;
end;

//------------------------------------------------------------------------------

procedure TSizeCtrl.toBack(CNTR: TControl);
var
  i, x: integer;
  res: TRegisteredObj;
  newList: TList;
begin
  newList := TList.Create;
  for i := 0 to fRegList.Count - 1 do
  begin
    res := TRegisteredObj(fRegList[i]);
    if (integer(res.fControl) = integer(CNTR)) then
    begin
      x := i;
      break;
    end;
  end;

  for i := 0 to fRegList.Count - 1 do
  begin
    if i = x then
      continue;
    newList.Add(fRegList[i]);
  end;


  newList.Add(fRegList[x]);

  fRegList.Free;
  fRegList := newList;
end;


//------------------------------------------------------------------------------

procedure TSizeCtrl.toFront(CNTR: TControl);
var
  i, x: integer;
  res: TRegisteredObj;
  newList: TList;
begin
  newList := TList.Create;
  for i := fRegList.Count - 1 downto 0 do
  begin
    res := TRegisteredObj(fRegList[i]);
    if (integer(res.fControl) = integer(CNTR)) then
    begin
      x := i;
    end;
  end;

  for i := fRegList.Count - 1 downto 0 do
  begin
    if i = x then
      continue;
    newList.Add(fRegList[i]);
  end;

  newList.Add(fRegList[x]);

  fRegList.Free;
  fRegList := newList;
end;

{ TMovePanel }

constructor TMovePanel.Create(AOwner: TComponent;AsizeCtrl:TSizeCtrl);
begin
  inherited CreateNew(nil);
  Loaded;
  BorderStyle := bsNone;
  AutoSize := False;
  Visible := False;
  Position := poDesigned;
  FormStyle := fsStayOnTop;
  Visible := False;
  Parent := TWinControl(AOwner);
  fSizeCtrl := AsizeCtrl;
  OnPaint := DoPaint;
  OnResize := SetBoundsRect;
end;

//------------------------------------------------------------------------------

procedure TMovePanel.setfcanvas(fCanvas: TCanvas);
begin
  Canvas.Pen.Assign(fCanvas.Pen);
  Canvas.Brush.Assign(fCanvas.Brush);
  Color := fCanvas.Brush.Color;
end;

//------------------------------------------------------------------------------

procedure TMovePanel.SetBoundsRect;
begin
  invalidate;
end;

//------------------------------------------------------------------------------

procedure TMovePanel.DoPaint(Sender: TObject);
begin
  setfcanvas(fSizeCtrl.movePanelCanvas);
  if Canvas.Brush.Style = bsClear then
  begin
    TransparentColor := True;
    TransparentColorValue := Color;
  end
  else
  begin
    TransparentColor := False;
  end;
  AlphaBlend := fSizeCtrl.MovePanelAlphaBlend < 255;
  AlphaBlendValue := fSizeCtrl.MovePanelAlphaBlend;

  If Assigned(fSizeCtrl.MovePanelImage.Graphic)
  and not(fSizeCtrl.MovePanelImage.Graphic.Empty) then
  begin
     if fSizeCtrl.StretchMovePanelImage then
      Canvas.StretchDraw(Rect(0,0,Width,Height), fSizeCtrl.MovePanelImage.Graphic)
     Else
      Canvas.Draw(0,0,fSizeCtrl.MovePanelImage.Graphic);
  End
  Else
    Canvas.Rectangle(0, 0, Width, Height);
end;

end.